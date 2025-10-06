#import libraries 
library(tidyverse)
library(janitor)
library(odbc)
library(DBI)

cat(sprintf("Script started: %s\n", now()), 
    file = "script_log.txt", 
    append = TRUE)

################################### Detecting new files #################################################
# create the connection for your session
db <- DBI::dbConnect(odbc::odbc(), "coch_p2")

# read from the ambulances table and find all the dates currently in the ambulances table
existing_dates <-  DBI::dbGetQuery(db, "select * from InformationSandpitDB.Reports.NWAS_Imports") %>%
  distinct(date_at_hospital)

# List files in folder and put dates into a table
input_folder <- "Input\\"

file_names <- list.files(path = input_folder) |> 
  as_tibble() |>
  mutate(date = ymd(str_remove(value, ".csv"))) |>
  filter(!date %in% existing_dates$date_at_hospital)

files_to_read <- nrow(file_names)

print(paste(files_to_read, "new files detected"))

if (files_to_read == 0) {
  cat(sprintf("Script aborted due to no new files detected: %s\n", now()), 
      file = "script_log.txt", 
      append = TRUE)
  stop("No new files detected")
}

############################### Read in new files ########################################

ambulances_to_import <- tibble() # makes an empty data frame

header_row <- "HOSPITAL_NAME CLINIC_ATTENDED NWAS_CALL_NUMBER"

for (i in 1:files_to_read) {
  ###### Dynamically detect how many rows to skip ############
  filename <- file_names$value[i]
  temp0 <- read_csv(paste0(input_folder,filename), col_names = F, n_max = 100) |>
    mutate(strc = str_c(X1,X2,X3, sep = " ")) |>
    select(strc, everything())
  
  rows_to_skip <- (which(temp0 %>% pull(strc) == header_row)) - 1
  
  ###### Read in the file
  temp_df <- read_csv(paste0(input_folder,filename), skip = rows_to_skip) |>
    clean_names()
  
  ##### Union it with any other files
  ambulances_to_import <- bind_rows(ambulances_to_import, temp_df)
}

ncol <- ncol(ambulances_to_import)

if (ncol != 34) {
  cat(sprintf("Script aborted due to wrong number of cols in import data (!= 34): %s\n", now()), 
      file = "script_log.txt", 
      append = TRUE)
  stop("wrong number of cols")
}

############## Wrangle the ambulance data ######################################

ambulances_clean <- ambulances_to_import %>% 
  filter(hospital_name == 'Countess of Chester') |>
  mutate(date_at_hospital = dmy(date_at_hospital),
         date_time_at_hospital = as.POSIXct(date_at_hospital) + time_at_hospital,
         date_time_hospital_notified = if_else(time_hospital_notified >= time_at_hospital,
                                               as.POSIXct(date_at_hospital) + time_hospital_notified,
                                               as.POSIXct(date_at_hospital + days(1)) + time_hospital_notified,
                                               missing = NA),
         date_time_of_handover = if_else(time_of_handover >= time_at_hospital,
                                               as.POSIXct(date_at_hospital) + time_of_handover,
                                               as.POSIXct(date_at_hospital + days(1)) + time_of_handover,
                                               missing = NA),
         date_time_vehicle_clear = if_else(time_vehicle_clear >= time_at_hospital,
                                         as.POSIXct(date_at_hospital) + time_vehicle_clear,
                                         as.POSIXct(date_at_hospital + days(1)) + time_vehicle_clear,
                                         missing = NA),
         vehicle_callsign = str_to_upper(vehicle_callsign),
         primary_key = paste(nwas_call_number,"-",vehicle_callsign)) |>
  select(primary_key, everything())

total_rows <- nrow(ambulances_clean)
distinct_pk <- ambulances_clean |> distinct(primary_key) |> nrow()

if (total_rows != distinct_pk) {
  cat(sprintf("Script aborted due to multiple ambulances in nwas data with same pk: %s\n", now()), 
      file = "script_log.txt", 
      append = TRUE)
  stop("multiple ambulances in nwas data with same pk")
}

############## Import and Wrangle the ED data ##############################################

min_date <- min(file_names$date) - days(1)
max_date <- max(file_names$date) + days(2)

# construct select statement
ecds_sql <- str_glue("SELECT 
    	[ENCNTR_ID]
    	,[LocalPatientIdentifier]
    	,[CheckInDateTime]
    	,[CheckOutDateTime]
    	,[Arrival_Mode_Mnemonic]
    	,[Ambulance_Call_Identifier]
    	,[Conveying_Ambulance_Trust]
    	,[Conveying_Ambulance_Trust_Code]
    FROM
    	[CernerStaging].[BI].[ECDS_Attendances]
    WHERE
    	[CheckInDateTime] between '{min_date}' and '{max_date}'")

# run the sql query
ECDS <- DBI::dbGetQuery(db, ecds_sql) |> 
  clean_names()

DBI::dbDisconnect(db)

# wrangle
ecds_clean <- ECDS %>%
  filter(arrival_mode_mnemonic == "Ambulance" |
           !is.na(ambulance_call_identifier) |
           !is.na(conveying_ambulance_trust) |
           !is.na(conveying_ambulance_trust_code)) |>
  mutate(cleaned_call_id = str_extract(ambulance_call_identifier, '\\d{8}'), #any run of 8 digits
         cleaned_call_id = as.numeric(cleaned_call_id),
         ambulance_call_identifier = str_to_upper(ambulance_call_identifier),
         cleaned_vehicle_id = str_extract(ambulance_call_identifier, '[A-Z]+\\d{2,3}'), #any uppercase characters followed by 2 or 3 digits
         ambulance_primary_key = paste(cleaned_call_id,"-",cleaned_vehicle_id))

# flags duplicated primary keys - means they will not be joined to nwas data by prim key
dupe_pk <- ecds_clean |> 
  filter(!str_detect(ambulance_primary_key, "NA")) |> 
  count(ambulance_primary_key, sort = T) |>
  filter(n > 1)

ecds_clean <- ecds_clean |>
  group_by(ambulance_primary_key) |>
  mutate(rn = row_number()) |>
  ungroup() |>
  mutate(ambulance_primary_key = if_else(ambulance_primary_key %in% dupe_pk$ambulance_primary_key,
                                         paste(ambulance_primary_key,"-","dupe",rn),
                                         ambulance_primary_key)) |>
  select(-rn)

# check for if there are any duplicate amb primary keys
total_rows <- ecds_clean |> filter(!str_detect(ambulance_primary_key, "NA")) |> nrow()
distinct_pk <- ecds_clean |> filter(!str_detect(ambulance_primary_key, "NA")) |> distinct(ambulance_primary_key) |> nrow()

if (total_rows != distinct_pk) {
  cat(sprintf("Script aborted due to multiple ambulances in ecds data with same pk: %s\n", now()), 
      file = "script_log.txt", 
      append = TRUE)
  stop("multiple ambulances in ecds data with same pk")
}

################## Assigning an encounter id to each ambulance #################

# both call id and vehicle call sign match (call ids are not unique)
joined_nwas <- ambulances_clean %>% 
  left_join(ecds_clean, by = c('primary_key' = 'ambulance_primary_key')) %>%
  filter(abs(int_length(interval(check_in_date_time, date_time_at_hospital))) <= 86400) %>% # within 24 hrs
  mutate(match_type = 'primary_key') %>%
  filter(!is.na(encntr_id)) %>%
  select(1:40, match_type)

# vehicle id matches and check in within 30 mins of ambulance arriving at hospital
joined2 <- ambulances_clean %>%
  filter(!primary_key %in% joined_nwas$primary_key)%>%
  left_join(ecds_clean, by = c('vehicle_callsign' = 'cleaned_vehicle_id')) %>%
  filter(abs(int_length(interval(check_in_date_time, date_time_at_hospital))) <= 1800) %>%
  mutate(match_type = 'vehicle_id/arrival_time') %>%
  select(1:40, match_type)

# the remaining ambulances
unjoined <- ambulances_clean %>%
  filter(!primary_key %in% joined_nwas$primary_key,
         !primary_key %in% joined2$primary_key) %>%
  mutate(encntr_id = NA,
         match_type = 'unmatched')

# union those 3 tables together
unioned_df <- bind_rows(joined_nwas, joined2, unjoined) %>%
  mutate(
        date_of_call = dmy(date_of_call),
         extract_from_date = dmy_hm(extract_from_date),
         extract_to_date = dmy_hm(extract_to_date),
         extract_run_date = dmy_hm(extract_run_date),
        import_datetime = now()
  )

if (nrow(ambulances_clean) != nrow(unioned_df)) {
  cat(sprintf("Script aborted due to ambulances imported != ambulances for export: %s\n", now()), 
      file = "script_log.txt", 
      append = TRUE)
  stop("ambulances imported != ambulances for export")
}

### run check for duplicated encntr_ids / duplicated primary keys - write to log file, abort upload
total_rows <- nrow(unioned_df)
distinct_encntrids <- unioned_df %>% distinct(encntr_id) %>% nrow()
total_unjoined <- unjoined %>% nrow()
distinct_pk <- unioned_df %>% distinct(primary_key) %>% nrow()

if ((distinct_encntrids + total_unjoined) < total_rows) {
  cat(sprintf("Script aborted due to the same encounter mapping to multiple ambulances: %s\n", now()), 
      file = "script_log.txt", 
      append = TRUE)
  stop("Encounter mapping to multiple ambulances")
}

if (distinct_pk < total_rows) {
  cat(sprintf("Script aborted due to multiple ambulances having the same primary key (final dataset): %s\n", now()), 
      file = "script_log.txt", 
      append = TRUE)
  stop("Multiple ambulances in final dataset with same primary key")
}

############### Write data to the database #####################################
#stop("Temp stop") - useful for debugging

# Connect to the database
con <- dbConnect(odbc::odbc(), 
                 DSN = "coch_p2",
                 Database = "InformationSandpitDB")

# 1. Write predictions to a staging table
dbWriteTable(con, 
             name = Id(schema = "Reports", table = "NWAS_Imports_stage"),
             value = unioned_df,
             overwrite = TRUE)

# 2. Run the same merge SQL to upsert into the main table
merge_sql <- "
MERGE Reports.NWAS_Imports AS target
USING Reports.NWAS_Imports_stage AS source
ON target.primary_key = source.primary_key
WHEN MATCHED THEN
    UPDATE SET 
        hospital_name = source.hospital_name,
        clinic_attended = source.clinic_attended,
        nwas_call_number = source.nwas_call_number,
        date_of_call = source.date_of_call,
        date_at_hospital = source.date_at_hospital,
        postcode_sector = source.postcode_sector,
        call_category_gov_standard = source.call_category_gov_standard,
        nature_of_call = source.nature_of_call,
        vehicle_callsign = source.vehicle_callsign,
        hospital_nwas_area = source.hospital_nwas_area,
        time_at_hospital = source.time_at_hospital,
        time_hospital_notified = source.time_hospital_notified,
        time_of_handover = source.time_of_handover,
        time_vehicle_clear = source.time_vehicle_clear,
        overall_turnaround_tim_emins = source.overall_turnaround_tim_emins,
        haspenaltycandidate = source.haspenaltycandidate,
        nwas_time_stamp_present = source.nwas_time_stamp_present,
        acute_time_stamp_present = source.acute_time_stamp_present,
        rapid_handover_count = source.rapid_handover_count,
        arr_vto_ntf_ymins = source.arr_vto_ntf_ymins,
        ntf_yto_hov_rmins = source.ntf_yto_hov_rmins,
        hov_rto_cl_rmins = source.hov_rto_cl_rmins,
        nwas_breach = source.nwas_breach,
        acute_breach = source.acute_breach,
        a2h_0m_15m = source.a2h_0m_15m,
        a2h_15m_30m = source.a2h_15m_30m,
        a2h_30m_60m = source.a2h_30m_60m,
        a2h_60m_120m = source.a2h_60m_120m,
        a2h_over120m = source.a2h_over120m,
        a2h_over45m1 = source.a2h_over45m1,
        extract_from_date = source.extract_from_date,
        extract_to_date = source.extract_to_date,
        extract_run_date = source.extract_run_date,
        late_turnaround_reason = source.late_turnaround_reason,
        date_time_at_hospital = source.date_time_at_hospital,
        date_time_hospital_notified = source.date_time_hospital_notified,
        date_time_of_handover = source.date_time_of_handover,
        date_time_vehicle_clear = source.date_time_vehicle_clear,
        encntr_id = source.encntr_id,
        match_type = source.match_type,
        import_datetime = source.import_datetime,
        primary_key = source.primary_key
WHEN NOT MATCHED THEN
    INSERT (
        hospital_name, clinic_attended, nwas_call_number, date_of_call, 
        date_at_hospital, postcode_sector, call_category_gov_standard, 
        nature_of_call, vehicle_callsign, hospital_nwas_area, 
        time_at_hospital, time_hospital_notified, time_of_handover, 
        time_vehicle_clear, overall_turnaround_tim_emins, haspenaltycandidate, 
        nwas_time_stamp_present, acute_time_stamp_present, rapid_handover_count, 
        arr_vto_ntf_ymins, ntf_yto_hov_rmins, hov_rto_cl_rmins, nwas_breach, 
        acute_breach, a2h_0m_15m, a2h_15m_30m, a2h_30m_60m, a2h_60m_120m, 
        a2h_over120m, a2h_over45m1, extract_from_date, extract_to_date, 
        extract_run_date, late_turnaround_reason, date_time_at_hospital,
        date_time_hospital_notified,
        date_time_of_handover,
        date_time_vehicle_clear,
        encntr_id, match_type, import_datetime, primary_key
    )
    VALUES (
        source.hospital_name, source.clinic_attended, source.nwas_call_number, 
        source.date_of_call, source.date_at_hospital, source.postcode_sector, 
        source.call_category_gov_standard, source.nature_of_call, source.vehicle_callsign, 
        source.hospital_nwas_area, source.time_at_hospital, source.time_hospital_notified, 
        source.time_of_handover, source.time_vehicle_clear, source.overall_turnaround_tim_emins, 
        source.haspenaltycandidate, source.nwas_time_stamp_present, source.acute_time_stamp_present, 
        source.rapid_handover_count, source.arr_vto_ntf_ymins, source.ntf_yto_hov_rmins, 
        source.hov_rto_cl_rmins, source.nwas_breach, source.acute_breach, source.a2h_0m_15m, 
        source.a2h_15m_30m, source.a2h_30m_60m, source.a2h_60m_120m, source.a2h_over120m, 
        source.a2h_over45m1, source.extract_from_date, source.extract_to_date, 
        source.extract_run_date, source.late_turnaround_reason, source.date_time_at_hospital,
        source.date_time_hospital_notified,
        source.date_time_of_handover,
        source.date_time_vehicle_clear,
        source.encntr_id, source.match_type, source.import_datetime, source.primary_key
    );
"

# Execute the merge SQL
dbExecute(con, merge_sql)

# Close the connection when finished
dbDisconnect(con)

# Open file for appending and write the line
cat(sprintf("Script ran successfully at: %s\n", now()), 
    file = "script_log.txt", 
    append = TRUE)
