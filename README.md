# NWAS Data Import

### Status
This project is deployed and being maintained by @helenajr

### About the project
This is an engineering project to map NWAS ambulance data to their matching attendances in our ED data. This enables waits in ambulances to be more thoroughly reported on and investigated.

NWAS data is sourced via their access controlled (commissioning portal)[https://nwasnhstrust.sharepoint.com/sites/NWASCommissionerPortal/SitePages/Home].

Note: No data are shared in this repository.

### Project Structure

* nwas_import.R - This script imports and processes ambulance data from CSV files, checks for duplicates, and writes the cleaned data to a database.
* create_task_nwas.ps - This script sets up the task for running the script

### How to use
* Clone the repo to your desktop
* Check you have all the required packages installed
* Point the R script to the correct folder for data input
If you want to schedule the script to run automatically:
* Edit the powershell script for your own user name
* Run the powershell script to set up the recurring task

### Built with
Requirements file under construction.

### Contributing
Contributions and identification of issues are welcomed. For issues please raise a GitHub issue on this repo, or for general comment use discussions.
If you have been able to repurpose the code for your own organisation or found the code in this repo helpful then please let us know in discussions.

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/Feature`)
3. Commit your Changes (`git commit -m 'Add some Feature'`)
4. Push to the Branch (`git push origin feature/Feature`)
5. Open a Pull Request

### License
Unless stated otherwise, the codebase is released under the MIT Licence. This covers both the codebase and any sample code in the documentation.

See LICENSE for more information.
