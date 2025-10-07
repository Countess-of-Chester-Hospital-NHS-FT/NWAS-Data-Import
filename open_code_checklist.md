# Open Code Checklist
This checklist is taken from the [NHSE Repo Template](https://github.com/nhsengland/nhse-repository-template/blob/main/OPEN_CODE_CHECKLIST.md)
Please use this checklist to document adherence to best practice for published projects.

## When publishing your code you need to make sure:
  
### you’re clear about who owns the code and how others can use it

- [x] Does your code have an appropriate licence?
- [x] Is there a README and does it document intended purpose?
- [x] Is the README clear and concise?
- [x] Is this project software as a medical device (see MHRA guidance)? **No**
- [x] Who has responsibility for ongoing support, communications, resolution of issues for the code? @helenajr
- [x] Has a responsible disclosure process for security issues been defined? **GitHub Issues**
- [ ] Has semantic versioning been used? (**Optional**)
- [ ] Are package dependencies and libaries documented with versions? (**Optional**)

### You do not release information that should remain closed

- [x] The code / commit messages / issues doesn't include any sensitive, personal, secret or top secret data/information?
- [x] The code / commit messages / issues  doesn't include any unreleased policy? 
- [x] The code / commit messages / issues  doesn't include business sensitive algorithms (e.g. finance allocations)? 
- [x] Associated data transfers are conducted safely and securely? **Data only stored locally**
- [x] There are no credentials (passwords/tokens) contained in the source code / commit messages / issues ? (check in both current version and git history)
- [x] There are no SQL server addresses or connection strings in the source code / commit messages / issues ? **Servers referenced by pseudonymns**
- [x] The git history doesn't contain any sensitive information (e.g. at one time real data or credentials were in the code but have since been removed) 
- [x] Have notebook outputs been removed/checked for sensitive information? **No notebooks in this repo** 
- [ ] Is configuration written as code and separated from analytical code? (**Optional**) 
- [x] No sensitive information in any screenshots or figures in your outputs and documentation?

### Cyber security

- [x] Packages used have been assessed for cyber security risk? 

### An internal code review has been completed

- [x] Has a colleague/copilot reviewed the code for sensitive data content and security vulnerabilities? **Copilot review**
