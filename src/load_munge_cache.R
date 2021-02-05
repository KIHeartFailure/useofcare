# 1. Set options in config/global.dcf
# 2. Load packages listed in config/global.dcf
# 3. Import functions and code in lib directory
# 4. Load data in data directory
# 5. Run data manipulations in munge directory

memory.limit(size = 17000000000000)

ProjectTemplate::reload.project(
  reset = TRUE,
  data_ignore = "",
  munging = TRUE
)

ProjectTemplate::cache("tabvars")

ProjectTemplate::cache("flow")
ProjectTemplate::cache("ncontrols")

ProjectTemplate::cache("metaout")
ProjectTemplate::cache("metalm")
ProjectTemplate::cache("metalos")

ProjectTemplate::cache("pdata")
