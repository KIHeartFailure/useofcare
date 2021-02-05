
# Length of stay ----------------------------------------------------------

svlos <- left_join(
  pdata %>%
    select(LopNr, shf_indexdtm, censdtm),
  sv,
  by = "LopNr"
) %>%
  filter(
    !is.na(INDATUM),
    !is.na(UTDATUM),
    UTDATUM > shf_indexdtm, # stay in hospital counted even if belongs to "indexvisit"
    INDATUM <= censdtm
  ) %>%
  mutate(
    HDIA = paste0(" ", HDIA),
    INDATUM = pmax(INDATUM, shf_indexdtm + 1), # hosp time at or before index not considered
    UTDATUM = pmax(UTDATUM, INDATUM),
    UTDATUM = pmin(UTDATUM, censdtm) # hosp time after cens not considered
  ) %>%
  select(LopNr, INDATUM, UTDATUM, HDIA, shf_indexdtm, censdtm)

losfunc <- function(diakod, negate = FALSE, varname) {
  losname <- paste0("sos_out_loshosp", varname)
  losdata <- svlos %>%
    mutate(
      dia = stringr::str_detect(HDIA, diakod, negate = negate),
      los = as.numeric(UTDATUM - INDATUM)
    ) %>%
    filter(dia) %>%
    group_by(LopNr, shf_indexdtm) %>%
    summarise(!!losname := sum(los), .groups = "drop")

  pdata <<- left_join(
    pdata,
    losdata,
    by = c("LopNr", "shf_indexdtm")
  ) %>%
    mutate(!!losname := replace_na(!!sym(losname), 0))

  metatmp <- c(losname, paste0("ICD:", ifelse(negate, " Not ", ""), stringr::str_replace_all(diakod, "\\|", ",")))
  if (exists("metalos")) {
    metalos <<- rbind(metalos, metatmp) # global variable, writes to global env
  } else {
    metalos <<- metatmp # global variable, writes to global env
  }
}

losfunc(diakod = " ", varname = "any")

losfunc(varname = "cv", diakod = " I| J81| K761| G45| R57")

losfunc(varname = "noncv", diakod = " I| J81| K761| G45| R57", negate = TRUE)

colnames(metalos) <- c("Variable", "Code")
metalos <- metalos %>%
  as_tibble() %>%
  mutate(
    Code = gsub("[", "", Code, fixed = TRUE),
    Code = gsub("]", "", Code, fixed = TRUE),
    Code = gsub("(", "", Code, fixed = TRUE),
    Code = gsub(")", "", Code, fixed = TRUE),
    Code = gsub("?!", " excl.", Code, fixed = TRUE),
    Registry = "NPR (in)",
    Position = "HDIA",
    Period = "1-",
  )
