
# Treatments from DDR --------------------------------------------

lmtmp <- left_join(
  pdata %>%
    select(LopNr, shf_indexdtm),
  lm,
  by = "LopNr"
) %>%
  mutate(diff = as.numeric(EDATUM - shf_indexdtm)) %>%
  filter(diff >= -30.5 * 5, diff <= 14) %>%
  select(LopNr, shf_indexdtm, EDATUM, ATC)

lmtreats <- function(atc, treatname) {
  lmtmp2 <- lmtmp %>%
    mutate(
      atcneed = stringr::str_detect(ATC, atc)
    ) %>%
    filter(atcneed)

  treatname <- paste0("ddr_", treatname)

  lmtmp2 <- lmtmp2 %>%
    group_by(LopNr, shf_indexdtm) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(!!treatname := "Yes") %>%
    select(LopNr, shf_indexdtm, !!sym(treatname))

  pdata <<- left_join(
    pdata,
    lmtmp2,
    by = c("LopNr", "shf_indexdtm")
  ) %>%
    mutate(
      !!treatname := replace_na(!!sym(treatname), "No"),
      !!treatname := if_else(shf_indexdtm < ymd("2005-12-01"), NA_character_, !!sym(treatname)) # redundant, excluded obs
    )

  metatmp <- c(treatname, stringr::str_replace_all(atc, "\\|", ","))
  if (exists("metalm")) {
    metalm <<- rbind(metalm, metatmp) # global variable, writes to global env
  } else {
    metalm <<- metatmp # global variable, writes to global env
  }
}


lmtreats(atc = "^(A10BK0[1-6]|A10BD15|A10BD16|A10BD19|A10BD20|A10BD21|A10BD23|A10BD24|A10BD25|A10BX09|A10BX11|A10BX12)", treatname = "sglt2")

lmtreats(atc = "^A10A", treatname = "insulin")

lmtreats("^A10B", "oralantidiabetic")

lmtreats("^(A10BA|A10BD02|A10BD03|A10BD05|A10BD07|A10BD08|A10BD10|A10BD11|A10BD13|A10BD15|A10BD16|A10BD20|A10BD23|A10BD25)", "metformin")

lmtreats("^(A10BB|A10BC|A10BD02|A10BD04|A10BD06)", "sulfonylureas")

lmtreats("^A10BF", "agi")

lmtreats("^(A10BG|A10BD03|A10BD04|A10BD05|A10BD06|A10BD09)", "thiazolidinediones")

lmtreats("^(A10BJ|A10BX04|A10BX07|A10BX10|A10BX13|A10BX14)", "glp1a")

lmtreats("^(A10BH|A10BD07|A10BD08|A10BD09|A10BD10|A10BD11|A10BD13|A10BD19|A10BD21|A10BD24|A10BD25)", "dpp4i")

lmtreats("^(C09A|C09B)", "acei")

lmtreats("^(C09C|C09D(?!X04))", "arb")

lmtreats("^(C03(?!DA)|C07B|C07C|C07D|C08GA|C09BA|C09DA|C09DX01)", "diuretics")

lmtreats("^C07", "bbl")

lmtreats("^(C08|C07FB|C09BB|C09DB|C09DX01)", "ccb")

lmtreats("^C03DA", "mra")

lmtreats("^C09DX04", "arni")

lmtreats("^B01AC", "antiplatlet")

lmtreats("^B01A(?!C)", "anticoagulant")

lmtreats("^C10", "lipidlowering")

lmtreats("^C01AA05", "digoxin")

lmtreats("^C01DA", "nitrate")

lmtreats("^C01B", "antiarrhythmic")


pdata <- pdata %>%
  mutate_if(is_character, factor)

colnames(metalm) <- c("Variable", "ATC")
metalm <- metalm %>%
  as_tibble() %>%
  mutate(
    ATC = gsub("^", "", ATC, fixed = TRUE),
    ATC = gsub("(", "", ATC, fixed = TRUE),
    ATC = gsub(")", "", ATC, fixed = TRUE),
    ATC = gsub("?!", " excl.", ATC, fixed = TRUE),
    Registry = "Dispensed Drug Registry",
    Period = "-5mo-14days",
  )
