
# Additional variables from NPR -------------------------------------------

pdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = pdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "obesity",
  diakod = " E66",
  valsclass = "fac",
  warnings = FALSE,
  stoptime = -5 * 365.25
)

pdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = pdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  type = "com",
  name = "anemia",
  diakod = " D5| D6[0-4]",
  valsclass = "fac",
  warnings = FALSE,
  stoptime = -5 * 365.25
)

pdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = pdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  opvar = OP_all,
  type = "com",
  name = "pci",
  opkod = " FNG",
  valsclass = "fac",
  warnings = FALSE,
  stoptime = -5 * 365.25
)

pdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = pdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  opvar = OP_all,
  type = "com",
  name = "cabg",
  diakod = " Z951| Z955",
  opkod = " FN[A-F]| FNH",
  valsclass = "fac",
  warnings = FALSE,
  stoptime = -5 * 365.25
)

pdata <- create_sosvar(
  sosdata = patreg,
  cohortdata = pdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  opvar = OP_all,
  type = "com",
  name = "icdcrt",
  opkod = " FPG| FPE26",
  valsclass = "fac",
  warnings = FALSE,
  stoptime = -5 * 365.25
)

pdata <- create_sosvar(
  sosdata = patreg %>% filter(sos_source == "sv"),
  cohortdata = pdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = HDIA,
  noof = TRUE,
  type = "out",
  name = "nohospany",
  diakod = " ",
  censdate = censdtm,
  valsclass = "num",
  warnings = FALSE, 
  meta_reg = "NPR (in)"
)

pdata <- create_sosvar(
  sosdata = patreg %>% filter(sos_source == "sv"),
  cohortdata = pdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = HDIA,
  noof = TRUE,
  type = "out",
  name = "nohosphf",
  diakod = " I110| I130| I132| I255| I420| I423| I425| I426| I427| I428| I429| I43| I50| J81| K761| R57",
  censdate = censdtm,
  valsclass = "num",
  warnings = FALSE, 
  meta_reg = "NPR (in)"
)

pdata <- create_sosvar(
  sosdata = patreg %>% filter(sos_source == "sv"),
  cohortdata = pdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = HDIA,
  noof = TRUE,
  type = "out",
  name = "nohospcv",
  diakod = " I| J81| K761| G45| R57",
  censdate = censdtm,
  valsclass = "num",
  warnings = FALSE, 
  meta_reg = "NPR (in)"
)

pdata <- create_sosvar(
  sosdata = patreg %>% filter(sos_source == "sv"),
  cohortdata = pdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = HDIA,
  noof = TRUE,
  type = "out",
  name = "nohospnoncv",
  diakod = " I| J81| K761| G45| R57",
  diakodneg = TRUE,
  censdate = censdtm,
  valsclass = "num",
  warnings = FALSE, 
  meta_reg = "NPR (in)"
)

pdata <- create_sosvar(
  sosdata = patreg %>% filter(sos_source == "ov"),
  cohortdata = pdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = HDIA,
  type = "out",
  name = "visany",
  diakod = " ",
  censdate = censdtm,
  valsclass = "fac",
  warnings = FALSE, 
  meta_reg = "NPR (out)"
)

pdata <- create_sosvar(
  sosdata = patreg %>% filter(sos_source == "ov"),
  cohortdata = pdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = HDIA,
  noof = TRUE,
  type = "out",
  name = "novisany",
  diakod = " ",
  censdate = censdtm,
  valsclass = "num",
  warnings = FALSE, 
  meta_reg = "NPR (out)"
)

metaout <- metaout %>%
  mutate(Period = if_else(Period == "0--1826.25", "-5yrs-0", Period))