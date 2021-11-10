
# Outcomes hospitalizations ----------------------------------------------

svpatreg <- patreg %>%
  filter(sos_source == "sv")

svpatreg <- left_join(pdata %>%
  select(LopNr, shf_indexdtm, casecontrol, shf_ef_cat, ef_casecontrol, sos_outtime_death, sos_out_death, censdtm),
svpatreg,
by = "LopNr"
) %>%
  mutate(sos_outtime = difftime(INDATUM, shf_indexdtm, units = "days")) %>%
  filter(sos_outtime > 0 & INDATUM < censdtm)

ovpatreg <- patreg %>%
  filter(sos_source == "ov")

ovpatreg <- left_join(pdata %>%
  select(LopNr, shf_indexdtm, casecontrol, shf_ef_cat, ef_casecontrol, sos_outtime_death, sos_out_death, censdtm),
ovpatreg,
by = "LopNr"
) %>%
  mutate(sos_outtime = difftime(INDATUM, shf_indexdtm, units = "days")) %>%
  filter(sos_outtime > 0 & INDATUM < censdtm)

create_recevents <- function(diakod, diakodneg = FALSE, sosdata = svpatreg) {
  tmpsos <- sosdata %>%
    mutate(sos_out = stringr::str_detect(HDIA, diakod, negate = diakodneg)) %>%
    filter(sos_out) %>%
    select(LopNr, shf_indexdtm, casecontrol, shf_ef_cat, ef_casecontrol, sos_outtime_death, sos_out_death, censdtm, sos_outtime, sos_out)

  dataout <- bind_rows(
    pdata %>%
      select(LopNr, shf_indexdtm, casecontrol, shf_ef_cat, ef_casecontrol, sos_outtime_death, sos_out_death, censdtm),
    tmpsos
  ) %>%
    mutate(
      sos_out = if_else(is.na(sos_out), 0, 1),
      sos_outtime = if_else(is.na(sos_outtime), difftime(censdtm, shf_indexdtm, units = "days"), sos_outtime)
    )

  dataout <- dataout %>%
    group_by(LopNr, shf_indexdtm, sos_outtime) %>%
    arrange(desc(sos_out)) %>%
    slice(1) %>%
    ungroup()

  dataout <- dataout %>%
    mutate(LopNcasecontrol = paste0(LopNr, casecontrol)) %>%
    select(-LopNr)
}


pdata_rec_hospany <- create_recevents(
  diakod = " "
)
pdata_rec_hosphf <- create_recevents(
  diakod = " I110| I130| I132| I255| I420| I423| I425| I426| I427| I428| I429| I43| I50| J81| K761| R57"
)
pdata_rec_hospcv <- create_recevents(
  diakod = " I| J81| K761| G45| R57"
)
pdata_rec_hospnoncv <- create_recevents(
  diakod = " I| J81| K761| G45| R57",
  diakodneg = TRUE
)
pdata_rec_visany <- create_recevents(
  sosdata = ovpatreg,
  diakod = " "
)
