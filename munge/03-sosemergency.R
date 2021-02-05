
# Emergency visits --------------------------------------------------------

em <- bind_rows(
  sv %>% mutate(sos_source = "sv"),
  ovem %>% mutate(sos_source = "ov")
)

em <- left_join(
  pdata %>%
    select(LopNr, shf_indexdtm, censdtm),
  em %>%
    filter(MVO %in% c("046", "100") & PVARD == 2),
  by = "LopNr"
)

em <- em %>%
  filter(
    !is.na(INDATUM),
    INDATUM > shf_indexdtm,
    INDATUM <= censdtm
  ) %>%
  mutate(
    UTDATUM = pmax(UTDATUM, INDATUM),
    UTDATUM = coalesce(UTDATUM, INDATUM),
    sos_out_em = 1,
  ) %>%
  select(LopNr, INDATUM, UTDATUM, shf_indexdtm, sos_out_em, censdtm, sos_source)

# sv NOT emergency visit
svforem <- left_join(
  pdata %>%
    select(LopNr, shf_indexdtm, censdtm),
  sv %>%
    filter(!(MVO %in% c("046", "100") & PVARD == 2)),
  by = "LopNr"
) %>%
  filter(
    !is.na(INDATUM),
    !is.na(UTDATUM),
    UTDATUM > shf_indexdtm,
    INDATUM <= censdtm
  ) %>%
  mutate(
    UTDATUM = pmin(UTDATUM, censdtm)
  ) %>%
  select(LopNr, INDATUM, UTDATUM, shf_indexdtm, censdtm)

emsv <- left_join(
  em,
  svforem,
  by = "LopNr",
  suffix = c("_em", "_sv")
) %>%
  filter(INDATUM_sv == INDATUM_em) %>%
  group_by(LopNr, INDATUM_sv) %>%
  slice(1) %>%
  ungroup() %>%
  select(LopNr, INDATUM_em) %>%
  mutate(sos_out_emhosp = 1)

em <- left_join(
  em,
  emsv,
  by = c("LopNr", "INDATUM" = "INDATUM_em")
) %>%
  mutate(
    sos_out_emhosp = if_else(!is.na(sos_out_emhosp) | sos_source == "sv", 1, 0),
    sos_out_emnohosp = if_else(sos_out_emhosp == 1, 0, 1)
  )

em <- em %>%
  group_by(LopNr, shf_indexdtm) %>%
  summarise(
    sos_out_noem = sum(sos_out_em),
    sos_out_noemhosp = sum(sos_out_emhosp),
    sos_out_noemnohosp = sum(sos_out_emnohosp),
    .groups = "drop"
  )

pdata <- left_join(
  pdata,
  em,
  by = c("LopNr", "shf_indexdtm")
) %>%
  mutate(
    sos_out_noem = replace_na(sos_out_noem, 0),
    sos_out_noemhosp = replace_na(sos_out_noemhosp, 0),
    sos_out_noemnohosp = replace_na(sos_out_noemnohosp, 0)
  )
