

# Inclusion/exclusion criteria --------------------------------------------------------

pdata <- rsdata322 %>%
  filter(casecontrol == "Case")
flow <- c("Number of posts (cases) in SHFDB3", nrow(pdata))

pdata <- pdata %>%
  filter(!is.na(shf_ef))
flow <- rbind(flow, c("No missing EF", nrow(pdata)))

pdata <- pdata %>%
  filter(shf_indexdtm >= ymd("2005-12-01"))
flow <- rbind(flow, c("Indexdate >= 1 Dec 2005 (start DDR + 5 months)", nrow(pdata)))

pdata <- pdata %>%
  group_by(LopNr) %>%
  arrange(shf_indexdtm) %>%
  slice(1) %>%
  ungroup()
flow <- rbind(flow, c("First post/patient", nrow(pdata)))

pdata <- pdata %>%
  filter(ncontrols >= 1)
flow <- rbind(flow, c(">= 1 control", nrow(pdata)))

colnames(flow) <- c("Criteria", "N")


# Add controls ------------------------------------------------------------

pdatacontrols <- inner_join(
  pdata %>%
    select(LopNrcase, shf_ef, shf_indexdtm),
  rsdata322 %>%
    filter(casecontrol == "Control") %>%
    select(-shf_ef),
  by = c("LopNrcase", "shf_indexdtm")
)

pdata <- bind_rows(
  pdata,
  pdatacontrols
)

ncontrols <- pdata %>%
  filter(casecontrol == "Case") %>%
  count(ncontrols)
names(ncontrols) <- c("No controls", "No cases")
