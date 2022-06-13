

# Additional variables from mainly SHF ------------------------------------

pdata <- pdata %>%
  mutate(
    shf_primaryetiology_cat = factor(case_when(
      is.na(shf_primaryetiology) ~ NA_real_,
      shf_primaryetiology %in% c("IHD") ~ 1,
      TRUE ~ 2
    ),
    levels = 1:2, labels = c("Ischemic", "Non-ischemic")
    ),
    shf_indexyear_cat = case_when(
      shf_indexyear <= 2010 ~ "2005-2010",
      shf_indexyear <= 2015 ~ "2011-2015",
      shf_indexyear <= 2018 ~ "2016-2018"
    ),

    shf_age_cat = case_when(
      shf_age < 75 ~ "<75",
      shf_age >= 75 ~ ">=75"
    ),

    shf_ef_cat = factor(case_when(
      shf_ef == ">=50" ~ 3,
      shf_ef == "40-49" ~ 2,
      shf_ef %in% c("30-39", "<30") ~ 1
    ),
    labels = c("HFrEF", "HFmrEF", "HFpEF"),
    levels = 1:3
    ),

    shf_smoking_cat = factor(case_when(
      shf_smoking %in% c("Never") ~ 1,
      shf_smoking %in% c("Former", "Current") ~ 2
    ),
    labels = c("Never", "Former/Current"),
    levels = 1:2
    ),

    shf_anemia = case_when(
      is.na(shf_hb) ~ NA_character_,
      shf_sex == "Female" & shf_hb < 120 | shf_sex == "Male" & shf_hb < 130 ~ "Yes",
      TRUE ~ "No"
    ),

    shf_map_cat = case_when(
      shf_map <= 90 ~ "<=90",
      shf_map > 90 ~ ">90"
    ),

    shf_potassium_cat = factor(
      case_when(
        is.na(shf_potassium) ~ NA_real_,
        shf_potassium < 3.5 ~ 2,
        shf_potassium <= 5 ~ 1,
        shf_potassium > 5 ~ 3
      ),
      labels = c("normakalemia", "hypokalemia", "hyperkalemia"),
      levels = 1:3
    ),

    shf_heartrate_cat = case_when(
      shf_heartrate <= 70 ~ "<=70",
      shf_heartrate > 70 ~ ">70"
    ),

    shf_device_cat = factor(case_when(
      is.na(shf_device) ~ NA_real_,
      shf_device %in% c("CRT", "CRT & ICD", "ICD") ~ 2,
      TRUE ~ 1
    ),
    labels = c("No", "CRT/ICD"),
    levels = 1:2
    ),

    shf_bmi_cat = case_when(
      is.na(shf_bmi) ~ NA_character_,
      shf_bmi < 30 ~ "<30",
      shf_bmi >= 30 ~ ">=30"
    ),

    shf_gfrckdepi_cat = factor(case_when(
      is.na(shf_gfrckdepi) ~ NA_real_,
      shf_gfrckdepi >= 60 ~ 1,
      shf_gfrckdepi < 60 ~ 2,
    ),
    labels = c(">=60", "<60"),
    levels = 1:2
    ),

    ddr_rasiarni = if_else(ddr_acei == "Yes" |
      ddr_arni == "Yes" |
      ddr_arb == "Yes", "Yes", "No"),

    # outcomes
    # comp risk
    sos_out_hospany_cr = create_crevent(sos_out_hospany, sos_out_death),
    sos_out_hosphf_cr = create_crevent(sos_out_hosphf, sos_out_death),
    sos_out_hospcv_cr = create_crevent(sos_out_hospcv, sos_out_death),
    sos_out_hospnoncv_cr = create_crevent(sos_out_hospnoncv, sos_out_death),
    sos_out_visany_cr = create_crevent(sos_out_visany, sos_out_death),
    sos_out_deathcv_cr = create_crevent(sos_out_deathcv, sos_out_death),
    sos_out_deathnoncv_cr = create_crevent(sos_out_deathnoncv, sos_out_death),
    sos_out_revasc_cr = create_crevent(sos_out_revasc, sos_out_death),

    ef_casecontrol = factor(paste0(as.numeric(shf_ef_cat) * 2 + as.numeric(casecontrol) - 2),
      labels = c(paste0(rep(levels(shf_ef_cat), each = 2), " ", levels(casecontrol)))
    ),
    ef_ischemic = factor(paste0(as.numeric(shf_ef_cat) * 2 + as.numeric(shf_primaryetiology_cat) - 2),
      levels = 1:6,
      labels = c(paste0(rep(levels(shf_ef_cat), each = 2), " ", levels(shf_primaryetiology_cat)))
    )
  )

# income

inc <- pdata %>%
  group_by(shf_indexyear) %>%
  summarise(incmed = quantile(scb_dispincome,
    probs = 0.5,
    na.rm = TRUE
  ), .groups = "drop")

pdata <- left_join(
  pdata,
  inc,
  by = "shf_indexyear"
) %>%
  mutate(
    scb_dispincome_cat2 = case_when(
      scb_dispincome < incmed ~ 1,
      scb_dispincome >= incmed ~ 2
    ),
    scb_dispincome_cat2 = factor(scb_dispincome_cat2,
      levels = 1:2,
      labels = c("Below medium", "Above medium")
    )
  ) %>%
  select(-incmed)

# ntprobnp

ntprobnp <- pdata %>%
  group_by(shf_ef_cat) %>%
  summarise(
    ntmed = quantile(shf_ntprobnp,
      probs = 0.5,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

pdata <- left_join(
  pdata,
  ntprobnp,
  by = c("shf_ef_cat")
) %>%
  mutate(
    shf_ntprobnp_cat = case_when(
      shf_ntprobnp < ntmed ~ 1,
      shf_ntprobnp >= ntmed ~ 2
    ),
    shf_ntprobnp_cat = factor(shf_ntprobnp_cat,
      levels = 1:2,
      labels = c("Below medium", "Above medium")
    )
  ) %>%
  select(-ntmed)

pdata <- pdata %>%
  mutate_if(is_character, factor)
