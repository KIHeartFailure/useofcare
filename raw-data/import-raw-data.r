
ProjectTemplate::load.project(list(munging = FALSE, data_loading = FALSE))

memory.limit(size = 10000000000000)

pathrawdata <- "C:/Users/Lina/STATISTIK/Projects/20200225_shfdb3/dm/"


# Patient registry from SHFDB3 v 3.1.2, prepared in 08-prep_sosdata.R -----

load(file = paste0(pathrawdata, "data/patreg.RData"))

# Store as RData in /data folder ------------------------------------------

save(file = "./data/patreg.RData", list = c("patreg"))
rm(patreg)


# Patient registry for emergency visit and LOS -----------------------------

load(file = paste0(pathrawdata, "data/rawData_sossv.RData"))
sv3 <- sv3 %>% rename(HDIA = hdia)
sv <- bind_rows(sv2, sv3)
rm("sv1", "sv2", "sv3")

load(file = paste0(pathrawdata, "data/rawData_sosov.RData"))
ov3 <- ov3 %>% rename(HDIA = hdia)
ov <- bind_rows(ov2, ov3)
rm("ov1", "ov2", "ov3")
ovem <- ov %>%
  filter(MVO %in% c("046", "100") & PVARD == "2")

# Store as RData in /data folder ------------------------------------------

save(file = "./data/ovsv.RData", list = c("sv", "ovem"))

rm("sv", "ov", "ovem")


# LM registry  ------------------------------------------------------------

load(file = paste0(pathrawdata, "raw-data/SOS/lev3_15875_2019 Lina Benson/RData/lm.RData"))

lm <- lm %>%
  filter(
    ANTAL >= 0,
    EDATUM <= ymd("2018-12-31")
  )

# Store as RData in /data folder ------------------------------------------

save(file = "./data/lm.RData", list = c("lm"))
