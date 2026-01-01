############################################################
# Be-Resilient Citizen Science – Reproducible Workflow
# UNESCO – IHP-IX
# Author: [Tu nombre]
# Date: 2024
############################################################

# ==========================================================
# 0. LIBRARIES
# ==========================================================
library(tidyverse)
library(janitor)
library(lubridate)

# ==========================================================
# 1. DATA INGESTION & INITIAL HARMONISATION
# ==========================================================

data_raw <- read.csv("data.csv",
                     stringsAsFactors = FALSE)

# Preserve raw data
data <- data_raw %>%
  clean_names()

# Correct known typographical inconsistencies
data <- data %>%
  rename(
    dissolved_oxygen_ppm = dissolved_oxgen_ppm,
    salinity_ppt         = solinity_parts_per_thousand,
    salinity_percent     = salinity_percentage
  )

# ==========================================================
# 2. TEMPORAL STRUCTURING
# ==========================================================

data <- data %>%
  mutate(
    datetime = mdy_hm(time, tz = "Africa/Johannesburg"),
    date     = as_date(datetime)
  )

# ==========================================================
# 3. DATA TYPE VALIDATION
# ==========================================================

numeric_vars <- c(
  "ph",
  "dissolved_oxygen_ppm",
  "dissolved_oxygen_percent",
  "electrical_conductivity",
  "air_temperature",
  "nitrate",
  "phosphate",
  "salinity_ppt",
  "salinity_percent",
  "tds"
)

data <- data %>%
  mutate(across(all_of(numeric_vars),
                ~ suppressWarnings(as.numeric(.))))

# ==========================================================
# 4. CONSTRUCTION OF CLEAN DATASET
# ==========================================================

data_clean <- data %>%
  filter(
    rowSums(!is.na(select(., all_of(numeric_vars)))) > 0
  )

write.csv(data_clean,
          "data/processed/citizensciencedata_clean.csv",
          row.names = FALSE)

# ==========================================================
# 5. QUALITY ASSURANCE / QUALITY CONTROL (QA/QC)
# ==========================================================
# Thresholds based on APHA (2017), Wetzel (2001), Chapman (1996)

data_qc_flagged <- data_clean %>%
  mutate(
    flag_ph = !is.na(ph) & (ph < 4 | ph > 10),
    flag_do_ppm = !is.na(dissolved_oxygen_ppm) &
      (dissolved_oxygen_ppm < 0 | dissolved_oxygen_ppm > 20),
    flag_do_percent = !is.na(dissolved_oxygen_percent) &
      (dissolved_oxygen_percent < 0 | dissolved_oxygen_percent > 200),
    flag_ec = !is.na(electrical_conductivity) &
      (electrical_conductivity < 0 | electrical_conductivity > 2000),
    flag_temp = !is.na(air_temperature) &
      (air_temperature < 0 | air_temperature > 45),
    any_flag = flag_ph | flag_do_ppm | flag_do_percent |
      flag_ec | flag_temp
  )

write.csv(data_qc_flagged,
          "data/processed/citizensciencedata_qc_flagged.csv",
          row.names = FALSE)

# Strict dataset: remove physically implausible values only
data_qc_strict <- data_qc_flagged %>%
  filter(!any_flag)

write.csv(data_qc_strict,
          "data/processed/citizensciencedata_qc_strict.csv",
          row.names = FALSE)

# ==========================================================
# 6. DESCRIPTIVE STATISTICS
# ==========================================================

summary_biosphere <- data_qc_strict %>%
  group_by(biosphere) %>%
  summarise(
    n = n(),
    ph_mean = mean(ph, na.rm = TRUE),
    ph_sd = sd(ph, na.rm = TRUE),
    do_mean = mean(dissolved_oxygen_ppm, na.rm = TRUE),
    do_sd = sd(dissolved_oxygen_ppm, na.rm = TRUE),
    ec_mean = mean(electrical_conductivity, na.rm = TRUE),
    ec_sd = sd(electrical_conductivity, na.rm = TRUE),
    temp_mean = mean(air_temperature, na.rm = TRUE),
    temp_sd = sd(air_temperature, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(summary_biosphere,
          "outputs/tables/summary_biosphere.csv",
          row.names = FALSE)

# ==========================================================
# 7. INFERENTIAL STATISTICS
# ==========================================================

kruskal_ph <- kruskal.test(ph ~ biosphere,
                           data = data_qc_strict)

cor_temp_do <- cor.test(
  data_qc_strict$air_temperature,
  data_qc_strict$dissolved_oxygen_ppm,
  method = "spearman",
  use = "complete.obs"
)

save(kruskal_ph, cor_temp_do,
     file = "outputs/tables/statistical_tests.RData")

# ==========================================================
# 8. VISUALISATION
# ==========================================================

p_ph <- ggplot(data_qc_strict,
               aes(x = biosphere, y = ph)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "pH Distribution by Biosphere Reserve")

ggsave("outputs/figures/ph_boxplot.png",
       p_ph, width = 8, height = 5, dpi = 300)

# ==========================================================
# 9. SAMPLING EFFORT ANALYSIS
# ==========================================================

sampling_effort <- data_qc_strict %>%
  count(biosphere, river)

write.csv(sampling_effort,
          "outputs/tables/sampling_effort.csv",
          row.names = FALSE)

# ==========================================================
# 10. REPRODUCIBILITY
# ==========================================================
# All outputs are reproducible by running this script sequentially.
# Version control recommended via GitHub.
############################################################
