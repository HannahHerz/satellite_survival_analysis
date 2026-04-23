######################################### Merge Datasets

library(dplyr)
library(readxl)
library(readr)

setwd("C:/Users/Nutzer/Desktop/KULeuven/Time-to-Event Analysis/Assignment")

satcat <- read.csv("satcat.csv")
ucs <- read_excel("UCS-Satellite-Database 5-1-2023.xlsx")

# Define cutoff: download date of satcat
cutoff <- as.Date("2026-04-21")

### Prepare SATCAT
satcat <- satcat %>%
  mutate(
    NORAD_CAT_ID = as.numeric(NORAD_CAT_ID),
    LAUNCH_DATE = as.Date(LAUNCH_DATE, format = "%d/%m/%Y"),
    DECAY_DATE  = as.Date(DECAY_DATE,  format = "%d/%m/%Y"),
    OPS_STATUS_CODE = toupper(trimws(OPS_STATUS_CODE))
  )

# Status from documentation
satcat <- satcat %>%
  mutate(
    satcat_status_raw = case_when(
      OPS_STATUS_CODE %in% c("+", "P", "B", "S", "X") ~ "active",
      OPS_STATUS_CODE %in% c("-", "D") ~ "inactive",
      TRUE ~ NA_character_
    )
  )

# Remove inactive satellites without decay date
satcat_clean <- satcat %>%
  filter(!(OPS_STATUS_CODE %in% c("-", "D") & is.na(DECAY_DATE)))

# Keep only known status codes
satcat_clean <- satcat_clean %>%
  filter(OPS_STATUS_CODE %in% c("+", "P", "B", "S", "X", "-", "D"))

### Prepare UCS 
ucs <- ucs %>%
  mutate(`NORAD Number` = as.numeric(`NORAD Number`))

ucs_unique <- ucs %>%
  distinct(`NORAD Number`, .keep_all = TRUE)

# Keep only UCS variables that are still going to be used
ucs_keep <- ucs_unique %>%
  select(
    `Name of Satellite, Alternate Names`,
    `Country of Operator/Owner`,
    Users,
    Purpose,
    `Class of Orbit`,
    `Type of Orbit`,
    `Launch Mass (kg.)`,
    `Expected Lifetime (yrs.)`,
    `Launch Vehicle`,
    `NORAD Number`
  )

# Merge datasets (on Norad)
merged <- satcat_clean %>%
  inner_join(ucs_keep, by = c("NORAD_CAT_ID" = "NORAD Number"))

# Build survival outcome
merged <- merged %>%
  filter(!is.na(LAUNCH_DATE)) %>%
  filter(LAUNCH_DATE <= cutoff) %>%
  filter(is.na(DECAY_DATE) | DECAY_DATE >= LAUNCH_DATE) %>%
  mutate(
    event = if_else(!is.na(DECAY_DATE) & DECAY_DATE <= cutoff, 1L, 0L),
    end_date = if_else(event == 1L, DECAY_DATE, cutoff),
    end_date = as.Date(end_date, origin = "1970-01-01"),
    time_days = as.numeric(end_date - LAUNCH_DATE),
    activity_snapshot = if_else(event == 1L, "inactive", "active")
  )

# Keep final analysis columns
final <- merged %>%
  select(
    OBJECT_NAME,
    NORAD_CAT_ID,
    LAUNCH_DATE,
    DECAY_DATE,
    end_date,
    time_days,
    event,
    activity_snapshot,
    
    LAUNCH_SITE,
    ORBIT_CENTER,
    OBJECT_TYPE,
    OWNER,
    
    `Country of Operator/Owner`,
    Users,
    Purpose,
    `Class of Orbit`,
    `Type of Orbit`,
    `Launch Vehicle`,
    `Launch Mass (kg.)`,
    `Expected Lifetime (yrs.)`,
    
    # SATCAT orbit variables
    PERIGEE,
    APOGEE,
    INCLINATION,
    PERIOD
  ) %>%
  rename(
    name = OBJECT_NAME,
    norad_id = NORAD_CAT_ID,
    launch_date = LAUNCH_DATE,
    decay_date = DECAY_DATE,
    launch_site = LAUNCH_SITE,
    orbit_center = ORBIT_CENTER,
    orbit_class = `Class of Orbit`,
    orbit_type = `Type of Orbit`,
    object_type = OBJECT_TYPE,
    owner = OWNER,
    country = `Country of Operator/Owner`,
    users = Users,
    purpose = Purpose,
    launch_vehicle = `Launch Vehicle`,
    mass = `Launch Mass (kg.)`,
    expected_lifetime = `Expected Lifetime (yrs.)`,
    perigee = PERIGEE,
    apogee = APOGEE,
    inclination = INCLINATION,
    period = PERIOD
  )

### Sanity checks: missings etc.
summary(final)
colSums(is.na(final))
nrow(final)
table(final$event)

# Explore missings further
final %>%
  filter(is.na(apogee) | is.na(perigee)) %>%
  select(
    norad_id,
    name,
    apogee,
    perigee,
    inclination,
    period,
    orbit_class,
    purpose,
    time_days
  )

# Check consistency in Orbit classes 
final <- final %>%
  mutate(
    orbit_class_check = case_when(
      orbit_class == "LEO" & (perigee < 80 | apogee > 1700 | period > 120) ~ "inconsistent",
      orbit_class == "MEO" & (apogee <= 1700 | apogee >= 35700 | period < 120 | period > 1440) ~ "inconsistent",
      orbit_class == "GEO" & (apogee < 33000 | apogee > 39000 | period < 1300 | period > 1500) ~ "inconsistent",
      TRUE ~ "consistent"
    )
  )

table(final$orbit_class, final$orbit_class_check)

final %>%
  filter(orbit_class_check == "inconsistent") %>%
  select(name, orbit_class, perigee, apogee, period)

# 2 obervations have perigee = 0. Those satellites aren't orbiting -> Remove.
final <- final %>%
  filter(perigee > 0, apogee > 0)

# 5 observations are in wrong class -> assign correct one
final <- final %>%
  mutate(
    orbit_class = case_when(
      orbit_class == "LEO" & apogee >= 33000 ~ "GEO",
      orbit_class == "LEO" & apogee >= 1700 ~ "MEO",
      TRUE ~ orbit_class
    )
  )

# Remove helper variable
final <- subset(final, select = -c(orbit_class_check))

### Clean missing values
# Remove rows with missing key orbit variables from SATCAT
final <- final %>%
  filter(
    !is.na(perigee),
    !is.na(apogee),
    !is.na(inclination),
    !is.na(period)
  )

# Orbit type: create Unknown category
final <- final %>%
  mutate(
    orbit_type = if_else(is.na(orbit_type), "Unknown", orbit_type)
  )

# Median imputation for missings in Mass
final <- final %>%
  mutate(
    mass = if_else(
      is.na(mass),
      median(mass, na.rm = TRUE),
      mass
    ))

# Expected lifetime: missing indicator + median imputation
final <- final %>%
  mutate(
    expected_lifetime_missing = if_else(
      is.na(expected_lifetime), 1L, 0L
    ),
    expected_lifetime = if_else(
      is.na(expected_lifetime),
      median(expected_lifetime, na.rm = TRUE),
      expected_lifetime
    )
  )

### Check for extreme outliers
summary(final$mass)
summary(final$apogee)
summary(final$perigee)

# Check very light satellites 
final %>%
  filter(mass < 2) %>%  
  select(
    name, mass, purpose, orbit_class, orbit_center,
    perigee, apogee, time_days
  ) %>%
  arrange(mass)

# -> manual check showed that the mass is reasonable (=CubeSats)

# Check very low apogee (<100 km is not a real orbit)
final %>%
  filter(apogee < 100) %>%
  select(name, norad_id, apogee, perigee, orbit_class, purpose, time_days)

# -> even though extremely low values, physically possible (apogee > perigee)

### Final checks and save
summary(final)
colSums(is.na(final))
nrow(final)
table(final$event)

write.csv(final, "data/01_data_cleaned.csv", row.names = FALSE)
