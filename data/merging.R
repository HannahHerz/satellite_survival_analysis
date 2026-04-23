######################################### Merge Datasets

library(dplyr)
library(readxl)
library(readr)

setwd("C:/Users/Nutzer/Desktop/KULeuven/Time-to-Event Analysis/Assignment")

satcat <- read.csv("satcat.csv")
ucs <- read_excel("UCS-Satellite-Database 5-1-2023.xlsx")

# Avoid data leakage and set 01/05/2023 as "current date" => cutoff 
# (end of data collection from UCS)
cutoff <- as.Date("2023-05-01")

### Prepare SATCAT
satcat <- satcat %>%
  mutate(
    NORAD_CAT_ID = as.numeric(NORAD_CAT_ID),
    LAUNCH_DATE = as.Date(LAUNCH_DATE, format = "%d/%m/%Y"),   
    DECAY_DATE  = as.Date(DECAY_DATE, format = "%d/%m/%Y"),
    OPS_STATUS_CODE = toupper(trimws(OPS_STATUS_CODE))
  )

# Status from documentation (inactive are - and D)
satcat <- satcat %>%
  mutate(
    satcat_status_raw = case_when(
      OPS_STATUS_CODE %in% c("+", "P", "B", "S", "X") ~ "active",
      OPS_STATUS_CODE %in% c("-", "D") ~ "inactive",
      TRUE ~ NA_character_
    )
  )

# Inactive satellites without decay date cannot be used
satcat_clean <- satcat %>%
  filter(!(OPS_STATUS_CODE %in% c("-", "D") & is.na(DECAY_DATE)))

# Keep only known status codes with
satcat_clean <- satcat_clean %>%
  filter(OPS_STATUS_CODE %in% c("+", "P", "B", "S", "X", "-", "D"))

### Prepare UCS
ucs <- ucs %>%
  mutate(`NORAD Number` = as.numeric(`NORAD Number`))

ucs_unique <- ucs %>%
  distinct(`NORAD Number`, .keep_all = TRUE)

ucs_keep <- ucs_unique %>%
  select(
    `Name of Satellite, Alternate Names`,
    `Country/Org of UN Registry`,
    `Country of Operator/Owner`,
    `Operator/Owner`,
    Users,
    Purpose,
    `Detailed Purpose`,
    `Class of Orbit`,
    `Type of Orbit`,
    `Longitude of GEO (degrees)`,
    `Perigee (km)`,
    `Apogee (km)`,
    Eccentricity,
    `Inclination (degrees)`,
    `Period (minutes)`,
    `Launch Mass (kg.)`,
    `Dry Mass (kg.)`,
    `Power (watts)`,
    `Date of Launch`,
    `Expected Lifetime (yrs.)`,
    Contractor,
    `Country of Contractor`,
    `Launch Site`,
    `Launch Vehicle`,
    `COSPAR Number`,
    `NORAD Number`,
    Comments,
    `Source Used for Orbital Data`
  )

### Merge datasets 
merged <- satcat_clean %>%
  inner_join(ucs_keep, by = c("NORAD_CAT_ID" = "NORAD Number"))

### Build survival outcome
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

### Check data: missings
colSums(is.na(merged)) / nrow(merged)

### Keep only final analysis columns 
# i) remove double columns (keep those of UCS because it's save)
# ii) remove columns with lots of missings (keep expected lifetime even though
#     24% are missing, but it can be nice for exploration purpose)
final <- merged %>%
  select(
    OBJECT_NAME,
    NORAD_CAT_ID,
    LAUNCH_DATE,
    DECAY_DATE,
    time_days,
    event,
    end_date,
    time_days,
    activity_snapshot,
    
    LAUNCH_SITE,
    ORBIT_CENTER,
    'Class of Orbit',
    'Type of Orbit',
    OBJECT_TYPE,
    OWNER,
    'Country of Operator/Owner',
    Users,
    Purpose,
    
    'Launch Vehicle',
    'Perigee (km)',
    'Apogee (km)',
    'Inclination (degrees)',
    'Period (minutes)',
    'Launch Mass (kg.)',
    'Expected Lifetime (yrs.)'
    
  ) %>%
  rename(
    name = OBJECT_NAME,
    norad_id = NORAD_CAT_ID,
    launch_date = LAUNCH_DATE,
    decay_date = DECAY_DATE,
    launch_site = LAUNCH_SITE,
    orbit_center = ORBIT_CENTER,
    orbit_class = 'Class of Orbit',
    orbit_type = 'Type of Orbit',
    object_type = OBJECT_TYPE,
    owner = OWNER,
    country_owner = 'Country of Operator/Owner',
    users = Users,
    purpose = Purpose,
    launch_vehicle = 'Launch Vehicle',
    perigee = 'Perigee (km)',
    apogee = 'Apogee (km)',
    inclination = 'Inclination (degrees)',
    period = 'Period (minutes)',
    mass = 'Launch Mass (kg.)',
    expected_lifetime = 'Expected Lifetime (yrs.)'
  )

# Checks
summary(final)
colSums(is.na(final))
nrow(final)
table(final$event)

final %>%
  filter(is.na(perigee)) %>%
  select(
    perigee, apogee, inclination, mass,
    name, orbit_type 
  )

### Clean data: Missings

# Observations with Perigee missing have also Apogee NA (and 2 Inclination). 
# These are important covariates. Remove those lines instead of imputing them.
final <- final %>%
  filter(!is.na(perigee),
         !is.na(apogee))

# Orbit_type is important, create "Unknown" category for missing values
final <- final %>%
  mutate(orbit_type = if_else(
    is.na(orbit_type),
    "Unknown",
    orbit_type
  ))

# Mass has 238 missing, period has 54 missing inclination has 1 missing 
# Use median imputation 
final <- final %>%
  mutate(mass = if_else(
    is.na(mass),
    median(mass, na.rm = TRUE),
    mass
  ))

final <- final %>%
  mutate(period = if_else(
    is.na(period),
    median(period, na.rm = TRUE),
    period
  ))

final <- final %>%
  mutate(inclination = if_else(
    is.na(inclination),
    median(inclination, na.rm = TRUE),
    inclination
  ))

# Expected_lifetime (2042 missings), use median imputation and create binary variable for missing
final <- final %>%
  mutate(
    expected_lifetime_missing = if_else(
      is.na(expected_lifetime), 1, 0
    )
  )

final <- final %>%
  mutate(
    expected_lifetime = if_else(
      is.na(expected_lifetime),
      median(expected_lifetime, na.rm = TRUE),
      expected_lifetime
    )
  )
