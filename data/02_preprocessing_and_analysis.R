################################ Descriptive Analysis
library(ggplot2)
library(dplyr)
library(stringr)
library(reshape2)

getwd()
final_data <- read.csv("data/01_data_cleaned.csv")

### Basic overview
dim(final_data)
summary(final_data)

### Overview of numerical variables: mass, perigee, apogee, etc.
## Histograms
# Mass
ggplot(final_data, aes(x = mass)) +
  labs(title = "Histogram of Mass", x = "Mass", y = "Count") +
  geom_histogram(bins = 50, fill = "#2e00fa", color = "black") +
  theme(panel.background = element_blank())

# Perigee
ggplot(final_data, aes(x = perigee)) +
  labs(title = "Histogram of Perigee", x = "Perigee", y = "Count") +
  geom_histogram(bins = 50, fill = "#2e00fa", color = "black") +
  theme(panel.background = element_blank())

# Apogee
ggplot(final_data, aes(x = apogee)) +
  labs(title = "Histogram of Apogee", x = "Apogee", y = "Count") +
  geom_histogram(bins = 50, fill = "#2e00fa", color = "black") +
  theme(panel.background = element_blank())

# Inclination
ggplot(final_data, aes(x = inclination)) +
  labs(title = "Histogram of Inclination", x = "Inclination", y = "Count") +
  geom_histogram(bins = 50, fill = "#2e00fa", color = "black") +
  theme(panel.background = element_blank())

# Expected Lifetime
ggplot(final_data, aes(x = expected_lifetime)) +
  labs(title = "Histogram of Expected Lifetime", x = "Expected Lifetime", y = "Count") +
  geom_histogram(bins = 50, fill = "#2e00fa", color = "black") +
  theme(panel.background = element_blank())

# Time days
ggplot(final_data, aes(x = time_days)) +
  labs(title = "Histogram of Lifetime in Days", x = "Lifetime", y = "Count") +
  geom_histogram(bins = 50, fill = "#2e00fa", color = "black") +
  theme(panel.background = element_blank())

# Almost all of them are heavily right-skewed: transform mass, perigee, apogee
final_data <- final_data %>%
  mutate(
    log_mass = log(mass),
    log_perigee = log(perigee + 1),
    log_apogee = log(apogee + 1)
  )

# Check transformed variables
ggplot(final_data, aes(x = log_mass)) +
  labs(title = "Histogram of Mass (log-transformed)", x = "log(Mass)", y = "Count") +
  geom_histogram(bins = 50, fill = "#2e00fa", color = "black") +
  theme(panel.background = element_blank())

ggplot(final_data, aes(x = log_apogee)) +
  labs(title = "Histogram of Apogee (log-transformed)", x = "log(Apogee)", y = "Count") +
  geom_histogram(bins = 50, fill = "#2e00fa", color = "black") +
  theme(panel.background = element_blank())

ggplot(final_data, aes(x = log_perigee)) +
  labs(title = "Histogram of Perigee (log-transformed)", x = "log(Perigee)", y = "Count") +
  geom_histogram(bins = 50, fill = "#2e00fa", color = "black") +
  theme(panel.background = element_blank())

## Correlations
cor_mat <- final_data %>%
  select(perigee, apogee, inclination, mass, expected_lifetime) %>%
  cor(use = "complete.obs")

df_cor <- melt(cor_mat)

ggplot(df_cor, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(
    x = "",
    y = "",
    fill = "Correlation",
    title = "Correlation Between Numerical Variables"
  ) +
  geom_text(aes(label = round(value, 2)), size = 3) +
  theme_minimal()

# -> maybe don't keep both perigee and apogee, they contain almost the same info

# Check with expected life time versus life time
cor(final_data$expected_lifetime, final_data$time_days, use = "complete.obs")

ggplot(final_data, aes(x = expected_lifetime, y = time_days / 365)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "purple", se = FALSE) +
  labs(
    title = "Expected vs Actual Satellite Lifetime",
    x = "Expected Lifetime (years)",
    y = "Actual Lifetime (years)"
  ) +
  theme_classic()

##############################################################################

### Categorical variables: purpose, users, orbit_class, orbit_type, country etc
## Purpose
table(final_data$purpose)
length(unique(final_data$purpose))

# 30 categories -> 6: Communications, Earth Observation, Navigation, 
# Science (Space Science + Earth Science), Technology, Other
final_data <- final_data %>%
  mutate(
    purpose_grouped = case_when(
      str_detect(purpose, "Communications") ~ "Communications",
      str_detect(purpose, "Earth Observation|Earth Science") ~ "Earth Observation",
      str_detect(purpose, "Navigation|Positioning") ~ "Navigation",
      str_detect(purpose, "Space Science|Science") ~ "Science",
      str_detect(purpose, "Technology") ~ "Technology",
      TRUE ~ "Other"
    )
  )

final_data$purpose_grouped <- factor(
  final_data$purpose_grouped,
  levels = c("Communications", "Earth Observation", "Navigation", "Science", "Technology", "Other")
)

# Plot purpose vs lifetime
colors_6 <- c("darkblue", "#4b00d1", "#6a00b9", "#8800a0", "#b800a6", "#dc006d")

ggplot(final_data, aes(x = purpose_grouped, y = time_days, fill = purpose_grouped)) +
  geom_boxplot(
    color = "black",
    linewidth = 0.3,
    outlier.size = 0.5,
    outlier.alpha = 0.2
  ) +
  scale_fill_manual(values = colors_6) +
  scale_y_log10() +
  coord_cartesian(ylim = c(500, 12000)) +
  labs(
    title = "Lifetime by Purpose",
    x = "Purpose",
    y = "Lifetime (days, log scale)"
  ) +
  theme_classic() + 
  theme(
    legend.position = "none",
    axis.line = element_line(color = "black", linewidth = 0.3)
  )

# median lifetime per category
final_data %>%
  group_by(purpose_grouped) %>%
  summarise(median_lifetime = (median(time_days))/365)

## Users
table(final_data$users)
length(unique(final_data$users))

# 16 categories -> 5: Civil, Commercial, Government, Military, Mixed, Others
final_data <- final_data %>%
  mutate(
    users_grouped = case_when(
      grepl("/", users) ~ "Mixed",
      users == "Commercial" ~ "Commercial",
      users == "Government" ~ "Government",
      users == "Military" ~ "Military",
      users == "Civil" ~ "Civil",
      TRUE ~ "Other"
    )
  )

table(final_data$users_grouped)
length(unique(final_data$users_grouped))

final_data$country_grouped <- factor(
  final_data$purpose_grouped,
  levels = c("Civil", "Commercial", "Government", "Military", "Mixed")
)

colors_5 <- c("darkblue", "#4b00d1", "#6a00b9", "#b800a6", "#dc006d")

ggplot(final_data, aes(x = users_grouped, y = time_days, fill = users_grouped)) +
  geom_boxplot(
    color = "black",
    linewidth = 0.3,
    outlier.size = 0.5,
    outlier.alpha = 0.2
  ) +
  scale_fill_manual(values = colors_5) +
  scale_y_log10() +
  coord_cartesian(ylim = c(500, 12000)) +
  labs(
    title = "Lifetime by Users",
    x = "Users",
    y = "Lifetime (days, log scale)"
  ) +
  theme_classic() + 
  theme(
    legend.position = "none",
    axis.line = element_line(color = "black", linewidth = 0.3)
  )

## Orbit class
final_data <- final_data %>%
  mutate(orbit_class = toupper(orbit_class))
table(final_data$orbit_class)
length(unique(final_data$orbit_class))

# 4 categories -> keep as is

colors_4 <- c("darkblue", "#4b00d1", "#6a00b9", "#b800a6")
ggplot(final_data, aes(x = orbit_class, y = time_days, fill = orbit_class)) +
  geom_boxplot(
    color = "black",
    linewidth = 0.3,
    outlier.size = 0.5,
    outlier.alpha = 0.2
  ) +
  scale_fill_manual(values = colors_4) +
  scale_y_log10() +
  coord_cartesian(ylim = c(500, 12000)) +
  labs(
    title = "Lifetime by Orbit Altitude Class",
    x = "Orbit Altitude Class",
    y = "Lifetime (days, log scale)"
  ) +
  theme_classic() + 
  theme(
    legend.position = "none",
    axis.line = element_line(color = "black", linewidth = 0.3)
  )

## Orbit type
table(final_data$orbit_type)
length(unique(final_data$orbit_type))

# 9 categories -> 4 categories (keep only those separate that have high obs)
final_data <- final_data %>%
  mutate(
    orbit_type_grouped = case_when(
      orbit_type %in% c("Non-Polar Inclined") ~ "Inclined",
      orbit_type %in% c("Polar") ~ "Polar",
      orbit_type %in% c("Sun-Synchronous", "Sun-Synchronous near polar") ~ "Sun-Sync",
      TRUE ~ "Other"
    )
  )

final_data$orbit_type_grouped <- factor(
  final_data$orbit_type_grouped,
  levels = c("Inclined", "Polar", "Sun-Sync", "Other")
)

ggplot(final_data, aes(x = orbit_type_grouped, y = time_days, fill = orbit_type_grouped)) +
  geom_boxplot(
    color = "black",
    linewidth = 0.3,
    outlier.size = 0.5,
    outlier.alpha = 0.2
  ) +
  scale_fill_manual(values = colors_4) +
  scale_y_log10() +
  coord_cartesian(ylim = c(500, 12000)) +
  labs(
    title = "Lifetime by Orbit Geometry Type",
    x = "Orbit Geometry Type",
    y = "Lifetime (days, log scale)"
  ) +
  theme_classic() + 
  theme(
    legend.position = "none",
    axis.line = element_line(color = "black", linewidth = 0.3)
  )

## Country (Contractor)
table(final_data$country)
length(unique(final_data$country))

# Group Countries
european_countries <- c(
  "Austria", "Belgium", "Bulgaria", "Czech Republic", "Denmark",
  "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
  "Italy", "Lithuania", "Luxembourg", "Monaco", "Netherlands",
  "Norway", "Poland", "Portugal", "Slovenia", "Spain", "Sweden",
  "Switzerland", "Ukraine", "United Kingdom"
)

final_data <- final_data %>%
  mutate(
    country_grouped = case_when(
      country == "USA" ~ "US",
      country == "China" ~ "China",
      country == "Russia" ~ "Russia",
      country == "ESA" ~ "Europe", # it only has 57 observations (even though the effect differs, for modelling it should be separate)
      country %in% european_countries ~ "Europe",
      TRUE ~ "Other"
    )
  )

final_data$country_grouped <- factor(
  final_data$country_grouped,
  levels = c("China", "Europe", "Russia", "US", "Other")
)

table(final_data$country_grouped)

# Violin plot
ggplot(final_data, aes(x = country_grouped, y = time_days, fill = country_grouped)) +
  geom_violin(
    color = "black",
    linewidth = 0.3,
    alpha = 0.7,
    trim = TRUE
  ) +
  
  # Boxplot (summary)
  geom_boxplot(
    width = 0.15,
    color = "black",
    fill = "white",
    linewidth = 0.3,
    outlier.size = 0.4,
    outlier.alpha = 0.2
  ) +
  
  scale_fill_manual(values = colors_5) +
  scale_y_log10() +
  coord_cartesian(ylim = c(100, 10000)) +
  
  labs(
    title = "Lifetime by Country or Region",
    x = "Country/Region",
    y = "Lifetime (days, log scale)"
  ) +
  
  theme_classic() + 
  theme(
    legend.position = "none",
    axis.line = element_line(color = "black", linewidth = 0.3)
  )

## Launch Site
table(final_data$launch_site)
length(unique(final_data$launch_site))

# Group in regions
final_data <- final_data %>%
  mutate(
    launch_region = case_when(
      # USA (5046)
      launch_site %in% c("AFETR", "AFWTR", "KWAJ", "WLPIS", "KODAK") ~ "US",
      # Russia / Kazakhstan (840)
      launch_site %in% c("TYMSC", "PLMSC", "VOSTO", "SVOBO", "DLS", "KYMSC") ~ "Russia/Kazakhstan",
      # China (622)
      launch_site %in% c("XICLF", "TAISC", "JSC", "WSC", "YSLA", "SCSLA") ~ "China",
      # Europe (316)
      launch_site %in% c("FRGUI") ~ "Europe",
      # India (274)
      launch_site %in% c("SRILR") ~ "India",
      # Japan (only 43) -> other
      # launch_site %in% c("TANSC", "KSCUT", "SPKII") ~ "Japan",
      # Korea (only 5) -> other
      #launch_site %in% c("NSC", "JJSLA") ~ "Korea",
      # Iran (only 1) -> other
      #launch_site %in% c("SEMLS", "SMTS") ~ "Iran",
      # Sea / mobile platforms (only 36) -> other
      #launch_site %in% c("SEAL", "SUBL", "SNMLP") ~ "Sea/Mobile",
      # Everything else
      TRUE ~ "Other"
    )
  )

table(final_data$launch_region)

# violin plot
ggplot(final_data, aes(x = launch_region, y = time_days, fill = launch_region)) +
  geom_violin(
    color = "black",
    linewidth = 0.3,
    alpha = 0.7
  ) +
  geom_boxplot(
    width = 0.15,
    color = "black",
    fill = "white",
    linewidth = 0.3,
    outlier.size = 0.4,
    outlier.alpha = 0.2
  ) +
  scale_fill_manual(values = colors) +
  scale_y_log10() +
  coord_cartesian(ylim = c(100, 10000)) +
  labs(
    title = "Lifetime by Launch Region",
    x = "Launch Region",
    y = "Lifetime (days, log scale)"
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.line = element_line(color = "black", linewidth = 0.3)
  )

## Launch Vehicle
table(final_data$launch_vehicle)
length(unique(final_data$launch_vehicle))

# Group by family
final_data <- final_data %>%
  mutate(
    launch_vehicle_group = case_when(
      str_detect(launch_vehicle, regex("Falcon", ignore_case = TRUE)) ~ "Falcon",
      str_detect(launch_vehicle, regex("Long March", ignore_case = TRUE)) ~ "Long March",
      str_detect(launch_vehicle, regex("Soyuz", ignore_case = TRUE)) ~ "Soyuz",
      str_detect(launch_vehicle, regex("^PSLV|PSLV", ignore_case = TRUE)) ~ "PSLV",
      str_detect(launch_vehicle, regex("Atlas", ignore_case = TRUE)) ~ "Atlas",
      str_detect(launch_vehicle, regex("Ariane", ignore_case = TRUE)) ~ "Ariane",
      str_detect(launch_vehicle, regex("Delta", ignore_case = TRUE)) ~ "Delta",
      str_detect(launch_vehicle, regex("Proton", ignore_case = TRUE)) ~ "Proton",
      str_detect(launch_vehicle, regex("Electron", ignore_case = TRUE)) ~ "Electron",
      str_detect(launch_vehicle, regex("Vega", ignore_case = TRUE)) ~ "Vega",
      str_detect(launch_vehicle, regex("Dnepr", ignore_case = TRUE)) ~ "Dnepr",
      str_detect(launch_vehicle, regex("Rokot", ignore_case = TRUE)) ~ "Rokot",
      str_detect(launch_vehicle, regex("Minotaur", ignore_case = TRUE)) ~ "Minotaur",
      str_detect(launch_vehicle, regex("Space Shuttle", ignore_case = TRUE)) ~ "Space Shuttle",
      TRUE ~ "Other"
    )
  )

table(final_data$launch_vehicle_group)
length(unique(final_data$launch_vehicle_group))

# Plot for descriptive analysis
space_palette <- c(
  "darkblue",
  "#2e00fa",  
  "#4b00d1",
  "#6a00b9",
  "#8800a0",
  "#a000bc",
  "#b800a6",
  "#ca0086",
  "#dc006d",
  "#e40058",
  "#f2006f",
  "#ff3c8c",
  "#ff66a3",
  "#ff8fba",
  "#ffb8d1"
)

ggplot(final_data, aes(x = launch_vehicle_group, y = time_days, fill = launch_vehicle_group)) +
  geom_boxplot(
    color = "black",
    linewidth = 0.3,
    outlier.size = 0.5,
    outlier.alpha = 0.2
  ) +
  scale_fill_manual(values = space_palette) +
  scale_y_log10() +
  coord_cartesian(ylim = c(500, 12000)) +
  labs(
    title = "Lifetime by Launch Vehicle",
    x = "Launch Vehicle Group",
    y = "Lifetime (days, log scale)"
  ) +
  theme_classic() + 
  theme(
    legend.position = "none",
    axis.line = element_line(color = "black", linewidth = 0.3)
  )

# Check for correlation: likely overlap with orbit -> redundant
orbit_palette <- c("#ff8fba", "#a000bc","#2e00fa","#e40058")

ggplot(final_data, aes(x = launch_vehicle_group, fill = orbit_class)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = orbit_palette) +
  labs(
    title = "Orbit Distribution by Launch Vehicle",
    x = "Launch Vehicle Group",
    y = "Proportion"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Numerical check ->  moderate correlation
library(lsr)
cramersV(final_data$launch_vehicle_group, final_data$orbit_class)

# Check correlation with country
tab <- table(final_data$launch_vehicle_group, final_data$country_grouped)

prop_tab <- prop.table(tab, margin = 1)

round(prop_tab, 2)

df <- as.data.frame(prop_tab)

ggplot(df, aes(Var2, Var1, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(
    x = "Country Group",
    y = "Launch Vehicle",
    fill = "Proportion",
    title = "Launch Vehicle vs Country Group"
  ) +
  theme_minimal()

# -> strong association between launch vehicle and country group
# (most launch vehicles are specific to national space programs)
# => To avoid redundancy and multicollinearity, only include launch vehicle in model

## Orbit Center
table(final_data$orbit_center)
length(unique(final_data$orbit_center))

# useless -> remove
final_data <- subset(final_data, select = -c(orbit_center))

## Owner
table(final_data$owner)
length(unique(final_data$owner))

# similar to country -> remove
final_data <- subset(final_data, select = -c(owner))

## Object Type
table(final_data$object_type)
length(unique(final_data$object_type))

# useless (almost all are pay) -> remove
final_data <- subset(final_data, select = -c(object_type))

# rename for clarification and save
final_data <- final_data %>%
  rename(
    orbit_altitude_class = orbit_class,
    orbit_geometry_type = orbit_type
  )

write.csv(final_data, "data/02_data_grouped.csv", row.names = FALSE)

