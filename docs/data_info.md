# Data info

Working dataset for the survival analysis: `data/02_data_grouped.csv`
(produced by `data/01_merging_cleaning.R` and `data/02_preprocessing_and_analysis.R`).

## Source

Two cross-listed satellite registries merged on the NORAD catalogue id:

* **SATCAT** (Space-Track) — operational status code, launch / decay dates, orbital elements (perigee, apogee, inclination, period).
* **UCS Satellite Database (1 May 2023)** — country of operator/owner, users, purpose, mass, expected lifetime, launch vehicle, class of orbit, type of orbit.

Snapshot date used as administrative censoring time: **2026-04-21**.

## Sample

| n   | events (`event=1`, decayed) | censored (`event=0`, still active) |
|-----|-----------------------------|------------------------------------|
| 7 323 | 2 074 (28.3 %) | 5 249 (71.7 %) |

`time_days` ranges from 7 to 18 785 days (median 1 553).

## Survival quantities

| Variable    | Meaning |
|-------------|---------|
| `time_days` | Observed follow-up in days = `min(decay_date, 2026-04-21) − launch_date` |
| `event`     | 1 if the satellite has decayed before the snapshot, 0 if still active (right-censored) |

## Categorical variables

| Variable              | Levels (count after grouping) | Notes |
|-----------------------|-------------------------------|-------|
| `purpose_grouped`     | Communications · Earth Observation · Navigation · Science · Technology · Other (6) | grouped from 30 raw categories |
| `purpose_3`           | Communications · Earth Observation · Other (3) | **categorical variable used in Part B**; "Other" merges Navigation+Science+Technology+Other |
| `country_grouped`     | China · Europe · Russia · US · Other (5) | Europe = ESA + 25 European states |
| `users_grouped`       | Civil · Commercial · Government · Military · Mixed (5) | Mixed = any "/"-combined entry |
| `orbit_altitude_class`| LEO · MEO · GEO · ELLIPTICAL (4) | corrected for 5 mis-labelled rows |
| `orbit_geometry_type` | Inclined · Polar · Sun-Sync · Other (4) | Sun-Sync merges sun-synchronous + near-polar |
| `launch_region`       | US · Russia/Kazakhstan · China · Europe · India · Other (6) | from `launch_site` |
| `launch_vehicle_group`| 15 families (Falcon · Long March · Soyuz · …) | strongly correlated with `country_grouped` (Cramér's V high) → only one of the two should enter the regression model |

## Numerical variables

| Variable                    | Unit | Comment |
|-----------------------------|------|---------|
| `mass` (`log_mass`)         | kg   | strongly right-skewed → log used |
| `perigee` (`log_perigee`)   | km   | lowest orbital altitude; right-skewed |
| `apogee` (`log_apogee`)     | km   | highest orbital altitude; right-skewed; **highly correlated with `perigee`** — keep only one in models |
| `inclination`               | deg  | |
| `period`                    | min  | redundant with `apogee` (Kepler's third law) |
| `expected_lifetime`         | yrs  | UCS engineering forecast; missing values median-imputed |
| `expected_lifetime_missing` | 0/1  | indicator for the imputation above |

## Identifier / bookkeeping columns

`name`, `norad_id`, `launch_date`, `decay_date`, `end_date`, `activity_snapshot`,
plus the un-grouped raw versions `country`, `users`, `purpose`, `orbit_type`,
`launch_site`, `launch_vehicle` (kept for reference only).

## Modelling implications for Part C

* **Categorical variable forced into the Cox model**: `purpose_3` (chosen in Part B).
* **Avoid multicollinearity**: pick one of `{log_perigee, log_apogee}` and one of `{country_grouped, launch_vehicle_group}`; drop `period`.
* **Sample size / events**: 2 074 events is comfortable for a Cox model with ≈10 covariates (≥ 200 events per parameter).
* **Why parametric models still make sense in Part C(b)**: heavy administrative censoring at the right tail of `time_days` makes the Kaplan–Meier curves flatten well above 0; a parametric distribution can extrapolate past the snapshot.
