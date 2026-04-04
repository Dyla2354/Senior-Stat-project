library(dplyr)            # For data manipulation
library(tidyr)            # For reshaping data (long <-> wide)
library(ggplot2)          # For plotting
library(scales)           # For formatting axes (percent, comma, etc.)
library(MASS)
library(tidyverse)
library(nhanesA)
library(survey)
library(ggpubr)
library(corrplot)
library(broom)
library(ggeffects)
library(viridis)
library(segmented)

# ------------------------------------------------------------------------------
# Load Base Data
# ------------------------------------------------------------------------------
raw_data_12 <- read.csv(file.choose())  # Open file chooser to load CSV, for 12 Registries
raw_data_17 <- read.csv(file.choose())  # Open file chooser to load CSV, for 17 Registries
seer_rate_data_17 <- read.csv(file.choose()) #Open file chooser to load CSV for rates, 17 Registries
data_12 <- raw_data_12                  # Make a working copy for 12 Registries
data_17 <- raw_data_17                  # Make a working copy for 17 Registries
raw_rate_data_17 <- seer_rate_data_17
head(data_12)                           # Quick check of the first few rows
head(data_17)
head(seer_rate_data_17)

# ------------------------------------------------------------------------------
# SEER rate formatting 
# ------------------------------------------------------------------------------
seer_rate_data_17 <- seer_rate_data_17 %>%
  rename(
    Age_index = `Age.recode.with..1.year.olds.and.90.`,
    Year = Year.of.diagnosis,
    Cases = Count,
    Population = Population,
    Rate = Crude.Rate
  )

age_map <- c(
  "00", "01-04", "05-09", "10-14", "15-19",
  "20-24", "25-29", "30-34", "35-39", "40-44",
  "45-49", "50-54", "55-59", "60-64", "65-69",
  "70-74", "75-79", "80-84", "85-89", "90+"
)

seer_rate_data_17 <- seer_rate_data_17 %>%
  mutate(Age_range = age_map[Age_index + 1])

seer_rate_data_17 <- seer_rate_data_17 %>%
  mutate(
    Year = 2000 + Year
  )

head(seer_rate_data_17)

# ------------------------------------------------------------------------------
# SEER Rate Analysis
# ------------------------------------------------------------------------------
seer_rate_data_17 %>%
  filter(Age_range %in% c("00", "01-04", "05-09", "10-14", "15-19",
                          "20-24","25-29","30-34","35-39","40-44","45-49")) %>%
  group_by(Year, Age_range) %>%
  summarise(rate = sum(Cases)/sum(Population)*100000) %>%
  ggplot(aes(x = Year, y = rate, color = Age_range)) +
  geom_line() +
  theme_minimal()

# Compute Age Specific Rates
unique(seer_rate_data_17$Age_range)

seer_under50 <- seer_rate_data_17 %>%
  filter(Age_range %in% std_pop$Age_range)

seer_under50 <- seer_under50 %>%
  mutate(age_specific_rate = (Cases / Population) * 100000)


std_pop <- data.frame(
  Age_range = c("20-24","25-29","30-34","35-39","40-44","45-49"),
  weight = c(
    0.066,  # 20–24
    0.064,  # 25–29
    0.071,  # 30–34
    0.081,  # 35–39
    0.082,  # 40–44
    0.072   # 45–49
  )
) # These numbers come from the 2000 US standard population, which is the SEER standard

# Alignment Check
setdiff(seer_under50$Age_range, std_pop$Age_range)
setdiff(std_pop$Age_range, seer_under50$Age_range)

# Merge Weights
seer_under50 <- seer_under50 %>%
  left_join(std_pop, by = "Age_range")

# Age-adjusted rates
age_adjusted_rates <- seer_under50 %>%
  group_by(Year) %>%
  summarise(
    age_adjusted_rate = sum(age_specific_rate * weight, na.rm = TRUE),
    .groups = "drop"
  )

# Fit APC Model
apc_model <- lm(log(age_adjusted_rate) ~ Year, data = age_adjusted_rates)
summary(apc_model)

# Extract APC + 95% CI
beta <- coef(apc_model)["Year"]

APC <- (exp(beta) - 1) * 100

conf <- confint(apc_model)["Year", ]

APC_lower <- (exp(conf[1]) - 1) * 100
APC_upper <- (exp(conf[2]) - 1) * 100

APC
APC_lower
APC_upper

# Create Plot
  # Labels
apc_label <- paste0(
  "APC = ", round(APC, 2), "% (",
  round(APC_lower, 2), ", ",
  round(APC_upper, 2), ")"
)

ggplot(age_adjusted_rates, aes(x = Year, y = age_adjusted_rate)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  
  # Optional: smooth trend (log-linear fit)
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linetype = "dashed") +
  
  annotate(
    "text",
    x = min(age_adjusted_rates$Year) + 2,
    y = max(age_adjusted_rates$age_adjusted_rate),
    label = apc_label,
    hjust = 0,
    size = 5
  ) +
  
  labs(
    title = "Age-Adjusted Incidence of Early-Onset Colorectal Cancer",
    subtitle = "SEER 17 Registries, Age <50",
    x = "Year",
    y = "Incidence per 100,000 (Age-adjusted to 2000 U.S. standard population)"
  ) +
  
  theme_minimal(base_size = 14)

# ------------------------------------------------------------------------------
# Examine frequency of each variable
# ------------------------------------------------------------------------------
#lapply(data_12, table)  # Shows counts of each unique value per column
#lapply(data_17, table)  # Shows counts of each unique value per column

# ------------------------------------------------------------------------------
# Add NHANES_cycle column
# ------------------------------------------------------------------------------
# NHANES cycles are 2-year intervals
data_12_with_NHANES_cycle <- data_12 %>%
  mutate(NHANES_cycle = paste0(
    Year.of.diagnosis - (Year.of.diagnosis - 1999) %% 2,    # Start year
    "-",
    Year.of.diagnosis - (Year.of.diagnosis - 1999) %% 2 + 1 # End year
  ))
data_17_with_NHANES_cycle <- data_17 %>%
  mutate(NHANES_cycle = paste0(
    Year.of.diagnosis - (Year.of.diagnosis - 1999) %% 2,    # Start year
    "-",
    Year.of.diagnosis - (Year.of.diagnosis - 1999) %% 2 + 1 # End year
  ))

# ------------------------------------------------------------------------------
# Clean Age column
# ------------------------------------------------------------------------------
df_12 <- data_12_with_NHANES_cycle %>%
  rename(Age_range = `Age.recode.with..1.year.olds.and.90.`) # Rename for clarity
df_17 <- data_17_with_NHANES_cycle %>%
  rename(Age_range = `Age.recode.with..1.year.olds.and.90.`) # Rename for clarity

# Remove the text "years" from Age_range
df_12 <- df_12 %>%
  mutate(Age_range = gsub(" years", "", Age_range))
df_17 <- df_17 %>%
  mutate(Age_range = gsub(" years", "", Age_range))

# Define ordered factor levels for plotting and analysis
age_levels <- c("00","01-04","05-09","10-14","15-19","20-24","25-29",
                "30-34","35-39","40-44","45-49",
                "50-54","55-59","60-64","65-69",
                "70-74","75-79","80-84","85-89","90+")
df_12$Age_range <- factor(df_12$Age_range, levels = age_levels, ordered = TRUE)
df_17$Age_range <- factor(df_17$Age_range, levels = age_levels, ordered = TRUE)

# Condense Age_range into <40, 40-44, 45-49, 50+
df_12 <- df_12 %>%
  mutate(Age_group = case_when(
    Age_range %in% c("00","01-04","05-09","10-14","15-19",
                     "20-24","25-29","30-34","35-39") ~ "<40",
    Age_range == "40-44" ~ "40-44",
    Age_range == "45-49" ~ "45-49",
    Age_range %in% c("50-54","55-59","60-64","65-69",
                     "70-74","75-79","80-84","85-89","90+") ~ "50+",
    TRUE ~ NA_character_
  ))
df_12$Age_group <- factor(df_12$Age_group,
                          levels = c("<40", "40-44", "45-49", "50+"),
                          ordered = TRUE)
df_17 <- df_17 %>%
  mutate(Age_group = case_when(
    Age_range %in% c("00","01-04","05-09","10-14","15-19",
                     "20-24","25-29","30-34","35-39") ~ "<40",
    Age_range == "40-44" ~ "40-44",
    Age_range == "45-49" ~ "45-49",
    Age_range %in% c("50-54","55-59","60-64","65-69",
                     "70-74","75-79","80-84","85-89","90+") ~ "50+",
    TRUE ~ NA_character_
  ))
df_17$Age_group <- factor(df_17$Age_group,
                          levels = c("<40", "40-44", "45-49", "50+"),
                          ordered = TRUE)

# ------------------------------------------------------------------------------
# Calculate proportion of cases per age range and age group per NHANES cycle
# ------------------------------------------------------------------------------
# Age ranges
age_props_12 <- df_12 %>%
  group_by(NHANES_cycle, Age_range) %>%
  summarise(n = n(), .groups = "drop") %>%  # Count cases per age group per cycle
  tidyr::complete(NHANES_cycle, Age_range, fill = list(n = 0)) %>%
  group_by(NHANES_cycle) %>%
  mutate(proportion = n / sum(n))           # Convert counts to proportion
age_props_17 <- df_17 %>%
  group_by(NHANES_cycle, Age_range) %>%
  summarise(n = n(), .groups = "drop") %>%  # Count cases per age group per cycle
  tidyr::complete(NHANES_cycle, Age_range, fill = list(n = 0)) %>%
  group_by(NHANES_cycle) %>%
  mutate(proportion = n / sum(n))           # Convert counts to proportion
# Age groups
age_props_group_12 <- df_12 %>%
  group_by(NHANES_cycle, Age_group) %>%
  summarise(n = n(), .groups = "drop") %>%  # Count cases per age group per cycle
  tidyr::complete(NHANES_cycle, Age_group, fill = list(n = 0)) %>%
  group_by(NHANES_cycle) %>%
  mutate(proportion = n / sum(n))           # Convert counts to proportion
age_props_group_17 <- df_17 %>%
  group_by(NHANES_cycle, Age_group) %>%
  summarise(n = n(), .groups = "drop") %>%  # Count cases per age group per cycle
  tidyr::complete(NHANES_cycle, Age_group, fill = list(n = 0)) %>%
  group_by(NHANES_cycle) %>%
  mutate(proportion = n / sum(n))           # Convert counts to proportion

# Quick check: sums equal 1
age_props_12 %>%
  group_by(NHANES_cycle) %>%
  summarise(total = sum(proportion))
age_props_17 %>%
  group_by(NHANES_cycle) %>%
  summarise(total = sum(proportion))
age_props_group_12 %>%
  group_by(NHANES_cycle) %>%
  summarise(total = sum(proportion))
age_props_group_17 %>%
  group_by(NHANES_cycle) %>%
  summarise(total = sum(proportion))

# Pivot to wide format for easier viewing
age_props_12_wide <- age_props_12 %>%
  dplyr::select(-n) %>%
  tidyr::pivot_wider(
    names_from = Age_range,
    values_from = proportion
  )
age_props_17_wide <- age_props_17 %>%
  dplyr::select(-n) %>%
  tidyr::pivot_wider(
    names_from = Age_range,
    values_from = proportion
  )
age_props_group_12_wide <- age_props_group_12 %>%
  dplyr::select(-n) %>%
  tidyr::pivot_wider(
    names_from = Age_group,
    values_from = proportion
  )
age_props_group_17_wide <- age_props_group_17 %>%
  dplyr::select(-n) %>%
  tidyr::pivot_wider(
    names_from = Age_group,
    values_from = proportion
  )

# Quick check: sums equal 1
rowSums(age_props_12_wide[ , -1], na.rm = TRUE)  # exclude NHANES_cycle column
rowSums(age_props_17_wide[ , -1], na.rm = TRUE)
rowSums(age_props_group_12_wide[ , -1], na.rm = TRUE) 
rowSums(age_props_group_17_wide[ , -1], na.rm = TRUE)


# ------------------------------------------------------------------------------
# Plot proportion of age range per NHANES cycle
# ------------------------------------------------------------------------------
ggplot(age_props_12, aes(x = NHANES_cycle, y = proportion, fill = Age_range)) +
  geom_col() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(option = "turbo") +
  labs(y = "Proportion", x = "NHANES Cycle") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(age_props_17, aes(x = NHANES_cycle, y = proportion, fill = Age_range)) +
  geom_col() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(option = "turbo") +
  labs(y = "Proportion", x = "NHANES Cycle") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Interpretation:
# - Each bar shows the age distribution of cases in a given NHANES cycle.
# - Taller bars for younger ages indicate more cases among young adults.
# - Compare cycles to see whether younger age groups are increasing relative to older groups.

# Individual plots by cycle
ggplot(age_props_12, aes(x = Age_range, y = proportion, fill = Age_range)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ NHANES_cycle) +
  theme_minimal() +
  labs(x = "Age Range", y = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5))
ggplot(age_props_17, aes(x = Age_range, y = proportion, fill = Age_range)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ NHANES_cycle) +
  theme_minimal() +
  labs(x = "Age Range", y = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5))
# Interpretation:
# - Each panel represents one NHANES cycle.
# - Look for shifts in the peak proportion of cases.
# - If younger age bars are taller over time, it signals increasing proportion in younger adults.

# Axis flipped version for readability
ggplot(age_props_12, aes(x = Age_range, y = proportion, fill = Age_range)) + 
  geom_col() +
  facet_wrap(~ NHANES_cycle) +
  theme_minimal() +
  labs(x = "Age Range", y = "Proportion", fill = "Age Range") +
  theme(
    axis.text.y = element_text(size = 5)
  ) + 
  coord_flip()
ggplot(age_props_17, aes(x = Age_range, y = proportion, fill = Age_range)) + 
  geom_col() +
  facet_wrap(~ NHANES_cycle) +
  theme_minimal() +
  labs(x = "Age Range", y = "Proportion", fill = "Age Range") +
  theme(
    axis.text.y = element_text(size = 5)
  ) + 
  coord_flip()

# ------------------------------------------------------------------------------
# Plot proportion of age group per NHANES cycle
# ------------------------------------------------------------------------------
ggplot(age_props_group_12, aes(x = NHANES_cycle, y = proportion, fill = Age_group)) +
  geom_col() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(option = "turbo") +
  labs(y = "Proportion", x = "NHANES Cycle") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(age_props_group_17, aes(x = NHANES_cycle, y = proportion, fill = Age_group)) +
  geom_col() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(option = "turbo") +
  labs(y = "Proportion", x = "NHANES Cycle") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Interpretation:
# - Each bar shows the age distribution of cases in a given NHANES cycle.
# - Taller bars for younger ages indicate more cases among young adults.
# - Compare cycles to see whether younger age groups are increasing relative to older groups.

# Individual plots by cycle
ggplot(age_props_group_12, aes(x = Age_group, y = proportion, fill = Age_group)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ NHANES_cycle) +
  theme_minimal() +
  labs(x = "Age Group", y = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5))
ggplot(age_props_group_17, aes(x = Age_group, y = proportion, fill = Age_group)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ NHANES_cycle) +
  theme_minimal() +
  labs(x = "Age Group", y = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5))
# Interpretation:
# - Each panel represents one NHANES cycle.
# - Look for shifts in the peak proportion of cases.
# - If younger age bars are taller over time, it signals increasing proportion in younger adults.

# Axis flipped version for readability
ggplot(age_props_group_12, aes(x = Age_group, y = proportion, fill = Age_group)) + 
  geom_col() +
  facet_wrap(~ NHANES_cycle) +
  theme_minimal() +
  labs(x = "Age Group", y = "Proportion", fill = "Age Group") +
  theme(
    axis.text.y = element_text(size = 5)
  ) + 
  coord_flip()
ggplot(age_props_group_17, aes(x = Age_group, y = proportion, fill = Age_group)) + 
  geom_col() +
  facet_wrap(~ NHANES_cycle) +
  theme_minimal() +
  labs(x = "Age Range", y = "Proportion", fill = "Age Range") +
  theme(
    axis.text.y = element_text(size = 5)
  ) + 
  coord_flip()

# ------------------------------------------------------------------------------
# Focus on younger age groups (<50)
# ------------------------------------------------------------------------------
young_ages <- c("00","01-04","05-09","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49")

young_age_props_12 <- age_props_12 %>%
  filter(Age_range %in% young_ages)
young_age_props_17 <- age_props_17 %>%
  filter(Age_range %in% young_ages)

ggplot(young_age_props_12, aes(x = NHANES_cycle, y = proportion, fill = Age_range)) +
  geom_col(position = "stack") +
  theme_minimal() +
  labs(
    x = "NHANES Cycle",
    y = "Proportion of Cases",
    title = "Proportion of Colorectal Cancer Cases in Younger Age Groups Over Time"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(young_age_props_17, aes(x = NHANES_cycle, y = proportion, fill = Age_range)) +
  geom_col(position = "stack") +
  theme_minimal() +
  labs(
    x = "NHANES Cycle",
    y = "Proportion of Cases",
    title = "Proportion of Colorectal Cancer Cases in Younger Age Groups Over Time"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Interpretation:
# - Stacked bars show how each young age group contributes to total cases under 50.
# - Rising bars over cycles indicate an increasing burden of younger-onset colorectal cancer.
# - If specific younger groups (e.g., 30-39) grow faster, that’s a focus for early screening.

# ------------------------------------------------------------------------------
# Convert Age_range to approximate numeric midpoint
# ------------------------------------------------------------------------------
age_props_12 <- age_props_12 %>%
  mutate(Age_midpoint = case_when(
    Age_range == "00"    ~ 0,
    Age_range == "01-04" ~ 2.5,
    Age_range == "05-09" ~ 7,
    Age_range == "10-14" ~ 12,
    Age_range == "15-19" ~ 17,
    Age_range == "20-24" ~ 22,
    Age_range == "25-29" ~ 27,
    Age_range == "30-34" ~ 32,
    Age_range == "35-39" ~ 37,
    Age_range == "40-44" ~ 42,
    Age_range == "45-49" ~ 47,
    Age_range == "50-54" ~ 52,
    Age_range == "55-59" ~ 57,
    Age_range == "60-64" ~ 62,
    Age_range == "65-69" ~ 67,
    Age_range == "70-74" ~ 72,
    Age_range == "75-79" ~ 77,
    Age_range == "80-84" ~ 82,
    Age_range == "85-89" ~ 87,
    Age_range == "90+"   ~ 92
  ))
age_props_17 <- age_props_17 %>%
  mutate(Age_midpoint = case_when(
    Age_range == "00"    ~ 0,
    Age_range == "01-04" ~ 2.5,
    Age_range == "05-09" ~ 7,
    Age_range == "10-14" ~ 12,
    Age_range == "15-19" ~ 17,
    Age_range == "20-24" ~ 22,
    Age_range == "25-29" ~ 27,
    Age_range == "30-34" ~ 32,
    Age_range == "35-39" ~ 37,
    Age_range == "40-44" ~ 42,
    Age_range == "45-49" ~ 47,
    Age_range == "50-54" ~ 52,
    Age_range == "55-59" ~ 57,
    Age_range == "60-64" ~ 62,
    Age_range == "65-69" ~ 67,
    Age_range == "70-74" ~ 72,
    Age_range == "75-79" ~ 77,
    Age_range == "80-84" ~ 82,
    Age_range == "85-89" ~ 87,
    Age_range == "90+"   ~ 92
  ))

df_12 <- df_12 %>%
  mutate(Age_midpoint = case_when(
    Age_range == "00"    ~ 0,
    Age_range == "01-04" ~ 2.5,
    Age_range == "05-09" ~ 7,
    Age_range == "10-14" ~ 12,
    Age_range == "15-19" ~ 17,
    Age_range == "20-24" ~ 22,
    Age_range == "25-29" ~ 27,
    Age_range == "30-34" ~ 32,
    Age_range == "35-39" ~ 37,
    Age_range == "40-44" ~ 42,
    Age_range == "45-49" ~ 47,
    Age_range == "50-54" ~ 52,
    Age_range == "55-59" ~ 57,
    Age_range == "60-64" ~ 62,
    Age_range == "65-69" ~ 67,
    Age_range == "70-74" ~ 72,
    Age_range == "75-79" ~ 77,
    Age_range == "80-84" ~ 82,
    Age_range == "85-89" ~ 87,
    Age_range == "90+"   ~ 92
  ))
df_17 <- df_17 %>%
  mutate(Age_midpoint = case_when(
    Age_range == "00"    ~ 0,
    Age_range == "01-04" ~ 2.5,
    Age_range == "05-09" ~ 7,
    Age_range == "10-14" ~ 12,
    Age_range == "15-19" ~ 17,
    Age_range == "20-24" ~ 22,
    Age_range == "25-29" ~ 27,
    Age_range == "30-34" ~ 32,
    Age_range == "35-39" ~ 37,
    Age_range == "40-44" ~ 42,
    Age_range == "45-49" ~ 47,
    Age_range == "50-54" ~ 52,
    Age_range == "55-59" ~ 57,
    Age_range == "60-64" ~ 62,
    Age_range == "65-69" ~ 67,
    Age_range == "70-74" ~ 72,
    Age_range == "75-79" ~ 77,
    Age_range == "80-84" ~ 82,
    Age_range == "85-89" ~ 87,
    Age_range == "90+"   ~ 92
  ))

# ------------------------------------------------------------------------------
# Median age per NHANES cycle
# ------------------------------------------------------------------------------
median_age_12 <- age_props_12 %>%
  group_by(NHANES_cycle) %>%
  arrange(Age_midpoint) %>%
  mutate(cum_prop = cumsum(proportion)) %>%
  summarise(
    Median_Age = Age_midpoint[which(cum_prop >= 0.5)[1]]
  )
median_age_17 <- age_props_17 %>%
  group_by(NHANES_cycle) %>%
  arrange(Age_midpoint) %>%
  mutate(cum_prop = cumsum(proportion)) %>%
  summarise(
    Median_Age = Age_midpoint[which(cum_prop >= 0.5)[1]]
  )

ggplot(median_age_12, aes(x = NHANES_cycle, y = Median_Age, group = 1)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(
    x = "NHANES Cycle",
    y = "Median Age at Diagnosis",
    title = "Trend in Median Age at Diagnosis Over Time"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(median_age_17, aes(x = NHANES_cycle, y = Median_Age, group = 1)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(
    x = "NHANES Cycle",
    y = "Median Age at Diagnosis",
    title = "Trend in Median Age at Diagnosis Over Time"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Interpretation:
# - Line shows how the central tendency of age at diagnosis shifts over time.
# - A downward trend → younger onset is becoming more common.
# - Flat or upward trend → median age stable or increasing.

# ------------------------------------------------------------------------------
# Proportion of cases under age 50
# ------------------------------------------------------------------------------
under50_prop_12 <- age_props_12 %>%
  mutate(Under50 = ifelse(Age_midpoint < 50, proportion, 0)) %>%
  group_by(NHANES_cycle) %>%
  summarise(
    Proportion_Under50 = sum(Under50)
  )
under50_prop_17 <- age_props_17 %>%
  mutate(Under50 = ifelse(Age_midpoint < 50, proportion, 0)) %>%
  group_by(NHANES_cycle) %>%
  summarise(
    Proportion_Under50 = sum(Under50)
  )

ggplot(under50_prop_12, aes(x = NHANES_cycle, y = Proportion_Under50, group = 1)) +
  geom_line() + geom_point() +
  theme_minimal() +
  labs(
    x = "NHANES Cycle",
    y = "Proportion of Cases Under 50",
    title = "Proportion of Colorectal Cancer Cases Under Age 50 Over Time"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(under50_prop_17, aes(x = NHANES_cycle, y = Proportion_Under50, group = 1)) +
  geom_line() + geom_point() +
  theme_minimal() +
  labs(
    x = "NHANES Cycle",
    y = "Proportion of Cases Under 50",
    title = "Proportion of Colorectal Cancer Cases Under Age 50 Over Time"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Interpretation:
# - Shows the relative burden of younger-onset cases.
# - Rising line → true increase in younger-onset CRC or improved detection.
# - Plateau → younger proportion stable.

# ------------------------------
# Statistical trend analysis
# ------------------------------
# Convert NHANES_cycle to numeric start year for regression
under50_prop_12 <- under50_prop_12 %>%
  mutate(Start_Year = as.numeric(substr(NHANES_cycle, 1, 4)))
under50_prop_17 <- under50_prop_17 %>%
  mutate(Start_Year = as.numeric(substr(NHANES_cycle, 1, 4)))

# Linear trend (ordinary least squares)
lm_fit_12 <- lm(Proportion_Under50 ~ Start_Year, data = under50_prop_12)
summary(lm_fit_12)
lm_fit_17 <- lm(Proportion_Under50 ~ Start_Year, data = under50_prop_17)
summary(lm_fit_17)
# Interpretation:
# - Check coefficient for Start_Year:
#    * Positive & significant → proportion of cases <50 increases over time.
#    * p-value < 0.05 → statistically significant trend.
# - R-squared → fraction of variation in proportion explained by year.

# Binomial Model
under50_counts_12 <- df_12 %>%
  mutate(Under50 = ifelse(Age_midpoint < 50, 1, 0)) %>%
  group_by(NHANES_cycle) %>%
  summarise(
    Under50_cases = sum(Under50),
    Total_Cases = n(),
    Start_Year = as.numeric(substr(NHANES_cycle, 1, 4)),
    .groups = "drop"
  )
glm_fit_12 <- glm(
  cbind(Under50_cases, Total_Cases - Under50_cases) ~ Start_Year,
  family = binomial,
  data = under50_counts_17
)
summary(glm_fit_17)

under50_counts_17 <- df_17 %>%
  mutate(Under50 = ifelse(Age_midpoint < 50, 1, 0)) %>%
  group_by(NHANES_cycle) %>%
  summarise(
    Under50_cases = sum(Under50),
    Total_Cases = n(),
    Start_Year = as.numeric(substr(NHANES_cycle, 1, 4)),
    .groups = "drop"
  )
glm_fit_17 <- glm(
  cbind(Under50_cases, Total_Cases - Under50_cases) ~ Start_Year,
  family = binomial,
  data = under50_counts_17
)
summary(glm_fit_17)

# Segmented Regression
seg_fit_12 <- segmented(lm(Proportion_Under50 ~ Start_Year, data=under50_prop_12),
                        seg.Z = ~Start_Year)
summary(seg_fit_12)
seg_fit_17 <- segmented(lm(Proportion_Under50 ~ Start_Year, data=under50_prop_17),
                     seg.Z = ~Start_Year)
summary(seg_fit_17)

# Plot with regression line
ggplot(under50_prop_12, aes(x = Start_Year, y = Proportion_Under50)) +
  geom_point(color = "#1f78b4", size = 3) +         # points for each NHANES cycle
  geom_line(color = "#1f78b4", group = 1) +         # connect the points
  geom_smooth(method = "lm", se = TRUE, color = "#e31a1c", linetype = "dashed") + # regression line
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Year of Diagnosis",
    y = "Proportion of Cases Under Age 50",
    title = "Increasing Proportion of Colorectal Cancer Diagnosed Under Age 50",
    subtitle = "Red dashed line shows linear trend over time",
    caption = "Data: SEER (1992–2022)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggplot(under50_prop_17, aes(x = Start_Year, y = Proportion_Under50)) +
  geom_point(color = "#1f78b4", size = 3) +         # points for each NHANES cycle
  geom_line(color = "#1f78b4", group = 1) +         # connect the points
  geom_smooth(method = "lm", se = TRUE, color = "#e31a1c", linetype = "dashed") + # regression line
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Year of Diagnosis",
    y = "Proportion of Cases Under Age 50",
    title = "Increasing Proportion of Colorectal Cancer Diagnosed Under Age 50",
    subtitle = "Red dashed line shows linear trend over time",
    caption = "Data: SEER (1992–2022)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
# Interpretation:
# - Regression line confirms trend visually.
# - Confidence interval (shaded area) shows uncertainty around trend estimate.
# - Slope indicates rate of change in proportion under 50 per year.

# Plot with two regression lines broken at 2014
proportion_under50_17 <- proportion_under50_17 %>%
  mutate(
    Start_Year = as.numeric(substr(NHANES_cycle, 1, 4))
  )

pre_2014 <- proportion_under50_17 %>%
  filter(Start_Year <= 2014)

post_2014 <- proportion_under50_17 %>%
  filter(Start_Year >= 2014)

lm_pre <- lm(Proportion_Under50 ~ Start_Year, data = pre_2014)
lm_post <- lm(Proportion_Under50 ~ Start_Year, data = post_2014)

pre_2014$fit <- predict(lm_pre)
post_2014$fit <- predict(lm_post)

slope_pre <- round(coef(lm_pre)["Start_Year"], 4)
slope_post <- round(coef(lm_post)["Start_Year"], 4)

ggplot(proportion_under50_17, aes(x = Start_Year, y = Proportion_Under50)) +
  
  # raw data
  geom_point(size = 2) +
  geom_line(alpha = 0.6) +
  
  # pre-2014 model
  geom_line(
    data = pre_2014,
    aes(y = fit),
    size = 1.2
  ) +
  
  # post-2014 model
  geom_line(
    data = post_2014,
    aes(y = fit),
    size = 1.2,
    linetype = "dashed"
  ) +
  
  # breakpoint
  geom_vline(xintercept = 2014, linetype = "dotted") +
  
  labs(
    title = "Proportion of CRC Cases Under Age 50 Over Time",
    subtitle = "Piecewise Linear Trends (Pre-2014 vs Post-2014)",
    x = "Year",
    y = "Proportion Under 50"
  ) +
  
  theme_minimal(base_size = 14) + annotate("text", x = 2003, y = 0.10,
                                           label = paste0("Pre-2014 slope: ", slope_pre)) +
  annotate("text", x = 2016, y = 0.13,
           label = paste0("Post-2014 slope: ", slope_post))

coef(lm_pre)["Start_Year"]
coef(lm_post)["Start_Year"]

# Polynomial Trend
pm_fit_12 <- lm(Proportion_Under50 ~ poly(Start_Year, 2), data = under50_prop_12)
summary(pm_fit_12)
pm_fit_17 <- lm(Proportion_Under50 ~ poly(Start_Year, 2), data = under50_prop_17)
summary(pm_fit_17)

# ------------------------------------------------------------------------------
# Investigate tumor characteristics
# ------------------------------------------------------------------------------
# Tumor deposits (binary 0/1)
df_12 <- df_12 %>%
  mutate(Tumor_Deposit_Present = case_when(
    Tumor.Deposits.Recode..2010.. %in% c("Blank(s)", "No tumor deposits", 
                                         "Not documented/assessed; Indeterminate; No mention in path report; No resection") ~ 0,
    TRUE ~ 1  # any documented tumor deposits
  ))
df_17 <- df_17 %>%
  mutate(Tumor_Deposit_Present = case_when(
    Tumor.Deposits.Recode..2010.. %in% c("Blank(s)", "No tumor deposits", 
                                         "Not documented/assessed; Indeterminate; No mention in path report; No resection") ~ 0,
    TRUE ~ 1  # any documented tumor deposits
  ))

# Subset for post-2010 when tumor deposits recorded
df_12_post2010 <- df_12 %>% filter(Year.of.diagnosis >= 2010)
df_17_post2010 <- df_17 %>% filter(Year.of.diagnosis >= 2010)

tumor_trend_under50_post2010_12 <- df_12_post2010 %>%
  filter(Age_midpoint < 50) %>%
  group_by(NHANES_cycle) %>%
  summarise(Proportion_With_Deposits = mean(Tumor_Deposit_Present, na.rm = TRUE))
tumor_trend_under50_post2010_17 <- df_17_post2010 %>%
  filter(Age_midpoint < 50) %>%
  group_by(NHANES_cycle) %>%
  summarise(Proportion_With_Deposits = mean(Tumor_Deposit_Present, na.rm = TRUE))

# Plot tumor deposits trend
ggplot(tumor_trend_under50_post2010_12, aes(x = NHANES_cycle, y = Proportion_With_Deposits, group = 1)) +
  geom_line(color = "#1f78b4") +
  geom_point(color = "#1f78b4") +
  geom_smooth(method = "lm", se = TRUE, color = "#e31a1c", linetype = "dashed") +
  theme_minimal() +
  labs(
    x = "NHANES Cycle",
    y = "Proportion of Younger Cases With Tumor Deposits",
    title = "Aggressiveness Indicator in Younger-Onset Cases (2010+)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(tumor_trend_under50_post2010_17, aes(x = NHANES_cycle, y = Proportion_With_Deposits, group = 1)) +
  geom_line(color = "#1f78b4") +
  geom_point(color = "#1f78b4") +
  geom_smooth(method = "lm", se = TRUE, color = "#e31a1c", linetype = "dashed") +
  theme_minimal() +
  labs(
    x = "NHANES Cycle",
    y = "Proportion of Younger Cases With Tumor Deposits",
    title = "Aggressiveness Indicator in Younger-Onset Cases (2010+)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Interpretation:
# - Tumor deposits are an indicator of more aggressive cancer.
# - Rising trend → younger patients increasingly present with aggressive features.
# - Flat trend → earlier detection may explain higher proportion without increased aggressiveness.

# ------------------------------------------------------------------------------
# Tumor size conversion
# ------------------------------------------------------------------------------
df_12 <- df_12 %>%
  mutate(
    tumor_raw = as.character(CS.tumor.size..2004.2015.),
    
    # Replace known non-numeric values first
    tumor_clean = case_when(
      tumor_raw %in% c("Blank(s)", "000","995","996","997","998","999","888","1022") ~ NA_character_,
      tumor_raw == "990" ~ "0.5",
      tumor_raw == "991" ~ "10",
      tumor_raw == "992" ~ "20",
      tumor_raw == "993" ~ "30",
      tumor_raw == "994" ~ "40",
      TRUE ~ tumor_raw
    ),
    
    # Now safely convert ONCE
    Tumor_Size_Num = suppressWarnings(as.numeric(tumor_clean))
  )
df_17 <- df_17 %>%
  mutate(
    tumor_raw = as.character(CS.tumor.size..2004.2015.),
    
    # Replace known non-numeric values first
    tumor_clean = case_when(
      tumor_raw %in% c("Blank(s)", "000","995","996","997","998","999","888","1022") ~ NA_character_,
      tumor_raw == "990" ~ "0.5",
      tumor_raw == "991" ~ "10",
      tumor_raw == "992" ~ "20",
      tumor_raw == "993" ~ "30",
      tumor_raw == "994" ~ "40",
      TRUE ~ tumor_raw
    ),
    
    # Now safely convert ONCE
    Tumor_Size_Num = suppressWarnings(as.numeric(tumor_clean))
  )
# case_when() evaluates all RHS expressions, which can trigger coercion warnings.
# To avoid this, we first clean and standardize tumor size values as character,
# then convert to numeric in a single step.

# Median tumor size under 50
tumor_size_trend_12 <- df_12 %>%
  filter(Age_midpoint < 50 & !is.na(Tumor_Size_Num)) %>%
  group_by(NHANES_cycle) %>%
  summarise(Median_Tumor_Size = median(Tumor_Size_Num))
tumor_size_trend_17 <- df_17 %>%
  filter(Age_midpoint < 50 & !is.na(Tumor_Size_Num)) %>%
  group_by(NHANES_cycle) %>%
  summarise(Median_Tumor_Size = median(Tumor_Size_Num))

ggplot(tumor_size_trend_12, aes(x = NHANES_cycle, y = Median_Tumor_Size, group = 1)) +
  geom_line(color = "#1f78b4") +
  geom_point(color = "#1f78b4", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "#e31a1c", linetype = "dashed") +
  theme_minimal(base_size = 14) +
  labs(
    x = "NHANES Cycle",
    y = "Median Tumor Size (mm)",
    title = "Median Tumor Size in Patients Under 50 Over Time",
    subtitle = "Dashed red line shows linear trend"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(tumor_size_trend_17, aes(x = NHANES_cycle, y = Median_Tumor_Size, group = 1)) +
  geom_line(color = "#1f78b4") +
  geom_point(color = "#1f78b4", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "#e31a1c", linetype = "dashed") +
  theme_minimal(base_size = 14) +
  labs(
    x = "NHANES Cycle",
    y = "Median Tumor Size (mm)",
    title = "Median Tumor Size in Patients Under 50 Over Time",
    subtitle = "Dashed red line shows linear trend"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Interpretation:
# - Each point represents the median tumor size for patients under 50 in that cycle.
# - Downward trend → cancers are being detected at smaller sizes (suggests earlier detection).
# - Upward trend → cancers may be more advanced at diagnosis (possible biological shift).
# - Flat trend → no major change in tumor size at diagnosis over time.

# Mean tumor size under 50
tumor_deposits_trend_12 <- df_12 %>%
  filter(Age_midpoint < 50 & Year.of.diagnosis >= 2010) %>%
  group_by(NHANES_cycle) %>%
  summarise(Proportion_With_Deposits = mean(Tumor_Deposit_Present, na.rm = TRUE))
tumor_deposits_trend_17 <- df_17 %>%
  filter(Age_midpoint < 50 & Year.of.diagnosis >= 2010) %>%
  group_by(NHANES_cycle) %>%
  summarise(Proportion_With_Deposits = mean(Tumor_Deposit_Present, na.rm = TRUE))

ggplot(tumor_deposits_trend_12, aes(x = NHANES_cycle, y = Proportion_With_Deposits, group = 1)) +
  geom_line(color = "#33a02c") +
  geom_point(color = "#33a02c", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "#e31a1c", linetype = "dashed") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal(base_size = 14) +
  labs(
    x = "NHANES Cycle",
    y = "Proportion with Tumor Deposits",
    title = "Tumor Deposits in Patients Under 50 (2010+)",
    subtitle = "Indicator of tumor aggressiveness; dashed red line = trend"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(tumor_deposits_trend_17, aes(x = NHANES_cycle, y = Proportion_With_Deposits, group = 1)) +
  geom_line(color = "#33a02c") +
  geom_point(color = "#33a02c", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "#e31a1c", linetype = "dashed") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal(base_size = 14) +
  labs(
    x = "NHANES Cycle",
    y = "Proportion with Tumor Deposits",
    title = "Tumor Deposits in Patients Under 50 (2010+)",
    subtitle = "Indicator of tumor aggressiveness; dashed red line = trend"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Interpretation:
# - Tumor deposits are a marker of more aggressive disease.
# - Increasing trend → younger patients are presenting with more aggressive tumors.
# - Decreasing trend → cancers may be detected earlier or treated sooner.
# - Flat trend → no major change in aggressiveness over time.

# ------------------------------------------------------------------------------
# Combine multiple trends for plotting
# ------------------------------------------------------------------------------
plot_data_12 <- bind_rows(
  under50_prop_12 %>% dplyr::select(NHANES_cycle, value = Proportion_Under50) %>% mutate(Measure="Proportion <50"),
  tumor_size_trend_12 %>% dplyr::select(NHANES_cycle, value = Median_Tumor_Size) %>% mutate(Measure="Median Tumor Size (mm)"),
  tumor_deposits_trend_12 %>% dplyr::select(NHANES_cycle, value = Proportion_With_Deposits) %>% mutate(Measure="Tumor Deposits Proportion")
)
plot_data_17 <- bind_rows(
  under50_prop_17 %>% dplyr::select(NHANES_cycle, value = Proportion_Under50) %>% mutate(Measure="Proportion <50"),
  tumor_size_trend_17 %>% dplyr::select(NHANES_cycle, value = Median_Tumor_Size) %>% mutate(Measure="Median Tumor Size (mm)"),
  tumor_deposits_trend_17 %>% dplyr::select(NHANES_cycle, value = Proportion_With_Deposits) %>% mutate(Measure="Tumor Deposits Proportion")
)

ggplot(plot_data_12, aes(x = NHANES_cycle, y = value, group = 1)) +
  geom_line(color = "#1f78b4") +
  geom_point(color = "#1f78b4") +
  geom_smooth(method = "lm", se = TRUE, color = "#e31a1c", linetype = "dashed") +
  facet_wrap(~Measure, scales="free_y", ncol=1) +
  theme_minimal(base_size = 14) +
  labs(
    x = "NHANES Cycle",
    y = NULL,
    title = "Younger-Onset Colorectal Cancer Trends",
    subtitle = "Top: Proportion under 50 | Middle: Median tumor size | Bottom: Tumor deposits (2010+)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(plot_data_17, aes(x = NHANES_cycle, y = value, group = 1)) +
  geom_line(color = "#1f78b4") +
  geom_point(color = "#1f78b4") +
  geom_smooth(method = "lm", se = TRUE, color = "#e31a1c", linetype = "dashed") +
  facet_wrap(~Measure, scales="free_y", ncol=1) +
  theme_minimal(base_size = 14) +
  labs(
    x = "NHANES Cycle",
    y = NULL,
    title = "Younger-Onset Colorectal Cancer Trends",
    subtitle = "Top: Proportion under 50 | Middle: Median tumor size | Bottom: Tumor deposits (2010+)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ------------------------------------------------------------------------------
# Absolute number of cases vs proportion
# ------------------------------------------------------------------------------
df_12 <- df_12 %>%
  mutate(
    Age_Group = ifelse(Age_midpoint < 50, "<50", "50+"),
    Under50 = ifelse(Age_midpoint < 50, 1, 0)
  )
df_17 <- df_17 %>%
  mutate(
    Age_Group = ifelse(Age_midpoint < 50, "<50", "50+"),
    Under50 = ifelse(Age_midpoint < 50, 1, 0)
  )

cases_by_age_12 <- df_12 %>%
  group_by(NHANES_cycle, Age_Group) %>%
  summarise(Cases = n(), .groups = "drop")
cases_by_age_17 <- df_17 %>%
  group_by(NHANES_cycle, Age_Group) %>%
  summarise(Cases = n(), .groups = "drop")

proportion_under50_12 <- df_12 %>%
  group_by(NHANES_cycle) %>%
  summarise(Proportion_Under50 = mean(Under50), .groups = "drop")
proportion_under50_17 <- df_17 %>%
  group_by(NHANES_cycle) %>%
  summarise(Proportion_Under50 = mean(Under50), .groups = "drop")

plot_data_12 <- cases_by_age_12 %>%
  pivot_wider(names_from = Age_Group, values_from = Cases) %>%
  left_join(proportion_under50_12, by = "NHANES_cycle") %>%
  pivot_longer(cols = c("<50", "50+", "Proportion_Under50"),
               names_to = "Measure", values_to = "Value")
plot_data_17 <- cases_by_age_17 %>%
  pivot_wider(names_from = Age_Group, values_from = Cases) %>%
  left_join(proportion_under50_17, by = "NHANES_cycle") %>%
  pivot_longer(cols = c("<50", "50+", "Proportion_Under50"),
               names_to = "Measure", values_to = "Value")

ggplot(plot_data_12, aes(x = NHANES_cycle, y = Value, group = Measure,
                      color = Measure)) +
  geom_line(linewidth = 1) +
  geom_point() +
  facet_wrap(~Measure, scales = "free_y", ncol = 1,
             labeller = labeller(Measure = c("<50" = "Cases <50",
                                             "50+" = "Cases 50+",
                                             "Proportion_Under50" = "Proportion <50"))) +
  theme_minimal(base_size = 14) +
  labs(
    x = "NHANES Cycle",
    y = NULL,
    title = "Trends in Younger-Onset Colorectal Cancer",
    subtitle = "Top: Cases <50 | Middle: Cases 50+ | Bottom: Proportion of cases <50"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
ggplot(plot_data_17, aes(x = NHANES_cycle, y = Value, group = Measure,
                         color = Measure)) +
  geom_line(linewidth = 1) +
  geom_point() +
  facet_wrap(~Measure, scales = "free_y", ncol = 1,
             labeller = labeller(Measure = c("<50" = "Cases <50",
                                             "50+" = "Cases 50+",
                                             "Proportion_Under50" = "Proportion <50"))) +
  theme_minimal(base_size = 14) +
  labs(
    x = "NHANES Cycle",
    y = NULL,
    title = "Trends in Younger-Onset Colorectal Cancer",
    subtitle = "Top: Cases <50 | Middle: Cases 50+ | Bottom: Proportion of cases <50"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
# Interpretation:
# - Top panel: absolute number of <50 cases. Rising → true increase.
# - Middle panel: cases 50+. Stable or declining → relative proportion of <50 rises partly due to older cases.
# - Bottom panel: proportion <50 → confirms overall trend combining absolute and relative perspective.

plot_data_12 <- cases_by_age_12 %>%
  pivot_wider(names_from = Age_Group, values_from = Cases) %>%
  left_join(proportion_under50_12, by = "NHANES_cycle") %>%
  left_join(tumor_size_trend_12, by = "NHANES_cycle") %>%
  left_join(tumor_deposits_trend_12, by = "NHANES_cycle") %>%
  pivot_longer(cols = c("<50","50+","Proportion_Under50","Median_Tumor_Size","Proportion_With_Deposits"),
               names_to = "Measure", values_to = "Value")
plot_data_17 <- cases_by_age_17 %>%
  pivot_wider(names_from = Age_Group, values_from = Cases) %>%
  left_join(proportion_under50_17, by = "NHANES_cycle") %>%
  left_join(tumor_size_trend_17, by = "NHANES_cycle") %>%
  left_join(tumor_deposits_trend_17, by = "NHANES_cycle") %>%
  pivot_longer(cols = c("<50","50+","Proportion_Under50","Median_Tumor_Size","Proportion_With_Deposits"),
               names_to = "Measure", values_to = "Value")

plot_data_clean_12 <- plot_data_12 %>%
  filter(!is.na(Value))
plot_data_clean_17 <- plot_data_17 %>%
  filter(!is.na(Value))

ggplot(plot_data_clean_12, aes(x = NHANES_cycle, y = Value, group = 1)) +
  geom_line(color = "#1f78b4") +
  geom_point(color = "#1f78b4") +
  geom_smooth(method = "lm", se = TRUE, color = "#e31a1c", linetype = "dashed") +
  facet_wrap(~Measure, scales = "free_y", ncol = 1,
             labeller = labeller(Measure = c(
               "<50" = "Cases <50",
               "50+" = "Cases 50+",
               "Proportion_Under50" = "Proportion <50",
               "Median_Tumor_Size" = "Median Tumor Size <50 (mm)",
               "Proportion_With_Deposits" = "Tumor Deposits Proportion <50"
             ))) +
  theme_minimal(base_size = 14) +
  labs(
    x = "NHANES Cycle",
    y = NULL,
    title = "Younger-Onset Colorectal Cancer Trends and Tumor Characteristics",
    subtitle = "Counts, proportion under 50, median tumor size, and tumor deposits"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(plot_data_clean_17, aes(x = NHANES_cycle, y = Value, group = 1)) +
  geom_line(color = "#1f78b4") +
  geom_point(color = "#1f78b4") +
  geom_smooth(method = "lm", se = TRUE, color = "#e31a1c", linetype = "dashed") +
  facet_wrap(~Measure, scales = "free_y", ncol = 1,
             labeller = labeller(Measure = c(
               "<50" = "Cases <50",
               "50+" = "Cases 50+",
               "Proportion_Under50" = "Proportion <50",
               "Median_Tumor_Size" = "Median Tumor Size <50 (mm)",
               "Proportion_With_Deposits" = "Tumor Deposits Proportion <50"
             ))) +
  theme_minimal(base_size = 14) +
  labs(
    x = "NHANES Cycle",
    y = NULL,
    title = "Younger-Onset Colorectal Cancer Trends and Tumor Characteristics",
    subtitle = "Counts, proportion under 50, median tumor size, and tumor deposits"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# ------------------------------------------------------------------------------
# Statistical analysis: linear trends, weighted regression, Poisson
# ------------------------------------------------------------------------------
# Weighted linear regression for proportion <50
total_cases_12 <- df_12 %>%
  group_by(NHANES_cycle) %>%
  summarise(Total_Cases = n(), .groups = "drop")
total_cases_17 <- df_17 %>%
  group_by(NHANES_cycle) %>%
  summarise(Total_Cases = n(), .groups = "drop")

under50_prop_12 <- under50_prop_12 %>%
  left_join(total_cases_12, by = "NHANES_cycle")
under50_prop_17 <- under50_prop_17 %>%
  left_join(total_cases_17, by = "NHANES_cycle")

model_12 <- lm(Proportion_Under50 ~ Start_Year, data = under50_prop_12, weights = Total_Cases)
summary(model_12)
model_17 <- lm(Proportion_Under50 ~ Start_Year, data = under50_prop_17, weights = Total_Cases)
summary(model_17)
# Interpretation:
# - Weighted by total cases to give more importance to cycles with more data.
# - Significant positive coefficient → robust increase in proportion under 50 over time.

# Poisson regression for counts <50
young_cases_12 <- df_12 %>% filter(Age_midpoint < 50)
young_cases_12 <- young_cases_12 %>%
  mutate(Start_Year = as.numeric(substr(NHANES_cycle, 1, 4)))
young_cases_17 <- df_17 %>% filter(Age_midpoint < 50)
young_cases_17 <- young_cases_17 %>%
  mutate(Start_Year = as.numeric(substr(NHANES_cycle, 1, 4)))

counts_12 <- young_cases_12 %>%
  group_by(Start_Year) %>%
  summarise(Cases = n())
counts_17 <- young_cases_17 %>%
  group_by(Start_Year) %>%
  summarise(Cases = n())

poisson_fit_12 <- glm(Cases ~ Start_Year, family = poisson(), data = counts_12)
summary(poisson_fit_12)
poisson_fit_17 <- glm(Cases ~ Start_Year, family = poisson(), data = counts_17)
summary(poisson_fit_17)
# Interpretation:
# - Models counts directly assuming Poisson distribution.
# - Significant positive coefficient → number of younger-onset cases is increasing, not just proportion.
# - If overdispersion detected, negative binomial regression would be preferred.

################# Important extreme overdispersion add negative binomial regression ##################### 
nb_fit_12 <- glm.nb(Cases ~ Start_Year, data = counts_12)
summary(nb_fit_12)
nb_fit_17 <- glm.nb(Cases ~ Start_Year, data = counts_17)
summary(nb_fit_17)

# Trend for tumor size 
# Median or mean tumor size by year
df_12 <- df_12 %>%
  mutate(Start_Year = as.numeric(substr(NHANES_cycle, 1, 4)))
tumor_trend_12 <- df_12 %>%
  filter(Age_midpoint < 50 & !is.na(Tumor_Size_Num)) %>%
  group_by(Start_Year) %>%
  summarise(Median_Size = median(Tumor_Size_Num))
df_17 <- df_17 %>%
  mutate(Start_Year = as.numeric(substr(NHANES_cycle, 1, 4)))
tumor_trend_17 <- df_17 %>%
  filter(Age_midpoint < 50 & !is.na(Tumor_Size_Num)) %>%
  group_by(Start_Year) %>%
  summarise(Median_Size = median(Tumor_Size_Num))

# Linear model
lm_size_12 <- lm(Median_Size ~ Start_Year, data = tumor_trend_12)
summary(lm_size_12)
lm_size_17 <- lm(Median_Size ~ Start_Year, data = tumor_trend_17)
summary(lm_size_17)
# Interpretation:
# - Linear trend in median tumor size for <50.
# - Significant negative slope → younger cancers being detected smaller over time (possibly due to earlier detection).
# - Flat or positive slope → no early detection effect; younger cases may be biologically appearing earlier.

################## Consider adding individual level logistic regression #######################

###------NHANES-----------------------------------------------------------------

# 1. Define cycles and labels
cycles <- list(
  suffix = c("", "B", "C", "D", "E", "F", "G", "H", "I", "P", "L"),
  label  = c("1999-2000", "2001-2002", "2003-2004", "2005-2006", "2007-2008", 
             "2009-2010", "2011-2012", "2013-2014", "2015-2016", "2017-2020", 
             "2021-2023")
)

# 2. Optimized fetch function
fetch_nhanes_cleaned <- function(s, label) {
  prefix <- if(s == "P") "P_" else ""
  suffix <- if(s %in% c("P", "")) "" else paste0("_", s)
  
  demo_raw <- nhanes(paste0(prefix, "DEMO", suffix))
  bmx_raw  <- nhanes(paste0(prefix, "BMX", suffix))
  
  if (any(sapply(list(demo_raw, bmx_raw), is.null))) return(NULL)
  
  weight_col <- intersect(c("WTMEC4YR", "WTMEC2YR", "WTMECPRP"), names(demo_raw))[1]
  
  df <- demo_raw %>%
    dplyr::select(SEQN, any_of(weight_col), SDMVSTRA, SDMVPSU, RIDAGEYR) %>%
    inner_join(dplyr::select(bmx_raw, SEQN, BMXBMI), by = "SEQN") %>%
    rename(
      raw_weight = !!weight_col,
      BMI = BMXBMI  
    ) %>% 
    mutate(
      NHANES_cycle = label,
      is_obese = as.numeric(BMI >= 30), 
      Age_group = case_when(
        RIDAGEYR < 40 ~ "<40", 
        RIDAGEYR >= 40 & RIDAGEYR <= 44 ~ "40-44",
        RIDAGEYR >= 45 & RIDAGEYR <= 49 ~ "45-49",
        RIDAGEYR >= 50 ~ "50+",
        TRUE ~ "Other"
      )
    ) %>%
    filter(Age_group != "Other")
  
  return(df)
}

# 3. Loading data
all_data <- map2_dfr(cycles$suffix, cycles$label, fetch_nhanes_cleaned) %>%
  mutate(
    NHANES_cycle = if_else(NHANES_cycle == "2021-2023", "2021-2022", NHANES_cycle),
    cycle_num    = as.numeric(factor(NHANES_cycle, levels = unique(NHANES_cycle))),
    final_weight = case_when(
      NHANES_cycle %in% c("1999-2000", "2001-2002") ~ raw_weight * 0.5,
      TRUE ~ raw_weight
    ) / n_distinct(NHANES_cycle) ## in NHANES raw 1999 - 2002 was provided 4 yr weights
  ) %>%
  filter(!is.na(BMI))

# 4.Check data before processing

data_check <- all_data %>%
  group_by(NHANES_cycle, Age_group) %>%
  summarize(
    total_n = n(),
    obese_cases = sum(is_obese, na.rm = TRUE),
    obese_perc = (obese_cases / total_n) * 100,
    .groups = "drop"
  )

full_grid <- expand.grid(
  NHANES_cycle = unique(all_data$NHANES_cycle),
  Age_group = unique(all_data$Age_group)
)

data_check_full <- full_grid %>%
  left_join(data_check, by = c("NHANES_cycle", "Age_group")) %>%
  replace_na(list(total_n = 0, obese_cases = 0, obese_perc = 0))

ggplot(data_check_full, aes(x = NHANES_cycle, y = Age_group, fill = total_n)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#fee0d2", high = "#de2d26", name = "Sample Size (n)") +
  geom_text(aes(label = total_n), color = "black", size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "NHANES Data Completeness (1999-2023)",
    subtitle = "Check for low 'n' before applying 3, 5, or 7-year lags",
    x = "Exposure Cycle",
    y = "Age Group"
  )


# 5. Single Survey Design Object
options(survey.lonely.psu = "adjust")

trend_design <- svydesign(
  id = ~SDMVPSU, 
  strata = ~SDMVSTRA, 
  weights = ~final_weight, 
  nest = TRUE, 
  data = all_data
)

get_prev <- function(formula, design, name) {
  svyby(formula, ~NHANES_cycle + Age_group, design, svymean, na.rm = TRUE) %>%
    mutate(
      Metric = name,
      pct = abs(is_obese) * 100, 
      se_pct = se * 100,
      lower = pct - (1.96 * se_pct),
      upper = pct + (1.96 * se_pct)
    )
}

plot_data <- get_prev(~is_obese, trend_design, "Obesity")

ggplot(plot_data, aes(x = NHANES_cycle, y = pct, color = Age_group, group = Age_group)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = Age_group), alpha = 0.1, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~Metric) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Obesity Trends: NHANES 1999-2022",
    subtitle = "Age-stratified prevalence with 95% Confidence Intervals",
    x = "Survey Cycle",
    y = "Prevalence (%)",
    fill = "Age Group",
    color = "Age Group"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.position = "bottom"
  )
### 
# Trend of obesity increases across all age groups. 
# The trend seems higher in group 40 - 44 & 45- 49 maybe due to smaller sample size 

### SEER data preparation before combining with NHANES ------
data<-raw_data_17
# 1. Filter and Prepare Data (1999-2023)
df_clean <- data %>%
  filter(Year.of.diagnosis >= 2001 & Year.of.diagnosis <= 2023) %>%
  rename(Age_range = `Age.recode.with..1.year.olds.and.90.`)%>%
  mutate(
    Start_Year = Year.of.diagnosis - (Year.of.diagnosis - 2007) %% 2,
    NHANES_cycle = paste0(Start_Year, "-", Start_Year + 1),
    Age_range = gsub(" years", "", Age_range),
    Age_group = case_when(
      Age_range < 40  ~ "<40",
      Age_range >= 40 & Age_range <= 44 ~ "40-44",
      Age_range >= 45 & Age_range <= 49 ~ "45-49",
      Age_range >= 50 ~ "50+",
      TRUE ~ NA_character_),
    Age_group = factor(Age_group, levels = c("<40", "40-44", "45-49", "50+")))

# 2. Calculate Proportions and Counts
cycle_summary <- df_clean %>%
  group_by(NHANES_cycle, Start_Year, Age_group) %>%
  summarise(Cases = n(), .groups = "drop") %>%
  group_by(NHANES_cycle) %>%
  mutate(
    Total_Cycle_Cases = sum(Cases),
    Proportion = Cases / Total_Cycle_Cases
  )
# Combined Insights
## 1.Generate summary with correct survey-weighted statistics
df_NH <- svyby(~is_obese + BMI, ~NHANES_cycle + Age_group, 
               design = trend_design, 
               svymean, na.rm = TRUE) %>%
  as_tibble() %>%
  rename(
    prev_obese = is_obese,  
    mean_bmi   = BMI,
    se_obese   = se.is_obese,
    se_bmi     = se.BMI
  ) %>%
  mutate(
    prev_obese_pct = prev_obese * 100,
    se_obese_pct   = se_obese * 100
  )

## 1.Generate summary with correct survey-weighted statistics
df_NH <- svyby(~is_obese + BMI, ~NHANES_cycle + Age_group, 
               design = trend_design, 
               svymean, na.rm = TRUE) %>%
  as_tibble() %>%
  rename(
    prev_obese = is_obese,  
    mean_bmi   = BMI,
    se_obese   = se.is_obese,
    se_bmi     = se.BMI
  ) %>%
  mutate(
    prev_obese_pct = prev_obese * 100,
    se_obese_pct   = se_obese * 100
  )

## 2. Lag year
lags_to_test <- c(0, 2, 4, 6) 

df_list <- lags_to_test %>% 
  set_names(paste0("df_NHSE_lag", .)) %>% 
  map(function(lg) { 
    
    # 2a. Shifting NHANES cycles forward by 'lg'
    df_NH_lag <- df_NH %>%
      rename(original_cycle = NHANES_cycle) %>%
      separate(original_cycle, into = c("y1", "y2"), sep = "-", convert = TRUE) %>%
      mutate(across(c(y1, y2), ~ .x + lg)) %>% 
      unite("NHANES_cycle", y1, y2, sep = "-") %>%
      mutate(start_year = as.numeric(substr(NHANES_cycle, 1, 4))) %>%
      filter(start_year >= (1999 + lg) & start_year <= 2021) %>%
      select(-start_year)
    
    # 2b. SEER data prep (Using 'lg' to align SEER filters)
    df_SE_agg <- cycle_summary %>%
      filter(NHANES_cycle %in% c(
        paste0(2017 + lg, "-", 2018 + lg), 
        paste0(2021 + lg, "-", 2022 + lg)
      )) %>%
      group_by(Age_group) %>%
      summarise(
        Start_Year        = 2017 + lg,
        NHANES_cycle      = paste0(Start_Year, "-", 2020 + lg),
        Cases             = sum(Cases, na.rm = TRUE),
        Total_Cycle_Cases = sum(unique(Total_Cycle_Cases)),
        Proportion        = Cases / Total_Cycle_Cases,
        .groups = "drop"
      )
    
    # Combine with remaining SEER cycles
    df_SE_clean <- cycle_summary %>%
      filter(!NHANES_cycle %in% c(
        paste0(2017 + lg, "-", 2018 + lg),
        paste0(2021 + lg, "-", 2022 + lg)
      )) %>%
      bind_rows(df_SE_agg) %>%
      mutate(start_year = as.numeric(substr(NHANES_cycle, 1, 4))) %>%
      filter(start_year >= (1999 + lg)) %>%
      arrange(start_year, Age_group) %>%
      select(-start_year, -any_of("Start_Year"))
    
    # 2c. Join data set
    df_NHSE_result <- full_join(df_NH_lag, df_SE_clean, by = c("NHANES_cycle", "Age_group")) %>%
      mutate(
        pct_obese = prev_obese * 100,
        hundred_cases = Cases / 100, 
        lag_applied = lg 
      ) %>%
      drop_na(pct_obese, hundred_cases)
    
    return(df_NHSE_result)
  })

# Combine everything into one master data frame
df_final_master <- bind_rows(df_list)

## 3. Trending plot
df_plot <- df_final_master %>% filter(Age_group != "50+")

# 3a. Corelation table
df_cor_labels <- df_plot %>%
  group_by(lag_applied, Age_group) %>%
  summarize(
    r_val = cor(pct_obese, hundred_cases, use = "complete.obs"),
    x_pos = -Inf, 
    y_pos = Inf,
    label = paste0("r = ", round(r_val, 2)),
    .groups = "drop"
  )

# 3b.Transform the Master Data to "Long" format
df_plot_long <- df_plot %>%
  pivot_longer(
    cols = c(pct_obese, hundred_cases), 
    names_to = "Metric", 
    values_to = "Value"
  ) %>%
  mutate(Metric = case_when(
    Metric == "pct_obese" ~ "Obesity (%)",
    Metric == "hundred_cases" ~ "Cancer Cases (x100)"
  ))

# 3c.Plot
ggplot(df_plot_long, aes(x = NHANES_cycle, y = Value, group = Metric, color = Metric)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_text(data = df_cor_labels, 
            aes(x = x_pos, y = y_pos, label = label),
            hjust = -0.2, vjust = 1.5, 
            inherit.aes = FALSE, 
            fontface = "bold", size = 4, color = "black") +
  
  facet_grid(lag_applied ~ Age_group, scales = "free_y") + 
  
  scale_color_manual(values = c(
    "Obesity (%)" = "#00008B", 
    "Cancer Cases (x100)" = "#DC3912"
  )) +
  
  theme_bw() +
  labs(
    title = "Sensitivity Analysis: Temporal Coupling of Obesity & Cancer",
    subtitle = "Comparing lag years (rows) to identify the strongest correlation",
    x = "NHANES Cycle (Lagged)",
    y = "Metric Value (Free Y-Axis)",
    color = "Health Metric"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "bottom"
  )

##

ggplot(df_plot, aes(x = pct_obese, y = hundred_cases, color = Age_group)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", size = 0.8,linetype="solid") +
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top", 
           size = 3.5, fontface = "bold", show.legend = FALSE) +
  facet_grid(lag_applied ~ Age_group) + 
  scale_color_viridis_d(option = "plasma") +
  theme_bw() +
  labs(
    title = "Obesity vs. Cancer Cases: Sensitivity Analysis",
    subtitle = "Rows represent different lag years; Columns represent age groups",
    x = "Obesity Prevalence (%)",
    y = "Cancer Cases (per 100k / 100)",
    color = "Age Group"
  ) +
  theme(
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

## 4. Statistical summary 
# 4a. Calculate correlations 
age_correlations_final <- df_final_master %>%
  group_by(lag_applied, Age_group) %>%
  summarise(
    R_Coefficient = cor(pct_obese, hundred_cases, use = "complete.obs"),
    Sample_Size   = n(),
    .groups = "drop"
  )

print("--- Final Correlation Results Across All Lags ---")
print(age_correlations_final)

comparison_table <- age_correlations_final %>%
  select(Age_group, lag_applied, R_Coefficient) %>%
  pivot_wider(names_from = lag_applied, values_from = R_Coefficient, names_prefix = "Lag_")

print("--- R-Coefficient Comparison Table ---")
print(comparison_table)

#4b. Regression 
final_executive_table <- df_final_master %>%
  filter(Age_group != "50+") %>%
  group_by(Lag_Years = lag_applied) %>%
  nest() %>%
  mutate(model = map(data, ~lm(hundred_cases ~ pct_obese + Age_group, data = .x))) %>%
  reframe(
    tidy(model[[1]], conf.int = TRUE) %>% filter(term == "pct_obese"),
    glance(model[[1]]) %>% select(adj.r.squared)
  ) %>%
  select(
    `Lag (Years)` = Lag_Years,
    `Obesity Beta` = estimate,
    `Lower CI` = conf.low,
    `Upper CI` = conf.high,
    `P-Value` = p.value,
    `Adjusted R2` = adj.r.squared
  ) %>%
  mutate(Sig = case_when(`P-Value` < 0.001 ~ "***", `P-Value` < 0.05 ~ "*", TRUE ~ "")) %>%
  mutate(across(where(is.numeric), ~round(.x, 5)))

print(final_executive_table)

### The trends in obesity & cancer is
## Extremely strong positive correlations in groups 40-44 & <40 
## Moderate to strong positive correlation in group 45-49, concerned about the smaller size
## Strong negative correlation in group >50+, might be: 
# Early dx in younger groups that might lower the trend of new dx in older group
# The small lag year impact? 
# The higher death rate in older group. 
### The correlations in group 45 - 49 is low might be due to the impact of insurance coverage from 45+
### Lag-Year = 2 or 4 years seem to be a good model, 
# 4 year lagging is more reasonable than 2 years
