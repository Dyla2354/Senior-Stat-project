library(dplyr)            # For data manipulation
library(tidyr)            # For reshaping data (long <-> wide)
library(ggplot2)          # For plotting
library(scales)           # For formatting axes (percent, comma, etc.)

# ------------------------------------------------------------------------------
# Load Base Data
# ------------------------------------------------------------------------------
raw_data <- read.csv(file.choose())  # Open file chooser to load CSV
data <- raw_data                     # Make a working copy
head(data)                           # Quick check of the first few rows

# ------------------------------------------------------------------------------
# Examine frequency of each variable
# ------------------------------------------------------------------------------
lapply(data, table)  # Shows counts of each unique value per column

# ------------------------------------------------------------------------------
# Add NHANES_cycle column
# ------------------------------------------------------------------------------
# NHANES cycles are 2-year intervals
data_with_NHANES_cycle <- data %>%
  mutate(NHANES_cycle = paste0(
    Year.of.diagnosis - (Year.of.diagnosis - 1999) %% 2,    # Start year
    "-",
    Year.of.diagnosis - (Year.of.diagnosis - 1999) %% 2 + 1 # End year
  ))

# ------------------------------------------------------------------------------
# Clean Age column
# ------------------------------------------------------------------------------
df <- data_with_NHANES_cycle %>%
  rename(Age_range = `Age.recode.with..1.year.olds.and.90.`) # Rename for clarity

# Remove the text "years" from Age_range
df <- df %>%
  mutate(Age_range = gsub(" years", "", Age_range))

# Define ordered factor levels for plotting and analysis
age_levels <- c("10-14","15-19","20-24","25-29",
                "30-34","35-39","40-44","45-49",
                "50-54","55-59","60-64","65-69",
                "70-74","75-79","80-84","85-89","90+")
df$Age_range <- factor(df$Age_range, levels = age_levels, ordered = TRUE)

# ------------------------------------------------------------------------------
# Calculate proportion of cases per age group per NHANES cycle
# ------------------------------------------------------------------------------
age_props <- df %>%
  group_by(NHANES_cycle, Age_range) %>%
  summarise(n = n(), .groups = "drop") %>%  # Count cases per age group per cycle
  group_by(NHANES_cycle) %>%
  mutate(proportion = n / sum(n))           # Convert counts to proportion

# Pivot to wide format for easier viewing
age_props_wide <- age_props %>%
  select(-n) %>%
  pivot_wider(names_from = Age_range, values_from = proportion)

# ------------------------------------------------------------------------------
# Plot proportion of age groups per NHANES cycle
# ------------------------------------------------------------------------------
ggplot(age_props, aes(x = NHANES_cycle, y = proportion, fill = Age_range)) +
  geom_col() +
  theme_minimal() +
  labs(y = "Proportion", x = "NHANES Cycle") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Interpretation:
# - Each bar shows the age distribution of cases in a given NHANES cycle.
# - Taller bars for younger ages indicate more cases among young adults.
# - Compare cycles to see whether younger age groups are increasing relative to older groups.

# Individual plots by cycle
ggplot(age_props, aes(x = Age_range, y = proportion, fill = Age_range)) +
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
ggplot(age_props, aes(x = Age_range, y = proportion, fill = Age_range)) + 
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
young_ages <- c("10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49")

young_age_props <- age_props %>%
  filter(Age_range %in% young_ages)

ggplot(young_age_props, aes(x = NHANES_cycle, y = proportion, fill = Age_range)) +
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
# - If specific younger groups (e.g., 30-39) grow faster, that's a focus for early screening.

# ------------------------------------------------------------------------------
# Convert Age_range to approximate numeric midpoint
# ------------------------------------------------------------------------------
df <- df %>%
  mutate(Age_midpoint = case_when(
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
median_age <- df %>%
  group_by(NHANES_cycle) %>%
  summarise(Median_Age = median(Age_midpoint, na.rm = TRUE))

ggplot(median_age, aes(x = NHANES_cycle, y = Median_Age, group = 1)) +
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
# - A downward trend ??? younger onset is becoming more common.
# - Flat or upward trend ??? median age stable or increasing.

# ------------------------------------------------------------------------------
# Proportion of cases under age 50
# ------------------------------------------------------------------------------
under50_prop <- df %>%
  mutate(Under50 = ifelse(Age_midpoint < 50, 1, 0)) %>%
  group_by(NHANES_cycle) %>%
  summarise(Proportion_Under50 = mean(Under50))

ggplot(under50_prop, aes(x = NHANES_cycle, y = Proportion_Under50, group = 1)) +
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
# - Rising line ??? true increase in younger-onset CRC or improved detection.
# - Plateau ??? younger proportion stable.

# ------------------------------
# Statistical trend analysis
# ------------------------------
# Convert NHANES_cycle to numeric start year for regression
under50_prop <- under50_prop %>%
  mutate(Start_Year = as.numeric(substr(NHANES_cycle, 1, 4)))

# Linear trend (ordinary least squares)
lm_fit <- lm(Proportion_Under50 ~ Start_Year, data = under50_prop)
summary(lm_fit)
# Interpretation:
# - Check coefficient for Start_Year:
#    * Positive & significant ??? proportion of cases <50 increases over time.
#    * p-value < 0.05 ??? statistically significant trend.
# - R-squared ??? fraction of variation in proportion explained by year.

# Plot with regression line
ggplot(under50_prop, aes(x = Start_Year, y = Proportion_Under50)) +
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
    caption = "Data: SEER (1992-2022)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
# Interpretation:
# - Regression line confirms trend visually.
# - Confidence interval (shaded area) shows uncertainty around trend estimate.
# - Slope indicates rate of change in proportion under 50 per year.

# ------------------------------------------------------------------------------
# Investigate tumor characteristics
# ------------------------------------------------------------------------------
# Tumor deposits (binary 0/1)
df <- df %>%
  mutate(Tumor_Deposit_Present = case_when(
    Tumor.Deposits.Recode..2010.. %in% c("Blank(s)", "No tumor deposits", 
                                         "Not documented/assessed; Indeterminate; No mention in path report; No resection") ~ 0,
    TRUE ~ 1  # any documented tumor deposits
  ))

# Subset for post-2010 when tumor deposits recorded
df_post2010 <- df %>% filter(Year.of.diagnosis >= 2010)

tumor_trend_under50_post2010 <- df_post2010 %>%
  filter(Age_midpoint < 50) %>%
  group_by(NHANES_cycle) %>%
  summarise(Proportion_With_Deposits = mean(Tumor_Deposit_Present, na.rm = TRUE))

# Plot tumor deposits trend
ggplot(tumor_trend_under50_post2010, aes(x = NHANES_cycle, y = Proportion_With_Deposits, group = 1)) +
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
# - Rising trend ??? younger patients increasingly present with aggressive features.
# - Flat trend ??? earlier detection may explain higher proportion without increased aggressiveness.

# ------------------------------------------------------------------------------
# Tumor size conversion
# ------------------------------------------------------------------------------
df <- df %>%
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
tumor_size_trend <- df %>%
  filter(Age_midpoint < 50 & !is.na(Tumor_Size_Num)) %>%
  group_by(NHANES_cycle) %>%
  summarise(Median_Tumor_Size = median(Tumor_Size_Num))

ggplot(tumor_size_trend, aes(x = NHANES_cycle, y = Median_Tumor_Size, group = 1)) +
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
# - Downward trend ??? cancers are being detected at smaller sizes (suggests earlier detection).
# - Upward trend ??? cancers may be more advanced at diagnosis (possible biological shift).
# - Flat trend ??? no major change in tumor size at diagnosis over time.

# Mean tumor size under 50
tumor_deposits_trend <- df %>%
  filter(Age_midpoint < 50 & Year.of.diagnosis >= 2010) %>%
  group_by(NHANES_cycle) %>%
  summarise(Proportion_With_Deposits = mean(Tumor_Deposit_Present, na.rm = TRUE))

ggplot(tumor_deposits_trend, aes(x = NHANES_cycle, y = Proportion_With_Deposits, group = 1)) +
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
# - Increasing trend ??? younger patients are presenting with more aggressive tumors.
# - Decreasing trend ??? cancers may be detected earlier or treated sooner.
# - Flat trend ??? no major change in aggressiveness over time.

# ------------------------------------------------------------------------------
# Combine multiple trends for plotting
# ------------------------------------------------------------------------------
plot_data <- bind_rows(
  under50_prop %>% select(NHANES_cycle, value = Proportion_Under50) %>% mutate(Measure="Proportion <50"),
  tumor_size_trend %>% select(NHANES_cycle, value = Median_Tumor_Size) %>% mutate(Measure="Median Tumor Size (mm)"),
  tumor_deposits_trend %>% select(NHANES_cycle, value = Proportion_With_Deposits) %>% mutate(Measure="Tumor Deposits Proportion")
)

ggplot(plot_data, aes(x = NHANES_cycle, y = value, group = 1)) +
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
df <- df %>%
  mutate(
    Age_Group = ifelse(Age_midpoint < 50, "<50", "50+"),
    Under50 = ifelse(Age_midpoint < 50, 1, 0)
  )

cases_by_age <- df %>%
  group_by(NHANES_cycle, Age_Group) %>%
  summarise(Cases = n(), .groups = "drop")

proportion_under50 <- df %>%
  group_by(NHANES_cycle) %>%
  summarise(Proportion_Under50 = mean(Under50), .groups = "drop")

plot_data <- cases_by_age %>%
  pivot_wider(names_from = Age_Group, values_from = Cases) %>%
  left_join(proportion_under50, by = "NHANES_cycle") %>%
  pivot_longer(cols = c("<50", "50+", "Proportion_Under50"),
               names_to = "Measure", values_to = "Value")

ggplot(plot_data, aes(x = NHANES_cycle, y = Value, group = Measure,
                      color = Measure)) +
  geom_line(size = 1) +
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
# - Top panel: absolute number of <50 cases. Rising ??? true increase.
# - Middle panel: cases 50+. Stable or declining ??? relative proportion of <50 rises partly due to older cases.
# - Bottom panel: proportion <50 ??? confirms overall trend combining absolute and relative perspective.

plot_data <- cases_by_age %>%
  pivot_wider(names_from = Age_Group, values_from = Cases) %>%
  left_join(proportion_under50, by = "NHANES_cycle") %>%
  left_join(tumor_size_trend, by = "NHANES_cycle") %>%
  left_join(tumor_deposits_trend, by = "NHANES_cycle") %>%
  pivot_longer(cols = c("<50","50+","Proportion_Under50","Median_Tumor_Size","Proportion_With_Deposits"),
               names_to = "Measure", values_to = "Value")

plot_data_clean <- plot_data %>%
  filter(!is.na(Value))

ggplot(plot_data_clean, aes(x = NHANES_cycle, y = Value, group = 1)) +
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
total_cases <- df %>%
  group_by(NHANES_cycle) %>%
  summarise(Total_Cases = n(), .groups = "drop")

under50_prop <- under50_prop %>%
  left_join(total_cases, by = "NHANES_cycle")

model <- lm(Proportion_Under50 ~ Start_Year, data = under50_prop, weights = Total_Cases)
summary(model)
# Interpretation:
# - Weighted by total cases to give more importance to cycles with more data.
# - Significant positive coefficient ??? robust increase in proportion under 50 over time.

# Poisson regression for counts <50
young_cases <- df %>% filter(Age_midpoint < 50)
young_cases <- young_cases %>%
  mutate(Start_Year = as.numeric(substr(NHANES_cycle, 1, 4)))

counts <- young_cases %>%
  group_by(Start_Year) %>%
  summarise(Cases = n())

poisson_fit <- glm(Cases ~ Start_Year, family = poisson(), data = counts)
summary(poisson_fit)
# Interpretation:
# - Models counts directly assuming Poisson distribution.
# - Significant positive coefficient ??? number of younger-onset cases is increasing, not just proportion.
# - If overdispersion detected, negative binomial regression would be preferred.

################# Important extreme overdispersion add negative binomial regresion ##################### 

# Trend for tumor size 
# Median or mean tumor size by year
df <- df %>%
  mutate(Start_Year = as.numeric(substr(NHANES_cycle, 1, 4)))
tumor_trend <- df %>%
  filter(Age_midpoint < 50 & !is.na(Tumor_Size_Num)) %>%
  group_by(Start_Year) %>%
  summarise(Median_Size = median(Tumor_Size_Num))

# Linear model
lm_size <- lm(Median_Size ~ Start_Year, data = tumor_trend)
summary(lm_size)
# Interpretation:
# - Linear trend in median tumor size for <50.
# - Significant negative slope ??? younger cancers being detected smaller over time (possibly due to earlier detection).
# - Flat or positive slope ??? no early detection effect; younger cases may be biologically appearing earlier.

################## Consider adding individual level logistic regression #######################
