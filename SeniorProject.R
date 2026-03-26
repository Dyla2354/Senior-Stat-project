library(dplyr)
library(nhanesA)
library(stringr)
library(ggplot2)

#Example to get the data from the first year 2003-2004

demo2003 <- nhanes("DEMO_C")
bmx2003 <- nhanes("BMX_C")
paq2003 <- nhanes("PAQ_C")
dr1tot2003 <-nhanes("DR1TOT_C")

names(paq2003)

data2003 <- demo2003 %>%
  select(SEQN, RIDAGEYR, RIAGENDR, RIDRETH1, DMDEDUC2, DMDMARTL, INDHHINC) %>%
  left_join(bmx2003 %>% select(SEQN, BMXBMI), by="SEQN") %>%
  left_join(paq2003 %>% select(SEQN, PAQ100), by="SEQN") %>%
  left_join(dr1tot2003 %>% select(SEQN, DR1TKCAL, DR1TPROT, DR1TCARB, DR1TTFAT, DR1TALCO, DR1TCAFF, DR1TSUGR, DR1TSFAT, DR1TCHOL), by="SEQN")

#The following for loop merges all the data together, with vigorous activity
#the last part removes it for now. I will continue to experiment with this

cycles <- c("C","D","E","F","G","H","I","J")
all_data <- list()

for (cycle in cycles) {
  
  demo <- nhanes(paste0("DEMO_", cycle))
  bmx  <- nhanes(paste0("BMX_", cycle))
  paq  <- nhanes(paste0("PAQ_", cycle))
  dr1  <- nhanes(paste0("DR1TOT_", cycle))
  
  if (cycle %in% c("C", "D")) {
    income_var <- "INDHHINC"
  } else {
    income_var <- "INDHHIN2"
  }
  
  if (cycle %in% c("C", "D")) {
    paq_var <- "PAD600"
  } else {
    paq_var <- "PAD630"
  }
  
  temp <- demo %>%
    select(SEQN, SDDSRVYR, RIDAGEYR, RIAGENDR, RIDRETH1, DMDEDUC2, DMDMARTL, all_of(income_var)) %>%
    left_join(bmx %>% select(SEQN, BMXBMI), by = "SEQN") %>%
    left_join(paq %>% select(SEQN, all_of(paq_var)), by = "SEQN") %>%
    left_join(
      dr1 %>% select(SEQN, DR1TKCAL, DR1TPROT, DR1TCARB, DR1TTFAT,
                     DR1TALCO, DR1TCAFF, DR1TSUGR, DR1TSFAT, DR1TCHOL),
      by = "SEQN"
    ) %>%
    mutate(cycle = cycle)
  
  names(temp)[names(temp) == income_var] <- "income"
  names(temp)[names(temp) == paq_var] <- "vigorous_activity"
  
  temp <- temp %>%
    mutate(
      income = as.character(income),
      vigorous_activity = as.character(vigorous_activity)
    )
  
  all_data[[cycle]] <- temp
}

nhanes_combined <- bind_rows(all_data)

nhanes_combined <- nhanes_combined %>%
  rename(
    year = SDDSRVYR,
    age = RIDAGEYR,
    sex = RIAGENDR,
    race = RIDRETH1,
    education = DMDEDUC2,
    marital_status = DMDMARTL,
    bmi = BMXBMI,
    total_calories = DR1TKCAL,
    protein = DR1TPROT,
    carbs = DR1TCARB,
    total_fat = DR1TTFAT,
    alcohol = DR1TALCO,
    caffine = DR1TCAFF,
    sugar = DR1TSUGR,
    total_saturated_fat = DR1TSFAT,
    cholesterol = DR1TCHOL
  )

# total rows
n_total <- nrow(nhanes_combined)

# rows with NO missing values
n_complete <- sum(complete.cases(nhanes_combined))

# rows that would be removed
n_removed <- n_total - n_complete

n_total
n_complete
n_removed

colSums(is.na(nhanes_combined))

#I decided to remove 3 variables:
#Activity, it's very messy in NHANES and there are 47000/80000 missing
#Education, there are also 30000 missing
#Marital Status, there are also 30000 missing
#The bottom 2 i don't think will be extremely relevant to our data, and after
#making these changes, there are only 16000 rows that have missing values,
#which I think makes it worth it.

nhanes_clean <- nhanes_combined %>%
  select(
    year,
    age,
    sex,
    race,
    bmi,
    total_calories,
    protein,
    carbs,
    total_fat,
    alcohol,
    caffine,
    sugar,
    total_saturated_fat,
    cholesterol,
    income
  ) %>%
  filter(complete.cases(.)) %>%
  mutate(
    NHANES_cycle = stringr::str_extract(year, "\\d{4}-\\d{4}"),
    start_year = as.numeric(stringr::str_extract(NHANES_cycle, "^\\d{4}"))
  )

#Check to make sure these aren't drastically different
summary(nhanes_clean$age)
summary(nhanes_combined$age)
#our clean data set is slightly older, younger people are missing more data

table(nhanes_clean$sex)
table(nhanes_combined$sex)
#basically the same

#Also, in order to match our SEER data, we introduce the year and cycle variables
#above
#Below, we put the ages in the same buckets as with the SEER data

nhanes_clean <- nhanes_clean %>%
  mutate(
    Age_range = case_when(
      age >= 10 & age <= 14 ~ "10-14",
      age >= 15 & age <= 19 ~ "15-19",
      age >= 20 & age <= 24 ~ "20-24",
      age >= 25 & age <= 29 ~ "25-29",
      age >= 30 & age <= 34 ~ "30-34",
      age >= 35 & age <= 39 ~ "35-39",
      age >= 40 & age <= 44 ~ "40-44",
      age >= 45 & age <= 49 ~ "45-49",
      age >= 50 & age <= 54 ~ "50-54",
      age >= 55 & age <= 59 ~ "55-59",
      age >= 60 & age <= 64 ~ "60-64",
      age >= 65 & age <= 69 ~ "65-69",
      age >= 70 & age <= 74 ~ "70-74",
      age >= 75 & age <= 79 ~ "75-79",
      age >= 80 & age <= 84 ~ "80-84",
      age >= 85 & age <= 89 ~ "85-89",
      age >= 90 ~ "90+",
      TRUE ~ NA_character_
    )
  )

#Now to get grouped summaries to compare to SEER, we need to group everything:


nhanes_summary <- nhanes_clean %>%
  filter(!is.na(Age_range)) %>%
  group_by(NHANES_cycle, Age_range) %>%
  summarise(
    n = n(),
    mean_bmi = mean(bmi, na.rm = TRUE),
    obesity_rate = mean(bmi >= 30, na.rm = TRUE),
    mean_calories = mean(total_calories, na.rm = TRUE),
    mean_protein = mean(protein, na.rm = TRUE),
    mean_carbs = mean(carbs, na.rm = TRUE),
    mean_total_fat = mean(total_fat, na.rm = TRUE),
    mean_sugar = mean(sugar, na.rm = TRUE),
    mean_sat_fat = mean(total_saturated_fat, na.rm = TRUE),
    mean_cholesterol = mean(cholesterol, na.rm = TRUE),
    .groups = "drop"
  )
View(nhanes_summary)
head(nhanes_summary)

#To match other data set

nhanes_summary <- nhanes_summary %>%
  mutate(
    under50 = ifelse(Age_range %in% c(
      "10-14","15-19","20-24","25-29",
      "30-34","35-39","40-44","45-49"
    ), 1, 0)
  )

#this is how we can join the two data sets
#combined <- left_join(seer_summary, nhanes_summary,
                      #by = c("NHANES_cycle", "Age_range"))


ggplot(nhanes_summary, 
       aes(x = NHANES_cycle, y = mean_sugar, color = Age_range, group = Age_range)) +
  geom_line() +
  theme_minimal()

ggplot(nhanes_summary, 
       aes(x = NHANES_cycle, y = mean_calories, color = Age_range, group = Age_range)) +
  geom_line() +
  theme_minimal()

ggplot(nhanes_summary, 
       aes(x = NHANES_cycle, y = mean_bmi, color = Age_range, group = Age_range)) +
  geom_line() +
  theme_minimal()

#definately some good data here
ggplot(nhanes_summary, 
       aes(x = NHANES_cycle, y = obesity_rate, color = Age_range, group = Age_range)) +
  geom_line() +
  theme_minimal()

ggplot(nhanes_summary, 
       aes(x = NHANES_cycle, y = mean_cholesterol, color = Age_range, group = Age_range)) +
  geom_line() +
  theme_minimal()

ggplot(nhanes_summary, 
       aes(x = NHANES_cycle, y = mean_total_fat, color = Age_range, group = Age_range)) +
  geom_line() +
  theme_minimal()

young_groups <- c(
  "10-14","15-19","20-24","25-29",
  "30-34","35-39","40-44","45-49"
)

nhanes_under50 <- nhanes_summary %>%
  filter(Age_range %in% young_groups) %>%
  group_by(NHANES_cycle) %>%
  summarise(
    mean_sugar = mean(mean_sugar),
    mean_bmi = mean(mean_bmi),
    obesity_rate = mean(obesity_rate),
    mean_calories = mean(mean_calories),
    mean_protein = mean(mean_protein),
    mean_carbs = mean(mean_carbs),
    mean_total_fat = mean(mean_total_fat),
    mean_sat_fat = mean(mean_sat_fat),
    mean_cholesterol = mean(mean_cholesterol),
    .groups = "drop"
  )


ggplot(nhanes_under50, aes(x = NHANES_cycle, y = mean_sugar, group = 1)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Sugar Intake (<50) Over Time")

ggplot(nhanes_under50, aes(x = NHANES_cycle, y = mean_total_fat, group = 1)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Fat Intake (<50) Over Time")

ggplot(nhanes_under50, aes(x = NHANES_cycle, y = mean_bmi, group = 1)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "BMI (<50) Over Time")

ggplot(nhanes_under50, aes(x = NHANES_cycle, y = obesity_rate, group = 1)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Obesity Rate (<50) Over Time")

#Maybe we should revist activity level, perhaps another data
#set other than NHANES because it seems obesity rate and bmi are
#going significantly up, while the nutritional factors seem to
#not align similarly