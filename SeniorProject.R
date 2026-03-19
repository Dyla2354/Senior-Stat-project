library(dplyr)
library(nhanesA)

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

#without any activity

cycles <- c("C","D","E","F","G","H","I","J")
all_data <- list()

for (cycle in cycles) {
  
  demo <- nhanes(paste0("DEMO_", cycle))
  bmx  <- nhanes(paste0("BMX_", cycle))
  dr1  <- nhanes(paste0("DR1TOT_", cycle))
  
  if (cycle %in% c("C", "D")) {
    income_var <- "INDHHINC"
  } else {
    income_var <- "INDHHIN2"
  }
  
  temp <- demo %>%
    select(SEQN, SDDSRVYR, RIDAGEYR, RIAGENDR, RIDRETH1, DMDEDUC2, DMDMARTL, all_of(income_var)) %>%
    left_join(bmx %>% select(SEQN, BMXBMI), by = "SEQN") %>%
    left_join(
      dr1 %>% select(SEQN, DR1TKCAL, DR1TPROT, DR1TCARB, DR1TTFAT,
                     DR1TALCO, DR1TCAFF, DR1TSUGR, DR1TSFAT, DR1TCHOL),
      by = "SEQN"
    ) %>%
    mutate(cycle = cycle)
    names(temp)[names(temp) == income_var] <- "income"
  
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

