# Title: Sample Code - Data Cleaning
# Author: Yuna Baek
# Description: This script is created to clean data of school administrative data.
# Note: This code is not reproducible

library(readxl)
library(dplyr)
library(tidyr)

setwd("/Users/yunabaek/Desktop/AIR/AIR Data/STeLLA/Admin Raw Data/")
school23 <- read_excel("STeLLA_v2.xlsx", sheet = "school 2023")
school22 <- read_excel("STeLLA_v2.xlsx", sheet = "school 2022")

school23 <- school23 %>% rename(student = "student_id",
                            teacher_name = "teacher_of_record_tln")

# Deleting columns
school23 <- school23 %>%
  select(-c(original_subject, original_performance_level, performance_level, enrolled,
            tested, valid_test, bhn_group, functionally_delayed, 
            gifted, migrant, foster, homeless, military_dependent, t1234, 
            el_recently_arrived, enrolled_50_pct_district, enrolled_50_pct_school, 
            reason_not_tested, absent, ri_status, refused_to_test,
            residential_facility, percentile, acct_system, acct_school))

# Gender
school23$gender_num <- ifelse(school23$gender=="F", 1, 0)

# Race/Ethnicity
unique(school23$"reported_race") # Checking values
school23$race_num <- ifelse(school23$"reported_race"=="White", 6, 9)

## Renaming Variables
school23 <- school23 %>% rename(elig_eng_num = "el",
                                elig_sped_num = "special_ed",
                                elig_meal_num = "economically_disadvantaged")

# Reshaping data
school <- pivot_wider(data = school23, id_cols = c("student", "system", "system_name", "school", 
                                             "school_name", "teacher_name", "test", "semester", "grade", 
                                             "gender", "reported_race", "elig_sped_num", 
                                             "elig_meal_num", "elig_eng_num", "gender_num", "race_num"), 
                   names_from = "subject", values_from = "scale_score")

school22 <- pivot_wider(data = school22, id_cols = c("student_id"), 
                     names_from = "subject", values_from = "scale_score")
school22 <- school22 %>% rename(student = "student_id")

# Merging 22 and 23 data
school <- left_join(school, school22, by = "student")

# Renaming
school <- school %>% rename(ela_23 = "ELA.x",
                      math_23 = "Math.x",
                      sci_23 = "Science.x",
                      ela_22 = "ELA.y",
                      math_22 = "Math.y",
                      sci_22 = "Science.y")

# Replacing scores with non-NA values to remove student duplicate rows
school <- school %>%
  group_by(student) %>%
  summarise(
    across(
      -teacher_name, 
      ~ first(na.omit(.))
    ),
    teacher_name = {
      # Convert to character to unify column type
      teacher_name <- as.character(teacher_name)
      # Keep participating teacher out of two teachers
      if (any(na.omit(teacher_name) == "Last, First")) {
        "Last, First"
      } else {
        first(na.omit(teacher_name))
      }
    }
  )

# Drop rows if all three post-scores are missing
school_filtered <- school %>%
  filter(!(is.na(ela_23) & is.na(math_23) & is.na(sci_23)))

# Producing clean output
school_filtered <- school_filtered %>%
  select(student, system, system_name, school, school_name, teacher_name, grade, gender_num, race_num, elig_eng_num,
         elig_sped_num, elig_meal_num, test, ela_22, math_22, sci_22, ela_23, math_23, sci_23) %>%
  rename(district = system,
         district_name = system_name,
         gender = gender_num,
         race = race_num,
         elig_eng = elig_eng_num,
         elig_sped = elig_sped_num,
         elig_meal = elig_meal_num) %>%
  mutate(teacher_name = toupper(teacher_name))

# Saving File
date <- Sys.Date()
date <- format(date, "%m%d")
file_name <- paste0("c2_admin_", unique(school_filtered$school_name), "_", date, ".csv")
write.csv(school_filtered, file = file_name, row.names = FALSE)

# Cleaning environment
rm(school22, school23, school_filtered, school)
