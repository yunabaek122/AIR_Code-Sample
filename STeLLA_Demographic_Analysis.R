## ---------------------------
##
## EIR STeLLA: Analyzing Cohort 2 Demographics Data
##
## Author: Yuna Baek
## Email: ybaek@air.org
##
## Date Created: 2024-10-04
##
## ---------------------------
##
##  Outputs: (1) Merged master data (CSV)
##           (2) Demographics analysis results (Excel) 
##
## ---------------------------

# Setup
library(dplyr)
library(readr)
library(openxlsx)

setwd("/Users/yunabaek/Desktop/AIR/AIR Data/STeLLA/Admin Raw Data/Clean")

##---------------------------------------------------------------
##                    Binding Cohort 2 Data                     -
##---------------------------------------------------------------
file_paths <- list.files(pattern = "c2_*", full.names = TRUE)

data_frames <- lapply(file_paths, function(file) {
  df <- read_csv(file)
  # Standardizing student column type
  df <- df %>%
    mutate(student = as.character(student))
  return(df)
})

# Combine all data frames into one
cohort2_merged <- bind_rows(data_frames)

cohort2 <- cohort2_merged %>%
  mutate(cohort2 = 1) %>%
  select(student, first_name, middle_name, last_name, district_name, school_name,
         grade, teacher_name, cohort2, gender, race, elig_eng, elig_sped, elig_meal, test,
         ela_23, math_23, sci_23, ela_22, math_22, sci_22)

##---------------------------------------------------------------
##                        Master Roster                         -
##---------------------------------------------------------------

master <- read_xlsx("~/Desktop/AIR/AIR Data/STeLLA/Admin Raw Data/AIR Master Teacher Roster Cohort 2.xlsx")

master <- master %>%
  select("District", "School", "School Assignment", "State") %>%
  rename(district_name = District,
         school_name = School, 
         treatment = "School Assignment",
         state = State)

master <- master %>%
  distinct()

# Rename district
cohort2 <- cohort2 %>%
  mutate(district_name = case_when(
    str_count(district_name, " ") > 0 ~ sapply(str_split(district_name, " "), `[`, 1),
    TRUE ~ district_name  # Keep the original district_name if it has no spaces
  ))

# Left join based on school_name
cohort2_roster <- cohort2 %>%
  left_join(master, by = c("school_name", "district_name")) %>%
  select(student, first_name, middle_name, last_name, state, district_name, school_name, 
         grade, teacher_name, treatment, cohort2, everything())


# Converting treatment status to dummy variable
cohort2_roster <- cohort2_roster %>%
  mutate(treatment = ifelse(treatment == "T", 1, 0))

# Exporting
date <- Sys.Date()
date <- format(date, "%m%d")
file_name <- paste0("c2_admin_all_", date, ".csv")

write.csv(cohort2_roster, file = file_name, row.names = FALSE)

##---------------------------------------------------------------
##                          Analysis                            -
##---------------------------------------------------------------
c2_all <- read_csv("c2_admin_all_1108.csv")

# Function to calculate percentages
percentages <- function(df, df_name) {
  # Percent female students
  female <- sum(df$gender == 1, na.rm = TRUE) / nrow(df)
  
  # Percent students in each race/ethnicity group
  race_percentages <- sapply(1:9, function(x) sum(df$race == x, na.rm = TRUE) / nrow(df))
  names(race_percentages) <- c(
    "American Indian or Alaska Native",
    "Asian",
    "Black or African American",
    "Hispanic",
    "Native Hawaiian or Other Pacific Islander",
    "White",
    "Other",
    "Two or More",
    "Missing or Unknown"
  )
  
  # Percent students who are eligible for FRPL
  frpl <- sum(df$elig_meal == 1, na.rm = TRUE) / nrow(df)
  
  # Percent students who are ELLs
  ells <- sum(df$elig_eng == 1, na.rm = TRUE) / nrow(df)
  
  # Percent students who are eligible for special education
  sped <- sum(df$elig_sped == 1, na.rm = TRUE) / nrow(df)
  
  # Combine results into a data frame
  results <- data.frame(
    Demographics = c("Female", names(race_percentages), "FRPL", "ELL", "Special Education"),
    Percentage = c(female, race_percentages, frpl, ells, sped)
  )
  
  # Format the percentages to two decimal points
  results$Percentage <- round(results$Percentage * 100, 2)
  
  # Rename the column to distinguish by dataframe
  colnames(results)[2] <- paste(df_name, "(%)", sep = " ")
  
  return(results)
}


##----------------------------------------------------------------
##                      Subsetting Dataframe                     -
##----------------------------------------------------------------

# Treated
c2_treat <- c2_all %>%
  filter(treatment == 1)

# Control
c2_control <- c2_all %>%
  filter(treatment == 0)

# State = TN
c2_tn <- c2_all %>%
  filter(state == "TN")

# State = KY
c2_ky <- c2_all %>%
  filter(state == "KY")

##----------------------------------------------------------------
##                        Producing Output                       -
##----------------------------------------------------------------

perc_full <- percentages(c2_all, "Cohort_2_Full")
perc_treat <- percentages(c2_treat, "Treated")
perc_control <- percentages(c2_control, "Control")
perc_tn <- percentages(c2_tn, "Tennessee")
perc_ky <- percentages(c2_ky, "Kentucky")


#################################################################
##                      Exporting Results                      ##
#################################################################
wb <- createWorkbook()

# Function to add a dataframe to the workbook
add_percentages_sheet <- function(wb, df, sheet_name) {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet = sheet_name, df, startRow = 2, colNames = TRUE)
  writeData(wb, sheet = sheet_name, "Demographics Data", startRow = 1, startCol = 1, colNames = FALSE)
  setColWidths(wb, sheet = sheet_name, cols = 1:ncol(df), widths = "auto")
}

# Add each table to a new sheet
add_percentages_sheet(wb, perc_full, "Cohort_2_Full")
add_percentages_sheet(wb, perc_treat, "Treated")
add_percentages_sheet(wb, perc_control, "Control")
add_percentages_sheet(wb, perc_tn, "Tennessee")
add_percentages_sheet(wb, perc_ky, "Kentucky")

# Exporting workbook
date <- Sys.Date()
date <- format(date, "%m%d")
file_name <- paste0("C2_Demographics", date, ".xlsx")
saveWorkbook(wb, file = file_name, overwrite = TRUE)
