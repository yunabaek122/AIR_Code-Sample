## ---------------------------
## Yuna Baek: Code Sample
## 
##  1) Produce a clean data file
##  2) Conduct implementation analysis
## 
## ---------------------------

##################################################################
##                            Set-Up                            ##
##################################################################

#Packages
library(dplyr)
library(tidyr)
library(writexl)
library(openxlsx)
library(ggplot2)
library(stringr)

setwd("E:/EF+Math Evaluation/Data/Fraction Ball/Implementation Logs/")

# Import Files
activity <- read.csv("Clean Data/Teacher_Activity_Level_Analyses_File_0808.csv")
teacher <- read.csv("Clean Data/Teacher_Level_Analyses_File_0808.csv")

##################################################################
##                    Teacher-Level Analyses                    ##
##################################################################

# Summary Statistics
average_teachers <- teacher %>%
  summarise(
    # Average number of court activities
    court = mean(Num_Court_Activities, na.rm = TRUE),
    court_sd = sd(Num_Court_Activities, na.rm = TRUE),

    # Average time spent on court activities
    court_total = mean(Total_Minutes_Court, na.rm = TRUE),
    court_total_sd = sd(Total_Minutes_Court, na.rm = TRUE),

    # Average of teacher average time per court activity
    court_avg = mean(Avg_Minutes_Court, na.rm = TRUE),
    court_avg_sd = sd(Avg_Minutes_Court, na.rm = TRUE),

    # Average of teacher average percentage of court components completed
    court_fully_complete = mean(c(Avg_Pct_Fully_Complete_Court), na.rm = TRUE),
    court_fully_complete_sd = sd(c(Avg_Pct_Fully_Complete_Court), na.rm = TRUE),
    court_part_complete = mean(c(Avg_Pct_Part_Complete_Court), na.rm = TRUE),
    court_part_complete_sd = sd(c(Avg_Pct_Part_Complete_Court), na.rm = TRUE),
  )

# Long format
average_teachers_long <- average_teachers %>%
  pivot_longer(
    cols = everything(),
    names_to = "Item",
    values_to = "Value"
  ) %>%
  mutate(
    Type = ifelse(grepl("_sd$", Item), "SD", "Output"),
    Item = gsub("_sd$", "", Item)
  ) %>%
  pivot_wider(
    names_from = Type,
    values_from = Value
  ) %>%
  select(Item, Output, SD)

#################################################################
##                   Activity-Level Analyses                   ##
#################################################################

# Total number of teachers (unique Teacher ID)
total_teachers <- n_distinct(activity$Teacher_ID)

# Subsetting: Court Activities
court <- activity %>%
  filter(Activity_Type == "Court") %>%
  arrange(Activity_Number, Activity_Name) %>%
  select(Activity_Name, Activity_Number, everything())

##---------------------------------------------------------------
##                  % Teachers using activities                 -
##---------------------------------------------------------------

# court activities 
court_percent <- court %>%
  group_by(Activity_Number, Activity_Name) %>%
  summarise(
    Teacher_Count = n_distinct(Teacher_ID),  # Unique Teacher_IDs per activity
    Teachers_Percent = Teacher_Count / total_teachers
  )

# By grade level
# Grade 4
total_teachers4 <- n_distinct(activity$Teacher_ID[activity$Grade_Level == 4])
# court activities 
court_percent4 <- court %>%
  filter(Grade_Level == 4) %>%
  group_by(Activity_Number, Activity_Name) %>%
  summarise(
    Teacher_Count = n_distinct(Teacher_ID),  # Unique Teacher_IDs per activity
    Teachers_Percent = Teacher_Count / total_teachers4
  )

##---------------------------------------------------------------
##                  Average number of minutes                   -
##---------------------------------------------------------------
# Court
court_minute <- court %>%
  group_by(Activity_Number, Activity_Name) %>%
  summarise(
    Average_Minutes = mean(Activity_Minutes, na.rm = TRUE)
  )

# Grade 4
court_minute4 <- court %>%
  filter(Grade_Level == 4) %>%
  group_by(Activity_Number, Activity_Name) %>%
  summarise(
    Average_Minutes = mean(Activity_Minutes, na.rm = TRUE)
  )

# Grade 5
court_minute5 <- court %>%
  filter(Grade_Level == 5) %>%
  group_by(Activity_Number, Activity_Name) %>%
  summarise(
    Average_Minutes = mean(Activity_Minutes, na.rm = TRUE)
  )

##----------------------------------------------------------------
##        Average percentage of components fully completed       -
##----------------------------------------------------------------
# Court
court_fully <- court %>%
  group_by(Activity_Number, Activity_Name) %>%
  summarise(
    Average_Completion_Fully = mean(Pct_Fully_Complete, na.rm = TRUE), 
  )

##----------------------------------------------------------------
##      Average percentage of components partially completed     -
##----------------------------------------------------------------
# Court
court_part <- court %>%
  group_by(Activity_Number, Activity_Name) %>%
  summarise(
    Average_Completion_Partially = mean(Pct_Part_Complete, na.rm = TRUE), 
  )

##################################################################
##               Creating Activity Level Datafile               ##
##################################################################

activity_single <- activity %>%
  group_by(Activity_Number, Activity_Name, Activity_Type) %>%
  mutate(
    # Counting Non-NA values
    orient_count = ifelse(any(is.na(Complete_Orient)), NA, sum(Complete_Orient %in% c("Fully completed", "Partially completed", "Not completed"))),
    rotate_count = ifelse(any(is.na(Complete_Rotate)), NA, sum(Complete_Rotate %in% c("Fully completed", "Partially completed", "Not completed"))),

    # Percentages of fully completed
    Fully_Complete_Orient = sum(Complete_Orient %in% c("Fully completed")) / orient_count,
    Fully_Complete_Rotate = sum(Complete_Rotate %in% c("Fully completed")) / rotate_count,

    # Percentages of at least partially completed
    Part_Complete_Orient = sum(Complete_Orient %in% c("Fully completed", "Partially completed")) / orient_count, 
    Part_Complete_Rotate = sum(Complete_Orient %in% c("Fully completed", "Partially completed")) / rotate_count
  ) %>%
  select(ends_with("_count") | 
    starts_with("Fully_Complete") | starts_with("Part_Complete")
  ) %>%
  distinct()

write.csv(activity_single, "Clean Data/Activity_Level_Analyses_File_0808.csv", row.names = FALSE)

# Combining results
court_combined <- court_percent %>%
  left_join(court_minute, by = c("Activity_Name", "Activity_Number")) %>%
  left_join(court_fully, by = c("Activity_Name", "Activity_Number")) %>%
  left_join(court_part, by = c("Activity_Name", "Activity_Number")) %>%
  left_join(court_s, by = c("Activity_Name", "Activity_Number")) %>%
  select(-Activity_Type)

#################################################################
##                   Exporting Data files                      ##
#################################################################
# Replacing NA with string 
replace_na_with_na_string <- function(df) {
  df[] <- lapply(df, function(x) {
    if (is.character(x)) {
      x[is.na(x)] <- "NA"
    } else if (is.factor(x)) {
      x <- as.character(x)
      x[is.na(x)] <- "NA"
      x <- as.factor(x)
    } else {
      x[is.na(x)] <- "NA"
    }
    return(x)
  })
  return(df)
}

teacher <- replace_na_with_na_string(teacher)
activity <- replace_na_with_na_string(activity)

df <- createWorkbook()

# Teacher-Level Analyses
addWorksheet(df, "Teacher-Level")
writeData(df, sheet = "Teacher-Level", teacher, startRow = 2, colNames = TRUE)
          # keepNA = openxlsx_getOp("keepNA", TRUE),
          # na.string = openxlsx_getOp("na.string"),) 
setColWidths(df, sheet = "Teacher-Level", cols = 1:ncol(teacher), widths = "auto")

# Teacher-Activity-Level Analyses
addWorksheet(df, "Teacher_Activity-Level")
writeData(df, sheet = "Teacher_Activity-Level", activity, startRow = 2, colNames = TRUE)
setColWidths(df, sheet = "Teacher_Activity-Level", cols = 1:ncol(activity), widths = "auto")

# Save the workbook
saveWorkbook(df, "Analysis/Datafiles_all.xlsx", overwrite = TRUE)


##################################################################
##                        Results Graphs                        ##
##################################################################

##---------------------------------------------------------------
##     Scatterplot: activities (x-axis) by duration (y-axis)    -
##---------------------------------------------------------------

# Court
# Sort by Activity_Number for the plot
court_minute <- court_minute %>%
  mutate(Activity_Name = factor(Activity_Name, levels = unique(Activity_Name)))  # Reorder factor levels

p2_court_duration <- ggplot(court_minute, aes(x = Activity_Name, y = Average_Minutes, color = Activity_Name)) +
  geom_point(size = 2) +
  labs(
    title = "Activities by Duration",
    x = "Activities",
    y = "Duration"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_color_discrete(name = "Activity Name")
ggsave("Analysis/p2_court_duration.jpg", plot = p2_court_duration, width = 8, height = 6)

##---------------------------------------------------------------
##                Column Chart: % Use of Activity               -
##---------------------------------------------------------------
court_combined <- court_combined %>%
  mutate(Activity_Name = factor(Activity_Name, levels = unique(Activity_Name)))  # Reorder factor levels

# court
p3_court_use <- ggplot(court_combined, aes(x = Activity_Name, y = Teachers_Percent)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = sprintf("%.1f%%", Teachers_Percent)), vjust = -0.5, color = "black") +
  labs(
    title = "Percentage of Teachers Using Each Court Activity",
    x = "Activity Name",
    y = "Percentage (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggsave("Analysis/p3_court_use.jpg", plot = p3_court_use, width = 8, height = 6)
