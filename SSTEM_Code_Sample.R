## ---------------------------
## Yuna Baek: Code Sample
## ---------------------------

library(readxl)
library(dplyr)
library(tidyr)

# Load data
students <- read_excel("CLEAN_EBP_S-STEM_StudentData_Fall.xlsx")

# Create indicator variables
students <- students %>%
  mutate(
    grad_4 = ifelse(enrol == "Graduated" | left_reason == "Graduated", 1, 
                    ifelse(!is.na(enrol) & enrol != "Graduated" | !is.na(left_reason) & left_reason != "Graduated", 0, NA_real_)),
    grad_2 = ifelse(enrol == "Graduated" | left_reason %in% c("Graduated", "Transferred to a 4-year institution"), 1, 
                    ifelse(!is.na(enrol) & enrol != "Graduated" | !is.na(left_reason) & !left_reason %in% c("Graduated", "Transferred to a 4-year institution"), 0, NA_real_)),
    work = ifelse(next_step == "Entered the workforce", 1, 0),
    school = ifelse(next_step == "Continued school elsewhere", 1, 0)
  )

# Subset data for seniors and sophomores
seniors_4 <- filter(students, type == "4" & class == "Senior")
soph_2 <- filter(students, type == "2" & class == "Sophomore")

# Calculate overall graduation rates
graduation_4 <- mean(seniors_4$grad_4, na.rm = TRUE)
graduation_2 <- mean(soph_2$grad_2, na.rm = TRUE)

# Calculate scholarship correlations
corr_sch_grad_4 <- cor(seniors_4$scholarship, seniors_4$grad_4, use = "complete.obs")
corr_sch_grad_2 <- cor(soph_2$scholarship, soph_2$grad_2, use = "complete.obs")

# Explore EBP participation
students <- students %>% 
  mutate(ebp_participant = ifelse(EBP == "Yes", 1, 0))

# Summarize EBP statistics
eb_participation_rate <- mean(students$ebp_participant, na.rm = TRUE)

# Calculate correlation between EBP participation and graduation
eb_corr_grad_4 <- cor(seniors_4$ebp_participant, seniors_4$grad_4, use = "complete.obs")
eb_corr_grad_2 <- cor(soph_2$ebp_participant, soph_2$grad_2, use = "complete.obs")

# Create summary results table
results <- data.frame(
  Statistic = c("Overall Graduation Rate", "Scholarship & Graduation Correlation", "EBP Participation Rate", "EBP & Graduation Correlation"),
  `4-Year` = c(graduation_4, corr_sch_grad_4, eb_participation_rate, eb_corr_grad_4),
  `2-Year` = c(graduation_2, corr_sch_grad_2, eb_participation_rate, eb_corr_grad_2)
)

print(results)