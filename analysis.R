# Workbook 6: analyze NHANES data

# Set up
library(foreign)
library(survey)
library(Hmisc)

demo <- sasxport.get('DEMO_I.XPT')
alco <- sasxport.get('ALQ_I.XPT')

nhanes <- merge(x = demo, y = alco, by = 'seqn', all = TRUE)

wt_sum <- sum(nhanes$wtint2yr, na.rm = TRUE)
#this number represents the total US population at the time

#Analysis

#In ALQ151, we want 2 to be 0, and we want to ignore or disregard 7 and 9
nhanes$alq151[nhanes$alq151 == 2] <- 0
nhanes$alq151[nhanes$alq151 == 7] <- NA
nhanes$alq151[nhanes$alq151 == 9] <- NA

# create a survey design
nhanes_survey <- svydesign(
  id = ~sdmvpsu,
  nest = TRUE,
  strata = ~sdmvstra,
  weights = ~wtint2yr,
  data = nhanes
)

nhanes_mean <- svymean(~nhanes$alq151, nhanes_survey, na.rm = TRUE)

mean_by_gender <- svyby(~alq151, ~riagendr, nhanes_survey, svymean, na.rm = TRUE)
