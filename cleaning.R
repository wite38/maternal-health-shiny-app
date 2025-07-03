# Install packages
install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")


# Load libraries
library(readr)
library(dplyr)
library(ggplot2)
library(readr)

setwd("C:/Users/dawit habte/Desktop/myproject/my_project")

# Load data
maternal_data <- read_csv("maternal_health_risk_data_set.csv")


# View first few rows
head(maternal_data)

# Check structure
str(maternal_data)
#Renaming
maternal_data <- maternal_data %>%
  rename(
    Age = Age,
    SystolicBP = `SystolicBP`,
    DiastolicBP = `DiastolicBP`,
    BloodSugar = `BS`,
    BodyTemp = `BodyTemp`,
    HeartRate = `HeartRate`,
    RiskLevel = `RiskLevel`
  )
# Check missing values
colSums(is.na(maternal_data))
summary(maternal_data)

filter(maternal_data, Age < 15 | Age > 50)

maternal_data_filtered <- maternal_data %>%
  filter(Age >= 15 & Age <= 50)

filter(maternal_data, SystolicBP < 90 | SystolicBP > 180)

filter(
  !is.na(Age) & !is.na(SystolicBP) & !is.na(DiastolicBP) &
    !is.na(BloodSugar) & !is.na(BodyTemp) & !is.na(HeartRate) & !is.na(RiskLevel),
  Age >= 15 & Age <= 50,
  SystolicBP >= 60 & SystolicBP <= 220,
  DiastolicBP >= 30 & DiastolicBP <= 140,
  BloodSugar >= 2.5 & BloodSugar <= 20,  # Hypothetical reasonable limits
  BodyTemp >= 33 & BodyTemp <= 42,       # Human temp range
  HeartRate >= 35 & HeartRate <= 150     # Reasonable HR
) %>%
  mutate(RiskLevel = as.factor(RiskLevel))
  summary(maternal_data_filtered)
summary(maternal_data)
summary(maternal_data_filtered)

maternal_data_filtered <- maternal_data_filtered %>%
  mutate(RiskLevel = as.factor(RiskLevel))#mutate from character to factor
str(maternal_data_filtered$RiskLevel)

maternal_data_filtered <- maternal_data_filtered %>%
  mutate(
    RiskLevelNum = case_when(
      RiskLevel == "high risk" ~ 3,
      RiskLevel == "mid risk"  ~ 2,
      RiskLevel == "low risk"  ~ 1,
      TRUE ~ NA_real_  # for any unexpected values
    )
  )

write_csv(maternal_data_filtered, "cleaned_maternal_data")

