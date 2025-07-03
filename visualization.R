write_csv(maternal_data_filtered, "cleaned_maternal_data.csv")
# install.packages("readr")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("reshape2")

library(readr)
library(dplyr)
library(ggplot2)
library(reshape2)

# Load your cleaned data
clean_data <- read_csv("cleaned_maternal_data.csv")

# Quick look at the data
head(clean_data)
str(clean_data)
summary(clean_data)

# Descriptive statistics by RiskLevel
summary_by_risk <- clean_data %>%
  group_by(RiskLevel) %>%
  summarise(
    Count = n(),
    Mean_Age = mean(Age, na.rm = TRUE),
    Mean_SystolicBP = mean(SystolicBP, na.rm = TRUE),
    Mean_DiastolicBP = mean(DiastolicBP, na.rm = TRUE),
    Mean_BloodSugar = mean(BloodSugar, na.rm = TRUE),
    Mean_BodyTemp = mean(BodyTemp, na.rm = TRUE),
    Mean_HeartRate = mean(HeartRate, na.rm = TRUE)
  )

print("Summary statistics grouped by Risk Level:")
print(summary_by_risk)

# Visualization 1: Count by Risk Level
clean_data$RiskLevel <- factor(clean_data$RiskLevel, levels = c("low risk", "mid risk", "high risk"))

# Visualization 2: Age Distribution
ggplot(clean_data, aes(x = Age)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Age Distribution of Patients", x = "Age (years)", y = "Frequency")

# Visualization 3: Boxplot SystolicBP by Risk Level
ggplot(clean_data, aes(x = RiskLevel, y = SystolicBP, fill = RiskLevel)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Systolic Blood Pressure by Risk Level", x = "Risk Level", y = "Systolic BP (mmHg)") +
  scale_fill_brewer(palette = "Pastel1")

# Visualization 4: Scatterplot Age vs BloodSugar by Risk Level
ggplot(clean_data, aes(x = Age, y = BloodSugar, color = RiskLevel)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Age vs Blood Sugar by Risk Level", x = "Age", y = "Blood Sugar") +
  scale_color_brewer(palette = "Dark2")

# Visualization 5: Correlation Heatmap of Numeric Variables
num_vars <- clean_data %>%
  select(Age, SystolicBP, DiastolicBP, BloodSugar, BodyTemp, HeartRate)

cor_matrix <- cor(num_vars, use = "complete.obs")
melted_cor <- melt(cor_matrix)

ggplot(melted_cor, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  labs(title = "Correlation Heatmap of Numeric Variables", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(reshape2)
library(ggplot2)

# Select numeric columns including RiskLevelNum
num_vars <- clean_data %>%
  select(Age, SystolicBP, DiastolicBP, BloodSugar, BodyTemp, HeartRate, RiskLevelNum)

# Compute correlation matrix using complete observations only
cor_matrix <- cor(num_vars, use = "complete.obs")

# Melt correlation matrix for plotting
melted_cor <- melt(cor_matrix)

# Plot heatmap
ggplot(melted_cor, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                       limits = c(-1,1), name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap Including RiskLevelNum", x = "", y = "")

library(tidyr)

# Select relevant columns
plot_data <- clean_data %>%
  select(RiskLevel, Age, SystolicBP, DiastolicBP, BloodSugar) %>%
  pivot_longer(cols = -RiskLevel, names_to = "Variable", values_to = "Value")

# Plot boxplots for each variable by RiskLevel
ggplot(plot_data, aes(x = RiskLevel, y = Value, fill = RiskLevel)) +
  geom_boxplot() +
  facet_wrap(~ Variable, scales = "free_y") +   # separate panels for each variable
  theme_minimal() +
  labs(title = "Boxplots of Variables by Risk Level",
       x = "Risk Level",
       y = "Value") +
  scale_fill_brewer(palette = "Pastel1") +
  theme(legend.position = "none")

write_csv(clean_data, "cleaned_maternal_data.csv")
