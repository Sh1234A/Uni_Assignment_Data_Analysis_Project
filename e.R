# Load necessary libraries
library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)
library(e1071)
library(reshape2)
library(rpart)
library(vip)
library(scales)
library(tidyverse)
library(lubridate)
library(forecast)
library(caTools)
library(Metrics)
library(readr)
library(scales)


#================================================================================
# Load dataset
data <- read.csv("C:\\Users\\Shatha\\Desktop\\New folder\\hackingData_cleaned.csv")
#================================================================================


# Convert categorical variables to factors

data$Encoding <- as.factor(data$Encoding)  
data$OS <- as.factor(data$OS)


data$Loss <- as.numeric(data$Loss)
#================================================================================
                    # Descriptive & Exploratory Analysis
                    #====================================

 
## ## How are different encoded categories distributed by loss?



# Remove missing values in Encoding and Loss columns
data_filtered <- data %>% filter(!is.na(Encoding), !is.na(Loss))

# Get frequency distribution of Encoding types
encoding_freq <- data_filtered %>% count(Encoding) %>% arrange(desc(n))

# Compute total loss per Encoding type
encoding_loss <- data_filtered %>% group_by(Encoding) %>% summarise(Total_Loss = sum(Loss, na.rm = TRUE))

# Merge frequency and loss data
encoding_summary <- merge(encoding_freq, encoding_loss, by = "Encoding") %>% 
  arrange(desc(Total_Loss))

# Display summary table
print(encoding_summary, row.names = FALSE)
View(encoding_summary)

# Generate frequency table using base R
print(table(data_filtered$Encoding))

# Create a contingency table to cross-tabulate Encoding and Loss categories
encoding_loss_table <- data_filtered %>% 
  mutate(Loss_Category = cut(Loss, breaks = quantile(Loss, probs = seq(0,1,0.25), na.rm = TRUE), 
                             include.lowest = TRUE)) %>%
  count(Encoding, Loss_Category)

# Display cross-tabulation
print(encoding_loss_table, row.names = FALSE)
View(encoding_loss_table)


# Identify and classify variables
categorical_vars <- c("Encoding", "OS", "WebServer", "Country")
continuous_vars <- c("Ransom", "Loss", "DownTime")

# Distribution of Encoded Attack Types
pastel_colors <- scales::alpha(rainbow(length(unique(data$Encoding))), 0.6) 

attack_plot <- ggplot(data, aes(x = Encoding, fill = Encoding)) +
  geom_bar() +
  scale_fill_manual(values = pastel_colors) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Distribution of Encoding Types") +
  labs(x = "Encoding Type", y = "Count")

print(attack_plot)






#================================================================================
                     # Diagnostic & Hypothesis Testing
                     #================================


## Do certain encoded attack types result in significantly higher financial losses?



# Summary statistics of loss per encoded attack type
summary_stats <- data %>%
  group_by(Encoding) %>%
  summarise(mean_loss = mean(Loss, na.rm = TRUE),
            median_loss = median(Loss, na.rm = TRUE),
            sd_loss = sd(Loss, na.rm = TRUE),
            count = n())
print(summary_stats)

# Hypothesis Testing - ANOVA to check for significant differences in loss
anova_result <- aov(Loss ~ Encoding, data = data)
anova_summary <- summary(anova_result)
print(anova_summary)

# Post-hoc Tukey test if ANOVA is significant
if (anova_summary[[1]]["Pr(>F)"][1] < 0.05) {
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
}

# Box Plot Visualization
loss_encoding_plot <- ggplot(data, aes(x = Encoding, y = Loss, fill = Encoding)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16) +
  scale_fill_viridis_d() +  # Better color scheme
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Financial Loss by Encoding Type") +
  labs(x = "Encoding Type", y = "Financial Loss")

print(loss_encoding_plot)

#--------------------------------------------------------------------------------------------------
## Are encoded variables correlated with financial losses?
## Visualization: Heatmap 

# Convert categorical encoded variables to numerical for correlation analysis
encoded_numeric <- model.matrix(~ Encoding - 1, data = data)

# Combine numerical encoded variables with financial loss
cor_data <- cbind(encoded_numeric, Loss = data$Loss)

# Compute correlation matrix
cor_matrix <- cor(cor_data, use = "complete.obs")
print(cor_matrix)

# Hypothesis Testing - Spearman correlation for non-parametric data
cor_results <- apply(encoded_numeric, 2, function(x) cor.test(x, data$Loss, method = "spearman"))

# Extract and print correlation coefficients and p-values
cor_summary <- data.frame(
  Variable = names(cor_results),
  Correlation = sapply(cor_results, function(x) x$estimate),
  P_Value = sapply(cor_results, function(x) x$p.value)
)
print(cor_summary)

# Identify significant correlations
significant_vars <- cor_summary %>% filter(P_Value < 0.05)
print(significant_vars)

# Prepare the correlation matrix for heatmap
melted_cor_matrix <- melt(cor_matrix)

# Heatmap visualization
heatmap_plot <- ggplot(melted_cor_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "maroon", mid = "white", midpoint = 0) +
  ggtitle("Heatmap of Encoding vs. Loss") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate labels for readability

# Print the heatmap
print(heatmap_plot)

#================================================================================
                            # Predictive Modeling
                            #====================

## Can Encoding and other categorical variables be used to predict financial loss?

# One-Hot Encoding for categorical variables
data_onehot <- data  # Ensure Loss column is retained
if("Encoding" %in% colnames(data)) {
  encoded_matrix <- model.matrix(~Encoding - 1, data=data_onehot)
  data_onehot <- cbind(data_onehot[, !colnames(data_onehot) %in% c("Encoding")], encoded_matrix)
  }
  data_onehot$Encoding <- NULL


# Label Encoding for categorical variables
data_label <- data
if("Encoding" %in% colnames(data)) {
  data_label$Encoding <- factor(data_label$Encoding)
}





# Train-Test Split
set.seed(123)
split <- sample.split(data$Loss, SplitRatio = 0.8)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

dim(train_data)
dim(test_data)
dim(data)

# Train linear regression model
library(MASS)
numeric_cols <- sapply(train_data, is.numeric)
train_data_reduced <- train_data[, numeric_cols]
linear_model <- lm(Loss ~ ., data = train_data_reduced)
summary(linear_model)

# Predict on test set
predictions <- predict(linear_model, test_data)

# Metrics for regression performance
mae_metric <- mean(abs(test_data$Loss - predictions))
mse_metric <- mean((test_data$Loss - predictions)^2)
rmse_metric <- sqrt(mse_metric)
r2_metric <- summary(linear_model)$r.squared

# Print results
print("Linear Regression Model Performance:")
print(paste("MAE:", mae_metric))
print(paste("MSE:", mse_metric))
print(paste("RMSE:", rmse_metric))
print(paste("R-squared:", r2_metric))

# Visualization - Scatter Plot
scatter_plot <- ggplot(data_label, aes(x = Encoding, y = Loss)) +
  geom_point(color = "#69b3a2", alpha = 0.7, size = 4) +
  geom_smooth(method = "lm", color = "#ff4d4d", se = TRUE, linetype = "solid") +
  labs(title = "Encoding vs. Financial Loss",
       x = "Encoding Type",
       y = "Financial Loss") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 12),
        panel.grid.major = element_line(color = "#d3d3d3"),
        panel.grid.minor = element_blank()) +
  scale_x_discrete(labels = levels(data_label$Encoding))

print(scatter_plot)








#-----------------------------------------------------------------------------------------------
## How do different encoding techniques impact predictive performance?

# Convert OS to a factor
data$OS <- as.factor(data$OS)
print("Converted OS column to a factor.")

# Train-Test Split
set.seed(123)
split <- sample.split(data$Loss, SplitRatio = 0.8)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)
print("Performed Train-Test Split.")

#One-Hot Encoding
data_onehot <- data

if ("OS" %in% colnames(data_onehot)) {
  encoded_matrix <- model.matrix(~ OS - 1, data = data_onehot)  # One-hot encode OS
  data_onehot <- cbind(data_onehot[, !colnames(data_onehot) %in% c("OS")], encoded_matrix)
  print("Applied One-Hot Encoding on OS column.")
}

#  Ensure `Loss` column is retained
if (!"Loss" %in% colnames(data_onehot)) {
  data_onehot$Loss <- data$Loss
  print("Re-added Loss column after one-hot encoding.")
}

#  Train-Test Split for One-Hot Encoded Data
set.seed(123)
split <- sample.split(data_onehot$Loss, SplitRatio = 0.8)
train_data_onehot <- subset(data_onehot, split == TRUE)
test_data_onehot <- subset(data_onehot, split == FALSE)
print("Performed Train-Test Split on One-Hot Encoded Data.")

# Print structure of the training dataset after encoding
print("Structure of train_data_onehot:")
print(str(train_data_onehot))

# Ensure `Loss` exists after one-hot encoding
if (!"Loss" %in% colnames(train_data_onehot)) {
  stop("Error: Loss column is missing from train_data_onehot")
}

# Print summary of train_data_onehot
print("Summary of train_data_onehot:")
print(summary(train_data_onehot))





#================================================================================
                          # Prescriptive Analysis
                          #======================

# How should encoding strategies be optimized for cybersecurity risk analysis?

# Recommendation 1:
# If a particular encoding type is associated with high financial loss (above the 75th percentile),
# organizations should prioritize security improvements and consider alternative encoding techniques
# to mitigate risks.
# Compute percentiles
percentiles <- quantile(data$Loss, probs = c(0.75))

# Recommendation 2:
# If an encoding type has moderate or low financial loss, maintaining the current strategy is advised,
# but periodic reviews should be conducted to ensure continued security effectiveness.
# Categorize encoding types based on financial loss
data <- data %>%
  group_by(Encoding) %>%
  summarise(avg_loss = mean(Loss)) %>%
  mutate(recommendation = case_when(
    avg_loss > percentiles[1] ~ "Prioritize security improvements and consider alternatives",
    avg_loss <= percentiles[1] ~ "Maintain current strategy with periodic reviews"
  ))

# Recommendation 3:
# The faceted bar chart visualizes loss distribution across encoding types, helping to identify patterns
# that inform strategic cybersecurity decisions.
# Visualization: Faceted bar chart

# Display plot
plot1 <- ggplot(data, aes(x = Encoding, y = avg_loss, fill = recommendation)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Importance level",
       x = "Encoding Type", y = "Average Financial Loss",
       fill = "Security prioritization")

print(plot1)

