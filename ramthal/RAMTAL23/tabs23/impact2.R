# Load required libraries
library(dplyr)
library(lmtest)
library(sandwich)

# Ensure your data frame (df) contains these variables:
# Treatment: 1 if inside the project area, 0 otherwise
# Post: 1 for endline, 0 for baseline
# Y: Outcome variable (e.g., DI adoption)
# Control variables: Age, Gender, Caste, Education (or others)

# Step 0: organize clean df


# Step 1: Create the interaction term for DiD
df <- df %>%
  mutate(Treatment_Post = Treatment * Post)

# Step 2: Fit the DiD regression model
did_model <- lm(Y ~ Treatment + Post + Treatment_Post + Age + Gender + Caste + Education, data = df)

# Step 3: Summarize the model with robust standard errors
summary_did <- coeftest(did_model, vcov = vcovHC(did_model, type = "HC1"))

# Print the summary
print(summary_did)

# Step 4: Optional - Save the results to a CSV file
write.csv(as.data.frame(summary_did), "DiD_Results.csv")

# Step 5: Visualization (optional)
library(ggplot2)

# Group mean calculation for visualization
means <- df %>%
  group_by(Treatment, Post) %>%
  summarise(Mean_Y = mean(Y, na.rm = TRUE)) %>%
  mutate(Group = ifelse(Treatment == 1, "Treatment (Inside Project)", "Control (Outside Project)"),
         Time = ifelse(Post == 1, "Endline (2022)", "Baseline (2016)"))

# Visualization
ggplot(means, aes(x = Time, y = Mean_Y, group = Group, color = Group)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Difference-in-Differences: DI Adoption",
       x = "Time",
       y = "Mean DI Adoption Rate") +
  theme_minimal()









