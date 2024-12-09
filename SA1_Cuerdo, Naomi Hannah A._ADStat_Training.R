library(tidyverse)
library(car)
library(rstatix)

install.packages("rstatix")


data <-read.csv("C:/Users/naomi/Downloads/Alzheimers-Mice-Data.csv")

data$AD_Status <-as.factor(data$AD_Status)
data$Treatment <- as.factor(data$Treatment)

head(data)
summary(data)

#Training

# Boxplot
ggplot(data, aes(x = AD_Status, y = Training, fill = Treatment)) +
  geom_boxplot() +
  labs(title = "Boxplot of Training Errors by AD Status and Treatment")

# Normality Check
shapiro_results <- data %>%
  group_by(AD_Status, Treatment) %>%
  summarise(
    shapiro_stat = shapiro.test(Training)$statistic,
    p_value = shapiro.test(Training)$p.value
  )

print(shapiro_results)

#Levene Test
levene_test(Training ~ AD_Status * Treatment, data = data)

#2B Anova for Training
anova_training <- aov(Training ~ AD_Status * Treatment, data = data)
summary(anova_training)

#Post Hoc Analysis using Tukey's HSD
post_hoc <-TukeyHSD(anova_training, "Treatment", conf.level = 0.95)
print(post_hoc)




