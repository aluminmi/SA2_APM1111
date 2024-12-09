library(tidyverse)
library(car)
library(rstatix)

data <-read.csv("C:/Users/naomi/Downloads/Alzheimers-Mice-Data.csv")

data$AD_Status <-as.factor(data$AD_Status)
data$Treatment <- as.factor(data$Treatment)

head(data)
summary(data)

#Boxplot
ggplot(data, aes(x = AD_Status, y = Memory, fill = Treatment)) +
  geom_boxplot() +
  labs(title = "Boxplot of Memory Errors by AD Status and Treatment")

#Shapiro Wilk 
shapiro_results_memory <- data %>%
  group_by(AD_Status, Treatment) %>%
  summarise(
    shapiro_stat = shapiro.test(Memory)$statistic,
    p_value = shapiro.test(Memory)$p.value
  )
print(shapiro_results_memory)

#Levene Test
leveneTest(Memory ~ AD_Status * Treatment, data = data)

#2B ANOVA
anova_memory <- aov(Memory ~ AD_Status * Treatment, data = data)
summary(anova_memory)

#Post Hoc
post_hoc_memory <- TukeyHSD(anova_memory, "Treatment", conf.level = 0.95)
print(post_hoc_memory)
plot(post_hoc_memory)
