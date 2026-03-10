library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(packcircles)
library(ggforce)
library(vegan)
library(RColorBrewer)
library(plyr)
library(gridExtra)

cpa<-read.csv("crownarea.csv")

#compute means
mean_cpa<- cpa %>%
  group_by(age_2022) %>%
  dplyr::summarise(mean_cpa = mean(area, na.rm = TRUE),
                   se = sd(area) / sqrt(n()),
                   .groups = 'drop')
View(mean_cpa)

#plot means
cpa_plot<-ggplot(mean_cpa, aes(x = age_2022, y = mean_cpa)) +
  geom_point(aes(colour="pink"), size = 3) +
  scale_y_continuous(limits = c(0, 75)) + #plot for means
  geom_errorbar(
    aes(ymin = mean_cpa - se, ymax = mean_cpa + se),
    width = 0.2,  # Error bar width
    color = "black"
  ) +
  labs(x = "Cant age", y = "Mean estimated crown projection area (CPA) (sq. m)") +
  theme_minimal() +
  theme(legend.position = "top") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # remove major gridlines
    panel.grid.minor = element_blank()   # remove minor gridlines
  ) +
  geom_hline(yintercept = 0, color = "grey50") +  # horizontal center line
  geom_vline(xintercept = 0, color = "grey50") + # vertical center line
  theme(legend.position="none")
plot(cpa_plot)

#model changing CPA according to cant age

#quadratic regression
cpa_mod<-lm(sqrt(mean_cpa) ~ age_2022, mean_cpa)
summary(cpa_mod)
res_cpa<-resid(cpa_mod)
qqnorm(res_cpa)
qqline(res_cpa)
plot(density(res_cpa), main = "Residuals")

#plot quad regression
ggplot(mean_cpa, aes(x = age_2022, y = sqrt(mean_cpa))) +
  geom_point(colour = "pink", size=3) +
  scale_y_continuous(limits = c(0, 10)) + #plot for means +
  labs(x = "Cant age", y = "√Mean estimated crown projection area (sq. m)") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # remove major gridlines
    panel.grid.minor = element_blank()   # remove minor gridlines
  ) +
  geom_hline(yintercept = 0, color = "grey50") +  # horizontal center line
  geom_vline(xintercept = 0, color = "grey50") + # vertical center line
  theme(legend.position = "none") +
  geom_smooth(method = "lm", formula = y ~ x, color="black")

#log regression
cpa_mod<-lm(log(mean_cpa) ~ age_2022, mean_cpa)
summary(cpa_mod)
res_cpa<-resid(cpa_mod)
qqnorm(res_cpa)
qqline(res_cpa)
plot(density(res_cpa), main = "Residuals")