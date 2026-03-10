library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)

cpa_raw<-read.csv("crownarea.csv")
View(cpa_raw)

cpa<- cpa_raw %>%
  group_by(age_2022) %>%
  dplyr::summarise(mean_area = mean(area, na.rm = TRUE), 
                   se = sd(area) / sqrt(n()),
                   .groups = 'drop')

crown_area<-ggplot(cpa, aes(x = age_2022, y = mean_area)) +
  geom_point(aes (color = "pink", size=3)) +
  scale_y_continuous(limits = c(0, 70)) + #plot for means
  geom_errorbar(
    aes(ymin = mean_area - se, ymax = mean_area + se),
    width = 0.2,  # Error bar width
    color = "black"
  ) +
  labs(x = "Cant age in 2022", y = "Crown projection area (m^2)") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # remove major gridlines
    panel.grid.minor = element_blank()   # remove minor gridlines
  ) +
  geom_hline(yintercept = 0, color = "grey50") +  # horizontal center line
  geom_vline(xintercept = 0, color = "grey50") +
  theme(legend.position = "none")
plot(crown_area)

w_mm<-cpa %>%
  mutate(weight = 4.2332*((mean_area)^0.9851))

view(w_mm)
w_mm$weight<-as.numeric(w_mm$weight)

quad_model<-lm(sqrt(weight) ~ 0 + age_2022, data = w_mm)
summary(quad_model)
res<-resid(quad_model)
qqnorm(res)
qqline(res)
plot(density(res), main = "Residuals")