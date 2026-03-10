library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(packcircles)
library(ggforce)
library(gridExtra)

soil<-read.csv("Pita_etal_2023_soil_loi.csv")
View(soil)

SOC_mean <- soil %>%
  group_by(plotid) %>%
  dplyr::summarise(
    mean_SOC = mean(percent_total_organic_content),
    se_SOC = sd(percent_total_organic_content) / sqrt(n())  # Standard error of the mean
  )
View(SOC_mean)

SOC_mean_mean <- soil %>%
  dplyr::summarise(
    mean_SOC = mean(percent_total_organic_content),
    se_SOC = sd(percent_total_organic_content) / sqrt(n())  # Standard error of the mean
  )
View(SOC_mean_mean)

SOC_aban<-SOC_mean %>%
  filter(plotid=="1f"|plotid=="1ga"|plotid=="1h"|plotid=="1mc"|plotid=="1n")
View(SOC_aban)
SOC_aban<- SOC_aban %>%
  mutate(plotid=recode(plotid, "1f" = "K(a)","1ga" = "L(a)","1n" = "P(a)",
                        "1mc" = "N(a)","1h" = "M(a)", "1f1" = "K(a)","1ga1" = "L(a)",
                        "1n1" = "P(a)", "1mc1" = "N(a)","1h1" = "M(a)"))
year<-c("2023", "2023", "2023", "2023", "2023")
year_24<-c("2024", "2024", "2024", "2024", "2024")
SOC_aban<-cbind(SOC_aban, year)
SOC_aban2<-cbind(SOC_aban2, year_24)

ages<-soil %>%
  mutate(age=recode(plotid, "1caa"="0", "1cab"="14", "1f"="30", 
                        "1ga"="30", "1gb"="4","1gc"="3","1h"="30","1i"="6","1j"="21","1ma"="11","1mb"="10","1mc"="30","1n"="30","1o"="2","2e"="8",
                    "5a1"="2","5a2"="2","nc"="50", "nc (non cop)"="50"))
View(ages)
ages$age<-as.numeric(ages$age)
ages <- ages[ages$age != 50, ]
ages <- ages[ages$plotid != "5a1", ]
ages <- ages[ages$plotid != "5a2", ]
View(ages)

SOC_mean_ages <- ages %>%
  group_by(age) %>%
  dplyr::summarise(
    mean_SOC = mean(percent_total_organic_content),
    se_SOC = sd(percent_total_organic_content) / sqrt(n())  # Standard error of the mean
  )
View(SOC_mean_ages)

#2024

soil2<-read.csv("Pita_etal_2024_soil_loi.csv")
View(soil2)
soil2<-soil2 %>%
  mutate(plotid=(recode(plotid, "1o "="1o"))
  )

SOC_mean2 <- soil2 %>%
  group_by(plotid) %>%
  dplyr::summarise(
    mean_SOC = mean(percent_total_organic_content),
    se_SOC = sd(percent_total_organic_content) / sqrt(n())  # Standard error of the mean
  )

View(SOC_mean2)

SOC_aban2<-SOC_mean2 %>%
  filter(plotid=="1f"|plotid=="1ga"|plotid=="1h"|plotid=="1mc"|plotid=="1n")
View(SOC_aban2)

SOC_aban2<- SOC_aban2 %>%
  mutate(plotid=recode(plotid, "1f" = "K(a)","1ga" = "L(a)","1n" = "P(a)",
                       "1mc" = "N(a)","1h" = "M(a)", "1f1" = "K(a)","1ga1" = "L(a)",
                       "1n1" = "P(a)", "1mc1" = "N(a)","1h1" = "M(a)"))

ages2<-soil2 %>%
  mutate(age=recode(plotid, "1caa"="1", "1cab"="15", "1f"="31", 
                    "1ga"="31", "1gb"="5","1gc"="4","1h"="31","1i"="7","1j"="22","1ma"="12","1mb"="11","1mc"="31","1n"="31","1o"="3","2e"="8",
                    "5a1"="2","5a2"="2","nc"="50", "nc (non cop)"="50", "2c (e?)"="9"))
View(ages2)
ages2$age<-as.numeric(ages2$age)

SOC_mean_ages2 <- ages2 %>%
  group_by(age) %>%
  dplyr::summarise(
    mean_SOC = mean(percent_total_organic_content),
    se_SOC = sd(percent_total_organic_content) / sqrt(n())  # Standard error of the mean
  )

SOCfull<-rbind(SOC_mean_ages,SOC_mean_ages2)
View(SOCfull)

ggplot(SOCfull, aes(x = age, y = mean_SOC)) +
  geom_point(color = "black") +
  scale_y_continuous(limits = c(0, 25)) + #plot for means
  geom_errorbar(
    aes(ymin = mean_SOC - se_SOC, ymax = mean_SOC + se_SOC),
    width = 0.2,  # Error bar width
    color = "red"
  ) +
  labs(x = "Cant age", y = "Mean soil organic content (%)") +
  theme_minimal()

#separate out overlapping years

SOCfull_a <- SOCfull %>%
  filter (age!=c("30","31"))
View(SOCfull_a)
year<-c(rep("2023", times = 10), rep("2024", times = 10))
SOCfull_a<-cbind(SOCfull_a, year)

soil_act<-ggplot(SOCfull_a, aes(x = age, y = mean_SOC)) +
  geom_point(aes (color = as.factor(year)), size = 3) +
  scale_y_continuous(limits = c(0, 50)) + #plot for means
  geom_errorbar(
    aes(ymin = mean_SOC - se_SOC, ymax = mean_SOC + se_SOC),
    width = 0.2,  # Error bar width
    color = "black"
  ) +
  labs(x = "Cant age", y = "Mean soil organic content (%)") +
  scale_color_manual(name = NULL, values = c("2023" = "red", 
                                             "2024" = "blue")) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # remove major gridlines
    panel.grid.minor = element_blank()   # remove minor gridlines
  ) +
  geom_hline(yintercept = 0, color = "grey50") +  # horizontal center line
  geom_vline(xintercept = 0, color = "grey50") + # vertical center line 
  theme(legend.position = "none")

soil_aban<-ggplot(SOC_aban, aes(x = plotid, y = mean_SOC)) +
  geom_point(aes (color = as.factor(year)),size=3) +
  scale_y_continuous(limits = c(0, 50)) + #plot for means
  geom_errorbar(
    aes(ymin = mean_SOC - se_SOC, ymax = mean_SOC + se_SOC),
    width = 0.05,  # Error bar width
    color = "black"
  ) +
  geom_point(data=SOC_aban2, aes(x = plotid, y = mean_SOC, color = as.factor(year_24)), size = 3) +
  geom_errorbar(data=SOC_aban2, aes(ymin = mean_SOC - se_SOC, ymax = mean_SOC + se_SOC),
                width = 0.05,  # Error bar width
                color = "black"
  ) +
  scale_color_manual(name = NULL, values = c("2023" = "red", 
                                             "2024" = "blue")
  ) +
  labs(x = "Abandoned cant ID", y = "") +
  theme_minimal() +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # remove major gridlines
    panel.grid.minor = element_blank()   # remove minor gridlines
  ) +
  geom_hline(yintercept = 0, color = "grey50") +  # horizontal center line
  geom_vline(xintercept = 0, color = "grey50") + # vertical center line
  theme(legend.position = "right") 

grid.arrange(soil_act, soil_aban, nrow = 1)