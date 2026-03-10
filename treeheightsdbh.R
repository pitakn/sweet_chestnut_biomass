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

trees2023<-read.csv("heights_2023.csv")
View(trees2023)

trees2024<-read.csv("heights2024.csv")
View(trees2024)

#clean height data
heights_2324<-read.csv("heights_clean.csv")
heights_2324<-heights_2324[,-c(13)]
heights_2324<- heights_2324 %>%
  drop_na(height_m)
View(heights_2324)

#DESCRIPTIVE STATISTICS FOR HEIGHT

#average total tree height per cant both years
meanheight<- heights_2324 %>%
  group_by(plot_id, year) %>%
  dplyr::summarise(mean_height = mean(height_m, na.rm = TRUE), 
            se = sd(height_m) / sqrt(n()),
            .groups = 'drop')
View(meanheight)
summary(meanheight)
age<-c(0, 1, 14, 15, 30, 31, 30, 31, 4, 5, 3, 4, 30, 31, 6, 7, 20, 21, 11, 12, 10, 11, 30, 31,
        30, 31, 2, 3, 8, 9)
meanheight<-cbind(meanheight, age)
meanheight<-meanheight[order(meanheight$age, decreasing = FALSE),]

#max height per cant 2023
maxheight<-trees2023 %>%
  group_by(plot_id) %>%
  dplyr::summarise(max_height = max(height_m, na.rm = TRUE), 
            se = sd(height_m) / sqrt(n()),
            .groups = 'drop')
View(maxheight)

#min height per cant 2023
minheight<-trees2023 %>%
  group_by(plot_id) %>%
  dplyr::summarise(min_height = min(height_m, na.rm = TRUE), 
            se = sd(height_m) / sqrt(n()),
            .groups = 'drop')
View(minheight)

#min height per cant 24
minheight24<-height2 %>%
  group_by(plot_id) %>%
  dplyr::summarise(min_height = min(height_m, na.rm = TRUE), 
            se = sd(height_m) / sqrt(n()),
            .groups = 'drop')
View(minheight24)

#average total tree height per cant
meanheight24<- height2 %>%
  group_by(plot_id) %>%
  dplyr::summarise(mean_height = mean(height_m, na.rm = TRUE), 
            se = sd(height_m) / sqrt(n()),
            .groups = 'drop')
View(meanheight24)
summary(meanheight24)

#max height per cant
maxheight24<-height2 %>%
  group_by(plot_id) %>%
  dplyr::summarise(max_height = max(height_m, na.rm = TRUE), 
            se = sd(height_m) / sqrt(n()),
            .groups = 'drop')
View(maxheight24)

write.csv(meanheight, "meanheight_trial.csv")

#actively managed cants height
meanheight_act<-meanheight %>%
  filter(plot_id !="1f", plot_id != "1f ", plot_id != "1ga",
         plot_id != "1h",plot_id != "1mc",plot_id != "1n")
View(meanheight_act)
meanheight_act_23<-meanheight_act %>%
  filter(year == "2023")
meanheight_act_23<-meanheight_act_23 %>%
  mutate (age=recode(plot_id, "1caa" = 0, "1cab" = 14, "2e" = 8, "1gb" = 4,
                         "1gc" = 3, "1i" = 6, "1j" = 20, "1ma" = 11, "1mb" = 10,
                         "1o" = 2))
View(meanheight_act_23) 

meanheight_act_24<-meanheight_act %>%
  filter(year == "2024")
meanheight_act_24<-meanheight_act_24 %>%
  mutate (age=recode(plot_id, "1caa" = 1, "1cab" = 15, "2e" = 9, "1gb" = 5,
                     "1gc" = 4, "1i" = 7, "1j" = 21, "1ma" = 12, "1mb" = 11,
                     "1o" = 3))
View(meanheight_act_24)

meanheight_act_f<-rbind(meanheight_act_23, meanheight_act_24)
View(meanheight_act_f)

meanheight_act_f$age<-as.numeric(meanheight_act_f$age)

#full final active cant height
height_act<-ggplot(meanheight_act_f, aes(x = age, y = mean_height)) +
  geom_point(aes (color = as.factor(year)), size=3) +
  scale_y_continuous(limits = c(0, 25)) + #plot for means
  geom_errorbar(
    aes(ymin = mean_height - se, ymax = mean_height + se),
    width = 0.2,  # Error bar width
    color = "black"
  ) +
  labs(x = "Cant age", y = "Stool total height (m)") +
  scale_color_manual(name = "Year", values = c("2023" = "purple", 
                                             "2024" = "orange")) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # remove major gridlines
    panel.grid.minor = element_blank()   # remove minor gridlines
  ) +
  geom_hline(yintercept = 0, color = "grey50") +  # horizontal center line
  geom_vline(xintercept = 0, color = "grey50") +
  theme(legend.position = "right")

height_act

#average dominant tree height per cant
meanheight<- heights %>%
  group_by(plot_id, year) %>%
  dplyr::summarise(mean_height = mean(height_m),  
            se = sd(height_m) / sqrt(n()),
            .groups = 'drop')
View(meanheight)
summary(meanheight)

#DBH DATA
#dbh 2023
dbh2023<-read.csv("dbh2023.csv")
View(dbh2023)

#remove NA
dbh2023 <- dbh2023 %>%
  filter(dbh!="NA")

#add plot ages
dbh2023<- dbh2023 %>%
  dplyr::mutate(age=recode(plot_id, "1caa"="0", "1cab"="14", "1f"="30", 
                  "1ga"="30", "1gb"="4","1gc"="3","1h"="30","1i"="6","1j"="21","1ma"="11","1mb"="10","1mc"="30","1n"="30","1o"="2","2e"="8")
  )
View(dbh2023)

#average dbh of stems per tree
treemeandbh<- dbh2023 %>%
  group_by(age, tree_no) %>%
  dplyr::summarise(mean_dbh = mean(dbh, na.rm = TRUE), 
            se = sd(dbh) / sqrt(n()),
            .groups = 'drop')
View(treemeandbh)
summary(treemeandbh)

#remove abandoned cants, cant with no dbh msmts (i.e., 1caa in 2023)
treemeandbh$age<-as.numeric(treemeandbh$age)
treemeandbh <- treemeandbh %>%
  filter(age!="30", age!="5a1", age!="5a2")
View(treemeandbh)

#only abandoned cants
treemeandbh_aban<- dbh2023 %>%
  group_by(plot_id, tree_no) %>%
  dplyr::summarise(mean_dbh = mean(dbh, na.rm = TRUE), 
            se_SOC = sd(dbh) / sqrt(n()),
            .groups = 'drop')
View(treemeandbh_aban)

#filter for abandoned
treemeandbh_aban <- treemeandbh_aban %>%
  filter(plot_id %in% c("1f","1ga","1h","1mc","1n"))

#2024 dbh 

dbh2024<-read.csv("dbh2024.csv")
View(dbh2024)

dbh2024<-dbh2024 %>%
  filter(dbh!="NULL")

dbh2024$dbh<-as.numeric(dbh2024$dbh)

#mean dbh each tree
mean_dbh24<- dbh2024 %>%
  group_by(plot_id, tree_no) %>%
  dplyr::summarise(mean_dbh = mean(dbh, na.rm = TRUE), 
                   se_SOC = sd(dbh) / sqrt(n()),
                   .groups = 'drop')

#add ages
dbh2024<- dbh2024 %>%
  mutate(age=recode(plot_id, "1caa"="1", "1cab"="15", "1f"="31", 
                    "1ga"="31", "1gb"="5","1gc"="4","1h"="31","1i"="7","1j"="22","1ma"="12","1mb"="11","1mc"="31","1n"="31","1o"="3","2e"="9")
  )

#DBH BOTH YEARS
#combine dataframes
dbh2023<-dbh2023[, !names(dbh2023) %in% c("notes","X","age","circumference")]
dbh2024<-dbh2024[, !names(dbh2024) %in% c("notes", "age")]
dbh2024<-dbh2024 %>%
  mutate(plot_id=recode(plot_id, "1caa"="1caa1", "1cab"="1cab1", "1f"="1f1", 
                        "1ga"="1ga1", "1gb"="1gb1","1gc"="1gc1","1h"="1h1","1i"="1i1","1j"="1j1","1ma"="1ma1","1mb"="1mb1","1mc"="1mc1","1n"="1n1","1o"="1o1","2e"="2e1")
  )
dbhfull<-rbind(dbh2023,dbh2024)

#add years
year<-c(rep(2023, 1187), rep(2024, 1230))
dbhfull<-cbind(dbhfull, year)
dbhfull<-dbhfull %>%
  filter(plot_id!="5a1", plot_id!="5a2")
dbhfull$dbh<-as.numeric(dbhfull$dbh)

#FINAL DBH FIGURES

dbhfull<- dbhfull %>%
  mutate(age=recode(plot_id, "1caa"="0", "1cab"="14", "1f"="30", "1ga"="30", "1gb"="4",
                    "1gc"="3","1h"="30","1i"="6","1j"="20","1ma"="11","1mb"="10",
                    "1mc"="30","1n"="30","1o"="2","2e"="8", "1caa1"="1","1cab1"="15",
                    "1f1"="31","1ga1"="31","1gb1"="5","1gc1"="4","1h1"="31","1i1"="7",
                    "1j1"="21","1ma1"="12","1mb1"="11","1mc1"="31",
                    "1n1"="31", "1o1"="3", "2e1"="9"))

#dbh by individual tree
dbh_tree<- dbhfull %>%
  group_by(age, tree_no, year) %>%
  dplyr::summarise(mean_dbh = mean(dbh, na.rm = TRUE),
                   se = sd(dbh)/sqrt(n()),
                   .groups = 'drop')
View(dbh_tree)
dbh_tree$mean_dbh<-as.numeric(dbh_tree$mean_dbh)
dbh_tree$se<-as.numeric(dbh_tree$se)
dbh_tree$age<-as.numeric(dbh_tree$age)
dbh_tree$year<-as.character(dbh_tree$year)

#filter to actively managed cants
dbh_tree<-dbh_tree %>%
  filter(age!=30, age!=31)

#figure
dbh_act<-ggplot(dbh_tree, aes(x = age, y = mean_dbh)) +
  scale_color_manual(name = "Year", values = c("2023" = "#13ebdc", "2024" = "#f0c507")) +
  geom_point(aes(x = age, y = mean_dbh, color=as.factor(year)), size = 3) +
  scale_y_discrete(limits = c(0, 40)) + #plot for means
  geom_errorbar(
    aes(ymin = mean_dbh - se, ymax = mean_dbh + se),
    width = 0.2,  # Error bar width
    color = "black"
  ) +
  labs(x = "Cant age", y = "Mean tree stem dbh (cm)") +
  theme_minimal() +
  theme(legend.position = "top") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # remove major gridlines
    panel.grid.minor = element_blank()   # remove minor gridlines
  ) +
  geom_hline(yintercept = 0, color = "grey50") +  # horizontal center line
  geom_vline(xintercept = 0, color = "grey50") + # vertical center line
  geom_line(data = dbhfull_mean, aes(x = age, y = mean_dbh, color = "Cant mean")) + #actual means
  theme(legend.position="right")

dbh_act

#MODELLING DBH AND HEIGHT AS FUNCTION OF AGE (FIGURES FOUND IN SUPPLEMENTARY MATERIALS DOC)

#dbh model
dbh_mod<-lm(sqrt(mean_dbh) ~ 0 + age, dbh_tree)
summary(dbh_mod)
res_d<-resid(dbh_mod)
qqnorm(res_d)
qqline(res_d)
plot(density(res_d), main = "Residuals")

ggplot(dbh_tree, aes(x = age, y = log(mean_dbh))) +
  scale_color_manual(name = NULL, values = c("Predicted values" = "#FF6FFB", 
                                             "2023" = "#13ebdc", "2024" = "#f0c507")) +
  geom_point(aes(color=as.factor(year)), size = 3) +
  scale_y_continuous(limits = c(0, 5)) + #plot for means
  labs(x = "Cant age", y = "√Mean stool stem dbh (cm)") +
  theme_minimal() +
  theme(legend.position = "top") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # remove major gridlines
    panel.grid.minor = element_blank()   # remove minor gridlines
  ) +
  geom_hline(yintercept = 0, color = "grey50") +  # horizontal center line
  geom_vline(xintercept = 0, color = "grey50") + # vertical center line
  geom_smooth(method = "lm", formula = y ~ 0 + x, color = "black") #actual means

#log model
dbh_0<- dbgh %>% 
  filter(age !=0)

dbh_mod<-lm(log(mean_dbh) ~ 0 + age, dbh_0)
summary(dbh_mod)
res_d<-resid(dbh_mod)
qqnorm(res_d)
qqline(res_d)
plot(density(res_d), main = "Residuals")

#height model

meanheight_0<- meanheight_act %>% 
  filter(age !=0)

h_mod<-lm(sqrt(mean_height) ~ age, meanheight_act)
summary(h_mod)
res_h<-resid(h_mod)
qqnorm(res_h)
qqline(res_h)
plot(density(res_h), main = "Residuals")

ggplot(meanheight_act, aes(x = age, y = sqrt(mean_height))) +
  geom_point(colour = "orange", size=3) +
  scale_y_continuous(limits = c(0, 5)) + #plot for means +
  labs(x = "Cant age", y = "√Stool total height (m)") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # remove major gridlines
    panel.grid.minor = element_blank()   # remove minor gridlines
  ) +
  geom_hline(yintercept = 0, color = "grey50") +  # horizontal center line
  geom_vline(xintercept = 0, color = "grey50") + # vertical center line
  theme(legend.position = "none") +
  geom_smooth(method = "lm", formula = y ~ x, color="black")

#log model

meanheight_0<- meanheight_act %>% 
  filter(age !=0)

h_mod<-lm(log(mean_height) ~ age, meanheight_0)
summary(h_mod)
res_h<-resid(h_mod)
qqnorm(res_h)
qqline(res_h)
plot(density(res_h), main = "Residuals")

#mendelez-miguelez et al 2023
means<-cbind(mean_dbh, meanheight$mean_height)
View(means)
means<-means %>%
  mutate(weight5 = 0.3452*(mean_dbh^2*`meanheight24$mean_height`)^0.7202)

ggplot(means, aes(x=mean_dbh))

#INPUTTING HEIGHT AND DBH INTO EQ 1 (Mendelez-Miguelez et al. 2013)

#combine height and dbh data
finaltreedata<-cbind(dbhfull_mean, meanheight$mean_height)
finaltreedata<-cbind(finaltreedata, meanheight$se)
View(finaltreedata)

#rename variables for clarity
names(finaltreedata)[names(finaltreedata) == "meanheight$mean_height"] <- "mean_height"
names(finaltreedata)[names(finaltreedata) == "meanheight$se"] <- "height_se"
names(finaltreedata)[names(finaltreedata) == "se"] <- "dbh_se"

finaltreedata$mean_dbh<-as.numeric(finaltreedata$mean_dbh)
finaltreedata$mean_height<-as.numeric(finaltreedata$mean_height)

#calculate predicted biomass
finaltreedata<-finaltreedata %>%
  mutate(weight_w = 0.01391*((mean_dbh^2)*mean_height)^1.006) 
finaltreedata<-finaltreedata %>%
  mutate(weight_b = 0.004119*(mean_height^1.086)*(mean_dbh^2)^0.7889)
finaltreedata<-finaltreedata %>%
  mutate(weight_c = 0.5408*(mean_height^-1.439)*(mean_dbh^2)^1.386)
finaltreedata<-finaltreedata %>%
  mutate(weight_t = weight_w + weight_b + weight_c)
write.csv(finaltreedata, "mm_2013_weight.csv")