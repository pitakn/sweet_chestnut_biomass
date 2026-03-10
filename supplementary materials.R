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

#SUPPLEMENTARY FIGURES

#DBH example for box plots and computing variances between years

#clean the data
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

#add 2024
dbh2024<-read.csv("dbh2024.csv")
View(dbh2024)

#remove NA
dbh2024<-dbh2024 %>%
  filter(dbh!="NULL")

#combine data frames
dbh2024$dbh<-as.numeric(dbh2024$dbh)
dbh2023<-dbh2023[, !names(dbh2023) %in% c("notes","X","age","circumference")]
dbh2024<-dbh2024[, !names(dbh2024) %in% c("notes", "age")]
dbh2024<-dbh2024 %>%
  mutate(plot_id=recode(plot_id, "1caa"="1caa1", "1cab"="1cab1", "1f"="1f1", 
                        "1ga"="1ga1", "1gb"="1gb1","1gc"="1gc1","1h"="1h1","1i"="1i1","1j"="1j1","1ma"="1ma1","1mb"="1mb1","1mc"="1mc1","1n"="1n1","1o"="1o1","2e"="2e1")
  )
dbhfull<-rbind(dbh2023,dbh2024)

#BOXPLOTS
#compare changes between years using boxplots
fullmeandbh<- dbhfull %>%
  group_by(plot_id) %>%
  summarise(mean_dbh = mean(dbh, na.rm = TRUE), 
            var = sd(dbh)^2,
            .groups = 'drop')
View(fullmeandbh)
fullmeandbh$mean_dbh<-as.numeric(fullmeandbh$mean_dbh)

#boxplots
cab23<-fullmeandbh[fullmeandbh$plot_id =="1cab", "mean_dbh"]
View(cab23)
cab24<-fullmeandbh[fullmeandbh$plot_id =="1cab1", "mean_dbh"]
View(cab24)
test<-wilcox.test(cab23$mean_dbh,cab24$mean_dbh, exact=FALSE)
print(test)
boxplot(cab23$mean_dbh,cab24$mean_dbh,
        names = c("1cab 2023", "1cab2024"),
        ylab = "dbh (cm)")

f23<-fullmeandbh[fullmeandbh$plot_id =="1f", "mean_dbh"]
f24<-fullmeandbh[fullmeandbh$plot_id =="1f1", "mean_dbh"]
test<-wilcox.test(f23$mean_dbh,f24$mean_dbh, exact=FALSE)
print(test)
boxplot(f23$mean_dbh,f24$mean_dbh,
        names = c("1f 2023", "1f2024"),
        ylab = "dbh (cm)")

i23<-fullmeandbh[fullmeandbh$plot_id =="1i", "mean_dbh"]
i24<-fullmeandbh[fullmeandbh$plot_id =="1i1", "mean_dbh"]
test<-wilcox.test(i23$mean_dbh,i24$mean_dbh, exact=FALSE)
print(test)
boxplot(i23$mean_dbh,i24$mean_dbh,
        names = c("1i 2023", "1i 2024"),
        ylab = "dbh (cm)")

o23<-fullmeandbh[fullmeandbh$plot_id =="1o", "mean_dbh"]
o24<-fullmeandbh[fullmeandbh$plot_id =="1o1", "mean_dbh"]
test<-wilcox.test(o23$mean_dbh,o24$mean_dbh, exact=FALSE)
print(test)
boxplot(o23$mean_dbh,o24$mean_dbh,
        names = c("1o 2023", "1o 2024"),
        ylab = "dbh (cm)")

e23<-fullmeandbh[fullmeandbh$plot_id =="2e", "mean_dbh"]
e24<-fullmeandbh[fullmeandbh$plot_id =="2e1", "mean_dbh"]
test<-wilcox.test(e23$mean_dbh,e24$mean_dbh, exact=FALSE)
print(test)
boxplot(e23$mean_dbh,e24$mean_dbh,
        names = c("2e 2023", "2e 2024"),
        ylab = "dbh (cm)")

gb23<-fullmeandbh[fullmeandbh$plot_id =="1gb", "mean_dbh"]
gb24<-fullmeandbh[fullmeandbh$plot_id =="1gb1", "mean_dbh"]
test<-wilcox.test(gb23$mean_dbh,gb24$mean_dbh, exact=FALSE)
print(test)
boxplot(gb23$mean_dbh,gb24$mean_dbh,
        names = c("1gb 2023", "1gb 2024"),
        ylab = "dbh (cm)")

gc23<-fullmeandbh[fullmeandbh$plot_id =="1gc", "mean_dbh"]
gc24<-fullmeandbh[fullmeandbh$plot_id =="1gc1", "mean_dbh"]
test<-wilcox.test(gc23$mean_dbh,gc24$mean_dbh, exact=FALSE)
print(test)
boxplot(gc23$mean_dbh,gc24$mean_dbh,
        names = c("1gc 2023", "1gc 2024"),
        ylab = "dbh (cm)")

mb23<-fullmeandbh[fullmeandbh$plot_id =="1mb", "mean_dbh"]
mb24<-fullmeandbh[fullmeandbh$plot_id =="1mb1", "mean_dbh"]
test<-wilcox.test(mb23$mean_dbh,mb24$mean_dbh, exact=FALSE)
print(test)
boxplot(mb23$mean_dbh,mb24$mean_dbh,
        names = c("1mb 2023", "1mb 2024"),
        ylab = "dbh (cm)")

ma23<-fullmeandbh[fullmeandbh$plot_id =="1ma", "mean_dbh"]
ma24<-fullmeandbh[fullmeandbh$plot_id =="1ma1", "mean_dbh"]
test<-wilcox.test(ma23$mean_dbh,ma24$mean_dbh, exact=FALSE)
print(test)
boxplot(ma23$mean_dbh,ma24$mean_dbh,
        names = c("1ma 2023", "1ma 2024"),
        ylab = "dbh (cm)")

caa23<-fullmeandbh[fullmeandbh$plot_id =="1caa", "mean_dbh"]
caa24<-fullmeandbh[fullmeandbh$plot_id =="1caa1", "mean_dbh"]
boxplot(caa23$mean_dbh,caa24$mean_dbh,
        names = c("1caa 2023", "1caa 2024"),
        ylab = "dbh (cm)")

#VARIANCE TABLES
#calculate differences between same cant in different years
#variance among the differences

cab3<-dbhfull %>%
  filter(year!="2024", plot_id %in% c("1cab"))
View(cab3)
cab3<-cab3 %>%
  group_by(tree_no) %>%
  summarise(mean_dbh = mean(dbh, na.rm = TRUE),
            .groups = 'drop')
cab4<-dbhfull %>%
  filter(year!="2023", plot_id %in% c("1cab"))
cab4<-cab4 %>%
  group_by(tree_no) %>%
  summarise(mean_dbh = mean(dbh, na.rm = TRUE),
            .groups = 'drop')
View(cab4)
cab3<-cab3 %>%
  filter(tree_no!=5)
diffcab<-cab4$mean_dbh-cab3$mean_dbh
mean_diffcab<-mean(diffcab)
print(mean_diffcab)
var(diffcab)
hist(diffcab)

caa3<-dbhfull %>%
  filter(year!="2024", plot_id %in% c("1caa"))
View(caa3)
cab3<-cab3 %>%
  group_by(tree_no) %>%
  dplyr::summarise(mean_dbh = mean(dbh, na.rm = TRUE),
                   .groups = 'drop')
caa4<-dbhfull %>%
  filter(year!="2023", plot_id %in% c("1caa"))
caa4<-caa4 %>%
  group_by(tree_no) %>%
  dplyr::summarise(mean_dbh = mean(dbh, na.rm = TRUE),
                   .groups = 'drop')
View(caa4)

diffcaa<-caa4$mean_dbh-caa3$dbh
mean_diffcaa<-mean(diffcaa)
print(mean_diffcaa)
var(diffcaa)

gb3<-dbhfull %>%
  filter(year!="2024", plot_id %in% c("1gb"))
gb3<-gb3 %>%
  group_by(tree_no) %>%
  dplyr::summarise(mean_dbh = mean(dbh, na.rm = TRUE),
                   .groups = 'drop')
View(gb3)
gb4<-dbhfull %>%
  filter(year!="2023", plot_id %in% c("1gb"))
gb4<-gb4 %>%
  group_by(tree_no) %>%
  dplyr::summarise(mean_dbh = mean(dbh, na.rm = TRUE),
                   .groups = 'drop')
View(gb4)
gb3<-gb3 %>%
  filter(tree_no!=2)

diffgb<-gb4$mean_dbh-gb3$mean_dbh
mean_diffgb<-mean(diffgb)
print(mean_diffgb)
var(diffgb)

gc3<-dbhfull %>%
  filter(year!="2024", plot_id %in% c("1gc"))
gc3<-gc3 %>%
  group_by(tree_no) %>%
  dplyr::summarise(mean_dbh = mean(dbh, na.rm = TRUE),
                   .groups = 'drop')
View(gc3)
gc4<-dbhfull %>%
  filter(year!="2023", plot_id %in% c("1gc"))
gc4<-gc4 %>%
  group_by(tree_no) %>%
  dplyr::summarise(mean_dbh = mean(dbh, na.rm = TRUE),
                   .groups = 'drop')
View(gc4)
gc4<-gc4 %>%
  filter(tree_no!=4)

diffgc<-gc4$mean_dbh-gc3$mean_dbh
mean_diffgc<-mean(diffgc)
print(mean_diffgc)
var(diffgc)

i3<-dbhfull %>%
  filter(year!="2024", plot_id %in% c("1i"))
i3<-i3 %>%
  group_by(tree_no) %>%
  dplyr::summarise(mean_dbh = mean(dbh, na.rm = TRUE),
                   .groups = 'drop')
View(i3)
i4<-dbhfull %>%
  filter(year!="2023", plot_id %in% c("1i"))
i4<-i4 %>%
  group_by(tree_no) %>%
  summarise(mean_dbh = mean(dbh, na.rm = TRUE),
            .groups = 'drop')
View(i4)

diffi<-i4$mean_dbh-i3$mean_dbh
mean_diffi<-mean(diffi)
print(mean_diffi)
var(diffi)

e3<-dbhfull %>%
  filter(year!="2024", plot_id %in% c("2e"))
e3<-e3 %>%
  group_by(tree_no) %>%
  dplyr::summarise(mean_dbh = mean(dbh, na.rm = TRUE),
                   .groups = 'drop')
View(e3)
e4<-dbhfull %>%
  filter(year!="2023", plot_id %in% c("2e"))
e4<-e4 %>%
  group_by(tree_no) %>%
  summarise(mean_dbh = mean(dbh, na.rm = TRUE),
            .groups = 'drop')
View(e4)

diffe<-e4$mean_dbh-e3$mean_dbh
mean_diffe<-mean(diffe)
print(mean_diffe)
var(diffe)

o3<-dbhfull %>%
  filter(year!="2024", plot_id %in% c("1o"))
o3<-o3 %>%
  group_by(tree_no) %>%
  dplyr::summarise(mean_dbh = mean(dbh, na.rm = TRUE),
                   .groups = 'drop')
View(o3)
o4<-dbhfull %>%
  filter(year!="2023", plot_id %in% c("1o"))
o4<-o4 %>%
  group_by(tree_no) %>%
  dplyr::summarise(mean_dbh = mean(dbh, na.rm = TRUE),
                   .groups = 'drop')
View(o4)

diffo<-o4$mean_dbh-o3$mean_dbh
mean_diffo<-mean(diffo)
print(mean_diffo)
var(diffo)

ma3<-dbhfull %>%
  filter(year!="2024", plot_id %in% c("1ma"))
ma3<-ma3 %>%
  group_by(tree_no) %>%
  dplyr::summarise(mean_dbh = mean(dbh, na.rm = TRUE),
                   .groups = 'drop')
View(ma3)
ma4<-dbhfull %>%
  filter(year!="2023", plot_id %in% c("1ma"))
ma4<-ma4 %>%
  group_by(tree_no) %>%
  dplyr::summarise(mean_dbh = mean(dbh, na.rm = TRUE),
                   .groups = 'drop')
View(ma4)
ma3<-ma3 %>%
  filter(tree_no!=3)

diffma<-ma4$mean_dbh-ma3$mean_dbh
mean_diffma<-mean(diffma)
print(mean_diffma)
var(diffma)

j3<-dbhfull %>%
  filter(year!="2024", plot_id %in% c("1j"))
j3<-j3 %>%
  group_by(tree_no) %>%
  dplyr::summarise(mean_dbh = mean(dbh, na.rm = TRUE),
                   .groups = 'drop')
View(j3)
j4<-dbhfull %>%
  filter(year!="2023", plot_id %in% c("1j"))
j4<-j4 %>%
  group_by(tree_no) %>%
  dplyr::summarise(mean_dbh = mean(dbh, na.rm = TRUE),
                   .groups = 'drop')
View(j4)

diffj<-j4$mean_dbh-j3$mean_dbh
mean_diffj<-mean(diffj)
print(mean_diffj)
var(diffj)

f3<-dbhfull %>%
  filter(year!="2024", plot_id %in% c("1f"))
f3<-f3 %>%
  group_by(tree_no) %>%
  dplyr::summarise(mean_dbh = mean(dbh, na.rm = TRUE),
                   .groups = 'drop')
View(f3)
f4<-dbhfull %>%
  filter(year!="2023", plot_id %in% c("1f"))
f4<-f4 %>%
  group_by(tree_no) %>%
  summarise(mean_dbh = mean(dbh, na.rm = TRUE),
            .groups = 'drop')
View(f4)
f4<-f4 %>%
  filter(tree_no!=5)
f3<-f3 %>%
  filter(tree_no!=5)

difff<-f4$mean_dbh-f3$mean_dbh
mean_difff<-mean(difff)
print(mean_difff)
var(difff)

ga3<-dbhfull %>%
  filter(year!="2024", plot_id %in% c("1ga"))
ga3<-ga3 %>%
  group_by(tree_no) %>%
  dplyr::summarise(mean_dbh = mean(dbh, na.rm = TRUE),
                   .groups = 'drop')
View(ga3)
ga4<-dbhfull %>%
  filter(year!="2023", plot_id %in% c("1ga"))
ga4<-ga4 %>%
  group_by(tree_no) %>%
  dplyr::summarise(mean_dbh = mean(dbh, na.rm = TRUE),
                   .groups = 'drop')
View(ga4)
ga3<-ga3 %>%
  filter(tree_no!=2)

diffga<-ga4$mean_dbh-ga3$mean_dbh
mean_diffga<-mean(diffga)
print(mean_diffga)
var(diffga)

h3<-dbhfull %>%
  filter(year!="2024", plot_id %in% c("1h"))
h3<-h3 %>%
  group_by(tree_no) %>%
  dplyr::summarise(mean_dbh = mean(dbh, na.rm = TRUE),
                   .groups = 'drop')
View(h3)
h4<-dbhfull %>%
  filter(year!="2023", plot_id %in% c("1h"))
h4<-h4 %>%
  group_by(tree_no) %>%
  dplyr::summarise(mean_dbh = mean(dbh, na.rm = TRUE),
                   .groups = 'drop')
View(h4)
h3<-h3 %>%
  filter(tree_no!=5)

diffh<-h4$mean_dbh-h3$mean_dbh
mean_diffh<-mean(diffh)
print(mean_diffh)
var(diffh)

mc3<-dbhfull %>%
  filter(year!="2024", plot_id %in% c("1mc"))
mc3<-mc3 %>%
  group_by(tree_no) %>%
  dplyr::summarise(mean_dbh = mean(dbh, na.rm = TRUE),
                   .groups = 'drop')
View(mc3)
mc4<-dbhfull %>%
  filter(year!="2023", plot_id %in% c("1mc"))
mc4<-mc4 %>%
  group_by(tree_no) %>%
  dplyr::summarise(mean_dbh = mean(dbh, na.rm = TRUE),
                   .groups = 'drop')
View(mc4)

diffmc<-mc4$mean_dbh-mc3$mean_dbh
mean_diffmc<-mean(diffmc)
print(mean_diffmc)
var(diffmc)

n3<-dbhfull %>%
  filter(year!="2024", plot_id %in% c("1n"))
n3<-n3 %>%
  group_by(tree_no) %>%
  dplyr::summarise(mean_dbh = mean(dbh, na.rm = TRUE),
                   .groups = 'drop')
View(n3)
n4<-dbhfull %>%
  filter(year!="2023", plot_id %in% c("1n"))
n4<-n4 %>%
  group_by(tree_no) %>%
  dplyr::summarise(mean_dbh = mean(dbh, na.rm = TRUE),
                   .groups = 'drop')
View(n4)
n3<-n3 %>%
  filter(tree_no!=2)

diffn<-n4$mean_dbh-n3$mean_dbh
mean_diffn<-mean(diffn)
print(mean_diffn)
var(diffn)