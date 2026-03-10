library(dplyr)
library(ggplot2)

#FINAL
#quadratic model age and weight

#NOTE: mm_1323.csv includes data directly from mm_2013_weight.csv and data compiled in 2022_pred_weight.R

mm<-read.csv("mm_1323.csv")
View(mm)

mm$age_sq<-(mm$age)^2
mm$weight<-as.numeric(mm$weight)

mm$eq<- mm$eq %>%
  recode("cpa" = "CPA", "hd" = "Height and dbh")

#remove 0 point year 0
mm<-mm %>%
  dplyr::filter(weight!=0)

quad_model<-lm(sqrt(weight) ~ 0 + age, data = mm)
summary(quad_model)
res<-resid(quad_model)
qqnorm(res)
qqline(res)
plot(density(res), main = "Residuals")

log_model<-lm(log(weight) ~ 0 + age, data = mm)
summary(log_model)
res<-resid(log_model)
qqnorm(res)
qqline(res)
plot(density(res), main = "Residuals")

mm$predict_sq <- predict(quad_model)
mm<-cbind(mm, res)

ggplot(mm, aes(x=age, y=sqrt(weight))) +
  geom_smooth(method="lm", formula = y ~ 0 + x, se=TRUE, color="black") +
  scale_color_manual(name = "Cant ID", values = c("A" = "orange", "B" = "#0a9cf7", "C"="#e6d602", "D"="#12f9fc",
                                                  "E" = "#23fa0f", "F"= "#023b0c", "G" = "#13b098", "H" = "#870129",
                                                  "I" = "#707371", "J" = "#602dd6")) +
  scale_shape_manual(name = "Input variable", values = c("Height and dbh" = "circle", "CPA" = "triangle")) +
  geom_point(aes(x=age, y=sqrt(weight), color=as.factor(formal_id), shape=as.factor(eq)), size = 3) +
  labs(x = "Cant age", y = "√Predicted aboveground biomass (kg)") +
  theme_minimal() +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # remove major gridlines
    panel.grid.minor = element_blank()   # remove minor gridlines
  ) +
  geom_hline(yintercept = 0, color = "grey50") +  # horizontal center line
  geom_vline(xintercept = 0, color = "grey50") + # vertical center line
  theme(legend.position = "right")

#transformed y axis to non-square root (SUPPELEMENTARY FIGURE)
ggplot(mm, aes(x=age, y=weight_mm)) +
  geom_line(aes(x=age, y=predict_sq^2), color="black") +
  geom_point(data = mm, aes(x=age, y=weight_mm), size = 3, color = "#27cf70") +
  geom_ribbon(alpha=0.5) + 
  labs(x = "Cant age", y = "Predicted weight (kg)") +
  theme_minimal() +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # remove major gridlines
    panel.grid.minor = element_blank()   # remove minor gridlines
  ) +
  geom_hline(yintercept = 0, color = "grey50") +  # horizontal center line
  geom_vline(xintercept = 0, color = "grey50") + # vertical center line
  theme(legend.position = "right") 

#model for biomass of 2023 and 2024 measured trees (predicted biomass from Eqn 1 only)
mm_13<-mm %>%
  filter (eq == "hd")

mm13_mod <- lm(sqrt(weight) ~ 0 + age, data = mm_13)
summary(mm13_mod)
res_13<-resid(mm13_mod)
qqnorm(res_13)
qqline(res_13)
plot(density(res_13), main = "Residuals")

mm_13$predict <- (predict(mm13_mod))^2
mm_13$resid<-resid(mm13_mod)
View(mm_13)

ggplot(mm_13, aes(x=age, y=sqrt(weight))) +
  geom_line(aes(x=age, y=sqrt(predict))) +
  geom_smooth(method="lm", formula = y ~ 0 + x, se=TRUE, color="black", linetype=0) +
  geom_point(data = mm_13, aes(x=age, y=sqrt(weight)), size = 3, color="cyan") +
  labs(x = "Cant age", y = "√Predicted weight (kg)") +
  theme_minimal() +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # remove major gridlines
    panel.grid.minor = element_blank()   # remove minor gridlines
  ) +
  geom_hline(yintercept = 0, color = "grey50") +  # horizontal center line
  geom_vline(xintercept = 0, color = "grey50") + # vertical center line
  theme(legend.position = "right")

#model for biomass of 2022 trees with cpa estimation from LiDAR (predicted biomass from Eqn 2 only)
mm_cpa<-mm %>%
  filter (eq == "cpa")

cpa_mod <- lm(log(weight) ~ 0 + age, data = mm_cpa)
summary(cpa_mod)
res_cpa<-resid(cpa_mod)
qqnorm(res_cpa)
qqline(res_cpa)
plot(density(res_cpa), main = "Residuals")

mm_cpa$predict <- (predict(cpa_mod))^2
mm_cpa$resid<-resid(cpa_mod)
View(mm_cpa)

ggplot(mm_cpa, aes(x=age, y=sqrt(weight))) +
  geom_line(aes(x=age, y=sqrt(predict))) +
  geom_smooth(method="lm", formula = y ~ 0 + x, se=TRUE, color="black", linetype=0) +
  geom_point(data = mm_cpa, aes(x=age, y=sqrt(weight)), size = 3, color="gold") +
  labs(x = "Cant age", y = "√Predicted weight (kg)") +
  theme_minimal() +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # remove major gridlines
    panel.grid.minor = element_blank()   # remove minor gridlines
  ) +
  geom_hline(yintercept = 0, color = "grey50") +  # horizontal center line
  geom_vline(xintercept = 0, color = "grey50") + # vertical center line
  theme(legend.position = "right")

#soil as explanatory variable
om_w<-read.csv("soc_weight.csv")
View(om_w)

ggplot(om_w, aes(x = mean_SOC, y = weight_mm)) +
  scale_color_manual(name = NULL, values = c("2023" = "blue", "2024" = "red")) +
  geom_point(aes(x= mean_SOC, y = weight_mm, color=as.factor(year)), size=3) +
  labs(x = "Mean soil organic matter (%)", y = "Predicted weight (kg)") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # remove major gridlines
    panel.grid.minor = element_blank(),   # remove minor gridlines 
  ) +
  geom_hline(yintercept = 0, color = "grey50") +  # horizontal center line
  geom_vline(xintercept = 0, color = "grey50") # vertical center line

soil_model<-lm(weight_mm ~ mean_SOC, data=om_w)
om_w<-om_w %>%
  dplyr::filter(weight_mm!=0)
summary(soil_model)
