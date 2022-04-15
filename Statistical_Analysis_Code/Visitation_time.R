# Analysis of visitation time of corivds at carcasses
# Patrick Bragato
# patbragato@gmail.com
# 1st April 2022

# Libraries ----
library(ggplot2)
library(emmeans)
library(scales)
library(MuMIn)

# Loading and Preparing the Data ----
visitation_time <- read.csv("Data/visitation_time.csv")

summary(visitation_time)
str(visitation_time) # Checking factor classification

names(visitation_time)[1] <-  "carcass_drop" #changing first column name, always glitches

## Fixing factors

visitation_time$Season <-  as.factor(visitation_time$Season) # Changing to factor
visitation_time$Season <- factor(visitation_time$Season, c("Winter", "Spring"))

visitation_time$Flood <- as.factor(visitation_time$Flood)
visitation_time$Flood <- factor(visitation_time$Flood, c("Before Flood", "After Flood")) # Setting reference

visitation_time$Habitat <- as.factor(visitation_time$Habitat)
visitation_time$Habitat <- factor(visitation_time$Habitat,c("Closed", "Open"))

visitation_time$Site <- as.factor(visitation_time$Site) # May need to rename sites for each drop

visitation_time$carcass_drop <- as.factor(visitation_time$carcass_drop)
# Graphs ----
## Histogram
(visit_hist <- ggplot(visitation_time, aes(x = visit_time)) + 
   geom_histogram(binwidth = 60, colour = "#000000", fill = "#8497B0") + 
   geom_vline(aes(xintercept = mean(visit_time)), colour = "red", linetype = "dashed", size = 1) +
   xlab("\nVisitation time (minutes)") +
   ylab("Count\n") +
   theme_hist() +
   scale_x_continuous(labels = comma))

## Boxplot 
(visitation_box_habitat <- ggplot(visitation_time, aes(x = Flood, y = visit_time, fill = Season)) +
   geom_boxplot(alpha = 0.75, outlier.colour = "black") +
   scale_fill_manual(values = c("#8497B0", "#D66700")) +             # Adding custom colours
   ylab("Visitation time (minutes)\n") +                             
   xlab("\nHabitat")  +
   theme_box() +     
   stat_summary(fun = mean, shape = 7, colour = "red", position = position_dodge(0.75),
                aes(group = Season))
)

# Modelling ----
x.quasipoisson <- function(...) {
  res <- quasipoisson(...)
  res$aic <- poisson(...)$aic
  res
} # Allows QAIC to be calculated

options(na.action = "na.fail")

visit_global_model <- glm(visit_time ~ Habitat + Season + Flood + Habitat*Season*Flood, 
                          data = visitation_time, family = x.quasipoisson)
visit_1 <- glm(visit_time ~ Habitat + Season + Flood,
                data = visitation_time, family = x.quasipoisson)
visit_2 <- glm(visit_time ~ Habitat + Season + Habitat*Season, 
                     data = visitation_time, family = x.quasipoisson)
visit_3 <- glm(visit_time ~ Habitat + Flood + Habitat*Flood, 
                     data = visitation_time, family = x.quasipoisson)
visit_4 <- glm(visit_time ~ Season + Flood + Season*Flood, 
                      data = visitation_time, family = x.quasipoisson)
visit_5 <- glm(visit_time ~ Habitat + Season, 
                      data = visitation_time, family = x.quasipoisson)
visit_6 <- glm(visit_time ~ Habitat + Flood, 
                      data = visitation_time, family = x.quasipoisson)
visit_7 <- glm(visit_time ~ Season + Flood, 
                      data = visitation_time, family = x.quasipoisson)
visit_8 <- glm(visit_time ~ Habitat, data = visitation_time, family = x.quasipoisson)

visit_9 <- glm(visit_time ~ Season, data = visitation_time, family = x.quasipoisson)

visit_10 <- glm(visit_time ~ Flood, data = visitation_time, family = x.quasipoisson)


visit_chat<-sum(residuals(visit_global_model,"pearson")^2)/visit_global_model$df.residual

visit_model_sel <- model.sel(visit_1, visit_2, visit_3, 
                                   visit_4, visit_5, visit_6, visit_7,
                                   visit_8, visit_9, visit_10,rank = QAICc, 
                                   rank.args = alist(chat = visit_chat))

as.data.frame(visit_model_sel) 

# Looking into best model (Visit_4) ----
summary(visit_4)

# Residauals
visit_mu <- predict(visit_4, type = "response") # Predicted values
visit_resids <- visitation_time$visit_time - visit_mu # Residuals
visit_pearson <- visit_resids / sqrt(307.7996 * visit_mu) # Pearson Residuals
visit_resid_data <- data.frame(visit_mu, visit_pearson)

# Ploting Residuals
(visit_resid_plot <- ggplot(data = visit_resid_data, 
                                 aes(x = visit_mu, y = visit_pearson)) +
    geom_point(alpha = 1) + #Jitter to make intepretation easier
    theme_hist() +
    xlab("\nPredicted Values") +
    ylab("Scaled Pearson Residuals\n"))


# EM Means
visit_emm <- emmeans(visit_4, ~ Season*Flood)

contrast(visit_emm, "consec", simple = "each", combine = TRUE)
contrast(visit_emm, "consec", simple = "each", combine = TRUE, type = "response")



