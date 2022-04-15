# Anaylsis of the Discovery Time of Carcasses by Corvids
# Patrick Bragato
# patbragato@gmail.com
# 1st April 2022

# Libraries ----
library(ggplot2)
library(emmeans)
library(MuMIn)
library(extrafont)
library(scales)

# Loading and Preparing the Data ----
arrival_time_corvids <- read.csv("Data/aic_arrival_time.csv")

summary(arrival_time_corvids)
str(arrival_time_corvids) # Checking factor classification

names(arrival_time_corvids)[1] <-  "Habitat" #changing first column name, always glitches


## Reclassifying the classes into correct classification

arrival_time_corvids$Season <-  as.factor(arrival_time_corvids$Season)
arrival_time_corvids$Season <- factor(arrival_time_corvids$Season, c("Winter", "Spring"))

arrival_time_corvids$Flood <- as.factor(arrival_time_corvids$Flood)
arrival_time_corvids$Flood <- factor(arrival_time_corvids$Flood, c("Before Flood", "After Flood")) # Setting reference

arrival_time_corvids$Habitat <- as.factor(arrival_time_corvids$Habitat)
arrival_time_corvids$Habitat <- factor(arrival_time_corvids$Habitat, c("Closed", "Open"))

arrival_time_corvids$arrival_time_mins <-  as.numeric(arrival_time_corvids$arrival_time_mins)

# Graphs ----
## Histogram
(arrival_hist <- ggplot(arrival_time_corvids, aes(x = arrival_time_mins)) + 
   geom_histogram(binwidth = 1440, colour = "#000000", fill = "#8497B0") + 
   geom_vline(aes(xintercept = mean(arrival_time_mins)), colour = "red", linetype = "dashed", size = 1) +
   xlab("\nTime until discovery (minutes)") +
   ylab("Count\n") +
   theme_hist() +
   scale_x_continuous(labels = comma))

## Boxplot 
(arrival_box <- ggplot(arrival_time_corvids, aes(x = Season, y = arrival_time_mins, fill = Habitat)) + 
    geom_boxplot(alpha = 0.75, outlier.colour = "black", width = 0.75) +
    scale_fill_manual(values = c("#009E73", "#D55E00")) +             # Adding custom colours
    scale_y_continuous(labels = comma, limits = c(0, 20000)) +
    ylab("Time until discovery (minutes)\n") +
    theme_box() +      
    stat_summary(fun = mean, shape = 7, colour = "red", position = position_dodge(0.75),
                 aes(group = Habitat)) +
    facet_grid(~Flood, switch = "both"))


# Modelling ----
x.quasipoisson <- function(...) {
  res <- quasipoisson(...)
  res$aic <- poisson(...)$aic
  res
} # Allows QAIC to be calculated

options(na.action = "na.fail")


arrival_global_model <- glm(arrival_time_mins ~ Habitat + Season + Flood + Habitat*Season*Flood, 
                            data = arrival_time_corvids, family = x.quasipoisson)
mod1 <- glm(arrival_time_mins ~ Season + Flood + Habitat, data = arrival_time_corvids, 
             family = x.quasipoisson)
mod2 <- glm(arrival_time_mins ~ Habitat + Season + Habitat*Season, data = arrival_time_corvids, 
            family = x.quasipoisson)
mod3 <- glm(arrival_time_mins ~ Habitat + Flood + Habitat*Flood, data = arrival_time_corvids, 
            family = x.quasipoisson)
mod4 <- glm(arrival_time_mins ~ Season + Flood + Season*Flood, data = arrival_time_corvids, 
            family = x.quasipoisson)
mod5 <- glm(arrival_time_mins ~ Habitat + Season, data = arrival_time_corvids, 
             family = x.quasipoisson)
mod6 <- glm(arrival_time_mins ~ Habitat + Flood, data = arrival_time_corvids, 
             family = x.quasipoisson)
mod7 <- glm(arrival_time_mins ~ Season + Flood, data = arrival_time_corvids, 
             family = x.quasipoisson)
mod8 <- glm(arrival_time_mins ~ Habitat, data = arrival_time_corvids, 
            family = x.quasipoisson) 
mod9 <- glm(arrival_time_mins ~ Season, data = arrival_time_corvids, 
            family = x.quasipoisson)
mod10 <- glm(arrival_time_mins ~ Flood, data = arrival_time_corvids, 
            family = x.quasipoisson)

chat <-sum(residuals(arrival_global_model,"pearson")^2)/arrival_global_model$df.residual # Calculating dispersion parameter from global model

discovery_mod_sel <- model.sel(mod1, mod2, mod3, mod4, mod5, mod6,mod7, mod8,mod9, mod10,
                      rank = QAICc, rank.args = alist(chat = chat)) #  Ranking models by QAICc, using dispersion parameter from global model

as.data.frame(discovery_mod_sel) 


# Looking into best model (Model 1) ----
summary(mod1)

# Residauals
arrive_mu <- predict(mod1, type = "response") # Predicted values
arrive_resids <- arrival_time_corvids$arrival_time_mins - arrive_mu # Residuals
arrive_pearson <- arrive_resids / sqrt(3755.682 * arrive_mu) # Pearson Residuals
arrive_resid_data <- data.frame(arrive_mu, arrive_pearson)

# Ploting Residuals
(arrive_resid_plot <- ggplot(data = arrive_resid_data, 
                             aes(x = arrive_mu, y = arrive_pearson)) +
    geom_point(alpha = 1) + #Jitter to make intepretation easier
    theme_hist() +
    xlab("\nPredicted Values") +
    ylab("Scaled Pearson Residuals\n"))

