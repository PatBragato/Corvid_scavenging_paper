# Learning by corvids and effect of deployment time
# Honours Thesis
# Patrick Bragato
# patbragato@gmail.com
# 4th April 2022

# Libraries ----
library(ggplot2)  # for data visualisation
library(lme4)  # for models
library(hms)
library(sjPlot)
library(tidyr)

# Loading and Preparing the Data ----
corvid_learning <- read.csv("Data/Corvid_learning.csv")

names(corvid_learning)[1] <-  "carcass_drop" #changing first column name, always glitches

summary(corvid_learning)

# Making classes correct
corvid_learning$time_laid <- as_hms(corvid_learning$time_laid)

corvid_learning$Deploy_Number <- as.integer(corvid_learning$Deploy_Number)

corvid_learning$Season <-  as.factor(corvid_learning$Season)
corvid_learning$Season <- factor(corvid_learning$Season, c("Winter", "Spring"))

corvid_learning$Flood <- as.factor(corvid_learning$Flood)
corvid_learning$Flood <- factor(corvid_learning$Flood, c("Before Flood", "After Flood")) # Setting reference

corvid_learning$Habitat <- as.factor(corvid_learning$Habitat)
corvid_learning$Habitat <- factor(corvid_learning$Habitat, c("Closed", "Open"))


# Models ----
lm_learn <- glm(discovery_time ~ Deploy_Number, data = corvid_learning, 
                family = quasipoisson) # seeing if corvids learnt to follow researchers over the course of the study

summary(lm_learn) 

lm_day <- glm(discovery_time ~ time_laid, data = corvid_learning, 
              family = quasipoisson) # seeing if the time of day carcasses were deployed affected how long corvids took to find them

summary(lm_day) 

# Graphs ----
set_theme(base = theme_blank(),
          theme.font = "Cambria",
          axis.title.color = "#000000",
          axis.textcolor = "#000000",
          axis.linecolor = "#000000",
          axis.linecolor.y = "#000000",
          title.color = "white",
          geom.alpha = 0.5
)

plot_model(lm_learn, type = "pred", show.data = TRUE, dot.size = 1.5,
           axis.title = c("\nCarcass Deployment Number", 
                          "\nDiscovery Time (minutes)"))


(corvid_day <- ggplot(corvid_learning, aes(x = time_laid, y = discovery_time)) + 
        geom_point(alpha = .75) +
        theme_hist() +
        ylab("Discovery Time (minutes)\n") +                             
        xlab("\nTime of day carcass deployed") +
        stat_smooth(method = "glm"))

