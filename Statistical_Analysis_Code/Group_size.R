# Analysis for corvid group size at carcasses
# Patrick Bragato
# patbragato@gmail.com
# 1st April 2021

# Libraries ----
library(ggplot2)
library(emmeans)
library(MuMIn)
library(extrafont)
library(scales)
library(lme4)

# Loading and Preparing the Data ----
group_size <- read.csv("Data/group_size.csv")

summary(group_size)
str(group_size) # Checking factor classification

names(group_size)[1] <-  "carcass_drop" #changing first column name, always glitches

## Fixing factors

group_size$Season <-  as.factor(group_size$Season)
group_size$Season <-  factor(group_size$Season, c("Winter", "Spring"))

group_size$Flood <- as.factor(group_size$Flood)
group_size$Flood <- factor(group_size$Flood, c("Before Flood", "After Flood")) # Setting reference

group_size$Habitat <- as.factor(group_size$Habitat)
group_size$Habitat <- factor(group_size$Habitat,c("Closed", "Open"))

group_size$Site <- as.factor(group_size$Site) # May need to rename sites for each drop

group_size$group_size <-  as.numeric(group_size$group_size)

group_size$carcass_drop <- as.factor(group_size$carcass_drop)

# Making a histogram
(group_hist <- ggplot(group_size, aes(x = group_size)) + 
     geom_histogram(binwidth = 1, colour = "#000000", fill = "#8497B0") + # Changing colour and bin width
     geom_vline(aes(xintercept = mean(group_size)), colour = "red", linetype = "dashed", size = 1) +
     xlab("\nGroup Size") +
     ylab("Count\n") +
     theme_hist())    

# Modelling the data ----
group_mod1 <- glmer(group_size ~ Habitat + Flood + Season + (1|site_R), data = group_size, 
                    family = poisson)
group_mod2 <- glmer(group_size ~ Habitat + Season + Season*Habitat + (1|site_R), data = group_size, 
                    family = poisson)
group_mod3 <- glmer(group_size ~ Habitat + Flood + Flood*Habitat + (1|site_R), data = group_size, 
                    family = poisson)
group_mod4 <- glmer(group_size ~ Flood + Season + Flood*Season + (1|site_R), data = group_size, 
                    family = poisson)
group_mod5 <- glmer(group_size ~ Habitat +Season + (1|site_R), data = group_size, 
                    family = poisson)
group_mod6 <- glmer(group_size ~ Habitat +Flood + (1|site_R), data = group_size, 
                  family = poisson)
group_mod7 <- glmer(group_size ~ Season + Flood + (1|site_R), data = group_size, 
                    family = poisson)
group_mod8 <- glmer(group_size ~ Habitat + (1|site_R), data = group_size, family = poisson)

group_mod9 <- glmer(group_size ~ Season + (1|site_R), data = group_size, 
                    family = poisson)
group_mod10 <- glmer(group_size ~ Flood + (1|site_R), data = group_size, family = poisson)

options(na.action = "na.fail")

group_mod_sel <- model.sel(group_mod1, group_mod2, group_mod3, group_mod4, group_mod5, group_mod6,
                      group_mod7, group_mod8, group_mod9, group_mod10, rank = AIC)

as.data.frame(group_mod_sel)

# Looking into best model (Model 4) ----
# Residauals
group_resids <- resid(group_mod4, type = "pearson")
group_mu <- predict(group_mod4, type = "response") 
group_resid_data <- data.frame(group_mu,group_resids)

# Ploting Residuals
(group_resid_plot <- ggplot(data = group_resid_data, 
                             aes(x = group_mu, y = group_resids)) +
    geom_point(alpha = 1) + #Jitter to make intepretation easier
    theme_hist() +
    xlab("\nPredicted Values") +
    ylab("Pearson Residuals\n"))

# EM Means
group_emm <- emmeans(group_mod4, ~ Season*Flood)

contrast(group_emm, "consec", simple = "each", combine = TRUE)
contrast(group_emm, "consec", simple = "each", combine = TRUE, type = "response")


# Boxplot
(group_box <- ggplot(group_size, aes(x = Flood, y = group_size, fill = Season)) + 
    geom_boxplot(alpha = .75, outlier.colour = "black") +
    scale_fill_manual(values = c("#8497B0", "#D66700")) +             # Adding custom colours
    ylab("Group Size\n") +                             
    xlab("\nHabitat")  +
    theme_box() +               # Adding a margin
    stat_summary(fun = mean, shape = 7, colour = "red", position = position_dodge(0.75),
                 aes(group = Season)))
