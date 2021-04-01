# Theme Functions
theme_hist <- function(){
  theme_bw() +
    theme(axis.text = element_text(size = 12, colour = "#000000", family = "Calibri"),                          # Changing font size of axis labels and title
          axis.text.x = element_text(size = 12, face = "plain", colour = "#000000", family = "Cambria"),
          axis.title = element_text(size = 14, face = "plain", colour = "#000000", family = "Cambria"),  # face="plain" is the default, you can change it to italic, bold, etc. 
          panel.grid = element_blank(),                                 # Removing the grey grid lines
          plot.margin = unit(c(1,1,1,1), units = , "cm"),             # Putting a 1 cm margin around the plot
          axis.line = element_line(colour = "black"),
          legend.text = element_text(size = 12, colour = "#000000", family = "Calibri"),
          legend.title = element_text(size = 14, face = "plain", colour = "#000000", family = "Cambria"),
          strip.text = element_text(size = 14, face = "plain", colour = "#000000", family = "Cambria"),
          strip.background = element_rect(fill = "white", colour = "white"),
          strip.placement = "outside")
            }

theme_box <- function(){
  theme_bw() +
    theme(axis.text = element_text(size = 12, colour = "#000000", family = "Calibri"),                          # Changing font size of axis labels and title
          axis.text.x = element_text(size = 12, face = "plain", colour = "#000000", family = "Cambria"),
          axis.title.y = element_text(size = 14, face = "plain", colour = "#000000", family = "Cambria"),  # face="plain" is the default, you can change it to italic, bold, etc. 
          axis.title.x = element_blank(),
          panel.grid = element_blank(),                                 # Removing the grey grid lines
          plot.margin = unit(c(1,1,1,1), units = , "cm"),             # Putting a 1 cm margin around the plot
          axis.line = element_line(colour = "black"),
          legend.text = element_text(size = 12, colour = "#000000", family = "Calibri"),
          legend.title = element_text(size = 14, face = "plain", colour = "#000000", family = "Cambria"),
          strip.text = element_text(size = 14, face = "plain", colour = "#000000", family = "Cambria"),
          strip.background = element_rect(fill = "white", colour = "white"),
          strip.placement = "outside",
          
    )
}

# Loading Fonts
library(extrafont)
loadfonts(device = "win") # Loading fonts
