
# Packages ----------------------------------------------------------------

library(tidyverse) # ggplot, pipes ( %>% )
library(ggpubr) # ggarrange, organize panels



# Load data ---------------------------------------------------------------

data <- read.csv("Data/efficacy_univariate.csv") #output from "Diversity Indices" script
str(data) # check it out

data$Year <- as.factor(data$Year) # treat year as a factor, not a number



# Figures -----------------------------------------------------------------

# Live Stems

(live.stems <- ggplot(data, aes(x = Year, y = LiveStem)) +
  geom_jitter(aes(shape = Treatment, color = Treatment), #jitter the plots
  position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5),
  size = 3) +
  theme_classic() + # background is white
  stat_summary(
    aes(shape = Treatment), colour = "black", # want them black to stand out
    fun.data = "mean_se", fun.args = list(mult = 1), # average and standard error
    geom = "pointrange", size = 1,
    position = position_dodge(0.5)
  ) +
  labs(x = " ",
       y = expression(paste("Live Stem Density per m"^-2))) + # x and y labels
  scale_color_manual(values = c("#969696","#006d2c")) + # set colours here
  theme(panel.border = element_rect(fill = NA)) + # add full border
  theme(text = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)) +
  ylim(-10, 100)) # make the y axis this size


# Total Stems 

(total.stems <- ggplot(data, aes(x = Year, y = TotalStem)) +
    geom_jitter(aes(shape = Treatment, color = Treatment), #jitter the plots
                position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5),
                size = 3) +
    theme_classic() + # background is white
    stat_summary(
      aes(shape = Treatment), colour = "black", # want them black to stand out
      fun.data = "mean_se", fun.args = list(mult = 1), # average and standard error
      geom = "pointrange", size = 1,
      position = position_dodge(0.5)
    ) +
    labs(x = " ",
         y = expression(paste("Total Stem Density per m"^-2))) + # x and y labels
    scale_color_manual(values = c("#969696","#006d2c")) + # set colours here
    theme(panel.border = element_rect(fill = NA)) + # add full border
    theme(text = element_text(size = 14),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14)) +
    ylim(-10, 250)) # make the y axis this size



# Incident Light 

(light <- ggplot(data, aes(x = Year, y = IncidentLight)) +
    geom_jitter(aes(shape = Treatment, color = Treatment), #jitter the plots
                position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5),
                size = 3) +
    theme_classic() + # background is white
    stat_summary(
      aes(shape = Treatment), colour = "black", # want them black to stand out
      fun.data = "mean_se", fun.args = list(mult = 1), # average and standard error
      geom = "pointrange", size = 1,
      position = position_dodge(0.5)
    ) +
    labs(x = " ",
         y = expression(paste("Incident Light (%)"))) + # x and y labels
    scale_color_manual(values = c("#969696","#006d2c")) + # set colours here
    theme(panel.border = element_rect(fill = NA)) + # add full border
    theme(text = element_text(size = 14),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14)) +
    ylim(0, 100)) # make the y axis this size



# Canopy Height

(height <- ggplot(data, aes(x = Year, y = CanopyH)) +
    geom_jitter(aes(shape = Treatment, color = Treatment), #jitter the plots
                position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5),
                size = 3) +
    theme_classic() + # background is white
    stat_summary(
      aes(shape = Treatment), colour = "black", # want them black to stand out
      fun.data = "mean_se", fun.args = list(mult = 1), # average and standard error
      geom = "pointrange", size = 1,
      position = position_dodge(0.5)
    ) +
    labs(x = " ",
         y = expression(paste("Canopy Height (cm)"))) + # x and y labels
    scale_color_manual(values = c("#969696","#006d2c")) + # set colours here
    theme(panel.border = element_rect(fill = NA)) + # add full border
    theme(text = element_text(size = 14),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14)))


# Richness


(richness <- ggplot(data, aes(x = Year, y = richness)) +
    geom_jitter(aes(shape = Treatment, color = Treatment), #jitter the plots
                position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5),
                size = 3) +
    theme_classic() + # background is white
    stat_summary(
      aes(shape = Treatment), colour = "black", # want them black to stand out
      fun.data = "mean_se", fun.args = list(mult = 1), # average and standard error
      geom = "pointrange", size = 1,
      position = position_dodge(0.5)
    ) +
    labs(x = " ",
         y = expression(paste("Species Richness"))) + # x and y labels
    scale_color_manual(values = c("#969696","#006d2c")) + # set colours here
    theme(panel.border = element_rect(fill = NA)) + # add full border
    theme(text = element_text(size = 14),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14)))


# Shannon-Weiner

(shannon <- ggplot(data, aes(x = Year, y = H)) +
    geom_jitter(aes(shape = Treatment, color = Treatment), #jitter the plots
                position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5),
                size = 3) +
    theme_classic() + # background is white
    stat_summary(
      aes(shape = Treatment), colour = "black", # want them black to stand out
      fun.data = "mean_se", fun.args = list(mult = 1), # average and standard error
      geom = "pointrange", size = 1,
      position = position_dodge(0.5)
    ) +
    labs(x = " ",
         y = expression(paste("Shannon-Weiner (H')"))) + # x and y labels
    scale_color_manual(values = c("#969696","#006d2c")) + # set colours here
    theme(panel.border = element_rect(fill = NA)) + # add full border
    theme(text = element_text(size = 14),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14)))


# Pielou 

(pielou <- ggplot(data, aes(x = Year, y = J)) +
    geom_jitter(aes(shape = Treatment, color = Treatment), #jitter the plots
                position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5),
                size = 3) +
    theme_classic() + # background is white
    stat_summary(
      aes(shape = Treatment), colour = "black", # want them black to stand out
      fun.data = "mean_se", fun.args = list(mult = 1), # average and standard error
      geom = "pointrange", size = 1,
      position = position_dodge(0.5)
    ) +
    labs(x = " ",
         y = expression(paste("Pielou's J"))) + # x and y labels
    scale_color_manual(values = c("#969696","#006d2c")) + # set colours here
    theme(panel.border = element_rect(fill = NA)) + # add full border
    theme(text = element_text(size = 14),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14)))


# Panel -------------------------------------------------------------------

# Stem density, height, light

BACI <- ggarrange(live.stems, total.stems, light, height,
          common.legend = TRUE, # they all have the same legend
          legend = "bottom", # I want it at the bottom
          labels = "AUTO", # auto label
          hjust = c(-7,-6), # horizontal movement of labels
          vjust = 2, # vertical movement
          align = "hv")  # vertical and horizontal align the figures

ggsave("Figures/BACI_panel.TIFF",
       dpi = 300,
       height = 8.21,
       width = 11.7,
       units = "in")


# Diversity Indices

Diversity <- ggarrange(richness, shannon, pielou,
                  common.legend = TRUE, # they all have the same legend
                  legend = "bottom", # I want it at the bottom
                  labels = "AUTO", # auto label
                  hjust = c(-7,-6), # horizontal movement of labels
                  vjust = 2, # vertical movement
                  align = "hv")  # vertical and horizontal align the figures

ggsave("Figures/Diversity_panel.TIFF", 
       dpi = 300,
       height = 7.97,
       width = 12.1,
       units = "in")
       


# Water Level Attempts ----------------------------------------------------

# max growing season lake levels normalized to the long term NOAA avg for Erie
# y = (x - min) / (max - min)

# All of this only months April - September
# Lake Erie min = 173.38 (Sept 1934)
# Lake Erie max = 175.14 (May 2019)
#175.14 - 173.38

growing <- read.csv("Data/erie_growing_season_mean.csv") # April - Sept 2016 to 2019
growing$Max

(174.65 - 173.38)/(1.76) #2016 = 0.7215909
(174.83 - 173.38)/(1.76) #2017 = 0.8238636
(174.89 - 173.38)/(1.76) #2018 = 0.8579545
(175.14 - 173.38)/(1.76) #2019 = 1.00

Year <- as.factor(c("2016","2017","2018","2019", "2020"))
Depth <- as.numeric(c("0.7215909","0.8238636","0.8579545","1.00", "1.00"))
depth.norm <- data.frame(Year, Depth)

ggplot(depth.norm, aes(x = Year, y = Depth, group = 1)) +
  geom_line(size = 1) +
  ylim(0, 1)


live.stems +
    scale_y_continuous(name = "Live Stem Density per m^-2",
    sec.axis = sec_axis( trans=~./100, name="Lake Erie water depth")) 
