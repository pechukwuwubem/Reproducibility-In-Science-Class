Precious Chukwubem

Coding Challenge 3: Data Visualization 2

setwd("/Users/preciouschukwubem/Library/CloudStorage/OneDrive-AuburnUniversity/Reproducibility In Science")

library(tidyverse)
library(ggpubr)
install.packages("ggpubr")
install.packages("ggrepel")
library(ggrepel)
install.packages(ggplot)                
library(ggplot2)                
library(ggplot)

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

mycotoxin <- read.csv("mycotoxinData.csv", na.strings = "na")
head(mycotoxin)
mycotoxin$Treatment <- as.factor(mycotoxin$Treatment) 
mycotoxin$Cultivar <- as.factor(mycotoxin$Cultivar)  
str(mycotoxin)

cbbPalette <- c("#56B4E9", "#009E73")

#DON_PLOT1 

Plot1 <- ggplot(mycotoxin, aes(x = Treatment, y = DON, fill = Cultivar)) + 
  geom_boxplot(position = position_dodge(0.85)) + # add boxplot layer 
  xlab("") +  
  ylab("DON (ppm)") + # y label 
  geom_point(alpha = 0.6,pch = 21, color = "black", position = position_jitterdodge(0.05)) + 
  scale_fill_manual(values = cbbPalette)+ # transparency of the jittered points to 0.6. #Jitter points over the boxplot and fill the points and boxplots Cultivar with two colors from the cbbPallete  
  facet_wrap(~Cultivar)+ #faceted by Cultivar 
  theme_classic() # for classic theme  
Plot1

Plot1_bar <- ggplot(mycotoxin, aes(x = Treatment, y = DON, fill = Cultivar)) +
  stat_summary(fun = mean, 
               geom = "bar", 
               position = position_dodge(0.9), 
               width = 0.8) +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar", 
               position = position_dodge(0.9), 
               width = 0.2) +
  xlab("") +
  ylab("DON (ppm)") +
  scale_fill_manual(values = cbbPalette) +
  facet_wrap(~Cultivar) +
  theme_classic()

Plot1_bar

Plot1 <- ggplot(mycotoxin, aes(x = Treatment, y = DON, fill = Cultivar)) +  # Define aesthetics: x-axis as Treatment, y-axis as DON, and fill by Cultivar
  stat_summary(fun = mean, geom = "bar", position = position_dodge(0.9)) +  # Add bars representing mean DON values
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.3, 
               position = position_dodge(0.9)) +  # Add error bars representing the standard error of the mean
  xlab("") +  # Label the x-axis
  ylab("DON (ppm)") +  # Label the y-axis
  scale_fill_manual(values = cbbPalette) +  # Manually set fill colors for Cultivar from the cbbPalette
  theme_classic() +  # Use a classic theme for the plot
  facet_wrap(~Cultivar)  # Create separate panels for each Cultivar

Plot1

##Question 2 — Boxplot with distribution points

Plot1 <- ggplot(mycotoxin, aes(x = Treatment, y = DON, fill = Cultivar)) + 
  geom_boxplot(position = position_dodge(0.85)) +  # add boxplot layer
  
  geom_point(shape = 21, color = "black",
             position = position_jitterdodge(jitter.width = 0.2,
                                             dodge.width = 0.85)) +  # add jittered points
  
  xlab("") +  
  ylab("DON (ppm)") +  
  scale_fill_manual(values = cbbPalette) +  
  theme_classic()

Plot1

#Question 3 — Bar chart (mean ± SE) with distribution points

Plot1_bar <- ggplot(mycotoxin, aes(x = Treatment, y = DON, fill = Cultivar)) +
  
  stat_summary(fun = mean,
               geom = "bar",
               position = position_dodge(0.9),
               width = 0.8) +
  
  stat_summary(fun.data = mean_se,
               geom = "errorbar",
               position = position_dodge(0.9),
               width = 0.2) +
  
  geom_point(shape = 21, color = "black",
             position = position_jitterdodge(jitter.width = 0.2,
                                             dodge.width = 0.9)) +  # add jittered points
  
  xlab("") +
  ylab("DON (ppm)") +
  scale_fill_manual(values = cbbPalette) +
  theme_classic()

Plot1_bar


## — Boxplot with points and facet by Cultivar

Plot1 <- ggplot(mycotoxin, aes(x = Treatment, y = DON, fill = Cultivar)) + 
  geom_boxplot(position = position_dodge(0.85)) +  
  geom_point(shape = 21, color = "black",
             position = position_jitterdodge(jitter.width = 0.2,
                                             dodge.width = 0.85)) +  
  xlab("") +  
  ylab("DON (ppm)") +  
  scale_fill_manual(values = cbbPalette) +  
  facet_wrap(~Cultivar) +  # facet by cultivar
  theme_classic()

Plot1


#Question 3 — Bar chart (mean ± SE) with points and facet by Cultivar

Plot1_bar <- ggplot(mycotoxin, aes(x = Treatment, y = DON, fill = Cultivar)) +
  
  stat_summary(fun = mean,
               geom = "bar",
               position = position_dodge(0.9),
               width = 0.8) +
  
  stat_summary(fun.data = mean_se,
               geom = "errorbar",
               position = position_dodge(0.9),
               width = 0.2) +
  
  geom_point(shape = 21, color = "black",
             position = position_jitterdodge(jitter.width = 0.2,
                                             dodge.width = 0.9)) +
  
  xlab("") +
  ylab("DON (ppm)") +
  scale_fill_manual(values = cbbPalette) +
  facet_wrap(~Cultivar) +  # facet by cultivar
  theme_classic()

Plot1_bar


#Boxplot with transparent points and facet

Plot1 <- ggplot(mycotoxin, aes(x = Treatment, y = DON, fill = Cultivar)) + 
  geom_boxplot(position = position_dodge(0.85)) +  
  geom_point(shape = 21, 
             color = "black",
             alpha = 0.5,   # transparency added
             position = position_jitterdodge(jitter.width = 0.2,
                                             dodge.width = 0.85)) +  
  xlab("") +  
  ylab("DON (ppm)") +  
  scale_fill_manual(values = cbbPalette) +  
  facet_wrap(~Cultivar) +  
  theme_classic()

Plot1


## Bar chart (mean ± SE) with transparent points and facet
Plot1_bar <- ggplot(mycotoxin, aes(x = Treatment, y = DON, fill = Cultivar)) +
  
  stat_summary(fun = mean,
               geom = "bar",
               position = position_dodge(0.9),
               width = 0.8) +
  
  stat_summary(fun.data = mean_se,
               geom = "errorbar",
               position = position_dodge(0.9),
               width = 0.2) +
  
  geom_point(shape = 21, 
             color = "black",
             alpha = 0.5,   # transparency added
             position = position_jitterdodge(jitter.width = 0.2,
                                             dodge.width = 0.9)) +
  
  xlab("") +
  ylab("DON (ppm)") +
  scale_fill_manual(values = cbbPalette) +
  facet_wrap(~Cultivar) +
  theme_classic()

Plot1_bar


############################################################
# Q3 (5 pts): Change y-variable to X15ADON and MassperSeed_mg
# Save plots as separate R objects
############################################################

# 15ADON plot
p_15ADON <- ggplot(mycotoxin, aes(x = Treatment, y = X15ADON, fill = Cultivar)) +
  geom_boxplot(position = position_dodge(0.85)) +
  geom_point(
    alpha = 0.6,
    shape = 21,
    color = "black",
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.85)
  ) +
  xlab("") +
  ylab("15ADON") +
  scale_fill_manual(values = cbbPalette) +
  facet_wrap(~Cultivar) +
  theme_classic()

p_15ADON

# Seed mass plot
p_seedmass <- ggplot(mycotoxin, aes(x = Treatment, y = MassperSeed_mg, fill = Cultivar)) +
  geom_boxplot(position = position_dodge(0.85)) +
  geom_point(
    alpha = 0.6,
    shape = 21,
    color = "black",
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.85)
  ) +
  xlab("") +
  ylab("Seed Mass (mg)") +
  scale_fill_manual(values = cbbPalette) +
  facet_wrap(~Cultivar) +
  theme_classic()

p_seedmass

############################################################
# Q4 (5 pts): Combine 3 plots into one (3 columns x 1 row)
# common.legend = TRUE uses one shared legend for all plots
############################################################
fig_ABC <- ggarrange(
  p_DON, p_15ADON, p_seedmass,
  ncol = 3, nrow = 1,
  labels = c("A", "B", "C"),
  common.legend = TRUE
)

fig_ABC

# Answer for write-up:
# common.legend = TRUE makes one shared legend for the combined figure
# instead of repeating separate legends on each subplot.

############################################################
# Q5 (5 pts): Add pairwise t-tests with geom_pwc() to each plot
# Save as NEW objects, then combine again
############################################################

p_DON_pwc <- p_DON +
  geom_pwc(method = "t_test", label = "p.format", hide.ns = TRUE)

p_15ADON_pwc <- p_15ADON +
  geom_pwc(method = "t_test", label = "p.format", hide.ns = TRUE)

p_seedmass_pwc <- p_seedmass +
  geom_pwc(method = "t_test", label = "p.format", hide.ns = TRUE)

fig_ABC_pwc <- ggarrange(
  p_DON_pwc, p_15ADON_pwc, p_seedmass_pwc,
  ncol = 3, nrow = 1,
  labels = c("A", "B", "C"),
  common.legend = TRUE
)

fig_ABC_pwc

