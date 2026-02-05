install.packages("tidyverse")
library(tidyverse)
mtcars
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() + 
  geom_smooth(method = lm, se = FALSE)

ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_smooth(method = lm, se = FALSE) +
  geom_point(aes(color =hp, size = hp)) + 
  xlab("weight (tons)") +
  ylab("Miles per gallon") +
  scale_color_gradient(low = "green" , high = "grey") +
  
  getwd()
head(bull.richness)
str(bull.richness)

ggplot(bull.richness, aes(x = GrowthStage, y = richness, fill = Fungicide, color = Crop)) +
  geom_boxplot() +
  geom_point(position = position_jitterdodge()) +
  scale_fill_manual(values = c("#E87722", "#0C2340"))

ggplot(bull.richness, aes(x =GrowthStage, y = richness, fill = Fungicide)) +
  stat_summary(fun = mean, geom = "bar") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = "dodge")

  #bars with SE errors bars
ggplot(bull.richness, aes(x =GrowthStage, y = richness, group = Fungicide, color = Fungicide)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar")

  #Facetting
ggplot(bull.richness, aes(x =GrowthStage, y = richness, group = Fungicide, color = Fungicide)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar") +
  facet_wrap(~Treatment*Crop, scale ="free")



