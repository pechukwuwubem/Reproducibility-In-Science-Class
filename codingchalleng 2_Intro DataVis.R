Coding Challenge 2_Intro DataVis

PRECIOUS CHUKWUBEM

## Boxplot

library(ggplot2)

p_box <- ggplot(don, aes(x = treatment, y = DON, color = cultivar)) +
  geom_boxplot() +
  labs(y = "DON (ppm)", x = "")

p_box


##Bar chart with SE using stat_summary()
p_bar <- ggplot(don, aes(x = treatment, y = DON, fill = cultivar)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge()) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               width = 0.2, position = position_dodge(0.9)) +
  labs(y = "DON (ppm)", x = "")

p_bar


## Add raw data points

p_box <- p_box +
  geom_point(aes(fill = cultivar),
             shape = 21,
             color = "black",
             position = position_jitterdodge(jitter.width = 0.15))

p_bar <- p_bar +
  geom_point(aes(fill = cultivar),
             shape = 21,
             color = "black",
             position = position_jitterdodge(jitter.width = 0.15))

p_box
p_bar

## Facet by cultivar

p_box <- p_box + facet_wrap(~cultivar)
p_bar <- p_bar + facet_wrap(~cultivar)

p_box
p_bar

## dd transparency to points
p_box <- p_box +
  geom_point(aes(fill = cultivar),
             shape = 21,
             color = "black",
             alpha = 0.6,
             position = position_jitterdodge(jitter.width = 0.15))

p_bar <- p_bar +
  geom_point(aes(fill = cultivar),
             shape = 21,
             color = "black",
             alpha = 0.6,
             position = position_jitterdodge(jitter.width = 0.15))

p_box
p_bar


