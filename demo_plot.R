library(tidyverse)
n = 50
odds = data.frame(date = seq(1, n), prob = runif(n, min=0.30, max=0.46))
ggplot(data = odds) +
  geom_line(mapping = aes(x = date, y = prob)) +
  ylim(0, 1)+
  labs(title = "Probability that soil temp drops too low for sorghum germination, lower is better")

