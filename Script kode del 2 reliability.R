
## Part 2: reliability study, 1RM squat

# load packages
library(tidyverse)

# A data frame of two tests to calculate reliablilty from
# 1RM squat

df <- data.frame(t1 = c(67.5, 135, 137.5),
                 t2 = c(67.5, 137.5, 140.0))


### Calculation of technical error 


df %>%
  mutate(change = t2 - t1) %>%
  group_by() %>%
  summarise(sd.change = sd(change), 
            mean.test = mean(c(t1, t2)), 
            te.abs = (sd.change / sqrt(2)), 
            te.relative = (te.abs / mean.test) * 100) %>%
  print()


### Calculation of the smallest whortwhile change

df %>%
  rowwise() %>%
  mutate(m = mean(c(t1, t2))) %>%
  ungroup() %>%
  summarise(sd = sd(m),
            swc = 0.2 * sd) %>%
  print()

