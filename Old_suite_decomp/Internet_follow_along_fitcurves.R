#Following along with guy on internet

library(tidyverse)
library(broom)

t = 1:100
y1 = 22 + (53 - 22) * exp(-0.02 * t) %>% jitter(10)
y2 = 24 + (60 - 24) * exp(-0.01 * t) %>% jitter(10)


df_internet <- tibble(t = t, y = y1, sensor = 'sensor1') %>% 
  rbind(. , data.frame(t = t, y = y2, sensor = 'sensor2'))

sensor1 <- df_internet %>% filter(sensor == 'sensor1')
nls(y ~ yf + (y0 - yf) * exp(-alpha * t), 
    data = sensor1,
    start = list(y0 = 54, yf = 25, alpha = 1))
fit <- nls(y ~ SSasymp(t, g, pl, log_alpha), data = sensor1)
fit

fitted <- df_internet %>% 
  nest(-sensor) %>%
  mutate(
    fit = map(data, ~nls(y ~ SSasymp(t, yf, y0, log_alpha), data = .)),
    tidied = map(fit, tidy),
  augmented = map(fit, augment),
  )  

# Produce a table of fit parameters: y0, yf, alpha
fitted %>% 
  unnest(tidied) %>% 
  select(sensor, term, estimate)%>% 
  spread(term, estimate) %>% 
  mutate(alpha = exp(log_alpha))
