#Following along with guy on internet

library(tidyverse)
library(broom)

t = 1:100
y1 = 22 + (53 - 22) * exp(-0.02 * t) %>% jitter(10)
y2 = 24 + (60 - 24) * exp(-0.01 * t) %>% jitter(10)


df <- tibble(t = t, y = y1, sensor = 'sensor1') %>% 
  rbind(. , data.frame(t = t, y = y2, sensor = 'sensor2'))

sensor1 <- df %>% filter(sensor == 'sensor1')
nls(y ~ yf + (y0 - yf) * exp(-alpha * t), 
    data = sensor1,
    start = list(y0 = 54, yf = 25, alpha = 1))
fit <- nls(y ~ SSasymp(t, yf, y0, log_alpha), data = sensor1)
fit

log_alpha
