
library(tidyverse)
library(readxl)
library(here)

#read the 2018 calibration data

a <- read_xlsx(path = here("data/Copy of 2018 APR - Rhodamine Secondary Standard Calibration set FINAL.xlsx"), 
              sheet = "pc_test")

jpeg(here::here('output/temp1.jpg'))
ggplot(filter(a, source == 'goldfish', chla < 4000),
       aes(x=chla,
           y=phyco,
           color = group)) +
  geom_point(size = 3) + 
  annotate(geom="text", x = 1500, y = 80, label = 'oddandends/cmc_fluorometer_calibration.r') +
  ggtitle('Goldfish (green algae) culture: outliers chla gt 4000 removed')
dev.off()

jpeg(here::here('output/temp2.jpg'))
ggplot(filter(a, source == 'microcystis', chla > 0, chla < 1000),
       aes(x=chla,
           y=phyco,
           color = group)) +
  geom_point(size = 3) + 
    annotate(geom="text", x = 30, y = 1480, label = 'oddandends/cmc_fluorometer_calibration.r') +
  ggtitle('Microcystis culture: outliers chla gt 1000 and chla lt 0 removed')
dev.off()