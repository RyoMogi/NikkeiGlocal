library(tidyverse)
library(openxlsx)
Mycol <- c("#08306B", "#238B45", "#FD8D3C", "#D4B9DA", "#FFEDA0")
`%out%` = Negate(`%in%`)


#----- read data
# data is from 人口統計資料集. Accessed on 25/10/2024
ptfr <- read.xlsx("data/ptfr_T04-09.xlsx", sheet = "T04-09★", startRow = 3)[36, -1]
ctfr <- read.xlsx("data/ctfr_T04-11.xlsx", sheet = "T04-11★", startRow = 3)[35, -1]

d_ptfr <- ptfr %>% 
  gather(key = year, value = tfr) %>% 
  mutate(year = substr(year, 1, 4),
         year = as.numeric(as.character(year)),
         tfr = as.numeric(as.character(tfr)))
write.csv(d_ptfr, "Nikkei Glocal/out/data-rensai2024_2_2_ptfr.csv", row.names = F, fileEncoding = "UTF-8")

d_ctfr <- ctfr %>% 
  gather(key = cohort, value = tfr) %>% 
  mutate(cohort = substr(cohort, 1, 4),
         cohort = as.numeric(as.character(cohort)),
         tfr = as.numeric(as.character(tfr))) %>% 
  drop_na()
write.csv(d_ctfr, "Nikkei Glocal/out/data-rensai2024_2_2_ctfr.csv", row.names = F, fileEncoding = "UTF-8")
