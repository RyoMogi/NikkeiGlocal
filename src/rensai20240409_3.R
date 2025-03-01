library(tidyverse)
library(openxlsx)
Mycol <- c("#08306B", "#238B45", "#FD8D3C", "#D4B9DA", "#FFEDA0")
`%out%` = Negate(`%in%`)


#----- read data
# data is from Human Fertility Database. Accessed on 12/03/2024
ptfr <- read.xlsx("data/TFR.xlsx", sheet = "Total fertility rates", startRow = 2)[-1, ]
#tfr_t <- read.xlsx("data/adjTFR.xlsx", sheet = "Tempo-adjusted TFR", startRow = 2)[-1, ]
mabp <- read_table("data/HUNmabRRbo.txt", skip = 2)
p <- read_table("data/HUNasfrRRbo.txt", skip = 2)

ptfr_hungary <- ptfr %>% 
  as.data.frame() %>% 
  select(Year = COUNTRY, tfr = Hungary) %>% 
  mutate(Year = as.numeric(as.character(Year)),
         tfr = as.numeric(as.character(tfr)))


mabp_tm1 <- mabp %>% 
  select(Year, MAB1, MAB2, MAB3, MAB4, MAB5p)

add <- data.frame(Year = 2021:2022,
                  MAB1 = NA,
                  MAB2 = NA,
                  MAB3 = NA,
                  MAB4 = NA,
                  MAB5p = NA)

mabp_tp1 <- mabp_tm1[-c(1,2), ] %>% 
  bind_rows(add)

r <- (mabp_tp1 - mabp_tm1) / 2
d_r <- r %>% 
  mutate(Year = 1953:2021) %>% 
  gather(key = parity, value = r, -Year) %>% 
  mutate(parity = str_extract(parity, "(\\d)+"))

d_p <- p %>% 
  select(-ASFR) %>% 
  mutate(Age = case_when(Age == "12-" ~ "12",
                         Age == "55+" ~ "55",
                         T ~ Age),
         Age = as.numeric(as.character(Age))) %>% 
  gather(key = parity, value = p, -c(Year, Age)) %>% 
  mutate(parity = str_extract(parity, "(\\d)+"))

data <- d_r %>% 
  left_join(d_p, by = c("Year", "parity")) %>% 
  mutate(p_r = p / (1 - r)) %>% 
  group_by(Year, parity) %>% 
  summarise(sump_r = sum(p_r)) %>% 
  mutate(inside = 1 - exp(-sump_r)) %>% 
  group_by(Year) %>% 
  summarise(tfrp_sharp = sum(inside)) %>% 
  left_join(ptfr_hungary, by ="Year") %>% 
  gather(key = type, value = tfr, -Year)
  


data %>% 
  ggplot(aes(x = Year, y = tfr, colour = type, linetype = type)) +
  geom_line()


##
tfr_t_hungary <- tfr_t %>% 
  as.data.frame() %>% 
  select(Year2 = COUNTRY, adjtfr = Hungary)

data <- ptfr_hungary %>% 
  bind_cols(tfr_t_hungary) %>% 
  select(-Year2) %>% 
  gather(key = index, value = tfr, -Year) %>% 
  mutate(tfr = as.numeric(as.character(tfr)),
         tfr = ifelse(tfr == 0, NA, tfr),
         Year = as.numeric(as.character(Year)))
write.csv(data, "Nikkei Glocal/out/ptfr-data-rensai2024_3.csv", row.names = F, fileEncoding = "UTF-8")

data %>% 
  ggplot(aes(x = Year, y = tfr, colour = index)) +
  geom_line() +
  xlim(1980, 2020)
