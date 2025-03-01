library(tidyverse)
library(openxlsx)
Mycol <- c("#08306B", "#238B45", "#FD8D3C", "#D4B9DA", "#FFEDA0")
`%out%` = Negate(`%in%`)


#----- read data
# data is from Human Fertility Database. Accessed on 12/03/2024
ptfr <- read.xlsx("data/TFR.xlsx", sheet = "Total fertility rates", startRow = 2)[-1, ]
mabp <- read_table("data/HUNmabRRbo.txt", skip = 2)
women <- read_table("data/HUNexposRRpa.txt", skip = 2)
birth <- read_table("data/HUNbirthsRRbo.txt", skip = 2)

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
  mutate(parity = str_extract(parity, "(\\d)+"),
         parity = as.numeric(parity))

d_women <- women %>% 
  mutate(E0x = as.numeric(E0x),
         E1x = as.numeric(E1x),
         E2x = as.numeric(E2x),
         E3x = as.numeric(E3x),
         E4px = as.numeric(E4px),
         N0 = E0x,
         N1 = E0x + E1x,
         N2 = E0x + E1x + E2x,
         N3 = E0x + E1x + E2x + E3x,
         N4 = E0x + E1x + E2x + E3x + E4px)

d_birth <- birth %>% 
  mutate(Age = case_when(Age == "12-" ~ "12",
                         Age == "55+" ~ "55",
                         T ~ Age),
         Age = as.numeric(as.character(Age)))

d_p <- d_women %>% 
  left_join(d_birth, by = c("Year", "Age")) %>% 
  mutate(p_1 = B1 / N0,
         p_2 = B2 / N1,
         p_3 = B3 / N2,
         p_4 = B4 / N3,
         p_5 = B5p / N4) %>%
  select(Year, Age, p_1, p_2, p_3, p_4, p_5) %>% 
  gather(key = parity, value = p, -c(Year, Age)) %>% 
  mutate(parity = str_extract(parity, "(\\d)+"),
         parity = as.numeric(parity))

data <- d_r %>% 
  left_join(d_p, by = c("Year", "parity")) %>% 
  drop_na() %>% 
  mutate(p_r = p / (1 - r)) %>% 
  group_by(Year, parity) %>% 
  summarise(sump_r = sum(p_r)) %>% 
  mutate(inside = 1 - exp(-sump_r)) %>% 
  group_by(Year) %>% 
  summarise(tfrp_sharp = sum(inside)) %>% 
  left_join(ptfr_hungary, by ="Year") %>% 
  gather(key = type, value = tfr, -Year) %>% 
  mutate(type = ifelse(type == "tfr", "合計特殊出生率", "調整済み合計特殊出生率")) %>% 
  filter(Year >= 1975)
write.csv(data, "Nikkei Glocal/out/data-rensai2024_3.csv", row.names = F, fileEncoding = "UTF-8")

data %>% 
  ggplot(aes(x = Year, y = tfr, colour = type, linetype = type)) +
  geom_line(size = 1.2) +
  labs(x = "年次", y = "合計特殊出生率",
       caption = "データソース：Human Fertility Database. 作成者：茂木良平") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 12))
ggsave("Nikkei Glocal/out/rensai2024_3.png", width = 7.5, height = 5, bg = "white")
