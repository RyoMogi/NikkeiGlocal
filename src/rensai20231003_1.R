library(tidyverse)
library(openxlsx)
Mycol <- c("#08306B", "#238B45", "#FD8D3C", "#D4B9DA", "#FFEDA0")
`%out%` = Negate(`%in%`)


#----- read data
# data is from Human Fertility Database. Accessed on 25/05/2023
ptfr <- read.xlsx("data/TFR.xlsx", sheet = "Total fertility rates", startRow = 2)[-1, ]
# add Italian recent TFR: http://dati.istat.it/?lang=en. Accessed on 17/08/2023
ptfr_ita <- read.csv("data/ita_tfr_Istat.csv")
# Japanese TFR in 2021: https://www.mhlw.go.jp/toukei/saikin/hw/jinkou/kakutei21/dl/03_h1.pdf
# Finland TFR in 2022: https://www.stat.fi/en/publication/cl8mya8io1v2u0dutwqqanzqx

dptfr <- ptfr %>% 
  as.data.frame() %>% 
  rename(year = COUNTRY) %>% 
  mutate(Japan = ifelse(year == 2021, 1.3, Japan)) %>% 
  gather(key = country, value = ptfr, -year) %>% 
  mutate(ptfr = as.numeric(as.character(ptfr)),
         ptfr = ifelse(ptfr == 0, NA, ptfr),
         year = as.numeric(as.character(year))) %>% 
  drop_na()

dptfr_ita <- ptfr_ita %>% 
  select(country = Territory, year = TIME, ptfr = Value)

dptfr <- dptfr %>% 
  bind_rows(dptfr_ita) %>% 
  bind_rows(tibble(year = 2022, country = "Finland", ptfr = 1.32))

dptfr_sel <- dptfr %>% 
  filter(country %in% c("France", "Italy", "Japan",
                        "USA", "Finland")) %>% 
  mutate(country = case_when(country == "France" ~ "フランス", 
                             country == "Germany" ~ "ドイツ", 
                             country == "Italy" ~ "イタリア", 
                             country == "Japan" ~ "日本",
                             country == "United.Kingdom" ~ "イギリス", 
                             country == "USA" ~ "アメリカ", 
                             country == "Finland" ~ "フィンランド"),
         country = factor(country, levels = c("日本", "フランス", "ドイツ", "イタリア",
                                              "イギリス", "アメリカ", "フィンランド")))
write.csv(dptfr_sel, "out/rensai20231003_1.csv", row.names = F, fileEncoding = "UTF-8")

dptfr_sel %>% 
  ggplot(aes(x = year, y = ptfr, group = country, colour = country)) +
  geom_hline(yintercept = 2.1, linetype = "dashed") +
  geom_line(size = 1.1) +
  scale_colour_manual(values = Mycol) +
  labs(x = "年次", y = "合計特殊出生率",
       caption = "データソース：Human Fertility Database, Statistics of Finland, Istat, 人口動態統計. 作成者：茂木良平") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 12))
#ggsave("Nikkei Glocal/out/G7-ptfr.png", width = 7.5, height = 5, bg = "white")
