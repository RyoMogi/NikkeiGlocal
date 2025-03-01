library(tidyverse)
library(readxl)
Mycol <- c("#08306B", "#88CCEE", "#238B45", "#FD8D3C", "#D4B9DA", "#FFEDA0")
col7 <- c("#332288", "#88CCEE", "#117733", "#999933", "#FD8D3C", "#882255", "#DDDDDD")

# read data on % of never married population
# data is from National Institute of Population and Social Security Research. Accessed on 25/05/2023
pref_tfr <- read_excel("data/T12-33.xls", sheet = "T12-33★", skip = 1)[c(1, 3:49), ]
d_pref_tfr <- pref_tfr %>% 
  as.data.frame() %>% 
  gather(key = year, value = tfr, -"都道府県") %>% 
  rename(pref = "都道府県") %>% 
  mutate(year = as.numeric(str_replace_all(year, "[^0-9]", "")),
         tfr = as.numeric(as.character(tfr))) %>% 
  filter(year %in% c(1925, 1930, 1935, 1940, 1945, 1950, 1955, 1960, 1970:2021))

pref_sel <- d_pref_tfr %>%
  filter(year == 2021) %>% 
  arrange(tfr)

pref_sel <- pref_sel[c(1:3, 46:48), 1]

d_pref_tfr <- d_pref_tfr %>% 
  filter(pref %in% c("全国", pref_sel),
         year >= 1975)
write.csv(d_pref_tfr, "Nikkei Glocal/out/pref-tfr.csv", row.names = F, fileEncoding = "UTF-8")


d_pref_tfr %>% 
  ggplot(aes(x = year, y = tfr, group = pref, colour = pref)) +
  geom_line(size = 1.1) +
  scale_colour_manual(values = c("black", Mycol)) +
  labs(x = "年次", y = "合計特殊出生率",
       caption = "データソース：国立社会保障人口問題研究所『人口統計資料集2023年版』. 作成者：茂木良平") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15))
ggsave("Nikkei Glocal/out/jpn-pref-tfr.png", width = 7, height = 5, bg = "white")

# for text
d_pref_tfr %>% 
  filter(year %in% c(1975, 2021),
         pref %in% c("全国", pref_sel))

d_pref_tfr %>% 
  filter(year >= 1975) %>% 
  group_by(year) %>% 
  arrange(tfr) %>% 
  slice(1:3, 46:48) %>% View()

d_pref_tfr %>% 
  filter(year == 2021,
         tfr < 1.3)
