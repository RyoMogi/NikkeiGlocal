library(tidyverse)
library(openxlsx)
Mycol <- c("#08306B", "#238B45", "#FD8D3C", "#D4B9DA", "#FFEDA0")
`%out%` = Negate(`%in%`)


#----- read data
# data is from Human Fertility Database. Accessed on 12/07/2024
ptfr <- read.xlsx("data/TFR.xlsx", sheet = "Total fertility rates", startRow = 2)[-1, ]
adj_ptfr <- read.xlsx("data/adjTFR.xlsx", sheet = "Tempo-adjusted TFR", startRow = 2)[-1, ]


d_ptfr <- ptfr %>% 
  as.data.frame() %>% 
  select(Year = COUNTRY, tfr = Germany) %>% 
  mutate(tfr = case_when(Year == "2018" ~ "1.57",
                         Year == "2019" ~ "1.54",
                         Year == "2020" ~ "1.53",
                         Year == "2021" ~ "1.58",
                         Year == "2022" ~ "1.46",
                         T ~ tfr))

d_adj_ptfr <- adj_ptfr %>% 
  as.data.frame() %>% 
  select(Year = COUNTRY, adjtfr = Germany)

data <- d_ptfr %>% 
  left_join(d_adj_ptfr, by = "Year") %>% 
  gather(key = index, value = tfr, -Year) %>% 
  mutate(Year = as.numeric(as.character(Year)),
         tfr = as.numeric(as.character(tfr)),
         tfr = ifelse(tfr == 0, NA, tfr),
         index = ifelse(index == "tfr", "合計特殊出生率", "調整済み合計特殊出生率"),
         index = factor(index, levels = c("合計特殊出生率", "調整済み合計特殊出生率")))
write.csv(data, "Nikkei Glocal/out/data-rensai2024_5.csv", row.names = F, fileEncoding = "UTF-8")

data %>% 
  ggplot(aes(x = Year, y = tfr, linetype = index)) +
  geom_hline(yintercept = 2.1, linetype = "dotted", col = "red") +
  geom_line(size = 1.2) +
  scale_linetype_manual(values = c("solid", "dotted")) +
  labs(x = "年次", y = "合計特殊出生率",
       caption = "データソース：Human Fertility Database. 作成者：茂木良平") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 12))
ggsave("Nikkei Glocal/out/rensai2024_5.png", width = 7.5, height = 5, bg = "white")

####
data <- ptfr %>% 
  as.data.frame() %>% 
  select(Year = COUNTRY, Germany, Germany.East, Germany.West) %>%
  gather(key = region, value = tfr, -Year) %>% 
  mutate(Year = as.numeric(as.character(Year)),
         tfr = as.numeric(as.character(tfr)),
         tfr = ifelse(tfr == 0, NA, tfr),
         region = case_when(region == "Germany" ~ "ドイツ",
                            region == "Germany.East" ~ "東ドイツ",
                            region == "Germany.West" ~ "西ドイツ"))
write.csv(data, "Nikkei Glocal/out/data-rensai2024_5.csv", row.names = F, fileEncoding = "UTF-8")

data %>% 
  ggplot(aes(x = Year, y = tfr, colour = region, linetype = region)) +
  geom_hline(yintercept = 2.1, linetype = "dotted", col = "red") +
  geom_line(size = 1.2) +
  scale_colour_manual(values = c("black", Mycol[3:4])) +
  scale_linetype_manual(values = c("solid", "dotted", "dashed")) +
  labs(x = "年次", y = "合計特殊出生率",
       caption = "データソース：Human Fertility Database. 作成者：茂木良平") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 12))
ggsave("Nikkei Glocal/out/rensai2024_5.png", width = 7.5, height = 5, bg = "white")
