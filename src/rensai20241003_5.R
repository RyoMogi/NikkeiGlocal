library(tidyverse)
library(openxlsx)
Mycol <- c("#08306B", "#238B45", "#FD8D3C", "#D4B9DA", "#FFEDA0")
`%out%` = Negate(`%in%`)

#----- read data
# data is from Human Fertility Database. Accessed on 25/01/2025
cl <- read.xlsx("Nikkei Glocal/data/PARITY.xlsx", sheet = "Cohort childlessness", startRow = 2)

cl |> 
  filter(COUNTRY == 1976)

d_5 <- cl[-1,] |> 
  select(year = COUNTRY, Japan, Spain, Finland) |> 
  gather(key = country, value = cl, -year) |> 
  mutate(year = as.numeric(year),
         cl = as.numeric(cl),
         cl = ifelse(cl == 0, NA, cl),
         country = case_when(country == "Japan" ~ "日本",
                             country == "Finland" ~ "フィンランド",
                             country == "Spain" ~ "スペイン"),
         country = factor(country, levels = c("日本", "スペイン", "フィンランド")))
write.csv(d_5, "Nikkei Glocal/out/data-rensai2024_5.csv", row.names = F, fileEncoding = "UTF-8")

d_5 |> 
  ggplot(aes(x = year, y = cl, group = country, colour = country)) +
  geom_line(size = 1.2) +
  ylim(0, 30) +
  scale_colour_manual(values = Mycol[2:4]) +
  labs(x = "出生年", y = "女性の無子割合",
       caption = "データソース：Human Fertility Database. 作成者：茂木良平") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 12))
ggsave("Nikkei Glocal/out/rensai2024_5.png", width = 7.5, height = 5, bg = "white")
  
