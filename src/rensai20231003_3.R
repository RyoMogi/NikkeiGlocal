library(tidyverse)
library(openxlsx)
`%out%` = Negate(`%in%`)
Mycol <- c("#08306B", "#238B45", "#FD8D3C", "#D4B9DA", "#FFEDA0")

# データソース：第16回出生動向基本調査結果の概要
# https://www.ipss.go.jp/ps-doukou/j/doukou16/JNFS16gaiyo.pdf
partnerless <- data.frame(
  age = c("20-24", "25-29", "30-34", "20-24", "25-29", "30-34"),
  sex = c("男性", "男性", "男性", "女性", "女性", "女性"),
  prop = c(24, 26.1, 14.2, 30.9, 32.6, 20.6)
)

partnerless %>% 
  mutate(sex = factor(sex, levels = c("男性", "女性")),
         prop = 100 - prop,
         age = paste0(age, "歳"),
         age = factor(age, levels = c( "30-34歳", "25-29歳", "20-24歳"))) %>%
  ggplot(aes(x = prop, y = age, group = sex, fill = sex, label = prop)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(size = 4.5, hjust = 1.5, position = position_dodge(.9), colour = "white", show.legend = F) +
  scale_fill_manual(values = c(Mycol[2], Mycol[3])) +
  xlim(0, 100) +
  labs(x = "交際相手がいない人の割合（％）", 
       caption = "データソース：第16回出生動向基本調査結果の概要. 作成者：茂木良平") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 12))
#ggsave("out/jpn-partnerless.png", width = 7.5, height = 5, bg = "white")