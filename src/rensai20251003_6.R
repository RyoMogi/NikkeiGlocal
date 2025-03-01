library(tidyverse)
library(ggrepel)
Mycol <- c("#08306B", "#238B45", "#FD8D3C", "#D4B9DA", "#FFEDA0")
`%out%` = Negate(`%in%`)

# データソース：厚生労働省「雇用均等基本調査」
# https://www.mhlw.go.jp/toukei/list/dl/71-r05/03.pdf
koyou_rate <- data.frame(
  year = c(1996, 1999, 2002, 2004, 2005, 2007:2023),
  f_rate = c(49.1, 56.4, 64, 70.6, 72.3, 89.7, 90.6, 85.6, 83.7, 87.8, 83.6, 83, 
             86.6, 81.5, 81.8, 83.2, 82.2, 83, 81.6, 85.1, 80.2, 84.1),
  m_rate = c(0.12, 0.42, 0.33, 0.56, 0.5, 1.56, 1.23, 1.72, 1.38, 2.63, 1.89, 2.03, 
             2.3, 2.65, 3.16, 5.14, 6.16, 7.48, 12.65, 13.97, 17.13, 30.1)
)
write.csv(koyou_rate, "Nikkei Glocal/out/jpn-parentleave_koyou.csv")

koyou_rate %>% 
  gather(key = sex, value = rate, -year) %>% 
  mutate(sex = ifelse(sex == "f_rate", "女性", "男性")) %>% 
  ggplot(aes(x = year, y = rate, group = sex, colour = sex, label = rate)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_text_repel(nudge_y = 5, show_guide = F) +
  ylim(0, 100) +
  scale_colour_manual(values = c(Mycol[3], Mycol[2])) +
  labs(x = "年次", y = "育児休業取得割合",
       caption = "データソース：厚生労働省「雇用均等基本調査」. 作成者：茂木良平") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.box = "vertical",
        strip.text.x = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        panel.spacing = unit(2, "lines"))
ggsave("Nikkei Glocal/out/jpn-parentleave_koyou.png", width = 7.5, height = 5, bg = "white")


# データソース：社人研「出生動向基本調査16回」
# https://www.ipss.go.jp/ps-doukou/j/doukou16/JNFS16gaiyo.pdf
jnfs_rate <- data.frame(
  year = c("1985-1989", "1990-1994", "1995-1999", "2000-2004", "2005-2009",
           "2010-2014", "2015-2018"),
  f_rate_all = c(6, 9, 12.6, 17.5, 24.1, 34.8, 43),
  m_rate_all = c(NA, NA, 0.2, 0.3, 0.8, 0.9, 3.7),
  f_rate_ful = c(34.1, 50.3, 67, 79.5, 85.9, 88.2, 92.6),
  m_rate_ful = c(NA, NA, 0.9, 0.7, 1.7, 1.7, 6.3)
)
write.csv(jnfs_rate, "Nikkei Glocal/out/jnfs_rate.csv")

jnfs_rate %>% 
  gather(key = key, value = value, -year) %>% 
  mutate(sex = str_sub(key, 1, 1),
         sex = ifelse(sex == "m", "男性", "女性"),
         var = str_sub(key, -3, -1),
         var = ifelse(var == "all", "総合", "妻が正規雇用"),
         var = factor(var, levels = c("総合", "妻が正規雇用"))) %>% 
  select(-key) %>% 
  ggplot(aes(x = year, y = value, group = sex, colour = sex, label = value)) +
  facet_wrap(~ var) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_text(nudge_y = 5, show_guide = F) +
  ylim(0, 100) +
  scale_colour_manual(values = c(Mycol[3], Mycol[2])) +
  labs(x = "第一子出生年", y = "育児休業取得割合",
       caption = "データソース：社人研「出生動向基本調査16回」. 作成者：茂木良平") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.box = "vertical",
        strip.text.x = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.spacing = unit(2, "lines"))
ggsave("Nikkei Glocal/out/jpn-parentleave_jnfs.png", width = 7.5, height = 5, bg = "white")
