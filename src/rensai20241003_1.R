library(tidyverse)
Mycol <- c("#08306B", "#238B45", "#FD8D3C", "#D4B9DA", "#FFEDA0")
`%out%` = Negate(`%in%`)

d <- data.frame(
  age = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
           "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", 
           "80-84", "85-89", "90-94", "95+"),
  men_zenkoku = c(3.76, 2.76, 1.45, -0.53, 2.33, 3.29, 3.04, 2.15, 1.32, 0.80, 0.27, -0.09, -0.46, -0.25,
                   -2.12, -1.57, -1.84, -2.57, -1.47, -0.76),
  men_daitoshi = c(3.94, 3, 1.6, -2.63, -2.59, 2.42, 2.41, 1.76, 1.1, 0.67, 0.07, -0.5, -0.92, -0.63, -2.62, 
                    -1.9, -2.17, -3.34, -2.23, -1.37),
  men_hidai = c(3.56, 2.51, 1.29, 1.69, 8.6, 4.36, 3.81, 2.61, 1.59, 0.97, 0.51, 0.3, -0.05, 0.11, -1.58,
                 -1.23, -1.54, -1.97, -0.93, -0.34),
  women_zenkoku = c(3.32, 2.54, 1.34, 0.26, 2.6, 2.6, 2.04, 1.25, 0.61, 0.17, 0, -0.56, -0.93, -0.49,
                     -1.9, -1.4, -1.62, -2.35, -0.87, 0.32),
  women_daitoshi = c(3.45, 2.73, 1.45, -2.28, -2.18, 1.67, 1.43, 0.94, 0.33, -0.17, -0.44, -1.13, -1.47,
                      -1.06, -2.55, -1.98, -2.36, -3.48, -2.04, -0.42),
  women_hidai = c(3.19, 2.35, 1.22, 2.98, 8.69, 3.74, 2.75, 1.6, 0.96, 0.59, 0.47, -0.04, -0.45, 0.08,
                   -1.22, -0.85, -1.01, -1.52, -0.07, 0.83)
) %>% 
  gather(key = indi, value = diff, -age) %>% 
  separate(col = indi, into = c("sex", "city"), sep = "_") %>% 
  mutate(diff = as.numeric(as.character(diff)),
         sex = ifelse(sex == "men", "男性", "女性"),
         city = case_when(city == "zenkoku" ~ "全国",
                          city == "daitoshi" ~ "大都市",
                          city == "hidai" ~ "非大都市"),
         city = factor(city, levels = c("全国", "大都市", "非大都市")),
         age = factor(age, levels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
                                      "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", 
                                      "80-84", "85-89", "90-94", "95+")))
write.csv(d, "Nikkei Glocal/out/data-rensai2024_2_1.csv", row.names = F, fileEncoding = "UTF-8")

d %>% 
  ggplot(aes(x = age, y = diff, group = city, linetype = city)) +
  facet_grid(~ sex) +
  geom_line() +
  scale_linetype_manual(values = c("solid", "dotted", "dashed")) +
  labs(x = "年齢",
       caption = "出典：小池・貴志（2020）") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_blank(),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 12))
ggsave("Nikkei Glocal/out/rensai2024_2_1.png", width = 7.5, height = 5, bg = "white")


