library(tidyverse)
library(openxlsx)
Mycol <- c("#08306B", "#238B45", "#FD8D3C", "#D4B9DA", "#FFEDA0")
`%out%` = Negate(`%in%`)


#----- read data
# data is from Human Fertility Database. Accessed on 12/03/2024
ptfr <- read.xlsx("data/TFR.xlsx", sheet = "Total fertility rates", startRow = 2)[-1, ]

data <- ptfr %>% 
  as.data.frame() %>% 
  select(Year = COUNTRY, tfr = United.Kingdom) %>%
  mutate(Year = as.numeric(as.character(Year)),
         tfr = as.numeric(as.character(tfr)),
         tfr = ifelse(tfr == 0, NA, tfr),
         tfr = case_when(Year == 2021 ~ 1.55,
                         Year == 2022 ~ 1.49,
                         T ~ tfr))
write.csv(data, "Nikkei Glocal/out/data-rensai2024_6.csv", row.names = F, fileEncoding = "UTF-8")

data %>% 
  ggplot(aes(x = Year, y = tfr)) +
  geom_hline(yintercept = 2.1, linetype = "dotted", col = "red") +
  geom_line(size = 1.2) +
  xlim(1970, 2022) +
  ylim(1.5, 2.1) +
  labs(x = "年次", y = "合計特殊出生率",
       caption = "データソース：Human Fertility Database and Office for National Statistics. 作成者：茂木良平") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 12))
ggsave("Nikkei Glocal/out/rensai2024_6.png", width = 7.5, height = 5, bg = "white")
