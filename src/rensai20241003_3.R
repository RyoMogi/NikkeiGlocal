library(tidyverse)
library(openxlsx)
library(readxl)
Mycol <- c("#08306B", "#238B45", "#FD8D3C", "#D4B9DA", "#FFEDA0")
`%out%` = Negate(`%in%`)


#----- read data
# data is from Vital Statistics, 人口統計資料集, 住民基本台帳. Accessed on 22/11/2024
ptfr <- read.xlsx("data/ptfr_T04-09.xlsx", sheet = "T04-09★", startRow = 3)[36, -1]
mabp <- read.csv("Nikkei Glocal/data/MAB_birthorder_pref.csv", skip = 14)
women2021 <- read.xlsx("Nikkei Glocal/data/pop_age_birthorder_pref_2021.xlsx", startRow = 2) %>% mutate(year = 2021)
women2020 <- read_excel("Nikkei Glocal/data/pop_age_birthorder_pref_2020.xls", skip = 1) %>% mutate(year = 2020)
women2019 <- read_excel("Nikkei Glocal/data/pop_age_birthorder_pref_2019.xls", skip = 1) %>% mutate(year = 2019)
women2018 <- read_excel("Nikkei Glocal/data/pop_age_birthorder_pref_2018.xls", skip = 1) %>% mutate(year = 2018)
women2017 <- read_excel("Nikkei Glocal/data/pop_age_birthorder_pref_2017.xls", skip = 1) %>% mutate(year = 2017)
women2016 <- read_excel("Nikkei Glocal/data/pop_age_birthorder_pref_2016.xls", skip = 1) %>% mutate(year = 2016)
birth <- read.csv("Nikkei Glocal/data/bx_birthorder_pref.csv", skip = 12)

#----- calculate r
d_ptfr <- ptfr %>% 
  gather(key = year, value = tfr) %>% 
  mutate(year = substr(year, 1, 4),
         year = as.numeric(as.character(year)),
         tfr = as.numeric(as.character(tfr)))

mabp_tm1 <- mabp %>% 
  select(code = "都道府県.特別区.指定都市再掲..コード", pref = "都道府県.特別区.指定都市再掲.", 
         year = "時間軸.年次.", MAB1 = "第1子", MAB2 = "第2子", MAB3 = "第3子") %>% 
  mutate(year = substr(year, 1, 4),
         year = as.numeric(year),
         year_t = year + 1,
         code = as.numeric(code)) %>% 
  arrange(code, year)

d_r <- mabp_tm1 %>% 
  select(-year_t) %>% 
  mutate(year_t = year - 1) %>% 
  left_join(mabp_tm1, by = c("code", "pref", "year_t")) %>% 
  mutate(MAB3.x = as.numeric(MAB3.x),
         MAB3.y = as.numeric(MAB3.y)) %>% 
  mutate(r1 = (MAB1.x - MAB1.y) / 2,
         r2 = (MAB2.x - MAB2.y) / 2,
         r3 = (MAB3.x - MAB3.y) / 2) %>% 
  select(code, pref, year_t, r1, r2, r3) %>% 
  drop_na() %>% 
  gather(key = parity, value = r, c(r1, r2, r3)) %>% 
  mutate(parity = str_extract(parity, "(\\d)+"),
         parity = as.numeric(parity))

#----- bx 
d_women <- women2021 %>% 
  filter(X3 == "女") %>% 
  mutate(`総数` = as.numeric(`総数`),
         `0歳～4歳` = as.numeric(as.character(`0歳～4歳`)),
         `5歳～9歳` = as.numeric(`5歳～9歳`),
         `10歳～14歳` = as.numeric(`10歳～14歳`),
         `15歳～19歳` = as.numeric(`15歳～19歳`),
         `20歳～24歳` = as.numeric(`20歳～24歳`),
         `25歳～29歳` = as.numeric(`25歳～29歳`),
         `30歳～34歳` = as.numeric(`30歳～34歳`),
         `35歳～39歳` = as.numeric(`35歳～39歳`),
         `40歳～44歳` = as.numeric(`40歳～44歳`),
         `45歳～49歳` = as.numeric(`45歳～49歳`),
         `50歳～54歳` = as.numeric(`50歳～54歳`),
         `55歳～59歳` = as.numeric(`55歳～59歳`),
         `60歳～64歳` = as.numeric(`60歳～64歳`),
         `65歳～69歳` = as.numeric(`65歳～69歳`),
         `70歳～74歳` = as.numeric(`70歳～74歳`),
         `75歳～79歳` = as.numeric(`75歳～79歳`),
         `80歳～84歳` = as.numeric(`80歳～84歳`),
         `85歳～89歳` = as.numeric(`85歳～89歳`),
         `90歳～94歳` = as.numeric(`90歳～94歳`),
         `95歳～99歳` = as.numeric(`95歳～99歳`),
         `100歳以上` = as.numeric(`100歳以上`)) %>% 
  select("団体コード" = X1, "都道府県名" = X2, "性別" = X3, year,
         "0～4歳" = "0歳～4歳", "5～9" = "5歳～9歳", "10～14" = "10歳～14歳", "15～19" = "15歳～19歳",
         "20～24" = "20歳～24歳", "25～29" = "25歳～29歳", "30～34" = "30歳～34歳", "35～39" = "35歳～39歳",
         "40～44" = "40歳～44歳", "45～49" = "45歳～49歳", "50～54" = "50歳～54歳", "55～59" = "55歳～59歳",
         "60～64" = "60歳～64歳", "65～69" = "65歳～69歳", "70～74" = "70歳～74歳", "75～79" = "75歳～79歳",
         "80～84" = "80歳～84歳", "85～89" = "85歳～89歳", "90～94" = "90歳～94歳", "95～99" = "95歳～99歳",
         "100歳以上") %>% 
  bind_rows(women2020) %>% 
  bind_rows(women2019) %>% 
  bind_rows(women2018) %>% 
  bind_rows(women2017) %>% 
  bind_rows(women2016) %>% 
  select(pref = "都道府県名", year, "15～19", "20～24", "25～29", "30～34", "35～39", "40～44", "45～49") %>% 
  gather(key = age_cate, value = pop, -c(pref, year)) %>% 
  mutate(age_cate = case_when(age_cate == "15～19" ~ "15-19",
                              age_cate == "20～24" ~ "20-24",
                              age_cate == "25～29" ~ "25-29",
                              age_cate == "30～34" ~ "30-34",
                              age_cate == "35～39" ~ "35-39",
                              age_cate == "40～44" ~ "40-44",
                              age_cate == "45～49" ~ "45-49"),
         pref = str_replace_all(pref, "[[:punct:]]", ""))

d_bx <- birth %>% 
  select(code = "都道府県.特別区.指定都市再掲..コード", pref = "都道府県.特別区.指定都市再掲.",
         year = "時間軸.年次.", age = "母の年齢.1歳階級.", B1 = "第1子", B2 = "第2子", 
         B3 = "第3子", B4 = "第4子", B5 = "第5子以上") %>% 
  filter(age %out% c("不詳", "総数")) %>% 
  mutate(age = substr(age, 1, 2),
         age = as.numeric(age),
         B1 = as.numeric(B1),
         B2 = as.numeric(B2),
         B3 = as.numeric(B3),
         B4 = as.numeric(B4),
         B5 = as.numeric(B5),
         B1 = ifelse(is.na(B1), 0, B1),
         B2 = ifelse(is.na(B2), 0, B2),
         B3 = ifelse(is.na(B3), 0, B3),
         B4 = ifelse(is.na(B4), 0, B4),
         B5 = ifelse(is.na(B5), 0, B5),
         B3p = B3 + B4 + B5,
         age_cate = case_when(age %in% 14:19 ~ "15-19",
                              age %in% 20:24 ~ "20-24",
                              age %in% 25:29 ~ "25-29",
                              age %in% 30:34 ~ "30-34",
                              age %in% 35:39 ~ "35-39",
                              age %in% 40:44 ~ "40-44",
                              age %in% 45:49 ~ "45-49",
                              age >= 50 ~ "50-"),
         year = substr(year, 1, 4),
         year = as.numeric(year)) %>% 
  filter(age <= 49) %>% 
  select(code, pref, year, age_cate, B1, B2, B3p) %>% 
  group_by(code, pref, year, age_cate) %>% 
  summarise(B1 = sum(B1),
            B2 = sum(B2),
            B3p = sum(B3p))

d_bx %>% 
  left_join(d_women, by = c("pref", "year", "age_cate")) %>% 
  group_by(pref, year) %>% 
  mutate(c1 = cumsum(B1),
         c2 = cumsum(B2),
         c3 = cumsum(B3p),
         N0 = pop - c1,
         N1 = c1 - c2,
         N2 = c2 - c3,
         N3 = c3)

#たぶん出生数からパリティを出すのは無理なはず、、、

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
