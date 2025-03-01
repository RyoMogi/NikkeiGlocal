library(tidyverse)
library(stringi)
Mycol <- c("#08306B", "#238B45", "#FD8D3C", "#D4B9DA", "#FFEDA0")
`%out%` = Negate(`%in%`)

#----- read data
# data is from Vital Statistics. 保管統計表　都道府県編（報告書非掲載表）. 出生.　表4
# https://www.e-stat.go.jp/stat-search/files?page=1&layout=datalist&toukei=00450011&tstat=000001028897&cycle=7&year=20230&month=0&tclass1=000001053058&tclass2=000001053061&tclass3=000001053074&tclass4=000001053084&result_back=1&tclass5val=0
# Accessed on 14/12/2024
file_list <- list.files(path = "Nikkei Glocal/data/nbirth_parity_pref", 
                        full.names = T)

# data is from 人口推計. 各年10月1日現在人口. 都道府県.　表10
# https://www.e-stat.go.jp/stat-search/files?page=1&layout=datalist&toukei=00200524&tstat=000000090001&cycle=7&year=20230&month=0&tclass1=000001011679&result_back=1&tclass2val=0
npop <- read.csv("Nikkei Glocal/data/popf_pref23.csv", skip = 10)[, c(10, 12:18)]

zenkoku <- read.csv(file("Nikkei Glocal/data/nbirth_parity_zenkoku.csv", encoding = "cp932"), skip = 4)[3:12, -1]

#----- clean data
read_alldata <- function(file_path) {
  pref_name <- tools::file_path_sans_ext(basename(file_path))
  
  data <- read.csv(file(file_path, encoding = "cp932"), skip = 6)[2:44, 2:8]
  
  data <- data |> mutate(pref = pref_name)
  
  return(data)
}

d_bp <- file_list %>%
  lapply(read_alldata) |> 
  bind_rows() |> 
  as.data.frame() |> 
  rename("age" = "X.1") |> 
  mutate(`第４子` = as.numeric(`第４子`),
         `第５子以上` = as.numeric(`第５子以上`),
         `第４子` = ifelse(is.na(`第４子`), 0, `第４子`),
         `第５子以上` = ifelse(is.na(`第５子以上`), 0, `第５子以上`),
         b4p = `第４子` + `第５子以上`) |> 
  select(-c(`第４子`, `第５子以上`)) |> 
  gather(key = parity, value = nb, -c(pref, age)) |> 
  filter(age %out% c("総　数", "１４歳以下", "５０歳", "５１歳", "５２歳",    
                     "５３歳", "５４歳", "５５歳以上")) |> 
  mutate(parity = case_when(parity == "総.数" ~ "all",
                            parity == "第１子" ~ "b1",
                            parity == "第２子" ~ "b2",
                            parity == "第３子" ~ "b3",
                            T ~ parity),
         age = substr(age, start = 1, stop = 2),
         age = trimws(age),
         age = stri_trans_general(age, "Fullwidth-Halfwidth"),
         age = as.numeric(age),
         age5 = case_when(age %in% 15:19 ~ "15-19",
                          age %in% 20:24 ~ "20-24",
                          age %in% 25:29 ~ "25-29",
                          age %in% 30:34 ~ "30-34",
                          age %in% 35:39 ~ "35-39",
                          age %in% 40:44 ~ "40-44",
                          age %in% 45:49 ~ "45-49"),
         nb = as.numeric(nb),
         nb = ifelse(is.na(nb), 0, nb),
         pref = case_when(pref == "aichi23" ~ "愛知県",
                          pref == "akita23" ~ "秋田県",
                          pref == "aomori23" ~ "青森県",
                          pref == "chiba23" ~ "千葉県",
                          pref == "ehime23" ~ "愛媛県",
                          pref == "fukui23" ~ "福井県",
                          pref == "fukuoka23" ~ "福岡県",
                          pref == "fukushima23" ~ "福島県",
                          pref == "gifu23" ~ "岐阜県",
                          pref == "gunma23" ~ "群馬県",
                          pref == "hiroshima23" ~ "広島県",
                          pref == "hokkaido23" ~ "北海道", 
                          pref == "hyogo23" ~ "兵庫県",
                          pref == "ibaraki23" ~ "茨城県",
                          pref == "ishikawa23" ~ "石川県",
                          pref == "iwate23" ~ "岩手県",
                          pref == "kagawa23" ~ "香川県",
                          pref == "kagoshima23" ~ "鹿児島県",
                          pref == "kanagawa23" ~ "神奈川県",
                          pref == "kochi23" ~ "高知県",
                          pref == "kumamoto23" ~ "熊本県",
                          pref == "kyoto23" ~ "京都府",
                          pref == "mie23" ~ "三重県",
                          pref == "miyagi23" ~ "宮城県",  
                          pref == "miyazaki23" ~ "宮崎県", 
                          pref == "nagano23" ~ "長野県",
                          pref == "nagasaki23" ~ "長崎県",
                          pref == "nara23" ~ "奈良県",
                          pref == "niigata23" ~ "新潟県",
                          pref == "oita23" ~ "大分県",
                          pref == "okayama23" ~ "岡山県",
                          pref == "okinawa23" ~ "沖縄県",
                          pref == "osaka23" ~ "大阪府",
                          pref == "saga23" ~ "佐賀県",
                          pref == "saitama23" ~ "埼玉県",
                          pref == "shiga23" ~ "滋賀県",
                          pref == "shimane23" ~ "島根県",
                          pref == "shizuoka23" ~ "静岡県",
                          pref == "tochigi23" ~ "栃木県",
                          pref == "tokushima23" ~ "徳島県",
                          pref == "tokyo23" ~ "東京都",
                          pref == "tottori23" ~ "鳥取県",
                          pref == "toyama23" ~ "富山県",
                          pref == "wakayama23" ~ "和歌山県",
                          pref == "yamagata23" ~ "山形県",
                          pref == "yamaguchi23" ~ "山口県",
                          pref == "yamanashi23" ~ "山梨県")) |> 
  group_by(pref, parity, age5) |>
  summarise(nb = sum(nb))
  

d_npop <- npop |>
  gather(key = age5, value = pop, -`全国.都道府県`) |> 
  mutate(age5 = case_when(age5 == "X15.19歳" ~ "15-19",
                          age5 == "X20.24歳" ~ "20-24",
                          age5 == "X25.29歳" ~ "25-29",
                          age5 == "X30.34歳" ~ "30-34",
                          age5 == "X35.39歳" ~ "35-39", 
                          age5 == "X40.44歳" ~ "40-44",
                          age5 == "X45.49歳" ~ "45-49"),
         pop = gsub(",", "", pop),
         pop = as.numeric(pop),
         pop = pop * 1000) |> 
  rename("pref" = "全国.都道府県")

d_zenkoku <- zenkoku |> 
  gather(key = age, value = nb, -`X.1`) |> 
  filter(age %out% c("総.数", "１４歳以下", "５０歳", "５１歳", "５２歳",    
                     "５３歳", "５４歳", "５５歳以上", "不.詳")) |> 
  rename(parity = `X.1`) |> 
  mutate(parity = case_when(parity == "第１子" ~ "b1",
                            parity == "第２子" ~ "b2",
                            parity == "第３子" ~ "b3",
                            T ~ "b4p"),
         age = substr(age, start = 1, stop = 2),
         age = trimws(age),
         age = stri_trans_general(age, "Fullwidth-Halfwidth"),
         age = as.numeric(age),
         age5 = case_when(age %in% 15:19 ~ "15-19",
                          age %in% 20:24 ~ "20-24",
                          age %in% 25:29 ~ "25-29",
                          age %in% 30:34 ~ "30-34",
                          age %in% 35:39 ~ "35-39",
                          age %in% 40:44 ~ "40-44",
                          age %in% 45:49 ~ "45-49"),
         nb = as.numeric(nb),
         nb = ifelse(is.na(nb), 0, nb)) |> 
  group_by(parity, age5) |>
  summarise(nb = sum(nb)) |> 
  mutate(pref = "全国")

d_bp <- d_bp |> 
  group_by(parity, age5) |> 
  summarise(nb = sum(nb)) |> 
  mutate(pref = "全国") |> 
  bind_rows(d_bp)

tfr_p <- d_bp |> 
  #bind_rows(d_zenkoku) |> 
  left_join(d_npop, by = c("pref", "age5")) |> 
  mutate(asfr = nb / pop * 5) |> 
  group_by(pref, parity) |> 
  summarise(tfr = sum(asfr)) |> 
  ungroup()
write.csv(tfr_p, "Nikkei Glocal/out/data-rensai2024_2_4.csv")

#----- plot
order_tfr <- tfr_p |> 
  filter(parity == "all",
         pref != "全国") |> 
  arrange(tfr)

d_fig <- tfr_p |> 
  filter(parity != "all",
         pref != "全国") |> 
  mutate(pref = factor(pref, levels = order_tfr$pref))

tfr_p_all <- tfr_p |> 
  filter(pref == "全国",
         parity != "all")

ggplot() +
  geom_point(data = d_fig, aes(x = tfr, y = pref, group = parity, colour = parity), size = 2.5) +
  geom_vline(xintercept = tfr_p_all[tfr_p_all$parity == "b1", ]$tfr, 
             linetype = "dashed", colour = Mycol[1]) +
  geom_vline(xintercept = tfr_p_all[tfr_p_all$parity == "b2", ]$tfr, 
             linetype = "dashed", colour = Mycol[2]) +
  geom_vline(xintercept = tfr_p_all[tfr_p_all$parity == "b3", ]$tfr, 
             linetype = "dashed", colour = Mycol[3]) +
  geom_vline(xintercept = tfr_p_all[tfr_p_all$parity == "b4p", ]$tfr, 
             linetype = "dashed", colour = Mycol[4]) +
  scale_colour_manual(values = Mycol[1:4], name = "出生順位",
                      labels = c("第1子", "第2子", "第3子", "第4子以上")) +
  labs(x = "合計特殊出生率",
       caption = "データソース：人口動態調査、人口推計. 茂木良平作成") +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(legend.position = "top",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 10, face = 2),
        axis.title = element_text(size = 15),
        axis.title.y = element_blank())
ggsave("Nikkei Glocal/out/tfr_parity_pref.png", width = 8, height = 8, bg = "white")

##
try1 <- tfr_p |> 
  filter(parity == "all",
         pref != "全国") |> 
  arrange(desc(tfr)) |> 
  mutate(rank = 1:47)

try2 <- tfr_p |> 
  filter(parity == "b1",
         pref != "全国") |> 
  arrange(desc(tfr)) |> 
  mutate(rank = 1:47)

try1 |> 
  left_join(try2, by = "pref") |> 
  mutate(diff = rank.x - rank.y) |> View()

tfr_p |> 
  filter(pref != "全国") |> 
  group_by(parity) |> 
  summarise(variance = var(tfr),
            mean = mean(tfr),
            max = max(tfr),
            min = min(tfr)) |> 
  mutate(diff = max - min)

# ver2
#tfr_p |> 
#  filter(parity != "all") |> 
#  left_join(order_tfr, by = c("pref")) |> 
#  select(pref, parity = "parity.x", tfr_p = "tfr.x", tfr = "tfr.y") |> 
#  ggplot(aes(x = tfr, y = tfr_p, group = pref)) +
#  facet_wrap(~ parity) +
  geom_point()