getwd()

library(readxl)
library(dplyr)
library(sf)
library(ggplot2)
library(tidyr)
library(stringr)

#загружаем версионный справочник
excel_sheets("/Users/abdulhalikoff/Documents/R/data_oktmo.xlsx")
data_oktmo <- read_excel("/Users/abdulhalikoff/Documents/R/data_oktmo.xlsx")
str(data_oktmo)
names(data_oktmo)

#загружаем данные по муниципалитетам
excel_sheets("/Users/abdulhalikoff/Documents/R/Urov-14a_2010-2022.xlsx")

#выбираем нужные листы с 2018 по 2022
mo_2018 <- read_excel("/Users/abdulhalikoff/Documents/R/Urov-14a_2010-2022.xlsx", sheet = "2018")
names(mo_2018)

#создаем копию для обработки
data_2018 <- mo_2018

#удаляем лишнюю колонку
data_2018 <- data_2018 %>%
  select(-"Содержание")

#переименуем
data_2018 <- data_2018 %>%
  rename(
    municipal_district_name_short = "...2",
    oktmo = "...3",
    income = "...4",
    social_payments = "...5",
    sum_income_payments = "...6",
    per_1_person = "...7"
  )

mo_2019 <- read_excel("/Users/abdulhalikoff/Documents/R/Urov-14a_2010-2022.xlsx", sheet = "2019")
names(mo_2019)
data_2019 <- mo_2019
#удаляем лишнюю колонку
data_2019 <- data_2019 %>%
  select(-"Содержание")

#переименуем названия колонок
data_2019 <- data_2019 %>%
  rename(
    municipal_district_name_short = "...2",
    oktmo = "...3",
    income = "...4",
    social_payments = "...5",
    sum_income_payments = "...6",
    per_1_person = "...7"
  )

#2020 год
mo_2020 <- read_excel("/Users/abdulhalikoff/Documents/R/Urov-14a_2010-2022.xlsx", sheet = "2020")
data_2020 <- mo_2020

#удаляем лишнюю колонку
data_2020 <- data_2020 %>%
  select(-"Содержание")

#переименуем названия колонок
data_2020 <- data_2020 %>%
  rename(
    municipal_district_name_short = "...2",
    oktmo = "...3",
    income = "...4",
    social_payments = "...5",
    sum_income_payments = "...6",
    per_1_person = "...7"
  )

#2021 год
mo_2021 <- read_excel("/Users/abdulhalikoff/Documents/R/Urov-14a_2010-2022.xlsx", sheet = "2021")
data_2021 <- mo_2021

#удаляем лишнюю колонку
data_2021 <- data_2021 %>%
  select(-"Содержание")

#переименуем названия колонок
data_2021 <- data_2021 %>%
  rename(
    municipal_district_name_short = "...2",
    oktmo = "...3",
    income = "...4",
    social_payments = "...5",
    sum_income_payments = "...6",
    per_1_person = "...7"
  )

#2022 год
mo_2022 <- read_excel("/Users/abdulhalikoff/Documents/R/Urov-14a_2010-2022.xlsx", sheet = "2022")
data_2022 <- mo_2022

#удаляем лишнюю колонку
data_2022 <- data_2022 %>%
  select(-"Содержание")

#переименуем названия колонок
data_2022 <- data_2022 %>%
  rename(
    municipal_district_name_short = "...2",
    oktmo = "...3",
    income = "...4",
    social_payments = "...5",
    sum_income_payments = "...6",
    per_1_person = "...7"
  )

#проверяем 2018 год
summary(data_2018)

#проверяем на пропуски
colSums(is.na(data_2018))

#видим одинаковое количество na в 4 колонках
data_2018[!complete.cases(data_2018), ]
print(data_2018[!complete.cases(data_2018), ], n = Inf)

#видим, что все na относятся не к МО, удалим
data_2018 <- data_2018 %>%
  drop_na()
colSums(is.na(data_2018))

#na нет, но при визуальной проверке заметил наличие строк, не отражающих конкретные муниципалитеты.
data_2018 %>%
  filter(municipal_district_name_short %in% c("Муниципальные районы", "Городские округа", "Административные районы", "Муниципальные округа", "Муниципальные районы Камчатского края, входящие в состав Корякского округа", "Городские округа Камчатского края, входящие в состав Корякского округа", "Городские округа с внутригородским делением")) %>%
  count(municipal_district_name_short)

nrow(data_2018)
nrow(mo_2018)
data_2018 <- data_2018 %>%
  filter(!(municipal_district_name_short %in% c("Муниципальные районы", "Городские округа", "Административные районы", "Муниципальные округа", "Муниципальные районы Камчатского края, входящие в состав Корякского округа", "Городские округа Камчатского края, входящие в состав Корякского округа", "Городские округа с внутригородским делением")))

#2019
nrow(data_2019)
data_2019 <- data_2019 %>%
  drop_na()
data_2019 <- data_2019 %>%
  filter(!(municipal_district_name_short %in% c("Муниципальные районы", "Городские округа", "Административные районы", "Муниципальные округа", "Муниципальные районы Камчатского края, входящие в состав Корякского округа", "Городские округа Камчатского края, входящие в состав Корякского округа", "Городские округа с внутригородским делением")))

#2020
nrow(data_2020)
data_2020 <- data_2020 %>%
  drop_na()
data_2020 <- data_2020 %>%
  filter(!(municipal_district_name_short %in% c("Муниципальные районы", "Городские округа", "Административные районы")))

#2021
nrow(data_2021)
data_2021 <- data_2021 %>%
  drop_na()
data_2021 <- data_2021 %>%
  filter(!(municipal_district_name_short %in% c("Муниципальные районы", "Городские округа", "Административные районы", "Муниципальные округа", "Муниципальные районы Камчатского края, входящие в состав Корякского округа", "Городские округа Камчатского края, входящие в состав Корякского округа", "Городские округа с внутригородским делением")))

#2022
nrow(data_2022)
data_2022 <- data_2022 %>%
  drop_na()
data_2022 <- data_2022 %>%
  filter(!(municipal_district_name_short %in% c("Муниципальные районы", "Городские округа", "Административные районы", "Муниципальные округа", "Муниципальные районы Камчатского края, входящие в состав Корякского округа", "Городские округа Камчатского края, входящие в состав Корякского округа", "Городские округа с внутригородским делением")))

#типы данных все указывает как символьные
names(data_2018)
str(data_2018)
# "municipal_district_name_short" "oktmo" оставляем символьными
#"income", "social_payments", "sum_income_payments", "per_1_person" переводим в numeric

#уберём пробелы между числами
data_2018_cleaned <- data_2018 %>%
  mutate(oktmo = gsub(" ", "", oktmo))
#не помогло

#не помогло. Пробелы между группами чисел так не удаляет.
data_2018_cleaned <- data_2018 %>%
  mutate(oktmo = str_replace_all(oktmo, " ", ""))

#Используем регулярку
data_2018_cleaned <- data_2018 %>%
  mutate(oktmo = str_replace_all(oktmo, "\\s+", ""))

unique(data_2018_cleaned$oktmo)
summary(data_2018_cleaned$oktmo)
str(data_2018_cleaned$oktmo)
unique(data_2018$oktmo)
unique(data_2018_cleaned$oktmo)

#чистим
data_2018_cleaned <- data_2018 %>%
  mutate(
    oktmo = str_replace_all(oktmo, "\\s+", ""),
    income = str_replace_all(income, "\\s+", ""),
    social_payments = str_replace_all(social_payments, "\\s+", ""),
    sum_income_payments = str_replace_all(sum_income_payments, "\\s+", ""),
    per_1_person = str_replace(per_1_person, "\\s+", "")
  )

#через mutate не дало изменить тип данных, делаем по-простому
data_2018_cleaned$income <- as.numeric(data_2018_cleaned$income)
data_2018_cleaned$social_payments <- as.numeric(data_2018_cleaned$social_payments)
data_2018_cleaned$sum_income_payments <- as.numeric(data_2018_cleaned$sum_income_payments)
data_2018_cleaned$per_1_person <- as.numeric(data_2018_cleaned$per_1_person)

str(data_2018_cleaned)
summary(data_2018_cleaned)

#2019
data_2019_cleaned <- data_2019 %>%
  mutate(
    oktmo = str_replace_all(oktmo, "\\s+", ""),
    income = str_replace_all(income, "\\s+", ""),
    social_payments = str_replace_all(social_payments, "\\s+", ""),
    sum_income_payments = str_replace_all(sum_income_payments, "\\s+", ""),
    per_1_person = str_replace(per_1_person, "\\s+", "")
  )

data_2019_cleaned$income <- as.numeric(data_2019_cleaned$income)
data_2019_cleaned$social_payments <- as.numeric(data_2019_cleaned$social_payments)
data_2019_cleaned$sum_income_payments <- as.numeric(data_2019_cleaned$sum_income_payments)
data_2019_cleaned$per_1_person <- as.numeric(data_2019_cleaned$per_1_person)

#2020
data_2020_cleaned <- data_2020 %>%
  mutate(
    oktmo = str_replace_all(oktmo, "\\s+", ""),
    income = str_replace_all(income, "\\s+", ""),
    social_payments = str_replace_all(social_payments, "\\s+", ""),
    sum_income_payments = str_replace_all(sum_income_payments, "\\s+", ""),
    per_1_person = str_replace(per_1_person, "\\s+", "")
  )

data_2020_cleaned$income <- as.numeric(data_2020_cleaned$income)
data_2020_cleaned$social_payments <- as.numeric(data_2020_cleaned$social_payments)
data_2020_cleaned$sum_income_payments <- as.numeric(data_2020_cleaned$sum_income_payments)
data_2020_cleaned$per_1_person <- as.numeric(data_2020_cleaned$per_1_person)

str(data_2020)
summary(data_2020)
str(data_2020_cleaned)
summary(data_2020_cleaned)


#2021
data_2021_cleaned <- data_2021 %>%
  mutate(
    oktmo = str_replace_all(oktmo, "\\s+", ""),
    income = str_replace_all(income, "\\s+", ""),
    social_payments = str_replace_all(social_payments, "\\s+", ""),
    sum_income_payments = str_replace_all(sum_income_payments, "\\s+", ""),
    per_1_person = str_replace(per_1_person, "\\s+", "")
  )

data_2021_cleaned$income <- as.numeric(data_2021_cleaned$income)
data_2021_cleaned$social_payments <- as.numeric(data_2021_cleaned$social_payments)
data_2021_cleaned$sum_income_payments <- as.numeric(data_2021_cleaned$sum_income_payments)
data_2021_cleaned$per_1_person <- as.numeric(data_2021_cleaned$per_1_person)
str(data_2021)
str(data_2021_cleaned)
summary(data_2021)
summary(data_2021_cleaned)

#2022
data_2022_cleaned <- data_2022 %>%
  mutate(
    oktmo = str_replace_all(oktmo, "\\s+", ""),
    income = str_replace_all(income, "\\s+", ""),
    social_payments = str_replace_all(social_payments, "\\s+", ""),
    sum_income_payments = str_replace_all(sum_income_payments, "\\s+", ""),
    per_1_person = str_replace(per_1_person, "\\s+", "")
  )

data_2022_cleaned$income <- as.numeric(data_2022_cleaned$income)
data_2022_cleaned$social_payments <- as.numeric(data_2022_cleaned$social_payments)
data_2022_cleaned$sum_income_payments <- as.numeric(data_2022_cleaned$sum_income_payments)
data_2022_cleaned$per_1_person <- as.numeric(data_2022_cleaned$per_1_person)
str(data_2022)
str(data_2022_cleaned)
summary(data_2022)
summary(data_2022_cleaned)

#версионный справочник и ОКТМО
#удаляем лишние символы, чтобы привести к единому формату с муниципальными данными
data_oktmo_clean <- data_oktmo %>%
  mutate(oktmo = str_replace_all(oktmo, "\\s+", ""),
         oktmo = gsub("[^0-9]", "", oktmo))

#в муниципальных данных ОКТМО из 8 цифр, в версионном справочнике из 11 цифр
#Последние используются для обозначения населенных пунктов. А так как у нас МО городского и районного уровня, предположим, что отличие тогда будет лишь в дополнительных 000 в конце в версионном справочнике
average_oktmo <- mean(nchar(data_oktmo_clean$oktmo))
average_oktmo

#раз среднее количество символов = 11, то это косвенно подтверждает предположение
#Создаем отдельную колонку с последними 3 символами из октмо
data_oktmo_clean <- data_oktmo_clean %>%
  mutate(zero_3 = str_sub(oktmo, -3, -1))
head(data_oktmo_clean$zero_3)
unique(data_oktmo_clean$zero_3)
print(mean(as.numeric(data_oktmo_clean$zero_3)))

#Гипотеза подтверждается, так что можно прибавить 000 в базе МО либо удалить в версионном справочнике
#Для того, чтобы понять как лучше, загрузим оставшийся файл с геоданными
geo_data <- st_read("/Users/abdulhalikoff/Documents/R/t_dict_municipal_districts_poly.gpkg")

#Видим, что коды территории в geo_data и data_oktmo не связаны с ОКТМО
#обрезаем лишние 000 в data_oktmo
data_oktmo_clean <- data_oktmo_clean %>%
  mutate(oktmo = str_sub(oktmo, 1, -4))

str(data_oktmo_clean$oktmo)
#проверим опять среднее количество символов
average_oktmo_1 <- mean(nchar(data_oktmo_clean$oktmo))
average_oktmo_1

#теперь тоже по 8 символов
#удалим лишнюю созданную колонку
data_oktmo_clean <- data_oktmo_clean %>%
  select(-"zero_3")
names(data_oktmo_clean)

#посмотрим geo_data
names(geo_data)
summary(geo_data)
str(geo_data)
primer_mo <- geo_data %>%
  filter(territory_id == "462")

ggplot(data = primer_mo) +
  geom_sf(aes(geometry = geom), fill = "blue", color = "green") +
  labs(title = paste("Муниципальный район ID:", "462")) +
  theme_minimal()

#объединим данные по годам
data_all <- bind_rows(
  mutate(data_2018_cleaned, year = 2018),
  mutate(data_2019_cleaned, year = 2019),
  mutate(data_2020_cleaned, year = 2020),
  mutate(data_2021_cleaned, year = 2021),
  mutate(data_2022_cleaned, year = 2022)
)

#проверим
str(data_all)
data_all %>%
  count(year)

sum(is.na(data_all))
n_distinct(data_all)
a = sum(data_2018_cleaned$income)
b = sum(data_2019_cleaned$income)
c = sum(data_2020_cleaned$income)
d = sum(data_2021_cleaned$income)
e = sum(data_2022_cleaned$income)

sum(a+b+c+d+e)
sum(data_all$income)

#сохраняем обработанные данные
saveRDS(data_all, "data_2018_2022.rds")
saveRDS(data_oktmo_clean, "data_oktmo.rds")
sf::st_write(geo_data, "geo_data.gpkg", layer = "geo_data_layer")

#приступаем к последней обработке и анализу
data_oktmo <- readRDS("data_oktmo.rds")
data_mo <- readRDS("data_2018_2022.rds")
geo_data <- st_read("/Users/abdulhalikoff/Documents/R/t_dict_municipal_districts_poly.gpkg")


#в data_oktm и geo_data данные по 2024 год, удалим обновления после 2022 года
#year_from - год начала актуальности записи для 
data_oktmo <- data_oktmo %>%
  filter(year_from <= 2022)
levels(as.factor(data_oktmo$year_from))
#[1] "2018" "2019" "2020" "2021" "2022"

geo_data <- geo_data %>%
  filter(year_from <= 2022)
levels(as.factor(geo_data$year_from))
#[1] "2018" "2019" "2020" "2021" "2022"

str(data_oktmo)
str(geo_data)
data_oktmo$territory_id <- as.character(data_oktmo$territory_id)


str(data_mo)
str(data_oktmo)
str(geo_data)

#Объединение data_mo с data_oktmo
data_2 <- data_mo %>%
  inner_join(data_oktmo, by = c("oktmo", "municipal_district_name_short")) %>%
  filter(year >= year_from & year <= year_to)

#вариант 2
data_2 <- data_mo %>%
  inner_join(data_oktmo, by = c("oktmo", "municipal_district_name_short"), relationship = "many-to-many") %>%
  filter(year >= year_from & year <= year_to)

# Шаг 2: Объединение с geo_data по territory_id
data_all <- data_2 %>%
  inner_join(geo_data, by = "territory_id")
names(data_mo)

# Шаг 3: Группировка и создание временных рядов
# Проверка изменений в типах, названиях и кодах
data_series <- data_all %>%
  group_by(territory_id) %>%
  summarise(
    social_payments = list(social_payments),
    income = list(income),
    sum_income_payments = list(sum_income_payments),
    per_1_person = list(per_1_person),
    year = list(year),
    oktmo = paste(unique(oktmo), collapse = ", "),
    municipal_district_type = paste(unique(municipal_district_type), collapse = ", "),
    municipal_district_name_short = paste(unique(municipal_district_name_short), collapse = ", "),
    territory_id = paste(unique(territory_id), collapse = ", "),
    region_code = paste(unique(region_code), collapse = ". "),
    region_name = paste(unique(region_name), collapse = ", ")
  )

#Проанализировать, как изменилась доля социальных выплат в структуре
#доходов населения муниципальных образований в 2022 году
#относительно 2018 года и предложить три типа визуализации.
#Предположить, чем могут объясняться выявленные паттерны

data_2018 <- data_all %>%
  filter(year == 2018)
data_2022 <- data_all %>%
  filter(year == 2022)
str(data_2018)
str(data_2022)

# Рассчитаем долю социальных выплат для каждого года
data_2018 <- data_2018 %>%
  mutate(social_share_2018 = social_payments / sum_income_payments)

data_2022 <- data_2022 %>%
  mutate(social_share_2022 = social_payments / sum_income_payments)

#не то, исправим
data_2018 <- data_2018 %>%
  mutate(social_share_2018 = social_payments / sum_income_payments) %>%
  select(municipal_district_name_short, oktmo, region_code, region_name, territory_id, social_share_2018, year)

# Преобразуем данные для 2022 года
data_2022 <- data_2022 %>%
  mutate(social_share_2022 = social_payments / sum_income_payments) %>%
  select(municipal_district_name_short, oktmo, region_code, region_name, territory_id, social_share_2022, year)

# Объединим данные по столбцам municipal_district_name_short и oktmo
data_1822 <- left_join(data_2018, data_2022, by = c("municipal_district_name_short", "oktmo"))

# Посмотрим на результат
head(data_1822)
data_1822 <- inner_join(data_2018, data_2022, by = c("municipal_district_name_short", "oktmo"))

#должны быть только совпадающие муниципалитеты
head(data_1822)
names(data_1822)

data_1822_2 <- data_1822 %>%
  rename(
    "Муниципалитет" = municipal_district_name_short,
    "Доля выплат 2018, проц." = social_share_2018,
    "Доля выплат 2022, проц." = social_share_2022,
    "Регион" = region_name.x,
    "Код региона" = region_code.x,
    "ОКТМО" = oktmo,
    "territory_id" = territory_id.x
  )
head(data_1822_2)
str(data_1822_2$`Доля выплат 2018, проц.`)

data_1822_2 <- data_1822_2 %>%
  mutate(
    `Доля выплат 2018, проц.` = `Доля выплат 2018, проц.` * 100,
    `Доля выплат 2022, проц.` = `Доля выплат 2022, проц.` * 100
  )

#Изменение доли социальных выплат
data_1822_2 <- data_1822_2 %>%
  mutate(
    Изменение = `Доля выплат 2022, проц.` - `Доля выплат 2018, проц.`  # Разница между долями
  )

data_1822_2$Изменение
summary(data_1822_2$Изменение)

#узнаем десятку муниципалитетов с наибольшим и наименьшим изменением
max10 <- data_1822_2[order(data_1822_2$Изменение, decreasing = TRUE) [1:10], ]
print(max10)
min10 <- data_1822_2[order(data_1822_2$Изменение, decreasing = FALSE) [1:10], ]
print(min10)

#узнаем десятку регионов с наибольшей долей соцвыплат среди доходов населения в 2018
max10_2018 <- data_1822_2[order(data_1822_2$`Доля выплат 2018, проц.`, decreasing = TRUE) [1:10], ]
max10_2018
max10_2018$Муниципалитет
max10_2018$Регион
max10_2022$Регион


min10_2018 <- data_1822_2[order(data_1822_2$`Доля выплат 2018, проц.`, decreasing = FALSE) [1:10], ]
min10_2018
max10_2022 <- data_1822_2[order(data_1822_2$`Доля выплат 2022, проц.`, decreasing = TRUE) [1:10], ]
max10_2022
min10_2018 <- data_1822_2[order(data_1822_2$`Доля выплат 2018, проц.`, decreasing = FALSE) [1:10], ]
min10_2018

#Средний уровень долей соцвыплат для 2018 и 2022 годов
mean_2018 <- mean(data_1822_2$`Доля выплат 2018, проц.`, na.rm = TRUE)
mean_2022 <- mean(data_1822_2$`Доля выплат 2022, проц.`, na.rm = TRUE)

#Медианный уровень долей соцвыплат для 2018 и 2022
median_2018 <- median(data_1822_2$`Доля выплат 2018, проц.`, na.rm = TRUE)
median_2022 <- median(data_1822_2$`Доля выплат 2022, проц.`, na.rm = TRUE)

#вывод
mean_2018
mean_2022
median_2018
median_2022

#визуализация
#Точки изменений по муниципалитетам
ggplot(data_1822_2, aes(x = Муниципалитет, y = Изменение)) +
  geom_point(color = "black") +
  labs(title = "Изменение доли социальных выплат по муниципалитетам (2022-2018)", x = "Муниципалитет", y = "Изменение доли")
#такое себе, можно лучше

#построим гистограмму
ggplot(data_1822_2, aes(x = Изменение)) +
  geom_histogram(binwidth = 0.05, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Распределение изменений доли социальных выплат (2018-2022)",
       x = "Изменение доли социальных выплат", 
       y = "Количество муниципалитетов") +
  theme_minimal()
#видим, в целом, нормальное распределение

ggplot(data_1822_2, aes(x = region_code.y, y = Изменение, color = factor(`Код региона`))) +
  geom_point(alpha = 0.7) +
  labs(title = "Изменения доли социальных выплат по регионам",
       x = "Код региона", y = "Изменение доли социальных выплат") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#ящики с усами
ggplot(data_1822_2, aes(x = factor(`Код региона`), y = Изменение)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Изменения доли социальных выплат по регионам",
       x = "Код региона", y = "Изменение доли социальных выплат") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#попробуем в регрессию
model <- lm(Изменение ~ `Доля выплат 2018, проц.`, data = data_1822_2)
summary(model)
#гомоскедастичность
plot(model$residuals)
#проверка на нормальность ошибок
hist(model$residuals)

#в общем, модель статистически значима, имеется отрицательная связь (которая нам уже известна)
#с трендовой линией
ggplot(data_1822_2, aes(x = `Доля выплат 2018, проц.`, y = Изменение)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Изменения доли социальных выплат относительно доли 2018 года",
       x = "Доля выплат 2018 года", y = "Изменение доли социальных выплат") +
  theme_minimal()

#карта
#не работает
str(geo_data$geom)

# Объединение данных с геометрией по идентификатору territory_id
data_1822_geo <- data_1822_2 %>%
  left_join(geo_data, by = "territory_id")
str(data_1822_geo)

#возьмём уровень региона
#фильтрация данных для Псковской области
data_pskov <- data_1822_geo %>%
  filter(Регион == "Псковская область")

ggplot(data_pskov) +
  geom_sf(aes(geometry = geom, fill = Изменение), color = "white") +
  scale_fill_gradient(low = "blue", high = "red", 
                      name = "Изменение доли выплат") +
  labs(title = "Изменение доли социальных выплат в Псковской области (2018-2022)",
       caption = "Данные: data_1822_geo") +
  theme_minimal()

#Ростовская область
data_rostov <- data_1822_geo %>%
  filter(Регион == "Ростовская область")

ggplot(data_rostov) +
  geom_sf(aes(geometry = geom, fill = Изменение), color = "white") +
  scale_fill_gradient(low = "blue", high = "red", 
                      name = "Изменение доли выплат") +
  labs(title = "Изменение доли социальных выплат в Ростовской области (2018-2022)",
       caption = "Данные: data_1822_geo")

#Татарстан
data_tatarstan <- data_1822_geo %>%
  filter(Регион == "Республика Татарстан")

ggplot(data_tatarstan) +
  geom_sf(aes(geometry = geom, fill = Изменение), color = "white") +
  scale_fill_gradient(low = "blue", high = "red", 
                      name = "Изменение доли выплат") +
  labs(title = "Изменение доли социальных выплат в Татарстане (2018-2022)",
       caption = "Данные: data_1822_geo")

#боксплот по 2018 и 2022 г
str(data_1822_geo)

#создаем длинный формате для удобства построения графика
data_box <- data_1822_geo %>%
  tidyr::pivot_longer(
    cols = c(`Доля выплат 2018, проц.`, `Доля выплат 2022, проц.`),
    names_to = "Год",
    values_to = "Доля выплат"
  )

# Построение боксплота
ggplot(data_box, aes(x = Год, y = `Доля выплат`)) +
  geom_boxplot() +
  labs(
    title = "Боксплот доли социальных выплат за 2018 и 2022 годы",
    x = "Год",
    y = "Доля выплат (%)"
  ) +
  theme_minimal()

#выбросы
#определение выбросов для 2018 года
outliers_2018 <- boxplot.stats(data_1822_geo$`Доля выплат 2018, проц.`)$out

#определение выбросов для 2022 года
outliers_2022 <- boxplot.stats(data_1822_geo$`Доля выплат 2022, проц.`)$out
outliers_2018
outliers_2022

#попробуем проверить совпадения среди выбросов
#получение муниципалитетов с выбросами в 2018 году
municipalities_2018 <- data_1822_geo %>%
  filter(`Доля выплат 2018, проц.` %in% outliers_2018) %>%
  pull(Муниципалитет)

#получение муниципалитетов с выбросами в 2022 году
municipalities_2022 <- data_1822_geo %>%
  filter(`Доля выплат 2022, проц.` %in% outliers_2022) %>%
  pull(Муниципалитет)

#поиск совпадающих муниципалитетов среди выбросов
common_mo <- intersect(municipalities_2018, municipalities_2022)
# Вывод совпадающих муниципалитетов
common_mo

summary(data_1822_geo$`Доля выплат 2018, проц.`)
summary(data_1822_geo$`Доля выплат 2022, проц.`)
str(data_1822_geo$Регион)

data_nc <- data_1822_geo %>%
  filter(Регион %in% c("Республика Адыгея", "Республика Дагестан", "Республика Ингушетия", 
                       "Кабардино-Балкарская Республика", "Карачаево-Черкесская Республика", 
                       "Республика Северная Осетия — Алания", "Чеченская Республика"))
str(data_nc)

#Средний уровень долей соцвыплат для 2018 и 2022 годов
mean_2018_nc <- mean(data_nc$`Доля выплат 2018, проц.`, na.rm = TRUE)
mean_2022_nc <- mean(data_nc$`Доля выплат 2022, проц.`, na.rm = TRUE)

#Медианный уровень долей соцвыплат для 2018 и 2022
median_2018_nc <- median(data_nc$`Доля выплат 2018, проц.`, na.rm = TRUE)
median_2022_nc <- median(data_nc$`Доля выплат 2022, проц.`, na.rm = TRUE)
mean_2018_nc
mean_2022_nc
median_2018_nc
median_2022_nc
max10_2018
max10_2022
#визуализация
library(ggplot2)

#длинный формат для удобства построения
data_long_nc <- data_nc %>%
  tidyr::pivot_longer(
    cols = c(`Доля выплат 2018, проц.`, `Доля выплат 2022, проц.`),
    names_to = "Год",
    values_to = "Доля выплат"
  )

#Боксплот
ggplot(data_long_nc, aes(x = Год, y = `Доля выплат`)) +
  geom_boxplot() +
  labs(
    title = "Боксплот доли социальных выплат за 2018 и 2022 годы в республиках Северного Кавказа",
    x = "Год",
    y = "Доля выплат (%)"
  ) +
  theme_minimal()

#точечный график СК
ggplot(data_nc, aes(x = `Доля выплат 2018, проц.`, y = `Доля выплат 2022, проц.`)) +
  geom_point(alpha = 0.6, color = "blue") +
  labs(
    title = "Точечный график доли социальных выплат в 2018 и 2022 годах",
    x = "Доля выплат в 2018 году (%)",
    y = "Доля выплат в 2022 году (%)"
  ) +
  theme_minimal()

#точечный график РФ
ggplot(data_1822_geo, aes(x = `Доля выплат 2018, проц.`, y = `Доля выплат 2022, проц.`)) +
  geom_point(alpha = 0.6, color = "blue") +
  labs(
    title = "Точечный график доли социальных выплат в 2018 и 2022 годах",
    x = "Доля выплат в 2018 году (%)",
    y = "Доля выплат в 2022 году (%)"
  ) +
  theme_minimal()

#гистограма
data_long_nc$Год <- factor(data_long_nc$Год, levels = c("Доля выплат 2018, проц.", "Доля выплат 2022, проц."))

ggplot(data_long_nc, aes(x = `Доля выплат`, fill = Год)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
  labs(
    title = "Гистограмма распределения доли социальных выплат",
    x = "Доля выплат (%)",
    y = "Количество муниципалитетов"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Доля выплат 2018, проц." = "red", "Доля выплат 2022, проц." = "blue"))