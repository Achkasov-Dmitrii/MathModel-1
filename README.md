# MathModel-1
Ecology modeling
#Ачкасов – для региона 27 рассчитайте урожайность пшеницы  
#в 2011 году, взяв для рассчета средние суммы активных температур 
#за предыдущие 9 лет, с 10 ближайших метеостанций на расстоянии до 150 км 
setwd("F:\\model")
getwd()
# Подключим библиотеки:
library(tidyverse)
library(rnoaa)
library(lubridate)

#Создадим векторы с данными для расчета: Р°:
af = c(0.00,0.00,0.00,32.11, 26.31,25.64,23.20,18.73,16.30,13.83,0.00,0.00)
bf = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03,8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
df = c(0.00,0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)
Kf = 300 # Коэффициент использования ФАР
Qj = 1600 # калорийность урожая культуры
Lj = 2.2 # сумма частей основной и побочной продукции
Ej = 25 # стандартная влажность культуры 

#station_data = ghcnd_stations() 
# write.csv(station_data, "station_data.csv") 
station_data = read.csv("station_data.csv")
#После получения всписка всех станций, получите список станций ближайших к столице вашего региона,создав таблицу с именем региона и координатами его столицы
Khabarovsk = data.frame(id = "KHABAROVSK", latitude = 48.29, longitude = 135.04)
Khabarovsk
#найдем станции, соответствующие критериям
Khabarovsk_around = meteo_nearby_stations(lat_lon_df = Khabarovsk, station_data = station_data,
                                             limit = 10, var = c("PRCP", "TAVG"),
                                             year_min = 2002, year_max = 2011)
Khabarovsk_around
# Определим список станций
Khabarovsk_id = Khabarovsk_around[["KHABAROVSK"]][["id"]]

summary(Khabarovsk_id)
str(Khabarovsk_id)

# попробуем для 2 метеостанции
# Khabarovsk_id [3]
# 
# dataAA = meteo_tidy_ghcnd(stationid = Khabarovsk_id [3],
# var="TAVG",
# date_min="2002-01-01",
# date_max="2011-12-31")
# dataAA

#создадим таблицу 
all_data = tibble()
for (i in 1:length(Khabarovsk_around$KHABAROVSK[,1]))
  
{
  
  print(i)
  print(Khabarovsk_id)
  # Загрузим данные для станции:
  
  data = meteo_tidy_ghcnd(stationid = Khabarovsk_id[i],
                          var="TAVG",
                          date_min="2002-01-01",
                          date_max="2011-12-31")
  print(data)
  #объединим данные в таблице
  all_data = bind_rows(
    all_data, data %>%
      #добавим колонки для группировки по году и месяцу
      mutate(year = year(date), month = month(date)) %>%
      group_by(month, year) %>%
      mutate(tavg=tavg/10)%>%
      filter(tavg>5)%>%
      #найдем суммарную среднюю активную температуру по месяцу за каждый год для станции
      summarise (sum = sum(tavg))
  )
}

write.csv(all_data, "all_Khabarovsk_data.csv")
clean_data = tibble()
# Изменения в таблице сохранятся в векторе clean_data.
clean_data = all_data %>%
  
  # Добавим колонку month для группировки данных:
  group_by(month) %>%
  
  # Найдем месячный d и cумму активных тмператур для каждой станции:
  summarise(s = mean(sum, na.rm = TRUE)) %>%
  
  #добавим данные из таблицы с показателем d
  # Добавим колонки для расчета:
  mutate(a = af[3:11], b = bf[3:11], d = df[3:11]) %>%
  
  # Рассчитаем урожайность для каждого месяца:
  mutate (fert = ((a + b * 1.0 * s) * d * Kf) / (Qj * Lj * (100-Ej)) )
#Согласно расчету, урожайность пшеницы в период с 2002 по 2011 год в Хабаровской области составила (ц/га):
Yield = sum(clean_data$fert)
Yield
