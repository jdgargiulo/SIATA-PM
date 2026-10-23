library(worldmet)
library(tidyverse)
library(openair)

meteo_horaria <- read_csv(
  "siata_data/JMC_medellin_horario_2021a2024.csv",
  col_types = cols(
    date = col_character()
  ),
  na = c("NA", "")
) %>%
  mutate(
    date = ymd_hms(
      if_else(
        nchar(date) == 10,
        paste(date, "00:00:00"),
        date
      ),
      tz = "UTC"
    )
  ) %>% 
  select(date, wd, ws, air_temp, rh, precip_24)

meteo_horaria_OH <- read_csv(
  "siata_data/OH_medellin_horario_2021a2024.csv",
  col_types = cols(
    date = col_character()
  ),
  na = c("NA", "")
) %>%
  mutate(
    date = ymd_hms(
      if_else(
        nchar(date) == 10,
        paste(date, "00:00:00"),
        date
      ),
      tz = "UTC"
    )
  ) %>% 
  select(date, wd, ws, air_temp, rh, precip_24)



ggplot(meteo_horaria_OH, aes(x = date, y = air_temp)) + 
  geom_smooth(se = T,col="#473C8B")+ #Aeropuerto OH
  geom_smooth(data = meteo_horaria, 
              aes(x = date, y = air_temp), se=T)+#Cordova
  labs(x = "Time", y = "Temp (°C)")


ggplot(meteo_horaria_OH, aes(x = date, y = rh)) + 
  geom_smooth(se = T,col="#473C8B")+ 
  geom_smooth(data = meteo_horaria, 
              aes(x = date, y = rh), se=T)+#cordova
  labs(x = "Time", y = "RH (%)")


meteogd_OH<-meteo_horaria_OH %>% filter(date>ymd("2021-03-01")& date<ymd("2024-01-31")) %>%
  group_by(periodos = cut(date, breaks = permuest2, include.lowest = TRUE)) %>% 
  summarise(temp_mean=mean(air_temp, na.rm=T),
            temp_Q1 = quantile(air_temp, 0.25, na.rm=T),
            temp_Q3 = quantile(air_temp, 0.75, na.rm=T),
            rh_mean=mean(rh, na.rm=T), 
            Horas_totales=n())

meteogd
meteogd_OH # Resumen clima por período, prácticamente constante, no es de interes

meteo_horaria %>% filter(date>ymd("2021-03-01")& date<ymd("2024-01-31")) %>% 
  group_by(periodos = cut(date, breaks = permuest2, include.lowest = TRUE)) %>%
  ggplot(aes(x = periodos, y = air_temp)) +
  geom_violin(width=0.8)+
  geom_boxplot(width=0.2,outlier.colour = "hotpink") +
  #geom_jitter(position = position_jitter(width = 0.1, height = 0), alpha = 1/2) +
  labs(title = "Temperatura por períodos de muestreo",
       subtitle = "Datos de Estación Meteorológica, Aeropuerto Cordova, Antioquia",
       caption = "Fuente: SIATA.", 
       x = "Período", y = "Temperatura (°C)") 





## Vientos
sum(is.na(meteo_horaria_OH$ws))/length(meteo_horaria_OH$ws)*100
sum(is.na(meteo_horaria_OH$wd))/length(meteo_horaria_OH$wd)*100


windRose(meteo_horaria_OH, s)
per1<-meteo_horaria_OH %>% filter(date>ymd("2021-03-01")& date<ymd("2022-01-24"))
per2<-meteo_horaria_OH %>% filter(date>ymd("2022-01-25")& date<ymd("2022-10-27"))
per3<-meteo_horaria_OH %>% filter(date>ymd("2022-10-28")& date<ymd("2023-03-23"))
per4<-meteo_horaria_OH %>% filter(date>ymd("2023-03-24")& date<ymd("2024-01-31"))


