#_________________________________________SIATA_________________________________________________
#Libraries 
library(tidyverse)
library(openair)
library(dplyr)
library(ggplot2)
library(lubridate)
library(writexl)
library(patchwork)


# Definir los períodos

# Se definen dos diferentes paraq ue se ajusten al formato de entrada
# de los datos
permuest <- as.Date(c("2021-03-01", "2022-01-24", "2022-10-27", "2023-03-24", 
                      "2024-01-31"))


permuest2 <- as.POSIXct(c("2021-03-01 00:00:00", "2022-01-24 00:00:00", 
                          "2022-10-27 00:00:00", "2023-03-24 00:00:00", 
                          "2024-01-31 00:00:00"))



# S1pm10gd$fecha <- as.POSIXct(S1pm10gd$fecha)
# S1pm25gd$fecha <- as.POSIXct(S1pm25gd$fecha)

periodos <- data.frame(
  inicio = permuest[-length(permuest)],
  fin = permuest[-1],
  dias   = as.numeric(permuest[-1] - permuest[-length(permuest)]),
  periodo = factor(
    seq_len(length(permuest) - 1),
    labels = paste0("Period ", seq_len(length(permuest) - 1))
  )
)




colores_periodos <- c(
  "Period 1" = "#B0C4DE",
  "Period 2" = "#98FB98",
  "Period 3" = "#FFD700",
  "Period 4" = "#F4A460"
)



# STATION 1
S1pm10_data <- read.csv("siata_data/S1pm10.csv", sep=";")
S1pm10_data$fecha<-dmy(S1pm10_data$fecha)
S1pm10gd <- S1pm10_data %>% 
  group_by(fecha=floor_date(fecha, '1 day')) %>% ## Agrupamiento temporal
  summarize(pm10_median = median(pm10), 
            pm10_mean=mean(pm10),
            masa_median=median(pm10)*24, #24 m3/dia es el caudal diario de medicion
            masa_mean=mean(pm10)*24, N=n())


ggplot(S1pm10gd, aes(x = fecha, y = pm10_median)) + 
  geom_line(col="#473C8B")+geom_smooth(se = FALSE,col="#473C8B")+ 
  labs(x = "Time", y = "PM10 (μg/m³)")

S1pm2.5_data <- read.csv("siata_data/S1pm25.csv")
S1pm2.5_data$fecha<-dmy(S1pm2.5_data$fecha)
S1pm2.5gd <- S1pm2.5_data %>% 
  group_by(fecha=floor_date(fecha, '1 day')) %>% ## Agrupamiento temporal
  summarize(pm2.5_median = median(pm2.5, na.rm = T), 
            pm2.5_mean = mean(pm2.5, na.rm = T), 
            masa_median=median(pm2.5)*24, #24 m3/dia es el caudal diario de medicion
            masa_mean=mean(pm2.5)*24, N=n())

ggplot(S1pm2.5gd, aes(x = fecha, y = pm2.5_median)) + 
  geom_line(col="#7D26CD")+geom_smooth(se = FALSE,col="#7D26CD")+ 
  labs(x = "Time", y = "PM2.5 (μg/m³)")

# Gráfico PM10
S1pm10 <- ggplot(S1pm10gd, aes(x = fecha, y = pm10_median)) +
  geom_rect(
    data = periodos,
    aes(
      xmin = inicio, xmax = fin,
      ymin = -Inf, ymax = Inf,
      fill = periodo
    ),
    inherit.aes = FALSE,
    alpha = 0.20
  ) +
  geom_line(color = "#473C8B") +
  geom_smooth(color = "#473C8B", se = TRUE) +
  scale_fill_manual(values = colores_periodos, name = "Periods") +
  labs(
    x = "Time",
    y = expression(PM[10] ~ "(" * mu * "g/m"^3 * ")")
  ) +
  theme_minimal()

# Gráfico PM2.5
S1pm2.5 <- ggplot(S1pm2.5gd, aes(x = fecha, y = pm2.5_median)) +
  geom_rect(
    data = periodos,
    aes(
      xmin = inicio, xmax = fin,
      ymin = -Inf, ymax = Inf,
      fill = periodo
    ),
    inherit.aes = FALSE,
    alpha = 0.20
  ) +
  geom_line(color = "#7D26CD") +
  geom_smooth(color = "#7D26CD", se = FALSE) +
  scale_fill_manual(values = colores_periodos, name = "Periods") +
  labs(
    x = "Time",
    y = expression(PM[2.5] ~ "(" * mu * "g/m"^3 * ")")
  ) +
  theme_minimal()


# Unir los paneles y compartir el eje temporal
(S1pm10 / S1pm2.5) +
  plot_layout(guides = "collect") &
  theme(legend.position = "top", plot.title = element_text(size = 12))&
  plot_annotation(title = "SATION 1",
                  subtitle = "Median of PM group by Day")

#agrupación de periodos
S1pm10per <- S1pm10_data %>%
  group_by(periodos = cut(fecha, breaks = permuest, include.lowest = TRUE)
  ) %>% summarise(pm10 = sum(pm10, na.rm = TRUE), N=n())
ggplot(S1pm10per, aes(x = periodos, y = pm10)) + geom_line(col="#473C8B")+ #no logro una linea que una puntos
  geom_point(col="#7D26CD")+ theme_minimal() + labs(x = "Time", y = "PM10 (μg/m³)")

S1pm2.5per <- S1pm2.5_data %>%
  group_by(periodos = cut(fecha, breaks = permuest, include.lowest = TRUE)
  ) %>% summarise(pm2.5 = sum(pm2.5, na.rm = TRUE))
ggplot(S1pm2.5per, aes(x = periodos, y = pm2.5)) + geom_line(col="#473C8B")+ #no logro una linea que una puntos
  geom_point(col="#7D26CD")+ theme_minimal() + labs(x = "Time", y = "PM2.5 (μg/m³)")


#metricas

S1pm10_metricas <- S1pm10gd %>%
  group_by(
    periodos = cut(fecha, breaks = permuest, include.lowest = TRUE)
  ) %>%
  summarise(
    masa_pm10_median = sum(masa_median, na.rm = TRUE) / 1000000,
    #masa_pm10_mean   = sum(masa_mean, na.rm = TRUE) / 1000000,
    #Q75_pm10         = quantile(pm10_median, 0.75, na.rm = TRUE),
    dias_supera_Q75  = sum(
      pm10_median > quantile(S1pm10_data$pm10, 0.75, na.rm = TRUE),
      na.rm = TRUE
    ),
    dias_supera_Q90  = sum(
      pm10_median > quantile(S1pm10_data$pm10, 0.90, na.rm = TRUE),
      na.rm = TRUE
    ),
    dias_del_periodo = n(),
    .groups = "drop"
  )



S1pm2.5_metricas <- S1pm2.5gd %>%
  group_by(
    periodos = cut(fecha, breaks = permuest, include.lowest = TRUE)
  ) %>%
  summarise(
    masa_pm2.5_median = sum(masa_median, na.rm = TRUE) / 1000000,
    #masa_pm10_mean   = sum(masa_mean, na.rm = TRUE) / 1000000,
    #Q75_pm10         = quantile(pm10_median, 0.75, na.rm = TRUE),
    dias_supera_Q75  = sum(
      pm2.5_median > quantile(S1pm2.5_data$pm2.5, 0.75, na.rm = TRUE),
      na.rm = TRUE
    ),
    dias_supera_Q90  = sum(
      pm2.5_median > quantile(S1pm2.5_data$pm2.5, 0.90, na.rm = TRUE),
      na.rm = TRUE
    ),
    dias_del_periodo = n(),
    .groups = "drop"
  )


#STATION 2

#Particulate matter
S2pm10_data<-read.csv("siata_data/S2pm10.csv", sep = ";")
S2pm10_data$fecha_hora<-dmy_hm(S2pm10_data$fecha_hora)
S2pm10gd <- S2pm10_data %>% 
  group_by(fecha_hora=floor_date(fecha_hora, '1 day')) %>% 
  summarize(pm10_median = median(pm10),
            pm10_mean = mean(pm10), 
            masa_median=median(pm10)*24, #24 m3/dia es el caudal diario de medicion
            masa_mean=mean(pm10)*24, N=n())

ggplot(S2pm10gd, aes(x = fecha_hora, y = pm10_median)) + 
  geom_line(col="#473C8B")+geom_smooth(se = FALSE,col="#473C8B")+ 
  labs(x = "Time", y = "PM10 (μg/m³)")



S2pm2.5fix_data<-read.csv("siata_data/S2pm25fix.csv", sep = ";")
S2pm2.5fix_data$fecha<-dmy(S2pm2.5fix_data$fecha)
S2pm2.5fixgd <- S2pm2.5fix_data %>% 
  group_by(fecha=floor_date(fecha, '1 day')) %>% 
  summarize(pm2.5fix_median = median(pm2.5fix),
            pm2.5fix_mean = mean(pm2.5fix), 
            masa_median=median(pm2.5fix)*24, #24 m3/dia es el caudal diario de medicion
            masa_mean=mean(pm2.5fix)*24, N=n())

ggplot(S2pm2.5fixgd, aes(x = fecha, y = pm2.5fix_median))+ 
  geom_line(col="#7D26CD")+geom_smooth(se = FALSE,col="#7D26CD")+ 
  labs(x = "Time", y = "PM2.5 (μg/m³)")
  
# Gráfico PM10
S2pm10 <- ggplot(S2pm10gd, aes(x = fecha_hora, y = pm10_median)) +
  geom_rect(
    data = periodos,
    aes(
      xmin = inicio, xmax = fin,
      ymin = -Inf, ymax = Inf,
      fill = periodo
    ),
    inherit.aes = FALSE,
    alpha = 0.20
  ) +
  geom_line(color = "#473C8B") +
  geom_smooth(color = "#473C8B", se = TRUE) +
  scale_fill_manual(values = colores_periodos, name = "Periods") +
  labs(
    x = "Time",
    y = expression(PM[10] ~ "(" * mu * "g/m"^3 * ")")
  ) +
  theme_minimal()

# Gráfico PM2.5
S2pm2.5 <- ggplot(S2pm2.5fixgd, aes(x = fecha, y = pm2.5fix_median)) +
  geom_rect(
    data = periodos,
    aes(
      xmin = inicio, xmax = fin,
      ymin = -Inf, ymax = Inf,
      fill = periodo
    ),
    inherit.aes = FALSE,
    alpha = 0.20
  ) +
  geom_line(color = "#7D26CD") +
  geom_smooth(color = "#7D26CD", se = FALSE) +
  scale_fill_manual(values = colores_periodos, name = "Periods") +
  labs(
    x = "Time",
    y = expression(PM[2.5] ~ "(" * mu * "g/m"^3 * ")")
  ) +
  theme_minimal()


# Unir los paneles y compartir el eje temporal
(S2pm10 / S2pm2.5) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom", plot.title = element_text(size = 12))&
  plot_annotation(title = "STATION 2",
                  subtitle = "Median of PM group by Day")


#agrupación de periodos

S2pm10per <- S2pm10_data %>%
  group_by(periodos = cut(fecha_hora, breaks = permuest2, include.lowest = TRUE)
  ) %>% summarise(pm10 = sum(pm10, na.rm = TRUE))
ggplot(S2pm10per, aes(x = periodos, y = pm10)) + geom_line(col="#473C8B")+ #no logro una linea que una puntos
  geom_point(col="#7D26CD")+ theme_minimal() + labs(x = "Time", y = "PM10 (μg/m³)")


S2pm2.5fixper <- S2pm2.5fix_data %>%
  group_by(periodos = cut(fecha, breaks = permuest, include.lowest = TRUE)
  ) %>% summarise(pm2.5 = sum(pm2.5fix, na.rm = TRUE))

ggplot(S2pm2.5fixper, aes(x = periodos, y = pm2.5)) + geom_line(col="#473C8B")+ #no logro una linea que una puntos
  geom_point(col="#7D26CD")+ theme_minimal() + labs(x = "Time", y = "PM10 (μg/m³)")


#metricas
S2pm10_metricas <- S2pm10gd %>%
  group_by(
    periodos = cut(fecha_hora, breaks = permuest2, include.lowest = TRUE)
  ) %>%
  summarise(
    masa_pm10_median = sum(masa_median, na.rm = TRUE) / 1000000,
    #masa_pm10_mean   = sum(masa_mean, na.rm = TRUE) / 1000000,
    #Q75_pm10         = quantile(pm10_median, 0.75, na.rm = TRUE),
    dias_supera_Q75  = sum(
      pm10_median > quantile(S2pm10_data$pm10, 0.75, na.rm = TRUE),
      na.rm = TRUE
    ), #Q90_pm10 = quantile(pm10_median, 0.90, na.rm = TRUE),
    dias_supera_Q90  = sum(
      pm10_median > quantile(S2pm10_data$pm10, 0.90, na.rm = TRUE),
      na.rm = TRUE
    ),
    dias_del_periodo = n(),
    .groups = "drop"
  )


S2pm2.5_metricas <- S2pm2.5fixgd %>%
  group_by(
    periodos = cut(fecha, breaks = permuest, include.lowest = TRUE)
  ) %>%
  summarise(
    masa_pm2.5_median = sum(masa_median, na.rm = TRUE) / 1000000,
    #masa_pm10_mean   = sum(masa_mean, na.rm = TRUE) / 1000000,
    #Q75_pm10         = quantile(pm10_median, 0.75, na.rm = TRUE),
    dias_supera_Q75  = sum(
      pm2.5fix_median > quantile(S2pm2.5fix_data$pm2.5fix, 0.75, na.rm = TRUE),
      na.rm = TRUE
    ), #Q90_pm10 = quantile(pm10_median, 0.90, na.rm = TRUE),
    dias_supera_Q90  = sum(
      pm2.5fix_median > quantile(S2pm2.5fix_data$pm2.5fix, 0.90, na.rm = TRUE),
      na.rm = TRUE
    ),
    dias_del_periodo = n(),
    .groups = "drop"
  )



#STATION 3


#Particular matter
S3pm2.5fix_data <- read.csv("siata_data/S3pm25fix.csv", sep=";")
S3pm2.5fix_data$fecha<-dmy_hm(S3pm2.5fix_data$fecha)
S3pm2.5fix_data$pm2.5fix<-as.integer(S3pm2.5fix_data$pm2.5fix)
S3pm2.5fix_sNA<-S3pm2.5fix_data %>% drop_na()
#length(S3pm2.5fix_data$fecha)-length(S3pm2.5fix_sNA$fecha)

S3pm2.5fixgd <- S3pm2.5fix_sNA %>% 
  group_by(fecha=floor_date(fecha, '1 day')) %>% 
  summarize(pm2.5fix_median = median(pm2.5fix),
            pm2.5fix_mean = mean(pm2.5fix), 
            masa_median=median(pm2.5fix)*24, #24 m3/dia es el caudal diario de medicion
            masa_mean=mean(pm2.5fix)*24, N=n())

ggplot(S3pm2.5fixgd, aes(x = fecha, y = pm2.5fix_median)) + 
  geom_line(col="#7D26CD")+geom_smooth(se = FALSE,col="#7D26CD")+ 
  labs(x = "Time", y = "PM2.5 (μg/m³)")


#agrupación de periodos
S3pm2.5fixper <- S3pm2.5fix_data %>%
  group_by(periodos = cut(fecha, breaks = permuest2, include.lowest = TRUE)
  ) %>% summarise(pm2.5 = sum(pm2.5fix, na.rm = TRUE))

ggplot(S3pm2.5fixper, aes(x = periodos, y = pm2.5)) + geom_line(col="#473C8B")+
  geom_point(col="#7D26CD")+ theme_minimal() + labs(x = "Time", y = "PM10 (μg/m³)")

S3pm2.5_metricas <- S3pm2.5fixgd %>%
  group_by(
    periodos = cut(fecha, breaks = permuest2, include.lowest = TRUE)
  ) %>%
  summarise(
    masa_pm2.5_median = sum(masa_median, na.rm = TRUE) / 1000000,
    #masa_pm10_mean   = sum(masa_mean, na.rm = TRUE) / 1000000,
    #Q75_pm10         = quantile(pm10_median, 0.75, na.rm = TRUE),
    dias_supera_Q75  = sum(
      pm2.5fix_median > quantile(S3pm2.5fix_data$pm2.5fix, 0.75, na.rm = TRUE),
      na.rm = TRUE
    ), #Q90_pm10 = quantile(pm10_median, 0.90, na.rm = TRUE),
    dias_supera_Q90  = sum(
      pm2.5fix_median > quantile(S3pm2.5fix_data$pm2.5fix, 0.90, na.rm = TRUE),
      na.rm = TRUE
    ),
    dias_del_periodo = n(),
    .groups = "drop"
  )


# Resumen

station<-as.factor(
  c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,3,3,3,3))
fraction<-as.factor(
  c(10,10,10,10,2.5,2.5,2.5,2.5,10,10,10,10,2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5))
period<-as.factor(
  c(1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4))

pm <- rbind(
  S1pm10_metricas %>%
    select(2:4) %>%
    rename(mass = masa_pm10_median,
           days_above_Q75 = dias_supera_Q75,
           days_above_Q90 = dias_supera_Q90),
  
  S1pm2.5_metricas %>%
    select(2:4) %>%
    rename(mass = masa_pm2.5_median, 
           days_above_Q75 = dias_supera_Q75,
           days_above_Q90 = dias_supera_Q90),
  
  S2pm10_metricas %>%
    select(2:4) %>%
    rename(mass = masa_pm10_median, 
           days_above_Q75 = dias_supera_Q75,
           days_above_Q90 = dias_supera_Q90),
  
  S2pm2.5_metricas %>%
    select(2:4) %>%
    rename(mass = masa_pm2.5_median, 
           days_above_Q75 = dias_supera_Q75,
           days_above_Q90 = dias_supera_Q90),
  
  S3pm2.5_metricas %>%
    select(2:4) %>%
    rename(mass = masa_pm2.5_median, 
           days_above_Q75 = dias_supera_Q75,
           days_above_Q90 = dias_supera_Q90))

pm_siata <- data.frame(
  station,
  fraction,
  period,
  pm)
