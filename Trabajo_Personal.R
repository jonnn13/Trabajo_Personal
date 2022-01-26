install.packages("nycflights13")
library(nycflights13)
library(lubridate) #??????
library(tidyverse)#????????????
vuelos= nycflights13::flights
vuelos_buenos= vuelos[complete.cases(vuelos),]
?flights
# Observamos que los datos de la arr_delay estan en minutos 

retrasos= vuelos[which(vuelos$arr_delay> 60), ]
a=dim(retrasos)
a[1]
print(paste0("Hay ", a[1], " vuelos que llegaron con retraso"))

vSF= vuelos[vuelos$dest== "SFO" | vuelos$dest== "OAK",]
a=dim(vSF)
a[1]
print(paste0("Hay ", a[1], " vuelos que volaron hacia San Francisco"))

vOp= vuelos[vuelos$carrier == "UA" | vuelos$carrier == "AA",]
a=dim(vOp)
a[1]
print(paste0("Hay ", a[1], " vuelos que volaron operados por United American (UA) o por American Airlines (AA)"))


vPrim= vuelos[vuelos$month=="4" | vuelos$month=="5" | vuelos$month=="6",]
a=dim(vPrim)
a[1]
print(paste0("Hay ", a[1], " vuelos que volaron en los meses de primavera"))


retrasados2= vuelos[which(vuelos$arr_delay> 60 & vuelos$dep_delay< 60), ]
a=dim(retrasados2)
a[1]
print(paste0("Hay ", a[1], " vuelos que llegaron con más de una hora de retraso y salieron con menos de una hora de retraso"))



retrasados3= vuelos[which(vuelos$arr_delay< 30 & vuelos$dep_delay> 60), ]
a=dim(retrasados3)
a[1]
print(paste0("Hay ", a[1], " vuelos que llegaron con menos de 30 minutos de retraso y salieron con más de una hora de retraso"))



vnocturnos= vuelos[which(vuelos$hour>=0 & vuelos$hour<=7 ),]
a=dim(vnocturnos)
a[1]
print(paste0("Hay ", a[1], " vuelos nocturnos"))

vNA= vuelos[is.na(vuelos$dep_time),]
a=dim(vNA)
a[1]
print(paste0("Hay ", a[1], " vuelos con valores desconocidos"))


#ejercicio 9
apply(X=is.na(vuelos), MARGIN= 2, FUN=sum)
print("Las variables que tienen valores desconocidos son: dep_time, dep_delay, arr_time, arr_delay, tailnum y air_time")


vordenados= vuelos[order(vuelos$dep_delay, na.last = F),]
tail(vordenados)

v_adelantados=vuelos[order(vuelos$dep_delay),]
head(v_adelantados)

#ejercicio 11
vuelos$velocidad=(vuelos$distance/vuelos$air_time)
v_rap=vuelos[order(vuelos$velocidad, na.last = F),]
# Estos son los vuelos más rápidos basandonos en la velocidad
tail(v_rap)

#Ejercicio 12
v_trayecto=vuelos_buenos[order(vuelos_buenos$distance, decreasing = T),]
head(v_trayecto)

# E13
tail(v_trayecto)
##### CAMBIARRR ESTA MALLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL




# E14
vuelos$dep_time_min= (vuelos$dep_time %/%100*60 + vuelos$dep_time %%100)
vuelos$sched_dep_time_min= (vuelos$sched_dep_time %/%100*60 + vuelos$sched_dep_time %%100)

# E15
vuelos$relacion= (vuelos$dep_time_min - vuelos$sched_dep_time_min - (vuelos$dep_delay))
table(vuelos$relacion)
##explicar!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


# E16
cancelado_por_dia <- 
  vuelos %>%
  mutate(cancelado = (is.na(arr_delay) | is.na(dep_delay))) %>%
  group_by(year, month, day) %>%
  summarise(
    cancelado_num = sum(cancelado),
    flights_num = n(),
  )

ggplot(cancelado_por_dia) +
  geom_point(aes(x = flights_num, y = cancelado_num))
print("A medida que aumentan el número de vuelos en un día, aumenta el número de cancelaciones")

# E17
cancelados_y_retrasos <- 
  vuelos %>%
  mutate(cancelado = (is.na(arr_delay) | is.na(dep_delay))) %>%
  group_by(year, month, day) %>%
  summarise(
    prop_cancelado = mean(cancelado),
    media_dep_delay = mean(dep_delay, na.rm = TRUE),
    media_arr_delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  ungroup()

ggplot(cancelados_y_retrasos) +
  geom_point(aes(x = media_dep_delay, y = prop_cancelado))

print("Podemos observar que hay una fuerte tendencia lineal de los datos por lo que los vuelos cancelado y los retrasados están relacionados")



# E18



# E19
vuelos %>%
  group_by(carrier) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(desc(arr_delay))

vuelos %>%
  group_by(carrier) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  arrange(desc(dep_delay))
print("La F9 es la que más se retrasa tanto en salidas como en llegadas")


# E20
vuelos %>%
  group_by(hour) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(arr_delay)
print("Para evitar los retrasos en la salida la mejor hora para volar son las 5 de la mañana")
