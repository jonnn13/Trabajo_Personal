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
v_trayecto=vuelos[order(vuelos$distance, decreasing = T),]
head(v_trayecto)

# E13
v_trayecto=vuelos[order(vuelos$distance, decreasing = F),]
tail(v_trayecto)





# E14
vuelos$dep_time_min= (vuelos$dep_time %/%100*60 + vuelos$dep_time %%100) 
vuelos$sched_dep_time_min= (vuelos$sched_dep_time %/%100*60 + vuelos$sched_dep_time %%100)


###vuelos$dep_time_min2= (vuelos$dep_time %/%100*60 + vuelos$dep_time %%100) %% 1440
####vuelos$sched_dep_time_min2= (vuelos$sched_dep_time %/%100*60 + vuelos$sched_dep_time %%100) %% 1440



# E15
vuelos$relacion= (vuelos$dep_time_min - vuelos$sched_dep_time_min - (vuelos$dep_delay))
table(vuelos$relacion)
print("Podemos correctamente la relaciones entre estas varibales en 327314 vuelos ya que en la resta obtenemos cero. Sin embrago, en 1207 vuelos no podemos observar correctamente esa relación ya que la resta nos da 1440, esto se debe a que el retraso al que ha ido sometido el vuelo hace que el este despege en un día diferente, es decir, que durante ese cambio de horario trascurre la media noche.")


# Como podemos observar existen 327314 vuelos en los cuales coincide que la diferencia entre (dep_time, hora a la que sale 
# realmente) menos (sched_dep_time, que es la hora a la que tenia programado salir) coincide con dep_delay que es el retraso 
# que llevan. En 1207 vuelos existe una diferencia de 1440, que se debe a que estos vuelos parten en media noche

# E16
cancelado_por_dia <- 
  vuelos %>%
  mutate(cancelado = (is.na(arr_delay) | is.na(dep_delay))) %>%
  group_by(year, month, day) %>%
  summarise(
    cancelado_num = sum(cancelado),
    flights_num = n(),
  )

#cancelado=(is.na(arr_delay) | is.na(dep_delay)))--> nos dice que los cancelados son los que tienen valores NA en arr_delay y dep_delay
 

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

ggplot(cancelados_y_retrasos) +
  geom_point(aes(x = media_arr_delay, y = prop_cancelado))

print("Podemos observar que hay una fuerte tendencia lineal de los datos en ambas gráficas por lo que la proporción de vuelos cancelados y los retrasados en la salida y en la llegada están relacionados")



# E18 MIRAR CON JAVI
cancelados_y_retrasos_A <- 
  vuelos %>%
  mutate(cancelado = (is.na(arr_delay) | is.na(dep_delay))) %>%
  group_by(origin, dest) %>% 
  summarise(
    prop_cancelado = mean(cancelado),
    media_dep_delay = mean(dep_delay, na.rm = TRUE),
    media_arr_delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  ungroup()

ggplot(cancelados_y_retrasos_A) +
  geom_point(aes(x = media_dep_delay, y = prop_cancelado))

ggplot(cancelados_y_retrasos_A) +
  geom_point(aes(x = media_arr_delay, y = prop_cancelado))



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


# E20 ************************************************************************
vuelos %>%
  group_by(hour) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  arrange(dep_delay)
print("Para evitar los retrasos en la salida la mejor hora para volar son las 5 de la mañana")


# E21
make_datetime_100 <- function(year, month, day, time) 
{ 
  make_datetime(year, month, day, time %/% 100, time %% 100) 
  }

flights_dt <- vuelos %>% 
  filter(!is.na(dep_time), !is.na(arr_time)) %>% 
  mutate( dep_time = make_datetime_100(year, month, day, dep_time), 
          arr_time = make_datetime_100(year, month, day, arr_time), 
          sched_dep_time = make_datetime_100(year, month, day, sched_dep_time), 
          sched_arr_time = make_datetime_100(year, month, day, sched_arr_time) ) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"))

flights_dt2<- flights_dt %>%
  mutate(dow = wday(sched_dep_time)) %>%
  group_by(dow) %>%
  summarise(
    dep_delay = mean(dep_delay),
    arr_delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  print(n = Inf)

flights_dt %>%
  mutate(wday = wday(dep_time, label = TRUE)) %>% #porque de dep_time
  group_by(wday) %>% 
  summarize(ave_dep_delay = mean(dep_delay, na.rm = TRUE)) %>% 
  ggplot(aes(x = wday, y = ave_dep_delay)) + 
  geom_bar(stat = "identity") 
print("El sábado es el día de la semana que menos retrasos en la salida hay")


# E22
retraso_acum<- vuelos %>% 
  filter(arr_delay > 0) %>% 
  group_by(dest) %>% 
  summarise(total_delay= sum(arr_delay))%>% 
  arrange(total_delay)





  
  
# E23   ?????


totalretra= sum(retraso_acum$total_delay)
retraso_acum2<- vuelos %>% 
  filter(arr_delay > 0) %>% 
  group_by(dest) %>% 
  summarise(total_delay= sum(arr_delay),
            prop_total_delay= total_delay / totalretra)%>% 
  arrange(prop_total_delay)
view(totalretra) 



  
# 24

flights_dt %>% 
  mutate(sched_dep_hour = hour(sched_dep_time)) %>% 
  group_by(sched_dep_hour) %>% 
  summarise(dep_delay = mean(dep_delay)) %>% 
  ggplot(aes(y = dep_delay, x = sched_dep_hour)) + 
  geom_point() + geom_smooth()


