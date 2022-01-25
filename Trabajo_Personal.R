install.packages("nycflights13")
library(nycflights13)
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
var_desc= vuelos[is.na(vuelos),is.na(vuelos)]
var_desc=is.na.data.frame(vuelos)


vordenados= arrange(vuelos_NA, desc(dep_delay))

