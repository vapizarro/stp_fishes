library(dplyr)

# Preparación de la base de datos 
## 1. Ingeso de las bases de datos y eliminar valores NA

bg<- data.frame("Specie" = bg$species,
                "Family"= bg$family,
                "Lat" = bg$decimalLatitude,
                "Lon" = bg$decimalLongitude,
                "Year" = bg$year)

# Insterar NA a aquellos que no tienen valores 
bg[bg$Specie %in% "",] <- NA
bg[bg$Lat%in% "",] <- NA
bg[bg$Lon %in% "",] <- NA
bg[bg$Year %in% "",] <- NA

#Remover NA y VALORES EN 0
bg<-bg[!is.na(bg$Specie),]
bg<-bg[!is.na(bg$Lat),]
bg<-bg[!is.na(bg$Lon),]
bg<-bg[!is.na(bg$Year),]

Bgo<- rbind(bg, bo)

#Eliminar duplicados 
Bgo <- Bgo[!duplicated(Bgo[,2:3]),] 

#Eliminar las coordenadas iguales
fin3  <- Bgo
largo <- length(fin3[,1])
for (i in 1:largo){
  if (fin3[i,1] == fin3[i,2]){
    print("iguales")
    print(i)
    print(fin3[i,1])
    print(fin3[i,2])
    print(fin3[i,])
  }
}

Bgo <- fin3[-c(),]

#Eliminar registros de antes de 1980
Bgo <- Bgo[ which(Bgo$Year >= 1980),]

#Eliminar los resgistros con igual latitud y longitud
fin  <- Bgo

largo <- length(Bgo[,1])
for (i in 1:largo){
  if (fin3[i,1] == fin3[i,2]){
    print("iguales")
    print(i)
    print(fin3[i,1])
    print(fin3[i,2])
    print(fin3[i,])
  }
}


#Guardar
write.csv(Bgo, "~/BGO.csv")



##Debido a que la base de datos es de gran tamaño para la capicidad de los
##computadores utilzidos, se subdividieron las biorregiones por las coordenadas

be<- filter(Bgo, Lat >= 50, Lat<= 70, Lon >= 10, Lon <= 45)#1
write.csv(be, "~/Datos/final_d01_nov2021.csv")
be<- filter(Bgo, Lat >= 30, Lat<= 50, Lon >= 20, Lon <= 50)#2
write.csv(be, "~/Datos/final_d02_nov2021.csv")
be<- filter(Bgo, Lat >= 40, Lat<= 70, Lon >= -20, Lon <= 20)#3
write.csv(be, "~/Datos/final_d03_nov2021.csv")
be<- filter(Bgo, Lat >= 60, Lon >= 0, Lon <= 40)#4
write.csv(be, "~/Datos/final_d04_nov2021.csv")
be<- filter(Bgo, Lat >= 25, Lat<= 50, Lon >= -10, Lon <= 40)#5
write.csv(be, "~/Datos/final_d05_nov2021.csv")
be<- cbind(filter(Bgo, Lat >= 60,  Lon <= -90),
           filter(Bgo, Lat >= 60, Lon >= -5))#6
write.csv(be, "~/Datos/final_d06_nov2021.csv")
be<- cbind(filter(Bgo,  Lat >= 25, Lat<= 70,  Lon <= -110),
           filter(Bgo,  Lat >= 40, Lat<= 70, Lon >= 130))#7
write.csv(be, "~/Datos/final_d07_nov2021.csv")
be<- filter(Bgo, Lat >= 45, Lon >= -130, Lon <= 25)#8
write.csv(be, "~/Datos/final_d08_nov2021.csv")
be<- cbind(filter(Bgo,  Lat >= 0, Lat<= 50,  Lon <= 115),
           filter(Bgo,  Lat >= 0, Lat<= 50, Lon >= 140))#9
write.csv(be, "~/Datos/final_d09_nov2021.csv")
be<- filter(Bgo, Lat >= -50, Lat<= 0, Lon >= -160, Lon <= -65)#10
write.csv(be, "~/Datos/final_d10_nov2021.csv")
be<- filter(Bgo, Lat >= 0, Lat<= 50, Lon >= -100, Lon <= -45)#11
write.csv(be, "~/Datos/final_d11_nov2021.csv")
be<- filter(Bgo, Lat >= -15, Lat<= 35, Lon >= -125, Lon <= -70)#12
write.csv(be, "~/Datos/final_d12_nov2021.csv")
be<- filter(Bgo, Lat >= -30, Lat<= 35, Lon >= 30)#13
write.csv(be, "~/Datos/final_d13_nov2021.csv")
be<- filter(Bgo, Lat >= 5, Lat<= 35, Lon >= 30, Lon <= 50)#14
write.csv(be, "~/Datos/final_d14_nov2021.csv")
be<- filter(Bgo, Lat >= -50, Lat<= -10, Lon >= 145)#15
write.csv(be, "~/Datos/final_d15_nov2021.csv")
be<- filter(Bgo, Lat >= -40, Lat<= 0, Lon >= 105, Lon <= 175)#16
write.csv(be, "~/Datos/final_d16_nov2021.csv")
be<- filter(Bgo, Lat >= -50, Lat<= 10, Lon <= -125)#17
write.csv(be, "~/Datos/final_d17_nov2021.csv")
be<- filter(Bgo, Lat >= 20, Lat<= 80, Lon >= -80, Lon <= 05)#18
write.csv(be,"~/Datos/final_d18_nov2021.csv")
be<- filter(Bgo, Lat >= -50, Lat<= 25, Lon >=  20, Lon <= 120)#19
write.csv(be, "~/Datos/final_d19_nov2021.csv")
be<- filter(Bgo, Lat >= -5, Lat<= 50, Lon >= 120, Lon <= 160)#20
write.csv(be, "~/Datos/final_d20_nov2021.csv")
be<- filter(Bgo, Lat >= -55, Lat<= 45, Lon >= -80, Lon <= 10)#21
write.csv(be, "~/Datos/final_d21_nov2021.csv")
be<- filter(Bgo, Lat >= -15, Lat<= 25, Lon >= -160, Lon <= -80)#22
write.csv(be, "~/Datos/final_d22_nov2021.csv")
be<- filter(Bgo, Lat >= -20, Lat<= 25, Lon >= -30, Lon <= 20)#23
write.csv(be, "~/Datos/final_d23_nov2021.csv")
be<- filter(Bgo, Lat >= -60, Lat<= -30, Lon >= -85, Lon <= -45)#24
write.csv(be, "~/Datos/final_d24_nov2021.csv")
be<- filter(Bgo, Lat >= -55, Lat<= -20, Lon >= -85, Lon <= -75)#25
write.csv(be, "~/Datos/final_d25_nov2021.csv")
be<- filter(Bgo, Lat >= -50, Lat<= -20, Lon >= 110, Lon <= 160)#26
write.csv(be, "~/Datos/final_d26_nov2021.csv")
be<- filter(Bgo, Lat >= -45, Lat<= -10, Lon >= 5, Lon <= 55)#27
write.csv(be, "~/Datos/final_d27_nov2021.csv")
be<- cbind(filter(Bgo,   Lat >= -65, Lat<= -20, Lon <= -150),
           filter(Bgo,  Lat >= -60, Lat<= -25, Lon >= 155))#28
write.csv(be, "~/Datos/final_d28_nov2021.csv")
be<- filter(Bgo, Lat >= 10, Lat<= 50, Lon >= 100, Lon <= 130)#29
write.csv(be, "~/Datos/final_d29_nov2021.csv")
be<- filter(Bgo, Lat<= -40)#30
write.csv(be, "~/Datos/final_d30_nov2021.csv")


