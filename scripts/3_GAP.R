library(sf)
library(dplyr)
library(ggplot2)


br<-st_read("capas/r1.shp")
bins = c(0, 0.60, 0.85, 1, 1.3)
labels = c('Bajo', 'Adecuado', 'Bueno', "DI")

br$class = cut(br$Coef_1, breaks = bins, labels = labels, right = FALSE)


crs<- st_crs(br)

### FAO analisis ####

fao <- st_read("C:/Art/Shape/F_AREAS.shp",
               crs = crs)

ggplot(data = br1, aes(Fill= Coef_1))+
  geom_sf()


rec<- data.frame(table(fao$F_AREA))

data<- data.frame()
for (i in rec$Var1) {
  fao1<- fao %>% filter(F_AREA == 21)
  
  fao1<- st_union(fao1)
  
  br1<- st_intersection(br, fao1)
  
  # Definir los límites de los intervalos y las etiquetas de clasificación
  bins = c(0, 0.60, 0.85, 1, 1.3)
  labels = c('Bajo', 'Adecuado', 'Bueno', "DI")
  
  br1$class = cut(br1$Coef_1, breaks = bins, labels = labels, right = FALSE)
  
  # Definir los límites de los intervalos y las etiquetas de clasificación
  bins = c(0, 0.60, 0.85, 1, 1.3)
  labels = c('Bajo', 'Adecuado', 'Bueno', "DI")

  br1$class = cut(br1$Coef_1, breaks = bins, labels = labels, right = FALSE)

 data1<- data.frame(A_FAO = i,
          Superficie = as.numeric(sum(st_area(br1))),
           Bajo =as.numeric(sum(st_area(filter(br1, class == "Bajo")))),
           Adecuado = as.numeric(sum(st_area(filter(br1, class == "Adecuado")))),
           Bueno = as.numeric(sum(st_area(filter(br1, class == "Bueno")))),
           DI = as.numeric(sum(st_area(filter(br1, class == "DI")))))
 data<- rbind(data, data1)
  
}

data$N_A<- rowSums(data[,3:6])
data$N_A<- data$Superficie- data$N_A

data<- data %>%
  mutate(P_NA = (N_A/Superficie)*100,
         P_DI = (DI/Superficie)*100,
         PBajo = (Bajo/Superficie)*100,
         PAdecuado = (Adecuado/Superficie)*100,
         PBueno = (Bueno/Superficie)*100)

write.table(data, "clipboard", sep="\t", row.names=F)


### AO analisis ####

AP<-st_read("C:/github/stp_fishes/shape/WDPA_Jun2023_Public_shp_2/WDPA_02.shp", 
            crs = crs)

data<- data.frame()
for (i in 1:30) {

b1<- br %>% filter(Realm == i)

AP1<- st_intersection(AP, b1)


data1<- data.frame(Biorregión = i,
                   Superficie = as.numeric(sum(st_area(AP1))),
                   Bajo =as.numeric(sum(st_area(filter(AP1, class == "Bajo")))),
                   Adecuado = as.numeric(sum(st_area(filter(AP1, class == "Adecuado")))),
                   Bueno = as.numeric(sum(st_area(filter(AP1, class == "Bueno")))),
                   DI = as.numeric(sum(st_area(filter(AP1, class == "DI")))))
data<- rbind(data, data1)
}

write.table(data, "clipboard", sep="\t", row.names=F)


