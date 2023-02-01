library(sf)
library(dplyr)
library(vegan)

#Cargar las biorregiones de Costello et al. 2017
br<-st_read("~/shape/MarineRealms.shp")
br<- st_as_sf(br, crs = "WGS84")

numberbio<- c(1)

##Grid polygon

##To analyze the data at a local scale, we subdivide the polygon into 1° (cell)
##grids, and then assign to each grid the geographically corresponding species occurrences.
brgr<- st_make_grid(br, cellsize = 1, what = "polygons" ,
                        crs = "WGS84", square = FALSE )

#Cargar los datos por biorregion 
bre<-br%>% dplyr::filter(Realm == numberbio)
base<- read.csv("~/Datos/final_d01_nov2021.csv")
base<-st_as_sf(base, coords= c("Long", "Lat"),
         crs = "WGS84")

##Cortar los datos en la biorregión
d<- base %>% st_as_sf() %>% st_intersection(bre)

##Tablas de resumen de variables
#Especies representadas en la base de datos de la biorregión
e<-data.frame(table(d$Especie))
#Familias representadas en la base de datos de la biorregión 
f<-data.frame(table(d$Familia))
#Tabla de datos
ED<-data.frame("Biorregión"=(c(numberbio)),
               "Datos"= (c(nrow(d))),
               "Especies"= (c(nrow(e))),
               "Familias"=(c(nrow(f))))


##Since species richness alone is not a descriptive statistic that allows us to understand 
##the distribution of species in the study area, we will use the Shannon index. This 
##indicates how evenly species are represented in the geographic space we are analyzing. 
##High numbers of this index (\>3) indicate that the community is very diverse, with typical 
##values being between 1-4.5 (Villarreal et al. 2004).
s<-transpose(e)
s= s[-c(1),]
s<-as.numeric(s)
ED<- cbind(ED,"Shannon"= diversity(s, "shannon"))
ED

##NOSE 
brgrilla<- brgr %>% dplyr::filter(Realm == numberbio)
brgrilla <- st_as_sf(brgrilla)
brgrilla<- mutate(brgrilla, Numero.x = c(1:c(nrow(brgrilla))))



##To analyze each grid (cell) as individual samples, we have to subdivide it by 
##grids of size 0.2 (pixels), then each of the grids will have 25 sampling sites 
##in which the species will be assigned. It is in this way that we can analyze each cell. 
##Then, merging both layers is indispensable, in order to subsequently cut off excess pixels.


brgrillas<- st_make_grid(brgrilla, cellsize = 0.2, what ="polygons",
                         crs = "WGS84", square = FALSE )

brgrillas <- st_as_sf(brgrillas)

brgrillas<-mutate(brgrillas, Numero= c(nrow(brgrillas)))
brg<-st_join(brgrillas, brgrilla, join= st_within)
brg <- brg[!is.na(brg$Numero.y),]


##To calculate the biodiversity indices and to be able to represent the spatial and 
##temporal distribution of data we are interested in, we must join the species occurrence
##points to the grid map resulting from the previous point, create a dataframe with the 
#data to be used and use the "vegan" and "fossil" packages to calculate the species 
##richness and Shannon indices. To analyze the distribution of data in the cells, 
##we must create a data matrix for each cell, so that the columns represent the species 
##present, and the rows represent the sampling sites, that is, the pixels of each cell. 
##As there are a large number of cells to be analyzed, we will use a function that allows 
##us to easily estimate each of them. For these analyses we use the packages "vegan" and "fossil".

#Unir las capas del shape de las biorregiones y  
brg<-st_join(brg, base)

brg<-data.frame(brg)

##Para determinar la riqueza de especies
Estimadores<- data.frame()
for (value in 1:9882) { 
  matriz <- create.matrix (filter(brg, brg$Numero.y== value),
                           tax.name = "V3", 
                           locality = "Numero.x");
  estimador <- data.frame(specpool(matriz));
  Estimadores <- rbind(Estimadores, c(as.numeric(estimador), value))
}
Estimadores<-data.frame(Estimadores)


ind<- data.frame()

ind<-mutate(brgrilla,"Riqueza"= c(as.numeric(Estimadores$Species)))
ind<-st_as_sf(ind)













