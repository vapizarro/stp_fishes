library(sf)
library(dplyr)
library(vegan)
library(fossil)
library(ggplot2)
library(reshape2)

#Cargar las biorregiones de Costello et al. 2017
br <- st_read("shape/Costello_equalarea.shp")
crs = st_crs(br)


# Visualizar la capa en un mapa utilizando ggplot()
ggplot() +
  geom_sf(data  = br)

##Grid polygon
numberbio<- 1
##To analyze the data at a local scale, we subdivide the polygon into 1° (cell)
##grids, and then assign to each grid the geographically corresponding species occurrences.
brgr <- st_make_grid(br, cellsize = 10, crs = st_crs(br),
                     what = "polygons", square = F)
numberbio <- c()

for (i in 1:30) {
  if (i < 10) {
    numero <- paste0("0", i)
  } else {
    numero <- as.character(i)
  }
  numberbio <- c(numberbio, numero)
}

#We use for to automate the script for each bioregion, the results are saved per bioregion as shapefiles.

for (i in numberbio) {


bre<- filter(br,Realm == as.numeric(i))

bre<- brgr %>% st_as_sf() %>% st_intersection(bre)

# Visualizar la capa en un mapa utilizando ggplot()
ggplot() +
  geom_sf(data = bre)

nombre <- paste0("Datos/final_d", i, "_nov2021.csv")

#Cargar los datos por biorregion 
base<- read.csv(nombre)
base<-st_as_sf(base, coords= c("Long", "Lat"),
         crs = crs)

ggplot() +
  geom_sf(data = bre)+
  geom_sf(data= base)

##Cortar los datos en la biorregión
d<- base %>% st_as_sf() %>% st_intersection(bre)

##Tablas de resumen de variables
#Especies representadas en la base de datos de la biorregión
e<-data.frame(table(d$species))
#Familias representadas en la base de datos de la biorregión 
f<-data.frame(table(d$Family))
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
#s<-transpose(e)
#s= s[-c(1),]
#s<-as.numeric(s)
#ED<- cbind(ED,"Shannon"= diversity(s, "shannon"))
#ED

##NOSE 
brge<-st_as_sf(bre)
brgrilla<- mutate(brge, Numero = c(1:c(nrow(brge))))


##To analyze each grid (cell) as individual samples, we have to subdivide it by 
##grids of size 0.2 (pixels), then each of the grids will have 25 sampling sites 
##in which the species will be assigned. It is in this way that we can analyze each cell. 
##Then, merging both layers is indispensable, in order to subsequently cut off excess pixels.

brgrillas<- st_make_grid(brgrilla, cellsize = 2, what ="polygons",
                         crs = st_crs(br), square = FALSE )

brgrillas <- st_as_sf(brgrillas)

brgrillas<-mutate(brgrillas, Numero= c(1:nrow(brgrillas)))
brg<-st_join(brgrillas, brgrilla, join= st_within)

brg<- st_intersection(brg, brgrilla)


##To calculate the biodiversity indices and to be able to represent the spatial and 
##temporal distribution of data we are interested in, we must join the species occurrence
##points to the grid map resulting from the previous point, create a dataframe with the 
#data to be used and use the "vegan" and "fossil" packages to calculate the species 
##richness and Shannon indices. To analyze the distribution of data in the cells, 
##we must create a data matrix for each cell, so that the columns represent the species 
##present, and the rows represent the sampling sites, that is, the pixels of each cell. 
##As there are a large number of cells to be analyzed, we will use a function that allows 
##us to easily estimate each of them. For these analyses we use the packages "vegan" and "fossil".

#Unir las capas del shape de las biorregiones y  la base de datos
brg<-st_intersection(brg, d, join = st_overlaps)

brg<-data.frame(brg)

z<- 1: nrow(bre)
z<- data.frame(table(brg$Numero))

##Para determinar la riqueza de especies
Estimadores<- data.frame()
for (value in z$Var1) { 
  bgre<- brg %>%
      filter(brg$Numero == value) %>%
    as.data.frame()
  
  matriz <- create.matrix(bgre,
                           tax.name = "species", 
                           locality = "Numero.x");
  estimador <- data.frame(specpool(matriz));
  Estimadores <- rbind(Estimadores, c(as.numeric(estimador), value))
}

Estimadores<-data.frame(Estimadores)

names(Estimadores) <- c("Species","chao"  , "chao.se", "jack1" ,"jack1.se" ,"jack2" ,"boot","boot.se","n" , "celda") 

#ind<- data.frame()


#ind<-mutate(brgrilla,"Riqueza"= c(as.numeric(Estimadores$Species)))
#ind<-st_as_sf(ind)

### 6. Aplicar los estimadores de riqueza de especies

#Para análizar la representatividad espacial de los datos disponibles en las plataformas ya 
#mencionadas, determinaremos la riqueza de especie presente en casa una de las celdas, a 
#partir de estimadores no parametricos. Aquellos estimadores, calculan la cantidad total de 
#especies que deberia haber es un sitio, según los datos de ocurrencia de especies. 
#Chao2 (Chao et al. 2009) analiza la ocurrencia de especies unicas que se repiten una o dos 
#veces en toda la muestra, para calcular cuantas especies más existen en la muestra. 
#A cada una de estas matrices se le aplican estimadores no parametricos que posteriormente 
#se comparan con la riqueza obtenida de la base de datos, lo cuál entrega como resultado un 
#indice de representatividad espacial. Determinaremos la cantidad de datos por celda como: 
#"Muy bajo", "Bajo", "Aceptables"e "Ideales" , segun estos el valor del indice obtenido. 
#Siendo los rangos de 0-0.2; 0.21-0.4; 0.41-0.7; 0.71-1.


#Para analizar la representatividad 
Estimadores<-data.frame("Numero"=as.numeric(Estimadores$celda), 
                        "Riqueza"=as.numeric(Estimadores$Species), 
                        "Chao2"=as.numeric(Estimadores$chao),
                        "Jackknife1"=as.numeric(Estimadores$jack1),
                        "Bootstrap"=as.numeric(Estimadores$boot))



##Calcular la estimación de número especies representadas, y número de especies estimadas (Indice de representatividad)
Irep<-data.frame( "Numero" = Estimadores$Numero,
                  "Riqueza" = Estimadores$Riqueza,
                  "Chao2" = c(Estimadores$Riqueza/Estimadores$Chao2),
                  "Jackknife1" =c(Estimadores$Riqueza/Estimadores$Jackknife1),
                  "Bootstrap" =c(Estimadores$Riqueza/Estimadores$Bootstrap))

Irep$coef<- (Irep$Chao2 + Irep$Jackknife1 + Irep$Bootstrap)/3 

brgrilla<-brgrilla %>%
  left_join(Irep, by = "Numero")

st_as_sf(brgrilla)

h<- brgrilla %>% st_as_sf() %>% st_intersection(bre)

h<- st_as_sf(h)
h2 <- st_collection_extract(h, "POLYGON")



ggplot() +
  geom_sf(data = h2, aes(fill = coef))

nombre <- paste0("C:/github/stp_fishes/capas/b", i, ".shp")

st_write(h2, nombre,  delete_dsn = TRUE )

}


#### Data analysis ####

numberbio <- c()
for (i in 1:30) {
  if (i < 10) {
    numero <- paste0("0", i)
  } else {
    numero <- as.character(i)
  }
  numberbio <- c(numberbio, numero)
}

brg<- data.frame()
for (i in numberbio) {
  
nombre <- paste0("capas/1/b", i, ".shp")

br <- st_read(nombre)
brg<- rbind(brg, br)

}

# If the species richness of the cell = 1, the coef will be 1.25 to be able to classify it later as "IR", insufficient records.

brg$Coef<- ifelse(brg$Riqueza == 1, 
                  1.25 ,
                  brg$coef)

st_write(brg, "capas/r10.shp",  delete_dsn = TRUE )


brg<- as.data.frame(brg)

  
#Define interval boundaries and cell sorting labels
br<-st_read("capas/r1.shp")
bins = c(0, 0.60, 0.85, 1, 1.3)
labels = c('Few', 'Sufficient', 'Adequate', "IR")

br$class = cut(br$coef, breaks = bins, labels = labels, right = FALSE)


#count the area by bioregion for each classification and the total area for each classification

data<- data.frame()
for (i in 1:30) {
  br1<- filter(br, Realm == i)
  
  data1<- data.frame(Bioregion = i,
                     Superficie = as.numeric(sum(st_area(br1))),
                     "F" =as.numeric(sum(st_area(filter(br1, class == "Few")))),
                     "S" = as.numeric(sum(st_area(filter(br1, class == "Sufficient")))),
                     "A" = as.numeric(sum(st_area(filter(br1, class == "Adequate")))),
                     "IR" = as.numeric(sum(st_area(filter(br1, class == "IR")))))
  data<- rbind(data, data1)
  
}


data$N_A<- rowSums(data[,3:6])
data$N_A<- data$Superficie - data$N_A

#define the area ratio for each classification

data<- data %>%
  mutate(P_NA = (N_A/Superficie)*100,
         P_IR = (IR/Superficie)*100,
         P_F = ('F'/Superficie)*100,
         P_S = (S/Superficie)*100,
         P_A = (A/Superficie)*100)


write.table(data, "clipboard", sep="\t", row.names=F)







