library(sf)
library(dplyr)
library(ggplot2)
library(rworldxtra)
library(cartography)
library(paletteer)
library(gridExtra)
library(readxl)
library(paletteer)


br<-st_read("shape/MarineRealms.shp")

brgrilla<- st_make_grid(bre, cellsize = 1, what = "polygons" ,
                        crs = "WGS84", square = FALSE )


h<- brgrilla %>% st_as_sf() %>% st_intersection(bre) %>% 
  mutate(n= c(1:118))

ggplot()+ geom_sf(data= h)


sf::sf_use_s2(FALSE)
data("countriesHigh")
## Mapa de Europa ####
eu <- st_as_sf(countriesHigh, crs = "WGS84") %>% filter()
box<- c(xmin = 0, ymin= 50, xmax= 32 , ymax= 68)
eu<- st_crop(eu, box)
eu<- select(eu, geometry)

ggplot()+ geom_sf(data= h) + geom_sf(data= eu)


png("Figuras/figura1.png", he)
choroLayer(x= h, var = "n",
           col = NA, legend.pos =  F)
plot(eu, col  = "#E3DEBF",  add = TRUE, bg = "lightblue1")

dev.off()


plot(br, col = NA , border = NA, 
     bg = "white")
choroLayer(x = br, var = "Realm", 
           col =paletteer_c("grDevices::Purple-Yellow", 30),
           nclass= 30, 
           border = "grey80", 
           lwd = 0.5, legend.pos = F , add = TRUE) 
labelLayer(x= br, 
           txt = "Realm", col = "black",r = 0.1, cex = 0.7)



####Grid ####

lay <- rbind(c(1,2,3),
             c(1,4,5))
grid.arrange(grobs = gs, layout_matrix = lay)



##########

sf::sf_use_s2(FALSE)

br<- st_read("shape/br.shp", crs = crs)



col1<- c("#D9D9D9", "#D9D9D9", "#D9D9D9", "#D9D9D9", "#D9D9D9", "#D9D9D9", "#D9D9D9", "#D9D9D9", "#D9D9D9",
         "#D9D9D9", "#D9D9D9", "#D9D9D9", "#D9D9D9", "#D9D9D9", "#D9D9D9", "#D9D9D9", "#D9D9D9", "#D9D9D9",
         "#D9D9D9", "#D9D9D9", "#D9D9D9", "#D9D9D9", "#D9D9D9", "#D9D9D9", "#D9D9D9", "#D9D9D9", "#D9D9D9",
         "#D9D9D9", "#D9D9D9", "#D9D9D9")

######
pdf("plot.pdf", width= 4, height = 2)
par( mar = c(0,0,0,0))
plot(st_geometry(br), col = NA , border = NA, 
     bg = "white",  lty = 3)
choroLayer(x = br, var = "Realm", 
           col = col1,
           nclass= 30, 
           border = "#545454", 
           lwd = 0.5, legend.pos = F , add = TRUE) 
labelLayer(x= br, 
           txt = "Realm", col = "black",r = 0.1, cex = 0.7,
           halo = TRUE)
dev.off()

br<- br %>% mutate(mean = Datos/Spr__2_)

######
pdf("plots.pdf", width= 4, height = 2)
par(mfrow = c(2, 2), mar = c(0,0,0,0))
#1
par(mar = c(0,0,0,0))
plot(st_geometry(br), col = NA , border = NA, 
     bg = "white")
choroLayer(x = br, var = "Datos", 
           col = paletteer_c("grDevices::Viridis", 5),
           nclass= 5, 
           border = "grey80", 
           lwd = 0.5, legend.pos = F , add = TRUE) 
layoutLayer(title = "", 
            author = " ",
            sources = "",
            frame = FALSE, scale = 5, tabtitle = TRUE,theme = "white")
#2
par(mar = c(0,0,0,0))
plot(st_geometry(br), col = NA , border = NA, 
     bg = "white")
choroLayer(x = br, var = "Especis", 
           col = paletteer_c("grDevices::Viridis", 5),
           nclass= 5, 
           border = "grey80", 
           lwd = 0.5, legend.pos = F , add = TRUE) 
layoutLayer(title = "", 
            author = " ",
            sources = "",
            frame = FALSE, scale = 5, tabtitle = TRUE,theme = "white")


#3
par(mar = c(0,0,0,0))
plot(st_geometry(br), col = NA , border = NA, 
     bg = "white")
choroLayer(x = br, var = "Familis", 
           col = paletteer_c("grDevices::Viridis", 5),
           nclass= 5, 
           border = "grey80", 
           lwd = 0.5, legend.pos = F , add = TRUE) 
layoutLayer(title = "", 
            author = " ",
            sources = "",
            frame = FALSE, scale = 5, tabtitle = TRUE,theme = "white")

#4
par(mar = c(0,0,0,0))
plot(st_geometry(br), col = NA , border = NA, 
     bg = "white")
choroLayer(x = br, var = "Shannon", 
           col = paletteer_c("grDevices::Viridis", 5),
           nclass= 5, 
           border = "grey80", 
           lwd = 0.5, legend.pos = F , add = TRUE) 
layoutLayer(title = "", 
            author = " ",
            sources = "",
            frame = FALSE, scale = 5, tabtitle = TRUE,theme = "white")

dev.off()

##legend
pdf("legend.pdf", width= 4, height = 2)
par(mar = c(0,0,0,0))
plot(st_geometry(br), col = NA , border = NA, 
     bg = "white")
choroLayer(x = br, var = "Shannon", 
           col = paletteer_c("grDevices::Viridis", 5),
           nclass= 5, 
           border = "grey80", 
           lwd = 0.5, legend.pos = "bottom" , add = TRUE) 
layoutLayer(title = "", 
            author = " ",
            sources = "",
            frame = FALSE, scale = 5, tabtitle = TRUE,theme = "white")

dev.off()


########### Mapas ############


br<-st_read("capas/r1.shp")

pdf("pdf1.pdf", height = 5, width = 9)
par(mar = c(0,0,0,0))
plot(st_geometry(br), col = NA , border = NA, 
     bg = "white")
choroLayer(x = br, var = "Coef_1", 
           col = c("#006992","#36C86E", "#FDE333","#4B0055"),
           breaks =  c(0, 0.60, 0.85, 1, 2),
           border = "grey80", 
           lwd = 0.2, legend.pos = F , add = TRUE) 
layoutLayer(title = "", 
            author = " ",
            sources = "",
            frame = FALSE, 
            scale = 5, 
            tabtitle = TRUE,
            theme = "white")
dev.off()

br <- st_read("shape/Costello_equalarea.shp")
pdf("pdf2.pdf", height = 5, width = 9)
par(mar = c(0,0,0,0))
plot(st_geometry(br), col = NA , border = "black", 
     bg = "white",lwd = 0.2)
dev.off()


pdf("pdf3.pdf", height = 5, width = 9)
par(mar = c(0,0,0,0))
plot(st_geometry(br), col = NA , border = NA, 
     bg = "white")
choroLayer(x = br, var = "Coef_1", 
           col =  c("#006992","#36C86E", "#FDE333","#4B0055"),
           breaks =  c(0, 0.60, 0.85, 1, 2),
           border = "grey80", 
           lwd = 0.5, add = TRUE,
           legend.pos = "top",
           legend.horiz = T,
           legend.frame = T)
layoutLayer(title = "", 
            author = " ",
            sources = "",
            frame = FALSE, 
            scale = 5, 
            tabtitle = TRUE,
            theme = "white")
dev.off()

br<-st_read("capas/r5.shp")
pdf("pdf4.pdf", height = 2.5, width = 4.5)
par(mar = c(0,0,0,0))
plot(st_geometry(br), col = NA , border = NA, 
     bg = "white")
choroLayer(x = br, var = "Coef_1", 
           col = c("#006992","#36C86E", "#FDE333","#4B0055"),
           breaks =  c(0, 0.60, 0.85, 1, 2),
           border = "grey80", 
           lwd = 0.2, legend.pos = F , add = TRUE) 
layoutLayer(title = "", 
            author = " ",
            sources = "",
            frame = FALSE, 
            scale = 5, 
            tabtitle = TRUE,
            theme = "white")
dev.off()

br<-st_read("capas/r10.shp")
pdf("pdf5.pdf", height = 2.5, width = 4.5)
par(mar = c(0,0,0,0))
plot(st_geometry(br), col = NA , border = NA, 
     bg = "white")
choroLayer(x = br, var = "Coef_1", 
           col = c("#006992","#36C86E", "#FDE333","#4B0055"),
           breaks =  c(0, 0.60, 0.85, 1, 2),
           border = "grey80", 
           lwd = 0.2, legend.pos = F , add = TRUE) 
layoutLayer(title = "", 
            author = " ",
            sources = "",
            frame = FALSE, 
            scale = 5, 
            tabtitle = TRUE,
            theme = "white")
dev.off()

br <- st_read("shape/Costello_equalarea.shp")
pdf("pdf6.pdf", 2.5, width = 4.5)
par(mar = c(0,0,0,0))
plot(st_geometry(br), col = NA , border = "black", 
     bg = "white",lwd = 0.1)
dev.off()

##### Cartogramas #####

br<-st_read("capas/r5.shp")

library(cartogram)
library(scales)


br <- st_read("shape/Costello_equalarea.shp")

br<- st_read("shape/br.shp", crs = crs)
Pendiente <- read_excel("C:/R/Art/Libro1.xlsx")

br<- merge(br, Pendiente)

br$Pendiente<- rescale(br$Pendiente)

cartograma <- cartogram_cont(br, "Pendiente")

cartograma<-cartograma[,c(6)]

colnames(cartograma)[1] <- "Riqueza"

pdf("fig1.pdf")
ggplot() +
  geom_sf(data = cartograma, aes(fill= "Riqueza")) +
  #scale_fill_manual(values = paletteer_d("colorBlindness::Blue2DarkRed12Steps"))+
  labs(fill = "Species Richness")
dev.off()

pdf("fig1.pdf")
plot(cartograma)
dev.off()


### ####
numberbio <- c()

for (i in 1:30) {
  if (i < 10) {
    numero <- paste0("0", i)
  } else {
    numero <- as.character(i)
  }
  numberbio <- c(numberbio, numero)
}

ab<- data.frame()
for (i in numberbio) {
  
  nombre <- paste0("Datos/final_d", i, "_nov2021.csv")
  
  #Cargar los datos por biorregion 
  base<- read.csv(nombre)
  b<- base %>%
    group_by(Year) %>%
    summarise(n= n())
  
  b$ac<- cumsum(b$n)
  b$B<- i

  ab<- rbind(ab, b)
}

ab1<- filter(ab, Year == 2021)

cols <- c("#BFC2C1","#4B0055", "#BFC2C1", "#BFC2C1","#BFC2C1","#BFC2C1", "#BFC2C1","#BFC2C1","#BFC2C1", "#BFC2C1",
          "#FDE333","#BFC2C1", "#BFC2C1","#BFC2C1","#BFC2C1","#BFC2C1", "#BFC2C1","#BFC2C1","#BFC2C1", "#BFC2C1",
          "#BFC2C1","#BFC2C1", "#BFC2C1","#BFC2C1","#BFC2C1","#BFC2C1", "#BFC2C1","#BFC2C1","#BFC2C1", "#BFC2C1")

pdf("plot.pdf", height = 3, width = 5)
ggplot(data= ab, aes(x= Year, y =ac, color = B))+
  geom_line(size= 1.2)+
  scale_color_manual(values = cols)+
  labs(y="Acumulated records")+
  guides(color = FALSE)+
  theme_light()
dev.off()


#### Pendiente ####

br<- st_read("shape/Costello_equalarea.shp")
Pendiente <- read_excel("C:/R/Art/Libro1.xlsx")

br<- merge(br, Pendiente)


pdf("pdf1.pdf", height = 5, width = 9)
par(mar = c(0,0,0,0))
plot(st_geometry(br), col = NA , border = NA, 
     bg = "white")
choroLayer(x = br, var = "Pendiente", 
           col = c("#FCFDBF","#FD9B6B", "#CC3F71","#2F1163"),
           breaks =  c(0, 0.5, 1, 2, 4),
           border = "grey80", 
           lwd = 0.2, 
           legend.pos = "bottom" , 
           legend.frame = T,
           add = TRUE) 
layoutLayer(title = "", 
            author = " ",
            sources = "",
            frame = FALSE, 
            scale = 5, 
            tabtitle = TRUE,
            theme = "white")
labelLayer(x= br, 
           txt = "Realm", col = "black",r = 0.1, cex = 0.8,
           halo = T)
dev.off()


pdf("plot.pdf", height = 5, width = 9)
par( mar = c(0,0,0,0))
plot(st_geometry(br), col = NA , border = NA, 
     bg = "white",  lty = 3)
choroLayer(x = br, var = "Realm", 
           col = col1,
           nclass= 30, 
           border = "#545454", 
           lwd = 0.5, legend.pos = F , add = TRUE) 
labelLayer(x= br, 
           txt = "Realm", col = "black",r = 0.1, cex = 0.7,
           halo = TRUE)
dev.off()

