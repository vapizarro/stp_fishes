library(sf)
library(dplyr)
library(ggplot2)
library(rworldxtra)
library(cartography)
library(paletteer)
library(gridExtra)


br<-st_read("shape/MarineRealms.shp", crs = "WGS84")
bre<-br%>% dplyr::filter(Realm == "1")



brgrilla<- st_make_grid(bre, cellsize = 1, what = "polygons" ,
                        crs = "WGS84", square = FALSE )


h<- brgrilla %>% st_as_sf() %>% st_intersection(bre) %>% 
  mutate(n= c(1:118))

ggplot()+ geom_sf(data= h)


sf::sf_use_s2(FALSE)
data("countriesHigh")
## Mapa de Europa 
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



####Grid

lay <- rbind(c(1,2,3),
             c(1,4,5))
grid.arrange(grobs = gs, layout_matrix = lay)



##########

sf::sf_use_s2(FALSE)

br<-st_read("shape/br.shp", crs = "WGS84")


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
choroLayer(x = br, var = "Especis", 
           col =carto.pal(pal1 = "turquoise.pal", n1 = 5),
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
choroLayer(x = br, var = "Familis", 
           col =carto.pal(pal1 = "turquoise.pal", n1 = 5),
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
choroLayer(x = br, var = "Shannon", 
           col =carto.pal(pal1 = "turquoise.pal", n1 = 5),
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
choroLayer(x = br, var = "mean", 
           col =carto.pal(pal1 = "turquoise.pal", n1 = 5),
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
plot(st_geometry(br), col = NA , border = 1, 
     bg = "white")
dev.off()
choroLayer(x = br, var = "Datos", 
           col =carto.pal(pal1 = "turquoise.pal", n1 = 5),
           nclass= 5, 
           border = "grey80", 
           lwd = 0.5, legend.pos = "bottom" , add = TRUE) 
layoutLayer(title = "", 
            author = " ",
            sources = "",
            frame = FALSE, scale = 5, tabtitle = TRUE,theme = "white")
dev.off()