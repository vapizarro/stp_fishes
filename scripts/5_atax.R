library(rfishbase)
library(dplyr)
library(ggplot2)

### 1. Agregar las caracteristicas ###


cts<- data.frame(species(a))
do <- do %>% drop_na()hist(table(cts$FamCode), main= "Histograma de representación de familias", 
                           xlab= "Familia")
hist(table(cts$Length), main= "Histograma de representación segun tamaño", 
     xlab = "Tamaño (cm)", breaks = 20)
hist(table(cts$DepthRangeDeep), main= "Histograma de representación según profundidad de hábitat", xlab= "Rango de profundidad (m)", ylab = "Frecuencia")
hist(table(cts$Vulnerability), main= "Histograma de representación según la vulnerabilidad", xlab = "Indice de vulnerabilidad")
imp<- data.frame(table(cts$Importance))
imp<- data.frame("Importancia" = c("Sin interés" , "Potencial", "De subsistencia",
                                   "Comercial"),
                 "Frecuencia" = c(460, 36, 174, 2478))




base<-merge(x = sp, y = cts, by.x = "species", by.y =  "Species" , all.x = TRUE)
base1<-merge(x = sp, y = cts, by.x = "species", by.y =  "Species" )



Tam <- transform(table(cut(b$Length[which(b$Length > 0 )],
                           breaks = 10)))

Prf <- transform(table(cut(b$DepthRangeDeep[which(b$DepthRangeDeep > 0 )], 
                           breaks =  c(0, 200, 1000, 3000, 6000, 7000, Inf))))

Imp<- data.frame(table(b$Importance))





t<- as.vector(Tam$Freq)
Tam<- cbind(Tam, "LogFrec" = log(c(t)))
Tam$LogFrec<- c(13.059990, 12.126147, 11.293301, 10.503340, 9.970819, 9.947122, 7.256297, 3.295837, 0, 3.806662)
Tam<- cbind(Tam, "LogClass" = log(c(it)))

p<- as.vector(Prf$Freq)
Prf<- cbind(Prf, "LogFrec" = log(c(p)))
Prf<- cbind(Prf, "LogClas" = log(c(ip)))


cor.test(Tam$LogClass, Tam$LogFrec)
cor.test(Prf$LogClas, Prf$LogFrec)


#library(extrafont)
loadfonts(device = "win", quiet = TRUE) 
#Tend1<- supsmu(Tam$LogClass, Tam$LogFrec, bass = 10)
plot(Tam$LogClass, Tam$LogFrec, 
     cex.lab = 1,
     cex.axis = 0.9,
     cex=0.9,
     pch = 16, 
     ylim=  c(0,15),
     las =1 ,
     xlab= "Log Species of size", 
     ylab = "Log Representation frequency ", 
     family = "Times New Roman", 
     col = "navyblue")
lines(Tend1, col= "Red", lty = 2, lwd = 2)
text(6, 14, expression("r =-0,79"), family = "Times New Roman")


#Tend2<- supsmu(Prf$LogClas, Prf$LogFrec, bass = 12)
myplot<-ggplot(Prf$LogClas,Prf$LogFrec,
               cex.lab = 1,
               cex.axis = 0.9,
               cex=0.9,
               pch = 16, 
               ylim=  c(0,15),
               las =1, 
               xlab= "Log Habitat depth", 
               ylab = "Log Representation frequency ", 
               family = "Times New Roman", 
               col = "navyblue") +
  lines(Tend2, col= "Red", lty = 3, lwd = 2)
#text(8.5, 14, expression("r= -0,72***"), family = "Times New Roman")

pie(imp$Frequency, imp$Importance,
    family = "Times New Roman", 
    col = color) 


T<-ggplot (Tam, aes(x= LogClass, y = LogFrec))+
  xlab("Log Representation frequency ") + ylab("Log Species of size")+
  geom_point(size=3,  colour = "blue") +
  geom_smooth(method="lm", colour = "red", se =FALSE)+
  theme(panel.grid = element_line(colour= "grey87"))+
  theme(panel.background = element_rect(fill = 'white'))+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
  annotate(geom = 'text', label = 'r = -0,79', x = 6.4, y = Inf, hjust = 0, vjust = 1)
#   geom_text(data = NULL, x = Inf, y = Inf, label = "r =-0,79")


P<-ggplot (Prf, aes(x= LogClas, y = LogFrec))+
  xlab("Log Representation frequency ") + ylab("Log Habitat depth")+
  geom_point(size=3,  colour = "blue") +
  geom_smooth(method="lm", colour = "red", se =FALSE)+
  theme(panel.grid = element_line(colour= "grey87"))+
  theme(panel.background = element_rect(fill = 'white'))+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
  annotate(geom = 'text', label = 'r = -0,83', x = 8.5, y = Inf, hjust = 0, vjust = 1)  
#geom_text( x = 8.3, y = 13, label = "r = -0.83", )
par(mfrow=c(2,2)) 

ggplot(imp,aes(x="",y= Porcentaje, fill=Importance))+
  geom_bar(stat = "identity", width = 1)+
  coord_polar(theta="y", start = 0)+
  theme(panel.background = element_rect(fill = 'white'))+
  scale_fill_brewer(palette = "Pastel2")+
  theme_void()



