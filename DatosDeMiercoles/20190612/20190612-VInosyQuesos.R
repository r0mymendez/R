rm(list=ls())
library(tidyverse)
library(emojifont)
library(ggplot2)
library(ggrepel)


#Informacion sobre quesos y vinos: 
#https://www.bonvivir.com/2013/01/17/quesos-vinos/
#https://www.vix.com/es/imj/gourmet/2008/03/26/guia-de-vinos-y-quesos
#https://vinepair.com/wine-blog/an-illustrated-guide-to-pairing-wine-and-cheese/


vinos <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-06-12/vinos.csv")
queso <- read_csv('queso.csv')
variedad=vinos%>%filter(is.na(variedad)==F,is.na(precio)==F)%>%
  group_by(variedad)%>%summarise(promedio=mean(precio,na.rm = T),
                                                cantidad=n())
queso$Queso=tolower(queso$Queso)
queso$Vino=tolower(queso$Vino)
queso=unique(queso)

variedad$variedad=tolower(variedad$variedad)
variedad$Cabernet_Sauvignon=ifelse(str_detect(variedad$variedad,"cabernet sauvignon"),1,0)
variedad$Zinfandel=ifelse(str_detect(variedad$variedad,"zinfandel"),1,0)
variedad$Tawnyport=ifelse(str_detect(variedad$variedad,"tawnyport"),1,0)
variedad$Jerez=ifelse(str_detect(variedad$variedad,"jerez"),1,0)
variedad$Chardonnay=ifelse(str_detect(variedad$variedad,"chardonnay"),1,0)
variedad$Sauvignon_Blanc=ifelse(str_detect(variedad$variedad,"sauvignon blanc"),1,0)
variedad$Pinot_Noir=ifelse(str_detect(variedad$variedad,"pinot noir"),1,0)
variedad$Merlot=ifelse(str_detect(variedad$variedad,"merlot"),1,0)
variedad$Gewürztraminer=ifelse(str_detect(variedad$variedad,"gewürztraminer"),1,0)
variedad$Champagne=ifelse(str_detect(variedad$variedad,"champagne"),1,0)
variedad$Barbera=ifelse(str_detect(variedad$variedad,"barbera"),1,0)
variedad$Nebbiolo=ifelse(str_detect(variedad$variedad,"nebbiolo"),1,0)
variedad$Riesling=ifelse(str_detect(variedad$variedad,"riesling"),1,0)
variedad$Chenin_Blanc=ifelse(str_detect(variedad$variedad,"chenin blanc"),1,0)
variedad$Pinot_Gris=ifelse(str_detect(variedad$variedad,"pinot gris"),1,0)         
variedad$malbec=ifelse(str_detect(variedad$variedad,"malbec"),1,0)
variedad$cabernet=ifelse(str_detect(variedad$variedad,"cabernet"),1,0)
variedad$syrah=ifelse(str_detect(variedad$variedad,"syrah"),1,0)
variedad$tinto=ifelse(str_detect(variedad$variedad,"tinto"),1,0)
variedad$prosecco=ifelse(str_detect(variedad$variedad,"prosecco"),1,0)
variedad$viognier=ifelse(str_detect(variedad$variedad,"viognier"),1,0)
variedad$pinot_grigio=ifelse(str_detect(variedad$variedad,"pinot grigio"),1,0)


variedad$total=variedad$Cabernet_Sauvignon+
  variedad$Zinfandel+
  variedad$Tawnyport+
  variedad$Jerez+
  variedad$Chardonnay+
  variedad$Sauvignon_Blanc+
  variedad$Pinot_Noir+
  variedad$Merlot+
  variedad$Gewürztraminer+
  variedad$Champagne+
  variedad$Barbera+
  variedad$Nebbiolo+
  variedad$Riesling+
  variedad$Chenin_Blanc+
  variedad$Pinot_Gris+
  variedad$malbec+
  variedad$cabernet+
  variedad$syrah+
  variedad$tinto+
  variedad$prosecco+
  variedad$viognier+
  variedad$pinot_grigio


variedadq= variedad%>%top_n(20,cantidad)  %>%
    filter(total>0)%>%gather(
                                key='concepto',
                                value='valor',
                                4:25)%>%
  filter(valor==1)%>%
  mutate(concepto=tolower(concepto))%>%
  mutate(concepto=gsub('_',' ',concepto))%>%
  inner_join(queso,by = c('concepto'='Vino'))%>%
  mutate(emoji=emoji('cheese'))


variedad1=vinos%>%filter(!is.na(variedad),!is.na(precio),!is.na(puntos))%>%
  select(variedad,precio,puntos)%>%
  group_by(variedad)%>%
  summarise(n=n(),prom.puntos=round(mean(puntos),0),
                                 prom.precios=round(mean(precio),0))%>%
  top_n(20,n)
variedad1$emoji=emoji("wine_glass")

img_a <- png::readPNG("1.png") 
a <- grid::rasterGrob(img_a, interpolate = T)   

ori=
  ggplot(variedad1, aes(x=variedad, y=n,color=variedad,size=prom.puntos,label=emoji)) +
  geom_segment(aes(x=variedad, xend=variedad, y=0, yend=n,color=variedad),
               show.legend = F,size=0.8) +
  geom_point(show.legend = F,alpha=0.4) +
  geom_text(family='EmojiOne',size=12,show.legend = F)+
  coord_flip()+
  geom_label_repel(aes(label=paste0('$ ',prom.precios)),size=4, show.legend = F,
                   data=variedad1,fill ='#fffeea')+
  labs(y='Cantidad',x='Variedades',caption = paste0(emoji('heart'),'@r0mymendez'))+
  theme(plot.background = element_rect(fill='#2a2a2a',colour = "#2a2a2a"),
        panel.background = element_rect(fill = "#2a2a2a", colour = "#2a2a2a"),
        axis.text = element_text(color='white',
                                 family = "Atma Light",size=15),
        plot.caption = element_text(color='white',size = 14, 
                                    family = "Atma Light",
                                    hjust = 1),
        plot.title = element_text(color='white',family = "Mr Bedfort",size=30,face = 'bold'),
        plot.subtitle = element_text(color = "white",size = 8),
        axis.title = element_text(color = "white",
                                  family = "Atma Light",size = 20),
        panel.grid.major.y = element_line(colour = "#fdfdf3", size = .1, linetype = 2),
        panel.grid.minor.y = element_line(colour = "#2a2a2a", size = .2),
        panel.grid.major.x = element_line(colour = "#fdfdf3", size = .2),
        panel.grid.minor = element_line(colour = "#fdfdf3", size = .2))+
  annotate("text", 
           label = paste0('Las 20 variedades de vino con \nmayor cantidad de reseñas'),
           y = 10000, x = 18, size = 10,
           family= "Atma Light",
           colour ="white")  +
  annotation_custom(a, xmin =0, xmax =10,
                    ymin=10000 ,ymax=15000) 

ori


gq=
  ggplot(variedadq%>%mutate(Queso=paste0(Queso,' ',emoji("black_circle"))),
       aes(x=concepto,y=Queso,fill=concepto,size=promedio*2))+
  geom_point(show.legend = F,shape = 21,alpha=0.8) +
  scale_size_continuous(range = c(1,10))  +
    labs(
         y=paste0('Queso ',emoji('cheese')),
         x=paste0('Variedades ',emoji('wine_glass')),
        title='Quesos y Vinos',
        subtitle = 'Combinaciones de tipos de quesos en base a la variedad de vino',
        caption = paste0(emoji('heart'),'@r0mymendez')
           )+
  theme(plot.background = element_rect(fill='#2a2a2a',colour = "#2a2a2a"),
        panel.background = element_rect(fill = "#2a2a2a", colour = "#2a2a2a"),
        axis.text = element_text(color='white',
                                 family = "Atma Light",size=15),
        plot.caption = element_text(color='white',size = 14, 
                                    family = "Atma Light",
                                    hjust = 1),
        plot.title = element_text(color='white',family = "Mr Bedfort",size=30,face = 'bold'),
        plot.subtitle = element_text(color = "white",size = 10,family = "Mr Bedfort"),
        axis.title = element_text(color = "white",
                                  family = "Atma Light",size = 20),
        axis.text.x = element_text(hjust = 1,angle = 45),
        panel.grid.major.y = element_line(colour = "#fdfdf3", size = .1, linetype = 2),
        panel.grid.minor.y = element_line(colour = "#2a2a2a", size = .2),
        panel.grid.major.x = element_line(colour = "#fdfdf3", size = .2),
        panel.grid.minor = element_line(colour = "#fdfdf3", size = .2))

gq