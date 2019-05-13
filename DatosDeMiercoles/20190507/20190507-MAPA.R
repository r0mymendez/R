rm(list=ls())


library(ggrepel)
library(ggthemes)
library(emojifont)
library(htmltab)
library(ggplot2)
library(dplyr)
require(maps)
require(viridis)

datos_uip <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-05-08/datos_uip.csv")

dfp=datos_uip
dfp$porcentaje_mujeres=ifelse(is.na(dfp$porcentaje_mujeres),0,dfp$porcentaje_mujeres)
dfp$cuota_genero=ifelse(is.na(dfp$cuota_genero),0,dfp$cuota_genero)
dfp$edad_elegibilidad=ifelse(is.na(dfp$edad_elegibilidad),0,dfp$edad_elegibilidad)
dfp$integrante_mas_joven=ifelse(is.na(dfp$integrante_mas_joven),0,dfp$integrante_mas_joven)
dfp$numero_integrantes=ifelse(is.na(dfp$numero_integrantes),0,dfp$numero_integrantes)

mapa <- map_data("world", region =dfp$countries)
mapa=mapa%>%filter(!(subregion %in% c('Isla Isabela','Isla San Cristobal',
                                      'Isla Santa Cruz','Isla Fernandina',
                                      'Isla San Salvador','Isla Santa Maria',
                                      'Easter Island')))


region.lab.data <- mapa %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))


dfp1=dfp%>%left_join(region.lab.data, by=c('countries'='region'))

alta=dfp1%>%filter(camara=='alta')

ggplot(mapa, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = group),colour='white')+
  geom_label_repel(aes(label = paste0(countries,'\n',porcentaje_mujeres,' %')),
                data = alta,  size = 4,  fill ='#fffeea',
                family="Atma Light" ,
                box.padding = unit(0.35, "lines"),
                point.padding = unit(0.3, "lines"))  +
  labs(title=paste0('Porcentaje de mujeres en la Camara Alta  ',emoji('womens')),
       subtitle = '\nAnalisis en America del sur de la participacion de mujeres en la camara alta del parlamento',
       caption=paste0(emoji('bird'),' @r0mymendez'))+
  annotate("rect", xmin = -60, xmax = -40, ymin =-55, ymax = -45, fill="#fffeea", colour="#a2a2a2") +
  annotate("text", label = paste0("El pais con mas alta \nparticipacion de mujeres es ",alta$País[which.max(alta$porcentaje_mujeres)] ),
            x = -50, y = -50, size = 5,  fill ='#fffeea',  family="Atma Light",
           colour ="black") +
  theme_economist() +
  theme(plot.title = element_text(family='Atma LIght',size='30',hjust=0.5),
        plot.subtitle = element_text(family='Atma LIght',size='15',hjust=0.5),
        plot.caption = element_text(family='Atma LIght',size='18',hjust=1,face = 'bold'),
        legend.position = "none")+ 
 scale_fill_viridis_c(option = 'D')  


