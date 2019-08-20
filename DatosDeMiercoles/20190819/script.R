rm(list=ls())

libertad <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-08-14/libertad.csv")


library(tidyverse)
library(grid)
library(gridExtra)
library(ggpubr)
library(ggrepel)
library(extrafont)
library(countrycode)
library(viridis)
library(viridisLite)


country=countrycode::codelist
country=country%>%select(genc3c,country.name.en)
country$genc3c[country$genc3c=='BRB']='BRD'
country$country.name.en[country$country.name.en=='United States']='USA'
country$country.name.en[country$country.name.en=='Hong Kong SAR China']='Hong Kong'

world_map <- map_data("world")
world_map$region[world_map$subregion=='Hong Kong']='Hong Kong'


dfp=libertad%>%
  left_join(country, by=c('codigo_iso'='genc3c'))%>%
  left_join(world_map, by=c('country.name.en'='region'))%>%
  filter(anio==max(libertad$anio))




menor_lib=libertad%>%filter(anio==2016)%>%top_n(-5,libertad_humana_puntaje)%>%
  select(pais,libertad_humana_puntaje)%>%
  arrange(libertad_humana_puntaje)

mayor_lib=libertad%>%filter(anio==2016)%>%top_n(5,libertad_humana_puntaje)%>%
  select(pais,libertad_humana_puntaje)%>%
  arrange(libertad_humana_puntaje)

dfp_menos=dfp%>%
  filter(pais %in% menor_lib$pais)%>%group_by(pais)%>%
  summarise(lat=mean(lat,na.rm = T),
            long=mean(long,na.rm = T))%>%
  left_join(menor_lib)
 
dfp_mayor=dfp%>%
  filter(pais %in% mayor_lib$pais)%>%group_by(pais)%>%
  summarise(lat=mean(lat,na.rm = T),
            long=mean(long,na.rm = T))%>%
  left_join(mayor_lib)

df_leyenda=rbind(dfp_mayor,dfp_menos)



vmax <- max(dfp$libertad_humana_puntaje, na.rm=T)
vmin <- min(dfp$libertad_humana_puntaje, na.rm=T)
ggplot() +
  geom_polygon(data=world_map, aes(x = long, y = lat, group = group),fill='grey')+
  geom_polygon(data=dfp, aes(x = long, y = lat, group = group,fill=libertad_humana_puntaje))+
  scale_fill_viridis(name="Human freedom", begin = 0, end = 1,
                     limits = c(vmin,vmax), na.value="gray99") +
  geom_label_repel(aes(label = paste0(pais,'\n',libertad_humana_puntaje),
                       x=long,
                       y=lat),
                   data = dfp_menos,  
                   size = 4,  fill ='#440154FF',
                   family="Atma Light" ,
                   color='white',fontface = 'bold',
                   box.padding = unit(0.35, "lines"),
                   point.padding = unit(0.3, "lines"))  +
  geom_label_repel(aes(label = paste0(pais,'\n',libertad_humana_puntaje),
                       x=long,
                       y=lat),
                   data = dfp_mayor,  
                   size = 5,  fill ='#e8fa5bff',
                   family="Atma Light" ,
                   color='black',fontface = 'bold',
                   box.padding = unit(0.35, "lines"),
                   point.padding = unit(0.3, "lines"))  +
labs(x='',y='',title = paste0('Human Freedom Index ',emojifont::emoji('earth_americas')),
     caption=paste0('informe año: 2016 ~ ',emojifont::emoji('heart'),' by @r0mymendez'),
     subtitle = '"La libertad humana es un concepto social que reconoce la dignidad de los individuos "')+
  theme_void()+
  theme(
    legend.background = element_rect(fill='#2a2a2a'),
    legend.text = element_text(color='white'),
    plot.background = element_rect(fill='#2a2a2a',color ='#2a2a2a' ),
    panel.background =element_rect(fill='#2a2a2a'),
    legend.position=c(0.11, 0.32),
    legend.key = element_rect(fill = "#2a2a2a", color = NA),
    legend.title = element_text(color = "white", size = 10, hjust = 0.5),
    plot.title = element_text(family =     "Pacifico"    ,
                              hjust = 0.5,
                              size=30,colour = '#73d055ff',
                              face = 'bold'),
    plot.subtitle =  element_text(family =     "Atma Light"     ,
                                  hjust = 0.5,
                                  size=15, face = 'bold',
                                  colour = '#73d055ff' ),
    plot.caption = element_text(family =     "Atma Light"     ,
                                hjust =0.8,
                                size=13, face = 'bold',
                                colour = '#73d055ff' )
  )

  
  
   