capitulos_rladies <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-06-26/capitulos_rladies.csv")


library(tidyverse)
library(grid)
library(gridExtra)
library(ggpubr)
library(ggrepel)
library(extrafont)


df_20=capitulos_rladies%>%top_n(20,miembros)


img_b <- jpeg::readJPEG("1.jpg")
b <- grid::rasterGrob(img_b, interpolate = T) 

  world_map <- map_data("world")
    
    ggplot() +
      geom_polygon(data=world_map, aes(x = long, y = lat, group = group),
                   fill="#2a2a2a", colour = "#c495f0")+
      geom_point(data=capitulos_rladies,aes(x=longitud,
                                            y=latitud,size=miembros
                               ),color='#88398A',show.legend = F) +
    #  annotation_custom(a, xmin =100, xmax =200,
    #                    ymin=50 ,ymax=90
    #  ) +
      annotation_custom(b, xmin =-200, xmax =-150,
                        ymin=-50 ,ymax=-90
      ) +
      geom_label_repel(aes(label=paste0(ciudad,':',miembros),x=longitud,
                           y=latitud),
                       size=3, show.legend = F,
                       data=df_20,fill ='#fffeea',
                       segment.color = 'white')+
      labs(x='',y='',title = paste0('Capitulos de RLadies  ',emojifont::emoji('two_women_holding_hands')),
           subtitle = 'Las 20 ciudades con mas miembros')+
      theme_void()+
      theme(
        legend.background = element_rect(fill='#2a2a2a'),
        legend.text = element_text(color='white'),
        plot.background = element_rect(fill='#2a2a2a',color ='#2a2a2a' ),
        panel.background =element_rect(fill='#2a2a2a'),
        legend.position=c(0.11, 0.32),
        legend.key = element_rect(fill = "#2a2a2a", color = NA),
        legend.title = element_text(color = "white", size = 10, hjust = 0.5),
        plot.title = element_text(family =     "Atma"    ,
                                  hjust = 0.5,
                                  size=30,colour = '#b87fed',
                                  face = 'bold'),
        plot.subtitle =  element_text(family =     "Gill Sans MT"    ,
                                      hjust = 0.5,
                                      size=12,colour = '#ad6aea',
                                      face = 'bold'), 
      )
    
    
    
    
