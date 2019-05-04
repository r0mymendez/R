library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

comercio_hispanoamerica_mundo <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-05-01/comercio_hispanoamerica_mundo_agregado.csv")
dfimp=comercio_hispanoamerica_mundo%>%
        filter(nombre_pais_destino=='Argentina',anio==2017,valor_importado_dolares>0)%>%
        select(nombre_pais_origen,valor_importado_dolares,nombre_comunidad_producto, color_comunidad_producto)%>%
        group_by(nombre_comunidad_producto)%>%
        summarise(valor_importado_dolares=sum(valor_importado_dolares))%>%
        top_n(10,valor_importado_dolares)%>%
        mutate(nombre_comunidad_producto=as.factor(nombre_comunidad_producto))


dfexp=comercio_hispanoamerica_mundo%>%
  filter(nombre_pais_origen=='Argentina',anio==2017,valor_exportado_dolares>0)   %>%     
  select(nombre_pais_destino,valor_exportado_dolares,nombre_comunidad_producto)   %>%
  group_by(nombre_comunidad_producto)%>%
  summarise(valor_exportado_dolares=sum(valor_exportado_dolares))%>%
  top_n(10,valor_exportado_dolares)%>%
  mutate(nombre_comunidad_producto=as.factor(nombre_comunidad_producto))


pimp=dfimp%>%group_by(nombre_pais_origen)%>%
  summarise(valor=sum(valor_importado_dolares))%>%
  top_n(5,valor)

pexp=dfexp%>%group_by(nombre_pais_destino)%>%
  summarise(valor=sum(valor_exportado_dolares))%>%
  top_n(5,valor)

dfexp=dfexp%>%filter(nombre_pais_destino %in% pexp$nombre_pais_destino)
dfimp=dfimp%>%filter(nombre_pais_origen %in% pimp$nombre_pais_origen)



ggplot(dfexp,
           aes(x=nombre_comunidad_producto,
               y=valor_exportado_dolares,
               fill=nombre_comunidad_producto))+
  geom_col(show.legend = T,colour = "black")  +
  scale_fill_manual(
    breaks =dfexp$nombre_comunidad_producto,
    values =c('Alimentos'="#8abdb6",                            
                    'Maquinaria'="#d1a1bc",                               
                    'Metales'="#a17cb0",                                  
                    'Metales Preciosos'="#7454a6",                        
                    'Productos Animales'="#74c0e2",                       
                    'Productos de Composición Vegetal y Animal'="#549e95",
                    'Productos Minerales'="#bcd8af",                      
                    'Productos Químicos'="#a8c380",                       
                    'Productos Vegetales'="#406662",                      
                    'Transporte'="#a1aafb")) +
  coord_flip()+
  coord_polar()+
  labs(x='',y='',title='Exportaciones Argentina 2017')+
  theme(  axis.text.x = element_blank()
        , axis.ticks = element_blank() 
        , panel.grid.major.x = element_line(colour = 'black')
        , axis.text.y = element_blank()
        , panel.grid.major=element_line(size=0.3,linetype = 2,colour="grey")
        , panel.background = element_rect(fill = 'white')
        , plot.background =  element_rect(fill = 'white')
        )
 


#col=c( Alimentos="#8abdb6",                            
#       Armas="#1c26b3",                                    
#       Arte y Antiguedades="#7485aa",                      
#       Artículos de Papel="#d05555",                       
#       Calzado y Gorras="#872a41",                         
#       Instrumentos="#5c57d9",                             
#       Maquinaria="#d1a1bc",                               
#       Metales="#a17cb0",                                  
#       Metales Preciosos="#7454a6",                        
#       Miscelánea="#4d6fd0",                               
#       Piedras y Cristales="#993f7b",                      
#       Pieles de Animales="#d6c650",                       
#       Plásticos y Gomas="#ede788",                        
#       Productos Animales="#74c0e2",                       
#       Productos de Composición Vegetal y Animal="#549e95",
#       Productos de Madera="#dc8e7a",                      
#       Productos Minerales="#bcd8af",                      
#       Productos Químicos="#a8c380",                       
#       Productos Vegetales="#406662",                      
#       Sin Especificar="#635b56",                          
#       Textiles="#bf3251",                                 
#       Transporte="#a1aafb" )


ggplot(dfimp,
       aes(x=nombre_comunidad_producto,
           y=valor_importado_dolares,
           fill=nombre_comunidad_producto))+
  geom_col(show.legend = T,colour = "black")  +
  scale_fill_manual(
    breaks =dfexp$nombre_comunidad_producto,
    values =c('Alimentos'="#8abdb6",                            
              'Maquinaria'="#d1a1bc",                               
              'Metales'="#a17cb0",      
              'Metales Preciosos'="#7454a6",  
              'Plásticos y Gomas'="#ede788",  
              'Productos Animales'="#74c0e2",                       
              'Productos de Composición Vegetal y Animal'="#549e95",
              'Productos Minerales'="#bcd8af",                      
              'Productos Químicos'="#a8c380",                       
              'Productos Vegetales'="#406662",                      
              'Transporte'="#a1aafb")) +
  coord_flip()+
  coord_polar()+
  labs(x='',y='',title='Importaciones Argentina 2017')+
  theme(  axis.text.x = element_blank()
          , axis.ticks = element_blank() 
          , panel.grid.major.x = element_line(colour = 'black')
          , axis.text.y = element_blank()
          , panel.grid.major=element_line(size=0.3,linetype = 2,colour="grey")
          , panel.background = element_rect(fill = 'white')
          , plot.background =  element_rect(fill = 'white')
          , legend.title=element_blank()
  )





