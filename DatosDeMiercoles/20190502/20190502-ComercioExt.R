library(tidyverse)
library(extrafont)
library(patchwork)

font_import()
font_import(pattern = "KOMIKAX.ttf")
loadfonts(device="win")
loadfonts()
fonts()


comercio_hispanoamerica_mundo <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-05-01/comercio_hispanoamerica_mundo_agregado.csv")
producto=data.frame(
  nombre_comunidad_producto=unique(comercio_hispanoamerica_mundo$nombre_comunidad_producto),
  nombre_corto=unique(comercio_hispanoamerica_mundo$nombre_comunidad_producto),
  stringsAsFactors = F
)


producto$nombre_corto=gsub("Productos de ","",producto$nombre_corto)
producto$nombre_corto=gsub("Productos ","",producto$nombre_corto)
producto$nombre_corto=gsub("Composición ","Comp. ",producto$nombre_corto)



dfexp=comercio_hispanoamerica_mundo%>%
  filter(nombre_pais_origen=='Argentina',anio==2017,valor_exportado_dolares>0)   %>%     
  select(nombre_pais_destino,valor_exportado_dolares,nombre_comunidad_producto)   %>%
  group_by(nombre_comunidad_producto)%>%
  summarise(valor_exportado_dolares=sum(valor_exportado_dolares))%>%
  top_n(10,valor_exportado_dolares)%>%
  mutate(nombre_comunidad_producto=as.factor(nombre_comunidad_producto))%>%
  inner_join(producto)  


paisexp2017=(comercio_hispanoamerica_mundo%>%
               filter(nombre_pais_origen=='Argentina',anio==2017)%>%
               group_by(nombre_pais_destino)%>%
               summarise(valor=sum(valor_exportado_dolares))%>%
               top_n(6,valor)%>%select(nombre_pais_destino))

####################### GRAL #############################
pexp=
  comercio_hispanoamerica_mundo%>%
  filter(nombre_pais_origen=='Argentina',
         anio==2017,
         nombre_pais_destino %in% paisexp2017$nombre_pais_destino)%>%
  select(nombre_pais_destino,valor=valor_exportado_dolares,
         nombre_comunidad_producto)%>%
  inner_join(producto) %>%
  top_n(20,valor)

pais=pexp%>%filter( nombre_corto=="Vegetales")%>%
  top_n(1,valor)%>%
  select(nombre_pais_destino)
pais=pais$nombre_pais_destino  


g_exp=
  ggplot(pexp,
         aes(x=nombre_corto,
             y=valor/1000000,
             fill=nombre_corto))+
  geom_col(show.legend = T,colour = "black")  +
  scale_fill_manual(
    breaks =pexp$nombre_corto,
    values =
      c( 'Alimentos'="#8abdb6",                            
         'Armas'="#1c26b3",                                    
         'Arte y Antiguedades'="#7485aa",                      
         'Artículos de Papel'="#d05555",                       
         'Calzado y Gorras'="#872a41",                         
         'Instrumentos'="#5c57d9",                             
         'Maquinaria'="#d1a1bc",                               
         'Metales'="#a17cb0",                                  
         'Metales Preciosos'="#7454a6",                        
         'Miscelánea'="#4d6fd0",                               
         'Piedras y Cristales'="#993f7b",                      
         'Pieles de Animales'="#d6c650",                       
         'Plásticos y Gomas'="#ede788",                        
         'Animales'="#74c0e2",                       
         'Comp. Vegetal y Animal'="#549e95",
         'Madera'="#dc8e7a",                      
         'Minerales'="#bcd8af",                      
         'Químicos'="#a8c380",                       
         'Vegetales'="#406662",                      
         'Sin Especificar'="#635b56",                          
         'Textiles'="#bf3251",                                 
         'Transporte'="#a1aafb" )
  ) +
  coord_flip()+
  coord_polar()  +
  annotate("text", x = 11, y = 4006.8, 
           label =
             paste0(pais,' es el pais que\nrecibe mas exp. Arg'),
           family = "AR CENA",
           size = 4, hjust = 0,
           color='#2a2a2a',face="bold") +
  annotate("segment", x = 10.8, y =  3806.8, xend = 10, 
           yend = 3806.8, colour = "#a1aafb",
           size=1.5,
           arrow = arrow(length = unit(0.5, "cm")))+
  guides(fill=guide_legend(title=""))+
  #facet_wrap(.~nombre_pais_destino) +
  labs(x='',y='',title='Categorias de productos exportados') +
  theme(  #axis.text.x = element_text(face = 'bold',size=10,color='#2a2a2a')
    legend.text = element_text(size=12,family = "AR CENA")
    ,axis.text.x = element_blank()
    , axis.ticks = element_blank() 
    , axis.text.y = element_blank()
    , panel.grid.major=element_line(size=0.3,linetype = 2
                                    ,colour="#D3D3D3")
    , panel.background = element_rect(fill = 'white')
    , plot.background =  element_rect(fill ='white')
    , axis.line = element_blank()
    , plot.title=  element_text(color="#2a2a2a",size=15,family = "AR CENA")
    , strip.background =element_rect(fill="#8abdb6")
    , strip.text = element_text(colour = '#2a2a2a')
  ) 
g_exp

######################## POR PAIS ######################

p_exp=
  ggplot(pexp,
         aes(x=nombre_corto,
             y=valor,
             fill=nombre_corto))+
  geom_col(show.legend = F,colour = "black")  +
  scale_fill_manual(
    breaks =pexp$nombre_corto,
    values =
      c( 'Alimentos'="#8abdb6",                            
         'Armas'="#1c26b3",                                    
         'Arte y Antiguedades'="#7485aa",                      
         'Artículos de Papel'="#d05555",                       
         'Calzado y Gorras'="#872a41",                         
         'Instrumentos'="#5c57d9",                             
         'Maquinaria'="#d1a1bc",                               
         'Metales'="#a17cb0",                                  
         'Metales Preciosos'="#7454a6",                        
         'Miscelánea'="#4d6fd0",                               
         'Piedras y Cristales'="#993f7b",                      
         'Pieles de Animales'="#d6c650",                       
         'Plásticos y Gomas'="#ede788",                        
         'Animales'="#74c0e2",                       
         'Comp. Vegetal y Animal'="#549e95",
         'Madera'="#dc8e7a",                      
         'Minerales'="#bcd8af",                      
         'Químicos'="#a8c380",                       
         'Vegetales'="#406662",                      
         'Sin Especificar'="#635b56",                          
         'Textiles'="#bf3251",                                 
         'Transporte'="#a1aafb" )
  ) +
  coord_flip()+
  coord_polar()+
  facet_wrap(.~nombre_pais_destino,ncol=3,nrow = 2) +
  labs(x='',y='',title='Principales exportaciones por pais') +
  theme(    axis.text.x = element_blank()#element_text(face = 'bold',size=6)
            , axis.ticks = element_blank() 
            , axis.text.y = element_blank()
            , panel.grid.major=element_line(size=0.3,
                                            linetype = 2,
                                            colour="#D3D3D3")
            , panel.background = element_rect(fill = 'white')
            , plot.background =  element_rect(fill ='white')
            , plot.title=  element_text(color="#2a2a2a",size=15,family = "AR CENA")
            , axis.line = element_blank()
            , legend.title=element_blank()
            #, panel.border = element_rect(color = "black")
            , strip.background =element_rect(fill="#406662",color = "black")
            , strip.text = element_text(colour = 'white',face = 'bold',size=14,family = "AR CENA")
  ) 
p_exp

#################### GRAFICA #######################
wrap_plots(g_exp + p_exp,
           heights  =  c(3, 0.8))+
  plot_annotation(title = "Exportaciones Argentinas 2017",
                  subtitle = "Los siguientes datos corresponden a las exportaciones realizada en el año 2017 por la Republica Argentina, adicionalmente se graficaron los seis paises 
                  tienen mayor volumen de exportaciones en dolares.",
                  caption = "Data:  Open Trade Statistics. | Visualizacion: @r0mymendez",
                  theme = theme(
                    plot.title=  element_text(color="#2a2a2a",size=24,family = "Komika Axis"),
                    plot.subtitle=element_text(color="#2a2a2a",size=14,family = "AR CENA"))
  )

ggsave("exportacion.png")







