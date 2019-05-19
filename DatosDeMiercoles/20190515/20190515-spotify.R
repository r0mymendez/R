#install.packages('httpuv')

rm(list=ls())
library(Rspotify)
library(tidyverse)
library(grid)
library(gridExtra)
library(ggpubr)
library(ggrepel)



keys <- spotifyOAuth("app", "client", "key")

paises_es <- c("Argentina", "Bolivia", "Chile", "Colombia", "Costa Rica",
               "Cuba","la Republica Dominicana", "Dominican Republic",
               "Ecuador", "El Salvador", "Equatorial Guinea", "España",
               "Guatemala", "Honduras", "México", "Nicaragua", "Panamá",
               "Paraguay", "Perú", "Puerto Rico", "Uruguay", "Venezuela")
user_playlists_1 <- getPlaylists("qn9el801z6l32l2whymqqs18p", token = keys)
user_playlists_2 <- getPlaylists("qn9el801z6l32l2whymqqs18p", 50, token = keys)
tops_50 <- rbind(user_playlists_1, user_playlists_2)
# encontré aparte el de venezuela que no estaba incluido
tops_50 <- rbind(tops_50, c("624oAiyjMdmpdJWIylharU", "El Top 50 de Venezuela", "suo2sbl91eeth3elwrfuq7qwn", 50))

paises <- purrr::map_chr(tops_50$name, ~ str_remove(.x, "El Top 50 de "))
bool_es <- purrr::map_lgl(paises, ~ .x %in% paises_es)
tops_50_es <- tops_50[bool_es, ]

viralcharts_user = "qn9el801z6l32l2whymqqs18p"

canciones_tops50_es <- purrr::map(tops_50_es$id[-length(tops_50_es$id)],
                                  ~ getPlaylistSongs(user_id = viralcharts_user,
                                                     .x,
                                                     token = keys))
canciones_tops50_es[[18]] <- getPlaylistSongs(user_id = "suo2sbl91eeth3elwrfuq7qwn",
                                              "624oAiyjMdmpdJWIylharU",
                                              token = keys)

dataset_canciones = tibble()
for (i in 1:length(canciones_tops50_es)) {
  dataset_canciones = rbind(dataset_canciones, cbind(canciones_tops50_es[[i]],
                                                     top = as.character(tops_50_es$name)[i],
                                                     numero = 1:nrow(canciones_tops50_es[[i]])))
}
features_canciones = tibble()
for (j in 1:nrow(dataset_canciones)) {
  features_canciones = rbind(features_canciones,
                             getFeatures(dataset_canciones$id[j], keys))
}


dataset_spotify = cbind(dataset_canciones, features_canciones)
fechas = purrr::map(unique(dataset_spotify$album_id), ~getAlbumInfo(.x, keys)[1, 6])
album_fechas =  tibble(album_id = unique(dataset_spotify$album_id),
                       fecha = as.character(unlist(fechas)))
dataset_spotify = dataset_spotify[, -2] %>%
  left_join(album_fechas, by = "album_id")

dataset_spotify = dataset_spotify %>%
  select(-id, -artist_id, - album_id, -uri, -analysis_url)

nombres_columnas = c("cancion", "popularidad", "artista", "artista_completo",
                     "album", "top_pais", "puesto", "bailabilidad", "energia",
                     "nota_musical", "volumen", "modo", "hablado", "acustico",
                     "instrumental","en_vivo", "positividad", "tempo",
                     "duracion", "tiempo_compas", "fecha")
colnames(dataset_spotify) <- nombres_columnas


df.tracks=dataset_spotify%>%count(artista)%>%top_n(10,n)
df.tracks$x=1
df.tracks$y=seq(0,4.5,by=0.5) 
df.tracks$desc=paste0(emoji('musical_note'),' ',
                      df.tracks$artista)
df.tracks$color=hue_pal()(10)

p0=df.tracks%>%mutate(artista=reorder(artista,n))%>%
  ggplot(aes(x=artista,y=n,fill=artista)) +
  geom_col(show.legend = F) +
  scale_fill_manual(breaks =df.tracks$artista,
                    values =c(
                      "Anuel Aa" ="#F8766D",
                      "Bad Bunny"="#D89000",
                      "Becky G"="#A3A500",
                      "Daddy Yankee"=  "#39B600", 
                      "J Balvin"=   "#00BF7D",     
                      "Karol G"  ="#00BFC4",       
                      "Ozuna"=  "#00B0F6" ,         
                      "Paulo Londra"=  "#9590FF", 
                      "Piso 21"=    "#E76BF3",      
                      "Sebastian Yatra"="#FF62BC"
                    ))+
  geom_label_repel(aes(label = paste0(desc,' (',n,')')),color='black',
                   data = df.tracks,  size = 4,  fill ='#fffeea',
                   family="Atma Light" ,
                   box.padding = unit(0.35, "lines"),
                   point.padding = unit(0.3, "lines"),
                   show.legend = F,fontface = 'bold',
                   hjust=0,vjust=0,
                   segment.size = 0,y = -500
  ) +
  coord_flip() +
  labs(x='',y='',title = 'Artistas mas populares')+
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank() ,
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    plot.background = element_rect(fill='#2a2a2a',color ='#2a2a2a' ),
    panel.background =element_rect(fill='#2a2a2a',color ='white' ),
    panel.spacing = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(family = "Atma Light",
                              hjust = 0,size=16,colour = 'white',
                              face = 'bold')
    
  )

p0
pop=arg%>%top_n(10,popularidad)
pop$id=seq_len(nrow(pop))
pop$cancion[12]="I Don't Care"

setwd('/home/romimendez/Romi/Proyectos/miercolesdata/20190514-spotify')
img_a <- png::readPNG("listen-on-spotify-logo.png") 
a <- grid::rasterGrob(img_a, interpolate = T) 


p1=  
  pop%>%mutate(cancion=reorder(cancion,puesto))%>%
  ggplot()+
  geom_col(aes(x=cancion,y=bailabilidad,fill=artista),
           position = "dodge",color='#2a2a2a',show.legend = F)+
  scale_fill_manual(breaks =df.tracks$artista,
                    values =c(
                      "Anuel Aa" ="#F8766D",
                      "Bad Bunny"="#D89000",
                      "Becky G"="#A3A500",
                      "Daddy Yankee"=  "#39B600", 
                      "J Balvin"=   "#00BF7D",     
                      "Karol G"  ="#00BFC4",       
                      "Ozuna"=  "#00B0F6" ,         
                      "Paulo Londra"=  "#9590FF", 
                      "Piso 21"=    "#E76BF3",      
                      "Sebastian Yatra"="#FF62BC",
                      "Sech"=      "#E9842C",    
                      "ROSALÍA"=  "#00A7FF" ,      
                      "Nicky Jam"=  "#7F96FF",  
                      "DJ Luian"=  "#BC81FF"  ,    
                      "Maluma"=  "#FF8000"     , 
                      "Pedro Capó"= "#21CD11",     
                      "Billie Eilish"="#FF62BF",
                      "Ed Sheeran"="#F5D033"
                    ))  +
  geom_point(aes(x=id,y=energia),color='blue',show.legend = F) +
  geom_line(aes(x=id,y=energia),color='blue',show.legend = F) +
  annotation_custom(a, xmin = 0.5, xmax = 22.5,
                    ymin = 0, ymax =0.2) +
     geom_label_repel(aes( x=cancion,y=bailabilidad,
                           label =paste0(puesto,' puesto')),
                   data = pop,  size = 4.5,  fill ='#fffeea',
                   family="Atma Light" ,
                   label.padding = 0.3,fontface = 'bold',
                   show.legend = F,
                   box.padding = unit(0.5, "lines"),
                   segment.color='white',
                   point.padding = unit(0.5, "lines"))    +
  geom_point(aes(x=id,y=hablado),color='white',show.legend = F)+
  geom_line(aes(x=id,y=hablado),color='white',show.legend = F) +
  labs(x='',y='bailabilidad',title='Top 10 en popularidad de canciones',
       subtitle =paste0('Ref: blanco ',emoji('arrow_right'),' hablado',
                  ' ,azul '   ,emoji('arrow_right'),' energia')
       )+
  theme_bw()+   
  theme(legend.position = 'top', 
        legend.spacing.x = unit(0.41, 'cm'),
        legend.text = element_text(margin = margin(t = 10)),
        axis.text.x = element_text(angle = 45, hjust = 0.5))+
  theme(plot.background = element_rect(fill='#2a2a2a',color ='#2a2a2a' ),
        legend.background = element_rect(fill='#2a2a2a',color ='#2a2a2a' ),
        axis.text =  element_text(colour='white'),
        axis.text.x = element_text(angle = 50,hjust = 1),
        axis.title = element_text(colour='white', family="Atma Light"),
        legend.title =  element_text(colour='white'),
        plot.title =  element_text(colour = 'white',size=16,
                                   family="Atma Light",face = 'bold'),
        plot.subtitle =  element_text(colour = 'white',size=12,hjust = 0.5,
                                   family="Atma Light",face = 'bold'),
        panel.background = element_rect(fill='#2a2a2a',color ='#2a2a2a' ))

  ggarrange(
   p0,p1,
    ncol = 2, nrow = 1,widths = c(0.4,1.4))+
    labs(caption='@r0mymendez    \n   ',title=paste0(emoji('musical_score'),'SPOTIFY ARGENTINA RANKING'),
         subtitle = ' ')+
    theme(plot.background = element_rect(fill='#2a2a2a',color ='#2a2a2a'),
          plot.caption = element_text(colour = 'white',size = 10,hjust = 1),
          plot.title  = element_text(colour = 'white',size = 40,hjust = 0.5,
                                     family ="Atma SemiBold" ))
  
  