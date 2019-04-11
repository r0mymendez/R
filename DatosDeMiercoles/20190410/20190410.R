pacman::p_load(jpeg, png, ggplot2, grid, neuropsychology)
library(tidyverse)
library(emojifont)

partidos_fifa_copa_mundial_procesado <- 
  readr::read_delim("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-10/partidos.txt",delim = "\t")

df=partidos_fifa_copa_mundial_procesado

dfp=rbind(df%>%select(anio,anfitrion,
                      equipo_1,goles=equipo_1_final)%>%
                      mutate(anfitrion=ifelse(anfitrion==equipo_1,1,0))%>%
                      select(anio,anfitrion,
                              equipo=equipo_1,goles),
          df%>%select(anio,anfitrion,
                      equipo_2,goles=equipo_2_final)%>%
            mutate(anfitrion=ifelse(anfitrion==equipo_2,1,0))%>%
            select(anio,anfitrion,
                   equipo=equipo_2,goles))

dfp=dfp%>%filter(anfitrion==1)%>%group_by(equipo)%>%summarise(totgol=sum(goles))%>%
  top_n(10,totgol)

imgage <- jpeg::readJPEG("cancha-dimensiones-futbol.jpg")

dfp%>%
  mutate(equipo = reorder(equipo, totgol)) %>%
ggplot( aes(equipo, totgol, fill = equipo))+
  labs(
    title=paste(emoji("soccer"),"EQUIPOS ANFITRIONES CON MAS GOLES",emoji("soccer"), 
                collapse=" "),
         subtitle="Partidos de las Copas del Mundo de Fútbol (1930 a 2018)", 
           x='',y='goles', 
        caption=paste0(emoji('point_right'),' @r0mymendez')) +
  annotation_custom(rasterGrob(imgage, 
                               width = unit(1,"npc"), 
                               height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
  geom_col(stat="identity", position = "dodge", width = .75, colour = 'white',
           alpha=0.85,show.legend = F)   +
  scale_fill_viridis(discrete = TRUE)+
   coord_flip()+
  geom_text(aes(label = round(totgol)), size = 4, fontface = 2, 
            colour = 'white', hjust = 1.6, vjust = 1) +
 theme(
    plot.title = element_text(color = "#3f704d", hjust =0.4,
                              size = 18, face = "bold"),
    plot.caption = element_text( face = "italic",hjust = 0,size=8),
    plot.background = element_rect(fill = "#f7f7f7"),
    plot.subtitle = element_text(color = "#3f704d", hjust =0.4,
                                 size = 10 ))
  


