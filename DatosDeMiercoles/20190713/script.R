rm(list=ls())

library(tidyverse)
library(ggrepel)
library(extrafont)
library(grid)
library(ggpubr)

pokemon <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-07-10/pokemon.csv")

df=pokemon%>%
      filter(nombre_traducido %in% c('Bulbasaur','Charmander','Squirtle'))

df.var=df%>%
  select(nombre_ingles,ataque,defensa,velocidad,fuerza_especial_ataque,
         fuerza_especial_defensa)%>%
  gather(key='caract',value = 'valor',2:6)


#GRAFICO G1
G1=
df.var%>%ggplot(aes(x=caract,y=valor,fill=nombre_ingles))+
  geom_col(position = 'dodge',color='#2a2a2a',show.legend = F)+
  scale_fill_manual(
    breaks =df.var$nombre_ingles,
    values =
      c( 'Charmander'="#FFAF4E",
         'Bulbasaur'='#8CDBA1',
         "Squirtle"='#ACD3EB'))+
  geom_text(aes(label=valor), position=position_dodge(width=0.9), 
            vjust=-0.25,family="Bangers",color='white',
            size=10)+
  ylim(0,80)+
  labs(y='',x='')+
  theme_bw()+
  theme(plot.background = element_rect(color='black',fill='black'),
        panel.background =  element_rect(color='black',fill='black'),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(size=14,family="Bangers",face = 'bold',
                                   color = 'white'))


vect=data.frame(
      x=1:3,y=1:3)

G2=
ggplot(vect,aes(x=x,y=y))+geom_point(color='black')+
  annotate("text", label = paste0('Total' ),
           x = 2, y = 2.6, size = 14, 
           family="Bungee",
           colour ="white") +
  annotate("text", label = paste0(df$nombre_ingles[1],': ',df$total[1] ),
           x = 2, y = 2.0, size = 10, 
           family="Bungee",
           colour ='#8CDBA1') +
  annotate("text", label = paste0(df$nombre_ingles[2],': ',df$total[2] ),
           x = 2, y = 1.6, size = 10, 
           family="Bungee",
           colour ="#FFAF4E") +
  annotate("text", label = paste0(df$nombre_ingles[3],': ',df$total[3] ),
           x = 2, y = 1.2, size = 10, 
           family="Bungee",
           colour ='#ACD3EB') +
  #coord_fixed() +
  scale_x_continuous(breaks = c(1,8,2,2.3,2.6))+
  theme_void()+
  theme(plot.background = element_rect(colour = 'black',fill='black'),
        panel.background = element_rect(colour =  'black',fill='black'))


#GRAFICO G4
setwd('/20190713-pokemon')
imgage <- png::readPNG('2.png')
G4=ggplot()+
  annotation_custom(rasterGrob(imgage, 
                               width = unit(1,"npc"), 
                               height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
  labs(caption = 'by @r0mymendez')+
  theme(plot.background = element_rect(colour = 'black',fill='black'),
        panel.background = element_rect(colour =  'black',fill='black'),
        plot.caption = element_text(color = 'white',size=10)
        )
#G4


ggarrange(
G1,
  (ggarrange(G2,G4,nrow = 1,ncol = 2,
           widths = c(1,1.3))),
 nrow = 2,ncol=1, heights =  c(1,0.7)
)+
  labs(title='Pokemon Data Analytics')+
  theme(plot.title = element_text(size=45,family = "Pokemon Solid"      ,
                                  hjust = 0.5,colour = 'white'),
        plot.background = element_rect(colour = 'black',fill='black'),
        panel.background = element_rect(colour = 'black',fill='black'))

