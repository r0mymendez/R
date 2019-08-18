rm(list=ls())
emperors <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-13/emperors.csv")

# Libraries
library(tidyverse)
library(grid)
library(ggpubr)

imgage1 <- jpeg::readJPEG("8.jpg")



df.as=
  emperors%>%
  count(cause)%>%
  ggplot(aes(x=cause,y=n))+
  annotation_custom(rasterGrob(imgage1, 
                               width = unit(1,"npc"), 
                               height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
  geom_segment( aes(x=cause, xend=cause, y=0, yend=n),show.legend = F) +
  geom_point(aes(size=n),show.legend = F,alpha=0.5,stroke=2,color='#2a2a2a')+
  scale_size(range = c(4, 15))+
  labs(x='',y='')+
  annotate("text", label = 'Cause of death',
           x = 6.5, y = 21, size = 15,   family="Jadyn Maria Free",
           colour ="black") +
  annotate("text", label = '_____________',
           x = 6.5, y = 20.8, size = 10,   family="Jadyn Maria Free",
           colour ="black") +
  theme_bw()+
  theme(plot.background = element_rect(color='#CAA867',fill='#CAA867'),
        panel.background =  element_rect(color='#CAA867',fill='#CAA867'),
        axis.text.y = element_text(size=10,family="Atma Light",face = 'bold',
                                   color = 'black'),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(size=20,family= "Fabiana",face = 'bold',
                                   color = 'black'))



df.as1=
  emperors%>%
  count(killer)%>%
  ggplot(aes(x=killer,y=n))+
  geom_segment( aes(x=killer, xend=killer, y=0, yend=n),show.legend = F) +
  geom_point(aes(size=n),show.legend = F,alpha=0.5,stroke=2,color='#2a2a2a')+
  scale_size(range = c(4, 10))+
  coord_flip()+
  annotate("text", label = 'Killers',
           x = 12, y = 16, size = 15,   family="Jadyn Maria Free",
           colour ="black") +
  theme_bw()+
  labs(x='',y='')+
  theme(plot.background = element_rect(color='#CAA867',fill='#CAA867'),
        panel.background =  element_rect(color='#CAA867',fill='#CAA867'),
        axis.text.x =  element_text(size=10,family="Atma Light",face = 'bold',
                                     color = 'black'),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.text.y = element_text(size=12,family= "Fabiana",
                                   color = 'black'))



ggarrange(df.as,df.as1,  nrow = 2,ncol=1,
 heights =  c(1,0.8))+
  labs(title='Roman Emperors',
       caption = 'by @r0mymendez')  +
  theme(plot.title = element_text(size=45,family = "Fabiana"      ,
                                  hjust = 0.5,colour = 'white'),
        plot.caption = element_text(size=15,family = "Atma"      ,
                                  hjust = 1,colour = 'white'),
        plot.background = element_rect(colour = 'black',fill='black'),
        panel.background = element_rect(colour = 'black',fill='black'))







