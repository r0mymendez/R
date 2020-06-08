rm(list=ls())

library(ggplot2)
library(tidyverse)
library(animation)
library(showtext)
library(ggpubr)
library(ggrepel)

df=read.csv('/Users/rominamendez/Downloads/datasets_674388_1186156_tv_shows.csv')

df_=df%>%
  gather(key='company',value='cant',c('Netflix','Hulu','Prime.Video','Disney.'))%>%
  filter(cant>0,Year>2010)%>%
  select(-c('type','cant','Rotten.Tomatoes','X'))

max.year=max(df_$Year)
min.year=min(df_$Year)

font_add_google("Gochi Hand", "gochi")
font_add_google("Schoolbell", "bell")
showtext_auto()

setwd('/Users/rominamendez/Downloads/')
img_a <- png::readPNG("netflix.png") 
netflix<- grid::rasterGrob(img_a, interpolate = T) 

img_a <- png::readPNG("primevideo.png") 
primevideo<- grid::rasterGrob(img_a, interpolate = T) 

img_a <- png::readPNG("hulu.png") 
hulu<- grid::rasterGrob(img_a, interpolate = T) 


img=list('Netflix'=netflix,
         'Hulu'=hulu,
         'Prime.Video'=primevideo)


invisible(
  saveGIF({
    
for (i in min.year:max.year){
  df_temp=df_%>%
    filter(Year==i)%>%
    count(company)%>%
    top_n(1,n)%>%
    top_n(1,company)
  
 g=
   ggplot(df_%>%filter(Year == i),
       aes(x=company,fill=company))+
  geom_bar(color='#2a2a2a',show.legend = F) +
  scale_fill_manual(
    breaks = df_$company,
    values =c(
        'Netflix'='#E50914',
        'Hulu'='#3DBB3D',
        'Prime.Video'='#00A8E1',
        'Disney.'='#3081C3')
)+
  labs(x='',y='qty',
       title = 'TV shows en Plataformas',
       subtitle = paste0('Año: ',i))+
  theme_bw()+
  theme(plot.background = element_rect(fill='#2a2a2a',color='#2a2a2a'),
        panel.background = element_rect(fill='#2a2a2a',color='#2a2a2a'),
        axis.title = element_text(color='white',size=24,face = "bold",family='bell'),
        axis.text = element_text(color='white',size=18,family='gochi'),
        panel.grid.major=element_line(size=0.3,linetype = 2
                                      ,colour="grey"),
        panel.grid.minor =element_line(size=0.3,linetype = 2
                                  ,colour="#2a2a2a"),
        plot.subtitle = element_text(hjust = 0.5,size=24,
                                     color='white',family='bell'),
        plot.title  = element_text(hjust = 0.5,size = 30,face="bold",
                                   color='white',family='bell')
        )+
        ylim(0,340)+
        annotation_custom(img[[df_temp$company]], xmin = 0, xmax = 5,
                     ymin = 280, ymax =350)  
 p3 <- 
   ggplot(data = NULL, aes(x = min.year:max.year , y = 1)) +
   geom_line() +
   geom_point(aes(fill = (x = min.year:max.year > i)), shape = 21, size = 5) +
   theme_void() +
   theme(legend.position = "none") +
   scale_fill_manual(values = c("#b2d1e0","white")) +
   geom_text(aes(x = i, y = 1, label = i), vjust = -1, size = 8,
             color='white',family='bell') +
   theme(panel.background = element_rect(fill = "#2a2a2a",
                                         colour = "#2a2a2a"))+
   theme(plot.background = element_rect(fill='#2a2a2a',color = 'black'))  
 
 print(ggarrange(g,p3,nrow = 2,ncol = 1,heights =  c(1.4,0.3)))
  
}

  })
)


