rm(list=ls())
ufo_sightings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")

library(tidyverse)
library(grid)
library(gridExtra)
library(ggpubr)
library(ggrepel)
library(extrafont)


df=ufo_sightings%>%count(ufo_shape)%>%filter(is.na(ufo_shape)==F)
df=ufo_sightings%>%
  mutate(year=str_sub(date_time, -10, -7))%>%count(year)%>%
  mutate(year=as.numeric(year))%>%
  filter(as.numeric(year) %in% 1990:2014)


df_ufo=ufo_sightings%>%
  mutate(year=str_sub(date_time, -10, -7))%>%
  mutate(year=as.numeric(year),
         shape=ufo_shape)%>%
  filter(as.numeric(year) %in% 1990:2014)

df_ufo$id=seq_len(nrow(df_ufo))
dfsample=df_ufo%>%filter(df_ufo$id %in% sample(df_ufo$id,10))


img_a <- png::readPNG("1.png") 
a <- grid::rasterGrob(img_a, interpolate = T) 

img_b <- png::readPNG("2.png") 
b <- grid::rasterGrob(img_b, interpolate = T) 

world_map <- map_data("world")
g2=
  ggplot() +
  geom_polygon(data=world_map, aes(x = long, y = lat, group = group),
               fill="#2a2a2a", colour = "#185060")+
  geom_point(data=df_ufo,aes(x=longitude,y=latitude,color=shape))+
  annotation_custom(a, xmin =100, xmax =200,
                    ymin=50 ,ymax=90
                   ) +
  annotation_custom(b, xmin =50, xmax =100,
                    ymin=-50 ,ymax=-90
  ) +
  geom_label_repel(aes(label=described_encounter_length,x=longitude,y=latitude),
                   size=4, show.legend = F,
                   data=dfsample,fill ='#fffeea')+
  labs(x='',y='',title = paste0('UFO Sightings around the world ',emojifont::emoji('alien')),
       subtitle = 'FROM 1990-2014')+
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
                              size=30,colour = '#00BFC4',
                              face = 'bold'),
    plot.subtitle =  element_text(family =     "Atma Light"    ,
                                  hjust = 0.5,
                                  size=15,colour = '#00BFC4',
                                  face = 'bold'), 
  )

g2