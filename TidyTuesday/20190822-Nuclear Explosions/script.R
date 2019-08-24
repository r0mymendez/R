rm(list=ls())
nuclear_explosions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-20/nuclear_explosions.csv")

library(rworldmap)
library(tidyverse)
library(ggplot2)
library(geosphere)
library(gpclib)
library('mapproj')
library(ggrepel)
library(gganimate)
library(ggpubr)
library(animation)


# World map
worldMap <- getMap()
world.points <- fortify(worldMap)
world.points$region <- world.points$id
world.df <- world.points[,c("long","lat","group", "region")]

max.year=max(nuclear_explosions$year)
min.year=min(nuclear_explosions$year)

invisible(
  saveGIF({
    
    
for (i in min.year:max.year){

worldmap <-
  ggplot() + 
  geom_polygon(data=worldMap, aes(x = long, y = lat, group = group),
               color="#FFD300", fill = "#2a2a2a") +
  theme_void()  +
  labs(title = 'Nuclear Explosions')+
  labs(caption = 'by @r0mymendez \n')+
  theme(
    legend.background = element_rect(fill='#2a2a2a'),
    legend.text = element_text(color='white'),
    plot.background = element_rect(fill='#FFD300',color ='#FFD300' ),
    panel.background =element_rect(fill='#FFD300')  ,
    legend.position=c(0.11, 0.32),
    legend.key = element_rect(fill = "#2a2a2a", color = NA),
    legend.title = element_text(color = "white", size = 20, hjust = 0),
    plot.title = element_text(family =    "Bangers"    ,
                              hjust = 0.5,
                              size=50,colour = '#2A2A2A',
                              face = 'bold'),
    plot.subtitle =  element_text(family =     "Atma Light"     ,
                                  hjust = 0.5,
                                  size=15, face = 'bold',
                                  colour = '#73d055ff' ),
    plot.caption = element_text(family ="Atma Light",
                                hjust =0.9,
                                size=20, face = 'bold',
                                colour = 'black' )
  )+
  geom_point(data =  nuclear_explosions%>%filter(year == i),
             aes(x = longitude,
                 y = latitude,
                 size=magnitude_surface+0.5,
                 color=magnitude_body),
             alpha=0.3,show.legend = F)+
  scale_color_gradient(low="#BF6068", high="#8C041D")+
  scale_size_continuous(range = c(1,20))  
  
  p3 <- ggplot(data = NULL, aes(x = min.year:max.year , y = 1)) +
  geom_line() +
  geom_point(aes(fill = (x = min.year:max.year > i)), shape = 21, size = 5) +
  theme_void() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("#b2d1e0","gold")) +
  geom_text(aes(x = i, y = 1, label = i), vjust = -1, size = 9,
            family="Bangers" ,color='white') +
  theme(panel.background = element_rect(fill = "	#fcfcfc", colour = "	#cccccc"))+
  theme(plot.background = element_rect(fill='#2a2a2a',color = 'black'))  

 print(ggarrange(worldmap,p3,nrow = 2,ncol = 1,heights =  c(1.4,0.3)))

}
  
    
      },
  movie.name = "nuclearExplosions.gif",  
  interval = 1,
  ani.width = 1200, 
  ani.height = 900))




?saveGIF


