rm(list=ls())
bird_counts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-18/bird_counts.csv")
setwd('.../tidy/20190621-bird')

library(grid)
library(emojifont)
library(tidyverse)
library(ggpubr)
library(ggrepel)

df=bird_counts%>%
  filter(how_many_counted>0)%>%
  group_by(species)%>%
  summarise(total=sum(how_many_counted))%>%
  top_n(1,total)

df.bird=bird_counts%>%
  filter(how_many_counted>0,species %in% df$species)%>%
  group_by(year)%>%
  summarise(total=sum(how_many_counted))

A1 <- png::readPNG("1.png")
A1 <- grid::rasterGrob(A1, interpolate = T) 
A2 <- png::readPNG("2.png")
A2 <- grid::rasterGrob(A2, interpolate = T) 

img_a <- png::readPNG("5c.png") 
a <- grid::rasterGrob(img_a, interpolate = T)

b <- jpeg::readJPEG("3.jpg") 


ggplot(df.bird,aes(x=year, y=total)) +
  geom_point(show.legend = F,color='#001C3E',alpha=0.7) +
  geom_line(show.legend = F,color='#32686B')+
  annotation_custom(a, ymin =30000, ymax =60000,
                    xmin=1890,xmax=2000)+
  annotation_custom(A1, ymin =52000, ymax =72000,
                    xmin=1920,xmax=2000)+
  annotation_custom(A2, ymin =40000, ymax =52000,
                    xmin=1910,xmax=1960)+
 annotate("text", label ='European Starling',
         x = 1938, y = 65000, size = 15,  fill ='#fffeea',  family="Jadyn Maria Free",
         colour ="black") +
  annotate("text", label ='(1925 - 2017)',
           x = 1940, y = 57000, size = 8,  fill ='#fffeea',  family="Atma Light",
           colour ="black") +
  scale_x_continuous(breaks = seq(1925,2017,4))+
  geom_label_repel(aes(label = total),color='white',
                   data = df.bird%>%top_n(5,total),  size = 4,  fill ='#9DACC0',
                   alpha=0.8,
                   family="Atma Light" ,
                   box.padding = unit(0.35, "lines"),
                   point.padding = unit(0.3, "lines"),
                   show.legend = F,fontface = 'bold',
                   hjust=0,vjust=0.8,
                   segment.size =0.8)+
  theme_minimal()+
  labs(caption = 'by @r0mymendez')+
  theme(plot.caption = element_text(size=14, family="Atma Light"),
        axis.title = element_text(size=30, family="Jadyn Maria Free"),
        plot.background = element_rect(fill = '#77939F'),
        panel.background = element_rect(fill = '#77939F'),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(linetype = 2),
        axis.text = element_text(color = 'white')
        )

  

