rm(list=ls())

library(tidyverse)
library(emojifont)
library(gridExtra)
library(grid)
library(ggrepel)
library(ggridges)

media_franchises <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-02/media_franchises.csv")

df=media_franchises%>%
    mutate(category=
              case_when(
                revenue_category== "Book sales"    ~ 'Book',
                revenue_category== "TV"    ~ "TV",
                revenue_category== "Video Games/Games"     ~ "Video Games",
                revenue_category== "Comic or Manga"     ~ "Comic",
                revenue_category== "Music"      ~ "Music" ,
                revenue_category== "Box Office"      ~ "Box Office",
                revenue_category== "Home Video/Entertainment"      ~ "Entertainment",
                revenue_category== "Merchandise, Licensing & Retail"      ~ "Merchandise"
              ),
            media=case_when(
                original_media=="Novel"             ~ "Novel",
                original_media=="Animated film"     ~ "film",
                original_media=="Video game"        ~ "Video game",
                original_media=="Manga"             ~ "Manga",
                original_media=="Comic book"        ~ "Comic",
                original_media== "Animated series"  ~ "series",
                original_media=="Greeting card"     ~ "card",
                original_media== "Film"             ~ "Film",
                original_media=="Visual novel"      ~ "novel",
                original_media=="Television series" ~ "series",
                original_media=="Anime"             ~ "Anime",
                original_media=="Cartoon character" ~ "Cartoon",
                original_media=="Cartoon"           ~ "Cartoon",
                original_media=="Animated cartoon"  ~ "Cartoon",
                original_media=="Comic strip"       ~ "Comic",
                original_media=="Musical theatre"   ~ "Musical",
                original_media=="Book"              ~ "Book"
            )
    
    )
               


df.media=df%>%group_by(media)%>%summarise(revenue=sum(revenue))
df.category=df%>%group_by(category)%>%summarise(revenue=sum(revenue))
df.creators=df%>%group_by(creators)%>%summarise(revenue=sum(revenue))

df.franchise=df%>%group_by(franchise)%>%summarise(revenue=sum(revenue))%>%
  top_n(10,revenue)

img_a <- png::readPNG("1.png") 
a <- grid::rasterGrob(img_a, interpolate = T) 

img_a <- png::readPNG("2.png") 
b <- grid::rasterGrob(img_a, interpolate = T) 

img_a <- png::readPNG("3.png") 
c <- grid::rasterGrob(img_a, interpolate = T) 

img_a <- png::readPNG("4.png") 
d <- grid::rasterGrob(img_a, interpolate = T) 

img_a <- png::readPNG("5.png") 
e <- grid::rasterGrob(img_a, interpolate = T) 

img_a <- png::readPNG("6.png") 
f <- grid::rasterGrob(img_a, interpolate = T) 

img_a <- png::readPNG("7.png") 
g <- grid::rasterGrob(img_a, interpolate = T) 

img_a <- png::readPNG("8.png") 
h <- grid::rasterGrob(img_a, interpolate = T) 

img_a <- png::readPNG("9.png") 
i <- grid::rasterGrob(img_a, interpolate = T) 

img_a <- png::readPNG("10.png") 
j <- grid::rasterGrob(img_a, interpolate = T) 

df.franchise%>%mutate(franchise=reorder(franchise,revenue))%>%
  ggplot()+
  geom_col(aes(x=franchise,y=revenue,fill=franchise),
           position = "dodge",color='#2a2a2a',show.legend = F)+
  annotation_custom(a, xmin = 9.5, xmax =10.5,
                    ymin = -30, ymax =50) +
  annotation_custom(b, xmin = 8.5, xmax =9.5,
                    ymin = -40, ymax =50) +
  annotation_custom(c, xmin = 7.5, xmax =8.6,
                    ymin = -25, ymax =50)+
  annotation_custom(d, xmin = 6.5, xmax =7.5,
                    ymin = -30, ymax =50)+
  annotation_custom(e, xmin = 5.0, xmax =6.5,
                    ymin = -30, ymax =50)+
  annotation_custom(f, xmin = 4.5, xmax =5.5,
                    ymin = -30, ymax =50)+
  annotation_custom(g, xmin = 3.5, xmax =4.5,
                    ymin = -20, ymax =50)+
  annotation_custom(h, xmin = 2.5, xmax =3.5,
                    ymin = -25, ymax =50)+
  annotation_custom(i, xmin = 1.0, xmax =3,
                    ymin = -30, ymax =50)+
  annotation_custom(j, xmin = 0.5, xmax =1.5,
                    ymin = -30, ymax =50)+
  labs(title = 'Media Franchise Powerhouses',x='')+
  theme_bw() +   
  theme(legend.position = 'top', 
        legend.spacing.x = unit(0.41, 'cm'),
        legend.text = element_text(margin = margin(t = 10),size=30))+
  theme(plot.background = element_rect(fill='#2a2a2a',color ='#2a2a2a' ),
        legend.background = element_rect(fill='#2a2a2a',color ='#2a2a2a' ),
        axis.text =  element_text(colour='white',family="Atma Light",size=12),
        axis.text.x = element_text(hjust = 1, angle=45),
        axis.title = element_text(colour='white', family="Atma Light",size=20),
        legend.title =  element_text(colour='white',size=20, family="Atma Light"),
        plot.title =  element_text(colour = 'white',size=50,hjust = 0.5,
                                   family="Atma Light",face = 'bold'),
        plot.subtitle =  element_text(colour = 'white',size=12,hjust = 0.5,
                                      family="Atma Light",face = 'bold'),
        panel.background = element_rect(fill='#2a2a2a',color ='#2a2a2a' ))




