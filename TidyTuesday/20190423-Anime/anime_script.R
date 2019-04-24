rm(list = ls())
library(ghibli)
library(tidyverse)
library(ggrepel)
library(patchwork)
library(scales)
library(extrafont)
library(showtext)
pacman::p_load(jpeg, png, ggplot2, grid, neuropsychology)
tidy_anime <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv")

df=tidy_anime%>%select(title_english,studio,type,source,scored_by,score,rating,rank,popularity)
df=unique(df%>%filter(is.na(title_english)==FALSE))

imgage1 <- png::readPNG("16.png")

 g0=
  df%>%count(rating)%>%filter(is.na(rating)==F)%>%
  mutate(rating=case_when(
    rating=="R - 17+ (violence & profanity)"~ 'R17 \n (violence & profanity)',
    rating=="PG-13 - Teens 13 or older"     ~ 'PG13 \n (Teens 13 or older)',
    rating=="PG - Children"                 ~ 'PG (Children)', 
    rating=="R+ - Mild Nudity"              ~ 'R+ \n (Mild Nudity)',
    rating=="G - All Ages"                  ~ 'G (All Ages)', 
    rating== "None"                          ~ 'None')
    )%>%
  mutate(rating=reorder(rating,n))%>%
  ggplot(aes(rating,n,fill=rating))+
  annotation_custom(rasterGrob(imgage1, 
                               width = unit(1,"npc"), 
                               height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
  
  geom_col(show.legend = F,color='black')+
  scale_fill_ghibli_d("MarnieMedium1")+
  coord_flip()+
  labs(x='',y='',title='Anime: classification and studios',subtitle='')+
  theme_bw()+
  theme(axis.text.y = element_text(color="#233a77",
                                 size=rel(0.7),hjust=0.5),
        axis.text.x = element_text(color="#233a77",
                                   size=rel(1)),
        axis.title = element_text(color="#233a77",
                                    size=rel(0.8)),
        text=element_text(size=16, family="Bangers"),
        plot.title = element_text(size = 25),
        )
 

imgage <- png::readPNG("16.png")


g1=
  df%>%count(studio)%>%filter(is.na(studio)==F)%>%
  top_n(5,n)%>%
  mutate(studio=reorder(studio,n))%>%
  ggplot(aes(studio,n,fill=studio))+
  annotation_custom(rasterGrob(imgage, 
                               width = unit(1,"npc"), 
                               height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
  geom_col(show.legend = F,color='black',alpha=0.6)+
  scale_fill_ghibli_d("MarnieMedium1")+
  coord_flip()+
  labs(x='',y='')+
  theme_bw()+
  theme(axis.text.y = element_text(color="#233a77",
                                   size=rel(0.8),hjust=0.5),
        axis.text.x = element_text(color="#233a77",
                                   size=rel(1)),
        axis.title = element_text(color="#233a77",
                                  size=rel(0.8)),
        text=element_text(size=16,  family="Bangers"),
        plot.title = element_text(size = 25)
  )



img_a <- png::readPNG("6.png") 
a <- grid::rasterGrob(img_a, interpolate = T) 


g2=
  df%>%
    select(studio,popularity,rating,score,rank)%>%
    mutate(rating=case_when(
      rating=="R - 17+ (violence & profanity)"~ 'R 17',
      rating=="PG-13 - Teens 13 or older"     ~ 'PG 13',
      rating=="PG - Children"                 ~ 'PG', 
      rating=="R+ - Mild Nudity"              ~ 'R+',
      rating=="G - All Ages"                  ~ 'G', 
      rating== "None"                          ~ 'None')
    )%>%
  filter(studio=='Toei Animation')%>%
  ggplot(aes(x=rank,y=popularity,color=rating)) +
  geom_point()+
  scale_color_ghibli_d("MarnieMedium1") +
  annotation_custom(a, xmin = 8000, xmax = 16000,
                     ymin = 0, ymax = 6000)  +
  labs(x = "Ranking", y = "Popularity",
       title = "Toei Animation Studio",
       subtitle = "")+
  theme_bw() +
  guides(col  = guide_legend(title = "classif."))+
  theme(
    text=element_text(size=16,  family="Bangers"),
    legend.text=element_text(size=rel(0.9)),
    panel.border=element_rect(color="#f4e3b5", fill=NA, size=1),
    panel.background = element_blank(),
    plot.title = element_text(size = 25)
  )


imgagec <- jpeg::readJPEG("sp4.jpg")


g4= ggplot() + annotation_custom(rasterGrob(imgagec, 
                                            width = unit(1,"npc"), 
                                            height = unit(1,"npc")), 
                                 -Inf, Inf, -Inf, Inf) 
  
(g0|g1) / (g2 + g4 + plot_layout(ncol=2,widths=c(2,1)))

  
