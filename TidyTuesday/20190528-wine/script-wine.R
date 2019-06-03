library(tidyverse)
library(ggpubr)
library(ggrepel)
library(tidytext)
library(packcircles)

df_wines <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")

df_wines_top_countries <- df_wines %>% 
  group_by(country) %>% 
  summarize(
    rating = mean(points, na.rm = T),
    n = n()
  ) %>% 
  mutate(rating = (rating - 80) / 20) %>% 
  filter(
    !is.na(country),
    n > 99
  ) %>%
  top_n(6, rating)

library(emojifont)
df_top_variety <- 
  df_wines %>% 
  filter(
    !is.na(country))%>% 
  group_by(country ) %>% 
  summarize(points = mean(points, na.rm = T), n = n()) %>% 
  filter(n>100)%>% 
  top_n(10,points)%>%
  mutate(icon=emoji('wine_glass'))

setwd('/home/romimendez/Romi/Proyectos/miercolesdata/tidy/20190527-wine')

df_wines_1=df_wines%>%count(points,variety)%>%
  filter(n>50)%>%top_n(10,points)


imgage <- jpeg::readJPEG("9.jpg")

g1=df_top_variety%>%mutate(country=reorder(country,-n))%>%
ggplot(aes(x=country,y=n,label=icon))+
  annotation_custom(rasterGrob(imgage, 
                               width = unit(1,"npc"), 
                               height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
  scale_fill_brewer(type='seq', palette='Reds')+
  geom_col(aes(fill=country),show.legend = F,alpha=0.2,color='#2a2a2a')+
    annotate("text", label = paste0('The top ten country Wine' ,emoji('wine_glass')),
           x = 5, y = 40000, size = 20,  fill ='#fffeea',  family="Jadyn Maria Free",
           colour ="black") +
  annotate("text", label = paste0('_________' ),
           x = 5, y = 40100, size = 20,  fill ='#fffeea',  family="Jadyn Maria Free",
           colour ="black") +
  annotate("text", label = paste0('These Countries have the Best \nAverage Classification of wines'),
           x = 6, y = 25000, size = 12,  fill ='#fffeea',  family="Jadyn Maria Free",
           colour ="black") +
  labs(y='Quantity',x='',caption = 'by @r0mymendez')+
  geom_label_repel(aes(label = paste0(round(points,0),' points avg')),color='black',
                   data = df_top_variety,  size = 4,  fill ='#E7D4B3',
                   alpha=0.8,
                   family="Atma Light" ,
                   box.padding = unit(0.35, "lines"),
                   point.padding = unit(0.3, "lines"),
                   show.legend = F,fontface = 'bold',
                   hjust=0,vjust=0.8,
                   segment.size = 0,
  ) +
  theme(axis.title.y =element_text(color='black',
                                   family = "Fabiana",
                                   size=15),
        plot.background = element_rect(fill='#CAA867'),
        axis.text.x = element_text(color='black',
                                 family = "Fabiana",
                                 size=15),
        axis.text.y =element_text(color='black',
                                  family = "Atma" ,
                                  size=10) )


data("stop_words")

dftext=df_wines%>%select(X1,description)
df_token=dftext%>%unnest_tokens(word,description)
df_token=df_token%>%anti_join(stop_words)
df_token=df_token%>%count(word)%>%top_n(50)


imgage1 <- jpeg::readJPEG("a1.jpeg")
packing <- circleProgressiveLayout(df_token$n, sizetype='area')
packing$radius=1.2*packing$radius
data = cbind(df_token, packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

g2=
  ggplot() + 
  annotation_custom(rasterGrob(imgage1, 
                               width = unit(1,"npc"), 
                               height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), 
               colour = "black", alpha = 0.6) +
  scale_fill_gradient(low='#FFF5F0', high='#FFE6E6')+
  geom_text(data = data, aes(x, y, label =word),size=6,family="Atma Light", color="black") +
  theme_void() + 
  theme(legend.position="none") + 
  #theme(axis.title = 'Palabras relacionadas con tecnologia y conocimientos mas frecuentes')
  coord_equal() +
  scale_size(range = c(10,1), guide = F) +
  labs(title='The most frequent words in the text description ', 
       caption='by @r0mymendez')+
  theme(plot.background = element_rect(fill='#CAA867'),
        plot.title = element_text(color='black',
                                  family = "Fabiana",
                                  size=25))



