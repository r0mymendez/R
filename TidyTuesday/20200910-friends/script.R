library(dplyr)
library(ggplot2)
library(ggnetwork)
library(dplyr)
library(geomnet)
library(viridis)
library(showtext)
library(ggplot2)
library(tidyverse)
library(animation)
library(ggpubr)
library(ggrepel)
library(extrafont)

# Instalar Schoolbell
#font_add("Schoolbell", "Schoolbell/Schoolbell-Regular.ttf")  # Use the actual file path
showtext_auto()

friends <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends.csv')


f=friends%>%mutate(from=speaker,to=speaker)

for(i in 2:nrow(f)){
  f$to[i]=f$from[i-1]
}

  f_=f%>%
    count(from,to)%>%
    filter(from!=to)%>%top_n(100,n)
  
p=
  ggplot(f_, aes(from_id = from, to_id = to)) +
  geom_net(aes(size = n, 
               color =from,
               group = from),
           ealpha = 0.25,
           curvature = 0.05,
           arrowsize = 0.5,
           linewidth = 0.4,
           labelon = TRUE,
           layout.alg = "kamadakawai", 
           ecolour="#bfbfbf",
           show.legend = F,
           fontsize = 2,
           vjust = -1.5,
           labelcolour =  "white",
           family='Schoolbell'
  ) +
  theme_net(base_family='Schoolbell')+
  labs(title="Friends network Graph",
       caption='by @r0mymendez')+
  theme_net()+
  theme(plot.background = 
          element_rect(color = 'black',fill='black'),
        panel.background =  
          element_rect(color = 'black',fill='black'),
        legend.background = 
          element_rect(color = 'black',fill='black'),
        plot.subtitle = element_text(hjust = 0.5,size=24,
                                     color='white',family='serif'),
        plot.title  = element_text(hjust = 0.5,size = 30,face="bold",
                                   color='white',family='Schoolbell'),
        plot.caption  = element_text(size=10,hjust = 0.9,
                                     color='white',
                                     family='Schoolbell')
        
  )

p
  

  
 