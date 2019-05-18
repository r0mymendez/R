
rm(list=ls())
nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")
nobel_winner_all_pubs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winner_all_pubs.csv")



library(patchwork)
library(grid)
library(gridExtra)
library(ggpubr)
library(tidyverse)
library(emojifont)
library(ggrepel)

emo=c(emoji('dollar'),
emoji('books'),
emoji('heart'),
emoji('microscope'),
emoji('globe_with_meridians'),
emoji('pill'))


df.category=nobel_winners%>%count(category)%>%top_n(10,n)%>%arrange((n))
df.category$x=1
df.category$y=seq(0,2.5,by=0.5) 
df.category$desc=paste0(df.category$category,' ',emo)
 

p0=
  df.category%>%mutate(category=reorder(category,n))%>%
  ggplot(aes(x=category,y=n))+
  geom_col(show.legend = F,fill='#2a2a2a')+
  geom_label_repel(aes(label = paste0(n,' in ',desc),color=category),
                   data = df.category,  size = 6,  fill ='#fffeea',
                   family="Atma Light" ,
                   box.padding = unit(0.35, "lines"),
                   point.padding = unit(0.3, "lines"),
                   show.legend = F,fontface = 'bold',
                   hjust=0,vjust=0,
                   segment.size = 0,y = -500
  ) +
  coord_flip() +
  labs(x='',y='',title = ' ')+
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank() ,
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    plot.background = element_rect(fill='#2a2a2a',color ='#2a2a2a' ),
    panel.background =element_rect(fill='#2a2a2a',color ='#2a2a2a' ),
    panel.spacing = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(family = "Atma Light",
                              hjust = 0.5,size=20,colour = 'white',
                              face = 'bold')
  )


df.country=nobel_winners%>%count(birth_country)%>%top_n(10,n)%>%arrange((n))
usa=nobel_winners%>%filter(birth_country=='United States of America')%>%
  count(category)

p1=
  ggplot(usa,aes(category,n,fill=category))+
  geom_col(color='black',show.legend = F)+
  coord_flip()+
  labs(y = paste0('Usa: ',sum(usa$n),' prizes'),x='')+
  coord_polar()+
  geom_label_repel(aes(label = paste0(n,' in ',category)),
                   data = usa,  size = 4,  fill ='#fffeea',
                   family="Atma Light" ,
                   box.padding = unit(1, "lines"),
                   point.padding = unit(0.3, "lines"),
                   segment.color='white',
                   show.legend = F)+
  theme(axis.title = element_text(family = "Atma Light",
                                    hjust = 0.5,size=20,colour = 'white'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(fill='#2a2a2a',color ='#2a2a2a' ),
        panel.background =element_rect(fill='#2a2a2a',color ='#2a2a2a' )
        )

uk=nobel_winners%>%filter(birth_country=='United Kingdom')%>%
  count(category)

p2=ggplot(uk,aes(category,n,fill=category))+
  geom_col(color='black',show.legend = F)+
  labs(y = paste0('Uk: ',sum(uk$n),' prizes'),x='')+
  coord_flip()+
  coord_polar()+
  geom_label_repel(aes(label = paste0(n,' in ',category)),
                   data = uk,  size = 4,  fill ='#fffeea',
                   family="Atma Light" ,
                   box.padding = unit(1, "lines"),
                   point.padding = unit(0.3, "lines"),
                   segment.color='white',
                   show.legend = F)+
  theme(axis.title = element_text(family = "Atma Light",
                                  hjust = 0.5,size=20,colour = 'white'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(fill='#2a2a2a',color ='#2a2a2a' ),
        panel.background =element_rect(fill='#2a2a2a',color ='#2a2a2a' )
        
  )


Germany=nobel_winners%>%filter(birth_country=='Germany')%>%
  count(category)

p3=ggplot(Germany,aes(category,n,fill=category))+
  geom_col(color='black',show.legend = F)+
  labs(y = paste0('Germany: ',sum(Germany$n),' prizes'),x='')+
  coord_flip()+
  coord_polar()+
  geom_label_repel(aes(label = paste0(n,' in ',category)),
                   data = Germany,  size = 4,  fill ='#fffeea',
                   family="Atma Light" ,
                   box.padding = unit(1, "lines"),
                   point.padding = unit(0.3, "lines"),
                   segment.color='white',
                   show.legend = F)+
  theme(axis.title = element_text(family = "Atma Light",
                                  hjust = 0.5,size=20,colour = 'white'),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(fill='#2a2a2a',color ='#2a2a2a' ),
        panel.background =element_rect(fill='#2a2a2a',color ='#2a2a2a' )
        
  )


df.genero=nobel_winners%>%filter(is.na(gender)==F)%>%count(gender,category)

p4=ggplot(df.genero,
       aes(x=category,y=n,fill=gender))+
  geom_col(position = "dodge",color='#2a2a2a',show.legend = F)+
  geom_label_repel(aes(label =paste0(n,' ',gender)),
                   data = df.genero,  size = 5,  fill ='#fffeea',
                   family="Atma Light" ,
                   label.padding = 0.3,fontface = 'bold',
                   show.legend = F,
                   box.padding = unit(0.5, "lines"),
                   segment.color='white',
                   point.padding = unit(0.5, "lines"))+
  labs(x='',y='',title='The novel prize gender')+
  theme_bw()+   
  theme(legend.position = 'top', 
        legend.spacing.x = unit(0.41, 'cm'),
        legend.text = element_text(margin = margin(t = 10))) +
  guides(fill=guide_legend(title=""))+
  theme(plot.background = element_rect(fill='#2a2a2a',color ='#2a2a2a' ),
        panel.background =element_rect(fill='#2a2a2a',color ='#2a2a2a' ),
        legend.background = element_rect(fill='#2a2a2a',color ='#2a2a2a' ),
        axis.text =  element_text(colour='white'),
        legend.title =  element_text(colour='white'),
        plot.title =  element_text(colour = 'white',
                           family="Atma Light",face = 'bold'))




ggarrange(
ggarrange(p0,p1,p2,p3,ncol=4,nrow = 1, heights = c(0.05,0.1,0.1,0.1),
          widths =  c(0.15,0.2,0.2,0.2)),
p4,
          ncol = 1, nrow = 2)+
  labs(caption='@r0mymendez    \n   ',title='THE NOBEL PRIZE')+
  theme(plot.background = element_rect(fill='#2a2a2a',color ='#2a2a2a'),
        plot.caption = element_text(colour = 'white',size = 10,hjust = 1),
        plot.title  = element_text(colour = 'white',size = 40,hjust = 0.5,
                                   family ="Atma SemiBold" ))








  