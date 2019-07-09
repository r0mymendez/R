rm(list=ls())

setwd('/home/romimendez/Romi/Proyectos/miercolesdata/storytellingwithdata/20190705')

file=read.csv('marvel_characters_info.csv',header = T,stringsAsFactors = F)
file_ch=read.csv('charcters_stats.csv',header = T,stringsAsFactors = F)

library(tidyverse)
library(grid)
library(extrafont)
library(ggpubr)
library(ggrepel)

df=file%>%
  inner_join(file_ch)%>%
 group_by(Alignment,Gender)%>%
  #group_by(Race)%>%
  summarise(count=n(),mean=round(mean(Total),0),sum=sum(Total))

df$Alignment=ifelse(df$Alignment=='-','No Data',df$Alignment)
df$Gender=ifelse(df$Gender=='-','No Data',df$Gender)

img_b <- png::readPNG("6.png") 
  
G3= ggplot() + annotation_custom(rasterGrob(img_b, 
                                              width = unit(1,"npc"), 
                                              height = unit(1,"npc")), 
                                   -Inf, Inf, -Inf, Inf) +
    theme(plot.background = element_rect(fill = 'black',color='black'),
          panel.background = element_rect(fill = 'black',color='black'))


G2=
  df%>%ggplot(aes(x=Alignment,y=mean,fill=Gender))+
  geom_col(show.legend = F,position = 'dodge',alpha=0.7,color='black')+
  coord_polar()+
  geom_label_repel(
                   aes(label = paste0('avg power: ',mean)),
                   color='black',
                   data = df,  size = 3.8,  fill ='#fffeea',
                   family="Atma Light" ,
                   box.padding = unit(0.35, "lines"),
                   point.padding = unit(0.3, "lines"),
                   show.legend = F,fontface = 'bold',
                   hjust=0,vjust=0,
                   segment.size = 1,
                   segment.col='#fffeea')+
    scale_fill_manual(
    breaks =df$Gender,
    values =
      c( 'No Data'="#85D6CB",                            
         'Female'="#E8707B",                                    
         'Male'="#0378A6"))+
  facet_wrap(.~Gender) +
  labs(x='',y='',title='Marvel superheroes stats and info',
       subtitle =  'Data: kaggle by @r0mymendez') +
  theme(plot.background = element_rect(fill = 'black',color='black'),
        panel.background = element_rect(fill = 'black',color = 'black'),
        axis.text = element_text(colour = 'white'),
        axis.line.x.bottom = element_blank(),
        axis.line.y = element_blank(),
        axis.title.x.top = element_blank(),
        axis.text.x = element_text(size=13,family ="Anton")
        , axis.text.y = element_blank()
        ,panel.grid.major.y = element_blank()
        ,panel.grid.major.x = element_line(colour = "#2a2a2a")
        , plot.title=  element_text(color="white",size=35,family ="Anton",hjust = 0.5)
        , strip.background =element_rect(fill="#a71814")
        , strip.text = element_text(colour = 'white',face ="bold",size=14,family ="Anton")
        ,legend.position="bottom"
        ,legend.text = element_text( size=10)
        , plot.subtitle =   element_text(colour = "white",
                                     size=10,family ="Anton",hjust = 1)
  )


 ggarrange(G3,G2,widths= c( 0.5, 1.2),
             ncol = 2, nrow = 1)  +
 theme(plot.background = element_rect(fill='black',color ='black'),
       panel.background = element_rect(fill='black',color ='black'), 
      plot.title=  element_text(colour = "white",
                                size=35,family ="Anton",hjust = 1),
      plot.caption =  element_text(colour = "white",
                                size=20,family ="Anton",hjust = 1)
       ) 

