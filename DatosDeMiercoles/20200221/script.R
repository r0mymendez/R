rm(list=ls())
library(tidyverse)
library(geomnet)
library(plotly)
library(dummies)
library(ggrepel)
library(grid)
library(gridExtra)
library(ggpubr)
library(extrafont)

imdb <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2020/2020-02-19/ranking_imdb.csv")


df_filtered <- imdb %>%  
  mutate(genero_=genero,
         genero=gsub('á','a',genero),
         genero=gsub('é','e',genero),
         genero=gsub('í','i',genero),
         genero=gsub('ó','o',genero),
         genero=gsub('ú','u',genero))%>%
  separate(genero,c('g1','g2','g3'),',')%>%
  mutate(
         g2=trimws(g2),
         g3=trimws(g3),
         g2=gsub(' ','.',g2),
         g3=gsub(' ','.',g3))



df_filtered1=model.matrix( ~ g1 - 1, data=df_filtered )
df_filtered1=data.frame(df_filtered1)
col=names(df_filtered1)
names(df_filtered1)=substr(col,3,nchar(col)) 
df_filtered=cbind(df_filtered,df_filtered1)  


for(i in 1:nrow(df_filtered)){
  a=trimws(df_filtered$g2[i])
  if(is.na(a)==F){
    for(j in names(df_filtered)[13:ncol(df_filtered)]){
      if(a==j){ df_filtered[i,j]=1}
    }
  }
  
  a=trimws(df_filtered$g3[i])
  if(is.na(a)==F){
    for(j in names(df_filtered)[13:ncol(df_filtered)]){
      if(a==j){ df_filtered[i,j]=1}
    }
  }
  print(i)
  
  print(i)
}


rm(df_filtered1,a,col,i,j)


df=df_filtered%>%
  gather(key='gen',value='cant',13:ncol(df_filtered))%>%
  filter(cant>0)

df4= df_filtered%>%
  gather(key='gen',value='cant',13:ncol(df_filtered))%>%
  filter(cant>0)%>%
  group_by(gen) %>% 
  summarise(total=mean(puntaje,na.rm = T),n=n())%>%
  filter(total>mean(imdb$puntaje,na.rm = T))%>%
  top_n(4,n)

p=ggplot(data = df4, 
       aes(x = gen, y = n,fill=gen)) +
  geom_col(color='black',show.legend = F)+
  labs(x='genero',y='cantidad')+
  coord_flip()+
  theme(plot.background = element_rect(fill='#2a2a2a',colour ='#2a2a2a' ),
        panel.background = element_rect(fill='#2a2a2a',colour ='#2a2a2a'),
        axis.text = element_text(color='white'),
        axis.title = element_text(color='white'),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_line(colour = "#fdfdf3", size = .1, linetype = 2))

p

df_year=df%>%
  filter(gen %in% df4$gen)%>%
  select(ranking,titulo,anio,puntaje,votos,direccion,
         duracion,ganancias,genero_)%>%
  mutate(genero=
           case_when(
           str_detect(genero_,'Biografía') ~ 'Biografia',
           str_detect(genero_,'Crimen') ~ 'Crimen',
           str_detect(genero_,'Drama') ~ 'Drama',
           str_detect(genero_,'Romántica') ~ 'Romántica')
  )%>%
  unique()

p0=ggplot(df_year,aes(x=anio,y=puntaje,fill=genero))+
  geom_tile(color = "black", size = 0.5,show.legend = F) +
  labs(x='año',y='puntaje',label='genero')+
  theme(plot.background = element_rect(fill='#2a2a2a',colour ='#2a2a2a' ),
        panel.background = element_rect(fill='#2a2a2a',colour ='#2a2a2a'),
        axis.text = element_text(color='white'),
        axis.title = element_text(color='white'),
        legend.position=c(0.08, 0.15),
        legend.text = element_text(color = "white", size = 10),
        legend.key = element_rect(fill = "white", color = NA),
        legend.title = element_text(color = "white", size = 10, 
                                    hjust = 0.5),
        legend.background = element_rect(fill='#2a2a2a'),
        panel.grid.major.y = element_line(colour = "#fdfdf3", size = .1, linetype = 2),
        panel.grid.minor.y = element_line(colour = "#2a2a2a", size = .2),
        panel.grid.major.x = element_line(colour = "#fdfdf3", size = .2),
        panel.grid.minor = element_line(colour = "#fdfdf3", size = .2))+
        geom_label_repel(aes(label = titulo,
                     x=anio,
                     y=puntaje,
                     color=genero),
                 data = df_year%>%
                   filter(votos>quantile(df_year$votos,0.999)),  
                 size = 4,  
                 #fill ='white',
                 family="Atma Light" ,
                 color='white',
                 #fontface = 'bold',
                 box.padding = unit(0.35, "lines"),
                 point.padding = unit(0.3, "lines"),
                 segment.color = 'white',
                 show.legend = F)  


ggarrange(
  p,p0,
  ncol = 2, nrow = 1,widths = c(0.4,1.4))+
  labs(caption='@r0mymendez    \n   ',title=paste0(emoji('movie_camera'),
                                                   'IMDB: Ranking de películas'),
       subtitle = paste0(min(df_year$anio),' - ',max(df_year$anio),'\n'))+
  theme(plot.background = element_rect(fill='#2a2a2a',color ='#2a2a2a'),
        panel.background =  element_rect(fill='#2a2a2a',color ='#2a2a2a'),
        plot.caption = element_text(colour = 'white',size = 10,hjust = 1),
        plot.subtitle =  element_text(colour = 'white',size = 20,hjust = 0.5),
        plot.title  = element_text(colour = 'white',size = 40,hjust = 0.5,
                                   family ="Impact"   ))



