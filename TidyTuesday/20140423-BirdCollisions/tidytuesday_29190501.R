rm(list=ls())
library(tidyverse)
library(extrafont)
library(gridExtra)
library(grid)
library(ggpubr)

bird_collisions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-30/bird_collisions.csv")
mp_light <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-30/mp_light.csv")
+ scale_color_jama()

#Gauge plots
df1=bird_collisions%>%
  mutate(year=format(as.Date(bird_collisions$date, format="%Y-%m/%d"),"%Y"))%>%
  filter(year %in% 1996:2016)%>%
        count(family)%>%
        mutate(
              prop=round(n/sum(n),2),
              label=paste0(round(n/sum(n)*100,0),'%')
                      )%>%
        top_n(5,n)%>%
        arrange(n)%>%
        mutate(val=c(1,1,2,3,3),val=as.factor(val))
      
df1$family=
  factor(df1$family,
            levels = c('Regulidae','Certhiidae',
                 'Turdidae','Parulidae','Passerellidae'))

a=df1%>%
ggplot(aes(fill = val, 
           ymax = prop, 
           ymin = 0, 
           xmax = 2, 
           xmin = 1)) +
  geom_rect(aes(ymax=1, ymin=0, xmax=2, xmin=1),
            fill ="#ece8bd") +
  geom_rect(color='white') + 
  coord_polar(theta = "y",start=-pi/2) + xlim(c(0, 2)) + ylim(c(0,2)) +
  geom_text(aes(x = 0, y = 0, label = label, colour=val), size=6.5, family="Mr Bedfort",fontface = "bold") +
  geom_text(aes(x=1.5, y=1.5, label=family), family="Mr Bedfort", size=6.5,colour='white') + 
  facet_wrap(~family, ncol = 5) +
  theme_void() +
  scale_fill_manual(values = c("1"="#C9146C", "2"="#DA9112", "3"="#129188")) +
  scale_colour_manual(values = c("1"="#C9146C", "2"="#DA9112", "3"="#129188")) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  guides(fill=FALSE) +
  guides(colour=FALSE)+
  theme(plot.background = element_rect(fill='#2a2a2a',colour = "#2a2a2a"),
        panel.background = element_rect(fill = "#2a2a2a", colour = "#2a2a2a"),
        axis.ticks = element_blank(),
        panel.border = element_blank()
  )





df2=bird_collisions%>%
  mutate(year=format(as.Date(bird_collisions$date, format="%Y-%m/%d"),"%Y"))%>%
  select(family,year,habitat)%>%
  count(family,year,habitat)%>%
  filter(year %in% 1996:2016)


b=ggplot(df2, aes(x=year,y=family,color=family))+
  geom_quasirandom(alpha=.9,aes(size=n*2),
                   groupOnX = FALSE, 
                   show.legend = FALSE)+
  labs(title='Bird Collisions between 1996-2016',
       subtitle='Data: Winger et al. 2019 (doi: 10.1098/rspb.2019.0364)',
       y='',
       caption='')+
      theme(plot.background = element_rect(fill='#2a2a2a',colour = "#2a2a2a"),
            panel.background = element_rect(fill = "#2a2a2a", colour = "#2a2a2a"),
            axis.text = element_text(color='white',family = "Lucida Calligraphy"),
            plot.caption = element_text(color='white'),
            plot.title = element_text(color='white',family = "Mr Bedfort",size=30,face = 'bold'),
            plot.subtitle = element_text(color = "white",size = 8),
            axis.title = element_text(color = "white",family = "Mr Bedfort",size = 20),
            panel.grid.major.y = element_line(colour = "#2a2a2a", size = .2),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_line(colour = "#fdfdf3", size = .2),
            panel.grid.minor = element_blank())

b


title='#Tidytuesday'

df <- data.frame(
    x = c(1, 1, 2, 2, 1.5),
    y = c(1, 2, 1, 2, 1.5),
    text = c("", "", "", "", title))
  
  
 g= ggplot(df, aes(x, y)) +
    geom_text(aes(label = text),color="white",size=10,family = "Mr Bedfort")+
    labs(x='',y='',caption='Visualization by @r0mymendez')+
    theme(
      plot.background  = element_rect(color = '#2a2a2a',fill='#2a2a2a'),
      panel.background = element_rect(color = '#2a2a2a',fill='#2a2a2a'),
      panel.grid =  element_line(colour = '#2a2a2a'),
      axis.text= element_blank(),
      axis.line=element_blank(),
      axis.ticks = element_blank(),
      plot.caption = element_text(color='white',hjust = 0)
      )
  
ggarrange(b,
  ggarrange(g,a,widths= c( 0.5, 1.2),
                      ncol = 2, nrow = 1) +
    theme(plot.background = element_rect(fill='#2a2a2a',color ='#2a2a2a'),
          panel.background = element_rect(fill='#2a2a2a',color ='#2a2a2a')), 
          heights = c(2, 0.7),
          ncol = 1, nrow = 2)  +
  theme(plot.background = element_rect(fill='#2a2a2a',color ='#2a2a2a' ))
