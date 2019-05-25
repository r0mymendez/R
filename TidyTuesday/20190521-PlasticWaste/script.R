rm(list=ls())

mismanaged_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv")

library(tidyverse)
library(countrycode)
library(emojifont)
library(ggrepel)
library(ggridges)
library(ggpubr)


mismanaged_vs_gdp1 <- mismanaged_vs_gdp %>%
  rename('PerCapMismanaged'=`Per capita mismanaged plastic waste (kilograms per person per day)`,
         'GDPperCapita'=`GDP per capita, PPP (constant 2011 international $) (Rate)`,
         'TotalPop'=`Total population (Gapminder)`)%>%
  filter(!is.na(PerCapMismanaged),
         Year==2010,Entity!='World')%>%
  mutate(TotalPopM=TotalPop/1000000,
         PerCapMismanaged=PerCapMismanaged,
         continent = countrycode(sourcevar = Entity, 
                                 origin = "country.name.en", 
                                 destination = "continent"))



gg <- 
  ggplot(mismanaged_vs_gdp1%>%top_n(15,PerCapMismanaged)%>%
           mutate(Entity=reorder(Entity,PerCapMismanaged)), 
         aes(x=Entity, y=PerCapMismanaged,fill=Entity)) + 
  geom_col(show.legend = F, color= '#2a2a2a',alpha = 0.75)+
  coord_flip()+
  geom_label_repel(
   aes(label=Entity),
   size=4, data=mismanaged_vs_gdp1%>%top_n(15,PerCapMismanaged),
   y=-10,
   fill ='#fffeea',
   family="Atma Light" ,
   box.padding = unit(0.35, "lines"),
   point.padding = unit(0.3, "lines"),
   show.legend = F,fontface = 'bold',
   hjust=0,vjust=0,
   segment.size = 0,
  ) +
  labs(title="The 15 countries with more per capita mismanaged \nplastic waste ",
       subtitle="", 
       x="",
       y="kg per person per day" 
      ) +
  theme(      panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_text(family = "Atma Light",size=15),
              plot.caption = element_text(family = "Atma Light",size=15),
              plot.title =  element_text(family = "Atma Light",size=13),
              panel.background = element_rect(fill = "#fffeea",
                                              colour = "#fffeea"),
              plot.background =  element_rect(fill = "#fffeea",
                                              colour = "#fffeea"),
              panel.grid.major.x = element_line(color = '#2a2a2a')
              
  )


gg
  
img_a <- png::readPNG("1.png") 
a <- grid::rasterGrob(img_a, interpolate = T) 

gg1=
  mismanaged_vs_gdp1 %>% 
  mutate(continent = factor(continent)) %>%
  filter(is.na(continent)==F)%>%
  ggplot(aes(y=continent,x=PerCapMismanaged,
             fill = continent, color = continent)) +
    geom_density_ridges(alpha = 0.25,
                      show.legend = F,
                      aes(point_color = continent), 
                      jittered_points = TRUE)   +
  annotation_custom(a, xmin =0.26, xmax = 0.35,
                    ymin=0 ,ymax=2) +
  labs(y='Continent',x='Per capita mismanaged plastic waste',
       title='Analysis of continents with mismanaged plastic waste')+
  theme(      panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
             # axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              plot.title =  element_text(family = "Atma Light",size=15),
              axis.title = element_text(family = "Atma Light",size=13),
              plot.caption = element_text(family = "Atma Light",size=13),
              panel.background = element_rect(fill = "#fffeea",
                                              colour = "#fffeea"),
              plot.background =  element_rect(fill = "#fffeea",
                                              colour = "#fffeea"),
              panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_line(color = '#2a2a2a',linetype = 3)
              
  )
gg1



ggarrange(
  gg,gg1,
  ncol = 2, nrow = 1,widths = c(0.9,1.4))+
  labs(caption=paste0("Source: Our World In Data | by @r0mymendez",emoji("heart")),
       subtitle = ' ',
       title=paste0('Global Plastic Waste - year: 2010',emoji('earth_americas'))) +
  theme(plot.background = element_rect(fill='#2a2a2a',color ='#2a2a2a'),
        plot.caption = element_text(colour = 'white',size = 13,hjust = 1,
                                    family= "Finger Paint"  ),
        plot.title  = element_text(colour = 'white',size = 30,hjust = 0.5,
                                   family = "Finger Paint"      ),
        plot.subtitle = element_text(colour = 'white',size = 10,hjust = 0.5,
                                   family ="Atma SemiBold" ),
        plot.margin = unit(c(1,0,0.1,0), "cm"))
