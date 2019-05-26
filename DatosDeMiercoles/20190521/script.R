rm(list=ls())
library(tidyverse)
library(countrycode)
library(emojifont)
library(extrafont)
library(ggrepel)
library(ggpubr)

tuberculosis_oms <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-05-22/tuberculosis_oms.csv")

countrycode=codelist%>% select(cowc,region,cow.name)
  
tuberculosis=tuberculosis_oms%>%
  gather(key='concepto',value='value',-c(1:4))%>%
  inner_join(countrycode,by=c('iso3'='cowc'))%>%
  filter(region=='South America', anio %in% c(1996:2012),
         is.na(value)==F)

tuberculosis=tuberculosis%>%
  mutate(postivo_frotis=ifelse(str_detect(tuberculosis$concepto,'fpp'),1,0),
         sexo=ifelse(str_detect(tuberculosis$concepto,'_h'),'Hombre','Mujer'))%>%
  filter(postivo_frotis==1)%>%
  mutate(edad=case_when(
    concepto %in% c("nuevos_fpp_h014","nuevos_fpp_m014")   ~ ' 0 - 14',
    concepto %in% c("nuevos_fpp_h1524","nuevos_fpp_m1524") ~ '15 - 24',
    concepto %in% c("nuevos_fpp_h2534","nuevos_fpp_m2534") ~ '25 - 34',
    concepto %in% c("nuevos_fpp_h3534","nuevos_fpp_m3534") ~ '35 - 44',
    concepto %in% c("nuevos_fpp_h4554","nuevos_fpp_m4554") ~ '45 - 54',
    concepto %in% c("nuevos_fpp_h5564","nuevos_fpp_m5564") ~ '55 - 64',
    concepto %in% c("nuevos_fpp_h65"  ,"nuevos_fpp_m65")   ~ 'Mayor 64'
  ))

tuberculosis$emoji=emoji('syringe')

a1=
  tuberculosis%>%group_by(sexo,edad)%>%summarise(s=sum(value))%>%
  ggplot(aes(edad,s,fill=sexo))+
  geom_col(color='black',position = "dodge",show.legend = T) +
  scale_fill_manual(values = c('Mujer'='#e68c7c','Hombre'='#0570b0'))+
  labs(title='Cantidad de casos por edad',
       subtitle = 'Paises: Argentina, Bolivia, Brasi, Chile, Colombia, Ecuador, Guyana, Perú, Surinám y Venezuela"')+
  theme_bw()+   
  theme(legend.justification=c(0,0), legend.position=c(0.84,0.7),
        legend.background = element_rect(fill="#fffeea",color='#a2a2a2'),
        legend.text = element_text(family = "Atma Light")) + 
  theme(      panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.title.y =element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_text(family = "Atma Light",face = 'bold'),
              #axis.text = element_rect(fill='white'),
              plot.subtitle =  element_text(family = "Atma Light",size=10,
                                            color='#a2a2a2'),
              plot.title =  element_text(family = "Anton" ,size=18,color='#a2a2a2'),
              axis.title = element_text(family = "Atma Light",size=13),
              plot.caption = element_text(family = "Atma Light",size=13),
              panel.background = element_rect(fill = "#fffeea",
                                              colour = "#fffeea"),
              plot.background =  element_rect(fill = "#fffeea",
                                              colour = "#fffeea"),
              panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_line(color = '#2a2a2a',linetype = 3)
              
  )
  

targ=tuberculosis%>%filter(pais=='Argentina')%>%
        group_by(anio,sexo,emoji)%>%
          summarise(s=sum(value))
targ.g=targ%>%top_n(10,s)


total=tuberculosis%>%filter(pais=='Argentina',anio==2012,sexo=='Mujer')%>%
  select(value)

arg=tuberculosis%>%filter(pais=='Argentina',anio==2012)%>%top_n(1,value)%>%
  select(edad,s=value,anio,sexo)%>%mutate(prop=round(s/sum(total$value)*100,0))

a2=
  ggplot(targ,
       aes(x=anio, y=s,color= sexo,fill=sexo,label=emoji)) + 
 geom_point(show.legend = F,shape = 21,alpha=0.8,aes(size=s/100)) +
 geom_text(family='EmojiOne',size=12,show.legend = F)+
 geom_label_repel(aes(label=s),size=3.5, show.legend = F,
    data=targ%>%filter(s>2700),fill ='#fffeea') +
 geom_line(show.legend = F,linetype='dotted') +
  scale_x_continuous(breaks = seq(1995,2013,by=3))+
  scale_color_manual(values = c('Mujer'='#67001f','Hombre'='#08306b'))+
  scale_fill_manual(values = c('Mujer'='#e68c7c','Hombre'='#0570b0'))+
  theme_bw()+   
  labs(title='Tuberculosis en Argentina',x='Año')+
  theme(      panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              # axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              plot.title =  element_text(family = "Anton" ,size=18,color='#a2a2a2'),
              axis.title = element_text(family = "Atma Light",size=13),
              plot.caption = element_text(family = "Atma Light",size=13),
              panel.background = element_rect(fill = "#fffeea",
                                              colour = "#fffeea"),
              plot.background =  element_rect(fill = "#fffeea",
                                              colour = "#fffeea"),
              panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_line(color = '#2a2a2a',linetype = 3)
              
  )



ggarrange(
  a1,a2,
  ncol = 2, nrow = 1)+
  labs(caption=paste0("Source: Organización Mundial de la Salud (OMS) | by @r0mymendez",emoji("heart")),
       subtitle = ' ',
       title=paste0('Analisis de tuberculosis en America del sur (1996 - 2012) ',
                    emoji('syringe'))) +
  theme(plot.background = element_rect(fill='#2a2a2a',color ='#2a2a2a'),
        plot.caption = element_text(colour = 'white',size = 13,hjust = 1,
                                    family= "Anton"  ),
        plot.title  = element_text(colour = 'white',size = 20,hjust = 0.5,
                                   family = "Anton"    ),
        plot.subtitle = element_text(colour = 'white',size = 10,hjust = 0.5,
                                     family ="Anton" ),
        plot.margin = unit(c(1,0,0.1,0), "cm"))






