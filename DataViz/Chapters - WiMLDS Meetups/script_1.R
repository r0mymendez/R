rm(list=ls())


library(rvest)
library(tidyverse)
library(grid)
library(gridExtra)
library(ggpubr)
library(ggrepel)
library(extrafont)


url='http://wimlds.org/chapters/'

pag=read_html(url)

chapter_url=pag%>%html_nodes('#pg-1155-0 a')%>%html_attr('href')
chapter=pag%>%html_nodes('#pg-1155-0 a')%>%html_text()

data=data.frame(chapter,chapter_url,chapter_meetup=NA,city=NA, 
                members=NA,stringsAsFactors = F)

for(i in 1:length(chapter_url)){
  tryCatch({
  pagm=read_html(chapter_url[i])
  data$chapter_meetup[i]=pagm%>%
    html_nodes('main#main p:nth-child(1) > a')%>%
    html_attr('href')
  
  print(i)
  print(data$chapter_meetup[i]) }, error=function(e){
    
    print(paste0('El error esta en el registro',i))
  })
}


data$chapter_meetup_url=
  ifelse(substr(data$chapter_meetup,1,6)=='https:' | 
           substr(data$chapter_meetup,1,6)=='http:/',
         data$chapter_meetup,paste0('https://',data$chapter_meetup))
data$chapter_meetup_url=
  ifelse(str_detect(data$chapter_meetup_url,'meetup.com'),
         data$chapter_meetup_url,
         NA)


for(i in 1:nrow(data)){
  tryCatch({
  if(is.na(data$chapter_meetup_url[i])==F){
  pagm=read_html(data$chapter_meetup_url[i])
    data$city[i]=pagm%>%
      html_nodes('main#mupMain div:nth-child(2) > div > div > div:nth-child(1) > ul > li > a > span')%>%html_text()
    data$members[i]=pagm%>% 
      html_node('.groupHomeHeaderInfo-memberLink')%>%
      html_text()  
  }
  print( data$city[i])
  print( data$members[i]) }, error=function(e){
    
    print(paste0('El error esta en el registro ',i))
  })
}

rm(pag,pagm,cant,chapter,chapter_meetup,chapter_url,i,lugar,url,wimlds)

#MAP
dfcity=read.csv('worldcities.csv',header=T,stringsAsFactors = F,encoding='UTF-8')
data1=cbind(data,data%>%
             separate(chapter,c("vcity", "vdepto", 'vcountry'), "," )%>%
             select(vcity,vdepto,vcountry)%>%mutate(
               vdepto=trimws(vdepto),
               vcountry=trimws(vcountry),
               vcountry=ifelse(is.na(vcountry),vdepto,vcountry),
               vdepto=ifelse(vdepto==vcountry,NA,vdepto)
             ))

data1$vdepto[4]="Québec"
data1$vcountry[4]="Canada"
data1$vcountry=ifelse(data1$vcountry=='USA','United States',data1$vcountry)
data1$vcity=ifelse(data1$vcity=='Bay Area','Bay Point',data1$vcity)
data1$vcity=ifelse(data1$vcity=='Trojmiasto','Gdansk',data1$vcity)
data1$vcity=ifelse(data1$vcity=='Amaravati','Amravati',data1$vcity)
data1$vcity=ifelse(data1$vcity=='Vacoas-Phoenix','Port Louis',data1$vcity)
data1$vcity=ifelse(data1$vcity=='Sophia-Antipolis','Nice',data1$vcity)
data1$lat=NA
data1$lng=NA
data1$members_value=data1$members
data1$members_value=gsub('\\,','',data1$members_value)
data1$members_value=gsub('\\???','',data1$members_value)
data1=data1%>%separate(members_value,c("cant", "term", " " ))
data1$cant=as.numeric(data1$cant)
data1[,14]=NULL


data_top_20=data1%>%
  filter(is.na(cant)==F)%>%top_n(20,cant)

for(i in 1:nrow(data)){
  a=data.frame()
  if(is.na(data1$vdepto[i])==F){
    a=dfcity%>%filter((city %in% data1$vcity[i]|
                       city_ascii %in% data1$vcity[i]),
                    admin_name %in% data1$vdepto[i],
                    country %in% data1$vcountry[i])%>%
      select(lat,lng)
  }else{
    a=dfcity%>%filter((city %in% data1$vcity[i]|
                        city_ascii %in% data1$vcity[i]),
                      country %in% data1$vcountry[i])%>%
      select(lat,lng)
  }
 
  if(is.na(a$lat[1])==T){
    a=dfcity%>%filter(city %in% data1$vcity[i],
                      country %in% data1$vcountry[i])%>%
      select(lat,lng)
  }
  if(is.na(a$lat[1])==T){
    a=dfcity%>%filter(admin_name %in% data1$vdepto[i],
                      country %in% data1$vcountry[i])%>%
      select(lat,lng)
  }
  if(is.na(a$lat[1])==T){
    a=dfcity%>%filter(admin_name %in% data1$vcity[i],
                      country %in% data1$vcountry[i])%>%
      select(lat,lng)
  }
  
  if(nrow(a)>0){
    data1$lat[i]=a$lat[1]
    data1$lng[i]=a$lng[1]
  }
  print(data1$lat[i])

}

img_b <-png::readPNG("1.jpeg")
b <- grid::rasterGrob(img_b, interpolate = T) 

img_c <-png::readPNG("3.png")
c <- grid::rasterGrob(img_c, interpolate = T)

world_map <- map_data("world")


ggplot() +
  geom_polygon(data=world_map, 
               aes(x = long, y = lat, group = group),
               fill="#2a2a2a", colour = "#799ABD")+
  geom_point(data=data1,aes(x=lng,y=lat,size=cant),
             color="white",alpha=0.7,show.legend = F)+
  labs(x='',y='',title = paste0('Chapters - WiMLDS Meetups  ',emojifont::emoji('two_women_holding_hands')),
       subtitle = 'The 20 cities with more members')+
  annotation_custom(b, xmin =-100, xmax =-180,
                    ymin=-30 ,ymax=-50)+
  annotation_custom(c, xmin =-90, xmax =-180,
                    ymin=-40 ,ymax=-70)+
  theme_void()+
  theme(
    legend.background = element_rect(fill='#2a2a2a'),
    legend.text = element_text(color='white'),
    plot.background = element_rect(fill='#2a2a2a',color ='#2a2a2a' ),
    panel.background =element_rect(fill='#2a2a2a'),
    legend.position=c(0.11, 0.32),
    legend.key = element_rect(fill = "#2a2a2a", color = NA),
    legend.title = element_text(color = "white", size = 10, hjust = 0.5),
    plot.title = element_text(family =     "Atma"    ,
                              hjust = 0.5,
                              size=30,colour = '#77D1EA',
                              face = 'bold'),
    plot.subtitle =  element_text(family =     "Gill Sans MT"    ,
                                  hjust = 0.5,
                                  size=12,colour = '#77D1EA',
                                  face = 'bold') 
  )+
  geom_label_repel(aes(label=paste0(vcity,':',cant),
                       x=lng,
                       y=lat),
                   size=3, show.legend = F,
                   data=data_top_20,fill ='#fffeea',
                   segment.color = 'white')+
  geom_label_repel(aes(label=paste0(vcity,':',cant),
                       x=lng,
                       y=lat),
                   size=3, show.legend = F,
                   data=data1%>%filter(vcity=='Buenos Aires'),
                   fill ='#799ABD',
                   color = 'white',
                   fontface = 'bold',
                   segment.color = 'white')
  



