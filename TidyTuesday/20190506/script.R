library(extrafont)
library(tidyverse)

student_ratio <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-07/student_teacher_ratio.csv")
unique(student_ratio$indicator)
df1=read.table('elements.txt',
               header = T,sep=',',stringsAsFactors = F)

df1$GroupName[113:118]='Others'


dff1=dff%>%filter(indicator=='Primary Education')%>%arrange(mean)
dff1=dff1[1:118,]

dft=data.frame(
  Column=df1$Column,
  Row=df1$Row,
  GroupName=df1$GroupName,
  continent=dff1$continent,
  codigo=dff1$country_code,
  country=dff1$country,
  value=round(dff1$mean,2),
  stringsAsFactors = F
)


loadfonts()

tile_width = 1
tile_height = 1



ggplot(dft, aes(Column, -Row)) + 
  geom_tile(data=dft, aes(fill=GroupName,width=tile_width, height=tile_height), 
            color="black",show.legend = F) + 
  geom_text( aes(label=codigo),parse=TRUE, nudge_y=.1, size=4)+
  geom_text( aes(label=value), nudge_x=-0.25, nudge_y=0.30,
            ha='left', va='top', fontweight='normal', size=3)+
  geom_text( aes(label=substr(country,1,10)), nudge_y=-0.125,size=3)+
  labs(x='',y='',caption = '@r0mymendez',
       title = 'The periodic table of education',
       subtitle = '\n Global Student to Teacher Ratios: Primary Education \n #TidyTuesday')+
  theme(plot.background = element_rect(fill='#2a2a2a')
        ,panel.background = element_rect(fill='#2a2a2a')
        ,axis.text.x = element_blank()
        ,axis.ticks = element_blank() 
        ,axis.text.y = element_blank()
        ,panel.grid.major= element_blank()
        ,axis.line = element_blank()
        ,plot.title = element_text(color = 'white',hjust = 0.5,size = 50,family = "Pacifico")
        ,plot.caption = element_text(color = 'white',size=18,family = "Pacifico")
        ,plot.subtitle = element_text(color = 'white',hjust = 0.5)
        ,panel.grid = element_line(colour = '#2a2a2a')
  )


ggsave("plot.jpg")

