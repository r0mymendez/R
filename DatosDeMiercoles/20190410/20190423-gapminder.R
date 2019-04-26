library(ghibli)
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(extrafont)
library(showtext)
library(gapminder)
library(emojifont)

#fonts
font_import(pattern = "MrBedfort-Regular.ttf")
loadfonts(device="win")
loadfonts()
fonts()



# Filter
df=gapminder
df_fil=df%>%filter(year==2007)%>%top_n(10,pop)

#colora: ggpomological
pomological_palette <- c(
  "#c03728" #red
  ,"#919c4c" #green darkish
  ,"#fd8f24" #orange brighter
  ,"#f5c04a" #yelloww
  ,"#e68c7c" #pink
  ,"#828585" #light grey
  ,"#c3c377" #green light
  ,"#4f5157" #darker blue/grey
  ,"#6f5438" #lighter brown
)

#image
img_a <- png::readPNG("C:/Users/ROMIMENDEZ/Desktop/ROMI/proyecto/20190423/logo.png") 
a <- grid::rasterGrob(img_a, interpolate = T) 


# Base Plot
gg <- 
  ggplot(df%>%filter(year==2007,continent != "Oceania"), 
         aes(x=gdpPercap, y=lifeExp,fill= continent,size=pop)) + 
  geom_point(show.legend = F,shape = 21,alpha=0.8) +
  scale_size_continuous(range = c(1,35))  +
  annotation_custom(a, xmin = 30000, xmax = 50000,
                    ymin = 10, ymax =100 ) +
   ylim(c(50, 87))+
  scale_fill_manual(values = pomological_palette)+
  labs(title="Pbi per capita Vs Esperanza de vida ( 2007 ) ",
       subtitle="", 
       y="Esperanza de vida",
       x="pib per capita", 
       caption=paste0("@r0mymendez",emoji("heart"))) +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.box = "horizontal",
        legend.key.size = unit(1, "cm"),
        plot.caption = element_text(size=12,family = "Lucida Calligraphy"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#fffeea",
                                        colour = "#fffeea"),
        plot.background =  element_rect(fill = "#fffeea",
                                        colour = "#fffeea"),
        plot.title=element_text(family="Mr Bedfort", size = 30),
        plot.subtitle = element_text(family="Mr Bedfort", size = 20),
        text=element_text(family="Mr Bedfort", size = 20),
        axis.text.x=element_text(colour="black", size = 14),
        axis.text.y=element_text(colour="black", size = 14))+
  geom_label_repel(
                aes(label=country),
            size=4, data=df_fil,family="Mr Bedfort",
            fill ='#fffeea') + 
  theme(legend.position = "None") 

gg



