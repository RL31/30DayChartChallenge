library(tidyverse)
library(gganimate)
library(ggimage)
library(extrafont)
library(png)
loadfonts(device="win")

data <- here::here("30DayChartChallenge","data","slope")

gambetta <- read.csv2(paste0(data,"/20.csv"),
                      sep = "\t") %>% 
  mutate( distance_totale=cumsum(Distance)) %>% 
  select(distance_totale,Elevation,Distance)
belleville <- read.csv2(paste0(data,"/19.csv"),
                        sep = "\t") %>% 
  mutate( distance_totale=cumsum(Distance)) %>% 
  select(distance_totale,Elevation,Distance)

rues <- gambetta %>% mutate(rue="Gambetta")%>%
  bind_rows(belleville %>% 
              mutate(rue="Belleville") ) %>% 
  group_by(rue) %>% 
  mutate(image=paste0(here::here("30DayChartChallenge","data","icons"),"/biking-solid.svg")
         #,
         # pente=replace_na(((Elevation/((lag(Elevation,n=1)+lag(Elevation,n=3))/2)-1)*100)*45/100,
         #                   45),
         #  pente=if_else(pente>0,pente+270,-pente),
         #  dist_pct=distance_totale/sum(Distance)*100
  )


graphique <- ggplot(data=rues,aes(x = distance_totale,y=Elevation,fill=rue)) +
  geom_ribbon(aes(ymax=Elevation,ymin=50),alpha=.6)+
  geom_image(aes(image = image,color=rue),
             size = 0.07,
             nudge_y=2) +
  scale_fill_manual(name="",
                    values=c("sienna2","olivedrab3"),
                    labels=c("Belleville"="Rue de Belleville","Gambetta"="Avenue Gambetta"))+
  scale_color_manual(name="",
                     values=c("sienna2","olivedrab3"),
                     labels=c("Belleville"="Rue de Belleville","Gambetta"="Rue de Gambetta"))+
  scale_x_continuous(name="Distance (m)" )+
  coord_cartesian(clip="off")+
  labs( title="Le chemin le plus court est souvent... plus pentu !",
        subtitle="Distance et dénivelé des rues Gambetta et Belleville à Paris",
        y="Altitude (m)",
        caption = "Source: BRouter.de\nTraitements et erreurs: @Re_Mi_La")+
  theme_minimal() +
  theme(text = element_text(family = "Calibri"),
        legend.position = "bottom",
        plot.caption = element_text(size=8,face="italic" ),
        plot.title = element_text(hjust=0,face="bold",size=17, color="olivedrab3"),
        plot.subtitle = element_text(hjust=0, color="olivedrab3"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(linetype="dashed"),
        panel.grid.minor.y = element_line(linetype="dashed"))+
  guides(color="none" )+
  transition_reveal(distance_totale)


animate(graphique,fps=10,duration=10, width=20,height=12,res=150, units="cm",end_pause=20)
anim_save(paste0(here::here("30DayChartChallenge","img"),"/slope.gif"))
