library(tidyverse)
library(lubridate)
library(extrafont)
loadfonts(device="win")

ref <- read.delim(paste0(here::here("30DayChartChallenge","data","globalchange"),"/ALADIN63_Historical_4062.txt"),sep=";")
rcp8.5 <- read.delim(paste0(here::here("30DayChartChallenge","data","globalchange"),"/ALADIN63_rcp8.5_4062.txt"),sep=";")

base <- ref %>% 
  bind_rows(rcp8.5) %>% 
  mutate(date=ymd(paste0(substr(Date,1,4),"-",substr(Date,5,6),"-",substr(Date,7,8)))) %>% 
  group_by(annee=year(date)) %>% 
  summarise(t_moy=mean(tasAdjust))

# min(base$t_moy)
# median(base$t_moy)
# max(base$t_moy)

ggplot()+
  geom_tile(data=base, aes(x=annee,y=1,fill=t_moy))+
  scale_fill_gradient2(name="",
                       low="dodgerblue4",mid="white",high="firebrick3",
                       midpoint=13.2,#16.4,#14.4,#13.2, #base %>% filter(annee==2021) %>% select(t_moy)
                       guide=FALSE )+
  geom_tile(data=base %>% filter(annee==2021), 
            aes(x=annee,y=1),
            fill="transparent",color="grey40",linetype="dotted",size=1)+
  geom_text(data=base %>% filter(annee==2021),
            aes(x=annee,y=.45,label=annee))+
  scale_x_continuous(limits = c(1950,2100), expand = c(0, 0)) +
  labs(title = "Toulouse, ville rouge",
       subtitle = "Température annuelle moyenne, 1950-2100, scénario RCP 8.5 (min.: 12,0°C ; réf. 2021 : 13,2°C ; max.: 18,8°C)",
       x="",
       caption="Source : Météo-France, CNRM-ALADIN63 RCP8.5\nwww.drias-climat.fr\nTraitements et erreurs : @Re_Mi_La")+
  theme_minimal()+
  theme(
    text=element_text(family = "Calibri"),
    plot.title = element_text(size=20,face="bold",color="firebrick3",hjust=0),
    plot.caption.position = "panel",
    legend.position = "bottom",
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.caption=element_text(size=9, face = "italic", colour = "grey40"),
    plot.subtitle=element_text(size=10,face="italic",hjust=0)
  )


ggsave(paste0(here::here("30DayChartChallenge","img"),"/globalchange.jpg"),
       width=30,height =10 ,units = "cm",dpi=300)
