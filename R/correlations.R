library(tidyverse)
library(lubridate)
library(extrafont)
loadfonts(device="win")

bicyclettes <- read.csv(paste0(here::here("30DayChartChallenge","data","correlation"),"/valeur_mensuelle.csv"),
                       sep=";",skip=2,encoding = "UTF-8") %>% 
  mutate(date=ymd(paste0(Période,"-01")))

ggplot()+
  geom_line(data = bicyclettes,
            aes(x=date,y=X,group=1),
            color="darkolivegreen4",size=1)+
  geom_vline(aes(xintercept = ymd("2020-03-01")),size=1,color="firebrick3",linetype="dotted")+
  scale_x_date(breaks="6 months",date_labels = "%b %Y" )+
  labs(title="L'offre et la demande : l'indice des prix \"bicyclettes\" repart à la hausse depuis mars 2020",
       subtitle = "Indice des prix à la consommation, base 2015, ensemble des ménages",
       x="",
       y="Indice base 2015",
       caption="Source: Insee, IPC\nTraitements et erreurs : @Re_Mi_La"
  )+
  theme_minimal()+
  theme(plot.title=element_text(size=15,face="bold",colour="darkolivegreen4"),
        text=element_text(size=11,family = "Calibri"),
        plot.caption=element_text(size=9, face = "italic", colour = "grey40"),
        plot.subtitle=element_text(size=12,face="italic"))


ggsave(paste0(here::here("30DayChartChallenge","img"),"/correlation.jpg"),
       width = 30,height =10 ,dpi=300, units = "cm")
