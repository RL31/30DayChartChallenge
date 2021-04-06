library(tidyverse)
library(ggimage)
library(ggtext)
#install.packages("patchwork",dependencies = T )
library(patchwork)
library(extrafont)

loadfonts(device = "win")
theme_set(theme_void(base_family = "Consolas"))

# paper and data here https://www.insee.fr/fr/statistiques/5013868

car <- paste0(here::here("30DayChartChallenge","data","icons"),"/car-solid2.png")
motorcycle <- paste0(here::here("30DayChartChallenge","data","icons"),"/motorcycle-solid2.png")
bus <- paste0(here::here("30DayChartChallenge","data","icons"),"/bus-solid2.png")
walking<- paste0(here::here("30DayChartChallenge","data","icons"),"/walking-solid2.png")
biking <- paste0(here::here("30DayChartChallenge","data","icons"),"/biking-solid2.png")

df <- tibble(x = c(rep(seq(0, 9*0.05, 0.05),each=10),
                   rep(seq(0, 9*0.05, 0.05),each=10),
                   rep(seq(0, 9*0.05, 0.05),each=10)),
             y = c(rep(seq(0, 9*0.05, 0.05), 30)),
             ville=c(rep("Toulouse",100),
                     rep("Strasbourg",100),
                     rep("Marseille",100)),
             image = c(rep(car, 54),
               rep(motorcycle, 3),
               rep(bus, 27),
               rep(walking, 8),
               rep(biking, 8),
               rep(car, 43),
               rep(motorcycle, 1),
               rep(bus, 29),
               rep(walking, 10),
               rep(biking, 17),
               rep(car, 55),
               rep(motorcycle, 6),
               rep(bus, 27),
               rep(walking, 10),
               rep(biking, 2)),
             color= c(rep("gray70",92),
               rep("chartreuse4",8),
               rep("gray70",83),
               rep("chartreuse4",17),
               rep("gray70",98),
               rep("chartreuse4",2)))

Toulouse <- ggplot(df %>% filter(ville=="Toulouse") , aes(x=x, y=y)) + 
  geom_image(aes(image=image,color=color), size=0.13) +
  scale_color_identity()+
  coord_cartesian(clip="off") +
  labs(title="Toulouse\n ")+
  theme_void()+
  theme(plot.margin = margin(t = 1, r = 1, b = 2, l = 1, unit = "cm"),
        plot.title = element_text(hjust=0.5,size=25, color="chartreuse4",
                              face="bold",family = "Calibri"),
    plot.caption = element_text(size=8,face="italic" ),
    plot.caption.position = "plot" 
  )

Marseille <- ggplot(df %>% filter(ville=="Marseille"), aes(x=x, y=y)) + 
  geom_image(aes(image=image,color=color), size=0.13) +
  scale_color_identity()+
  coord_cartesian(clip="off") +
  labs(title="Marseille\n ",
       caption = "source: Insee, Insee Première n°1835\ntraitement et erreurs : @Re_Mi_La")+
  theme(
    plot.margin = margin(t = 1, r = 1, b = 2, l = 1, unit = "cm"),
    plot.title = element_text(hjust=0.5,size=25,color="chartreuse4",
                              face="bold",family = "Calibri"),
    plot.caption = element_text(size=8,face="italic" ),
    plot.caption.position = "plot" 
  )

Strasbourg <- ggplot(df %>% filter(ville=="Strasbourg"), aes(x=x, y=y)) + 
  geom_image(aes(image=image,color=color), size=0.13) +
  scale_color_identity()+
  coord_cartesian(clip="off") +
  labs(title="Strasbourg\n "  )+
  theme(plot.margin = margin(t = 1, r = 1, b = 2, l = 1, unit = "cm"),
    plot.title = element_text(hjust=0.5,size=25,color="chartreuse4",
                              face="bold",family = "Calibri"),
    plot.caption = element_text(size=8,face="italic" ),
    plot.caption.position = "plot" 
  )


Strasbourg+Toulouse+Marseille

