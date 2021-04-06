library(tidyverse)
library(extrafont)
loadfonts(device = "win")

source(paste0(here::here("30DayChartChallenge","R"),"/geom_flat_violin.R"))

data <- here::here("30DayChartChallenge","data","aubenas")

archives <- read.csv(paste0(data,"/age_deces_aubenas.csv")) %>% 
  select(a1900,s1900) %>%
  rename(age=a1900,sexe=s1900) %>%
  mutate(annee=as.factor("1900")) %>% 
  bind_rows(read.csv(paste0(data,"/age_deces_aubenas.csv")) %>% 
              select(a1803,s1803) %>% 
              rename(age=a1803,sexe=s1803) %>% 
              mutate(annee=as.factor("1803")))
  
b2000 <- read.csv2(paste0(data,"/deces-2000.csv")) %>% 
  filter(lieudeces=="07019") %>% 
  mutate(age=2000-as.integer(substr(datenaiss,1,4)),
         annee="2000",
         sexe=if_else(sexe=="1","H","F")) %>% 
  select(sexe,age,annee)

base_ok <- archives %>% 
  bind_rows(b2000)

ggplot(data=base_ok %>% filter(sexe!=""),
       aes(x = annee,y=age)) +
  geom_flat_violin(aes(fill=sexe),
                   color=NA,
                   alpha=0.7,position = position_nudge(x = .2, y = 0)) +
  geom_jitter(aes(color=sexe),alpha=0.7,width=0.15)+
  scale_color_manual( name="",
    values=c("H"="plum4","F"="orange1"),
    labels=c("H"="Homme","F"="Femme"))+
  scale_fill_manual(name="",
    values=c("H"="plum4","F"="orange1"),
    labels=c("H"="Homme","F"="Femme"))+
  labs( title="Deux siècles de bouleversements démographiques à Aubenas",
        subtitle="Disparition de la mortalité infantile, hausse de l'espérance de vie...",
    x="Année de décès",
    y="Âge au décès",
    caption = "Source: Archives départementales de l'Ardèche\nInsee, état-civil 2000\nTraitement et erreurs : @Re_Mi_La"
      )+
  theme_minimal()+
  theme(
    text = element_text(family = "Calibri", color="gray20"),
    panel.grid= element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(size=8,face="italic" ),
    plot.title = element_text(size=18)
    )
