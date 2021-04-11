library(tidyverse)
library(forecast)
library(lubridate)
library(gganimate)
library(extrafont)
loadfonts(device = "win")

# Original data here : https://www.insee.fr/fr/statistiques/4487988?sommaire=4487854
#
# base_courbe <- function(AN){
#   fichier <- paste0(here::here("30DayChartChallenge","data","circle"),"/DC_",AN,"_det.csv")
#   par_jour <- read.csv2(fichier) %>% 
#     mutate(date=dmy(paste0(JDEC,"-",MDEC,"-",ADEC))) %>% 
#     select(date)%>% 
#     count(date)
# }
# 
# liste <- c("2018","2019","20202021")
# base <- map(liste,~base_courbe(.)) %>% 
#   reduce(bind_rows) %>% 
#   mutate(n_lisse=ma(n,7))
# 
# saveRDS(base,paste0(here::here("30DayChartChallenge","data","circle"),"/base.RDS"))

base <- readRDS(paste0(here::here("30DayChartChallenge","data","circle"),"/base.RDS"))


frame_count <- (max(base$date) - min(base$date))/ lubridate::ddays(1)
cycle_length <- 365

base_diapo <- map_df(seq_len(frame_count), ~base, .id = "id") %>% 
  mutate(id=as.integer(id),
         view_date = min(base$date)+ id - 1) %>% 
  filter(date<= view_date) %>% 
  mutate(days_ago = (view_date - date) / ddays(1),
         phase_dif = (days_ago %% cycle_length) / cycle_length,
         x_pos = -sin(2*pi * phase_dif),
         nearness = cos(2*pi * phase_dif),
         etiquette = case_when(
           day(date)==1 & days_ago < 365 ~ format(date, "%B %Y"),
           TRUE ~ ""),
         y_com=n_lisse*(1+0.1)) %>% 
  mutate(commentaire = case_when(
    date %in% c(ymd("2018-01-04"),ymd("2018-03-07"),ymd("2019-02-10")) & days_ago < 90 ~ "Grippe",
    date %in% c(ymd("2019-07-02"),ymd("2019-07-26"),ymd("2020-08-10")) & days_ago < 90 ~ "Canicule",
    date==ymd("2020-04-01") ~ "Première vague Covid-19",
    date==ymd("2020-11-01") ~ "Deuxième vague Covid-19",
    TRUE ~ ""))


graphique <- ggplot(base_diapo)+
  geom_path(aes(x=x_pos,y=n_lisse,alpha = nearness),
            color = "gray40",size = 1)+
  geom_text(aes(x=x_pos,y=1100, label=etiquette,alpha = nearness),
            size=4, color = "gray40", show.legend = FALSE)+
  geom_text(aes(x=x_pos,y=y_com,label=commentaire,alpha = nearness,fontface="italic"),
            size=4, color = "gray40", show.legend = FALSE )+
  labs( title="Covid-19 : un séisme pour la mortalité",
        subtitle=expression("Décès quotidiens depuis le 1"^er*" janvier 2018, effectifs lissés sur 7 jours"),
        x="",y="",
        caption="Source: Insee, état civil jusqu'au 2 avril 2021\nBulletins Santé Publique France pour les surmortalités exceptionnelles\nScript R : @JustTheSpring\nTraitements et erreurs : @Re_Mi_La"
  )+
  theme_minimal()+
  theme(plot.title=element_text(face="bold",colour="dodgerblue4"),
        text=element_text(size=15,family = "Calibri"),
        axis.line.x=element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid=element_blank(),
        plot.caption=element_text(size=9, face = "italic", colour = "grey40"),
        plot.subtitle=element_text(size=12,face="italic"))+
  guides(size = "none", alpha = "none", colour = "none") +
  transition_manual(id)

animate(graphique,fps=10,duration=75,width = 700,height = 400,end_pause=50)
anim_save(paste0(here::here("30DayChartChallenge","img"),"/sismogramme.gif"))
