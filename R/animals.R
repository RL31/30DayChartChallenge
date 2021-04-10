library(tidyverse)
library(lubridate)
library(extrafont)
library(ggimage)
library(grid)
loadfonts(device="win")

# 
# liste_animaux <- c("Chevreuils","Blaireaux","Lièvres","Sangliers","Renards","Hérons")
# 
# passages_animaux <- function(ANIMAL){
#   paths <- dir(path=paste0(here::here("animaux"),"/",ANIMAL), full.names=TRUE)
#   animal <- file.info(paths) %>%
#     tibble() %>%
#     select(mtime) %>%
#     mutate(mtime_filtre=round_date(mtime,unit="hour")) %>% 
#     distinct() %>%
#     mutate(heure=hms::as_hms(hour(round_date(mtime,unit="hour"))*3600)) %>%
#     count(heure) %>%
#     right_join(tibble(heure=c(rep(1:24))) %>%
#                  mutate(heure=hms::as_hms(heure*3600)),
#                by="heure") %>%
#     mutate(n=if_else(is.na(n),as.integer(0),n),
#            animal=ANIMAL,
#            anglais=case_when(
#              animal == "Chevreuils" ~ "Roe deer",
#              animal == "Blaireaux" ~ "Badger",
#              animal == "Lièvres" ~ "Hare",
#              animal == "Sangliers" ~ "Boar",
#              animal == "Renards" ~ "Fox",
#              animal == "Hérons" ~ "Heron",
#              TRUE ~ "PB"),
#            animal=paste0(animal," / ",anglais),
#            pct=n/sum(n)*100,
#            heure=as.POSIXct(heure))
#   return(animal)
# }
# 
# base <- map(liste_animaux,~passages_animaux(.)) %>%
#   reduce(bind_rows)
# saveRDS(base,paste0(here::here("30DayChartChallenge","data","animals"),"/animaux.RDS"))

base <- readRDS(paste0(here::here("30DayChartChallenge","data","animals"),"/animaux.RDS"))

ggplot(base,aes(x=heure,y=1,fill=pct))+
  geom_tile()+
  facet_wrap(~animal,ncol=1,strip.position = "top")+
  scale_x_datetime(name="Heure", breaks="2 hour", date_labels = "%H:%M",expand = c(0,0) )+
  scale_fill_gradient(name="%",low="gray95",high="darkolivegreen4")+
  guides(fill=guide_colorbar(barwidth = 5))+
  labs( title="A quelle heure passe le renard ?",
        subtitle="Heure de passage des animaux sauvages dans mon jardin",
        y="",
        caption = "Source: enregistrements vidéos/photos\nTraitements et erreurs: @Re_Mi_La")+
  theme_minimal() +
  coord_cartesian(clip="off")+
  theme(
    strip.text = element_text(hjust=0),
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    text = element_text(family = "Calibri"),
    legend.position = "bottom",
    plot.caption = element_text(size=8,face="italic" ),
    plot.title = element_text(hjust=0,face="bold",size=17, color="darkolivegreen4"),
    plot.subtitle = element_text(hjust=0, color="darkolivegreen4"),
    plot.margin = margin(2,2,2,2,"cm")
  )

ggsave(paste0(here::here("30DayChartChallenge","img"),"/animals.jpg"),
       dpi=150,width=20,height=15,units = "cm")
