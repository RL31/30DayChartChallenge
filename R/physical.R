install.packages(c("httr","jsonlite","directlabels"))
library(jsonlite)
library(httr)
library(directlabels)
library(tidyverse)
library(extrafont)
library(viridis)
loadfonts(device="pdf")

parametres<-tribble(
  ~nom,~lat1,~long1,~lat2,~long2,
  "Séponet",44.846361 , 4.173152,44.846376 , 4.179418,
  "Sépoux",44.838313, 4.176908,44.837156,4.189482,
  "Taupernas",44.864356 , 4.16399, 44.854804 , 4.167595,
  "Montfol",44.847806,4.154291,44.84306,4.164462,
  "Gerbier",44.844612,4.216325,44.842847,4.224114
)

profil_alti<-function(NOM,LAT1,LAT2,LONG1,LONG2,SAMP=30){
  sommet<-GET(
    paste0("https://wxs.ign.fr/choisirgeoportail/alti/rest/elevationLine.json?sampling=",
           SAMP,"&lon=",LONG1,"|",LONG2,"&lat=",LAT1,"|",LAT2,"&indent=true")) %>% 
    content("text") %>% 
    fromJSON(flatten = TRUE) %>% 
    as.data.frame() %>% 
    mutate(id=row_number(),
           nom=NOM)  
}

profils <- pmap_df(list(parametres$nom,
                        parametres$lat1,
                        parametres$lat2,
                        parametres$long1,
                        parametres$long2,
                        30),
                   profil_alti)

ggplot(data=profils,
                    aes(x=id,y=elevations.z,group=nom,color=nom))+
  geom_line(size=1.5,alpha=.7)+
  geom_dl(aes(label = nom), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
  scale_color_viridis(discrete = TRUE,option = "viridis")+
  labs(title="Des sommets presque normaux",
       subtitle="Profils altimétriques des sucs du plateau ardéchois",
       x="",
       y="Altitude (m)",
       caption="Source: IGN, API ALTI\nTraitements et erreurs: @Re_Mi_La")+
  coord_cartesian(clip="off")+
  theme_minimal()+
  theme(
    text = element_text(family = "Calibri"),
    plot.caption = element_text(size=8,face="italic" ),
    plot.title = element_text(hjust=0,face="bold",size=17, color=viridis_pal()(1) ),
    plot.subtitle = element_text(hjust=0, color=viridis_pal()(1)),
    plot.margin = margin(1,1,1,1,unit = "cm"),
    panel.grid = element_blank(),
    axis.text.x = element_blank()
  )+
  guides(color="none")
