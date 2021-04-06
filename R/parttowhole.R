library(tidyverse)
library(waffle)
#install.packages("waffle", repos = "https://cinc.rud.is")

# files can be download here : https://www.insee.fr/fr/information/4190491

polonais <- function(AN){
  annee <- read.csv2(paste0("deces-",AN,".csv")) %>% 
    filter(paysnaiss=="POLOGNE") %>% 
    separate(col=nomprenom,c("nom",NA),sep="[*]",extra = "drop") %>% 
    mutate(nom_sk=case_when(
      substr(nom,nchar(nom)-2,nchar(nom)) %in% c("SKI","SKY") ~ "Masculin",
      substr(nom,nchar(nom)-2,nchar(nom)) %in% c("SKA") ~ "Féminin",
      substr(nom,nchar(nom)-2,nchar(nom)) %in% c("SCY") ~ "Pluriel",
      TRUE ~ "Autre")) %>% 
    select(nom,nom_sk,datenaiss,commnaiss,sexe) 
  return(annee)
}

liste <- c(as.character(rep(1970:2019)))

base <- map(liste,~polonais(.)) %>% 
  reduce(bind_rows) %>% 
  filter(nom_sk!="Autre") %>% 
  mutate(sexe=if_else(sexe==1,"Hommes","Femmes")) %>% 
  count(sexe,nom_sk) %>% 
  group_by(sexe) %>% 
  mutate(pct=round(n/sum(n)*100),0) %>% 
  ggplot( aes(fill = nom_sk, values = n/10)) +
  geom_waffle(color = "gray40",size = 1, n_rows = 25, flip = TRUE) +
  facet_wrap(~sexe, nrow = 1, strip.position = "bottom",
             labeller =  ) +
  scale_fill_manual(name="Déclinaison",
                    labels=c("Masculin"="Maculine -ski","Féminin"="Féminine -ska"),
                    values=c("Masculin"="#FFFFFF","Féminin"="#D22630"))+
  scale_x_discrete() + 
  scale_y_continuous() +
  coord_equal(clip = "off" ) +
  labs(title = "Transcription de noms polonais à l'état civil : le féminin se décline parfois au masculin",
       subtitle = "Personnes nées en Pologne et décédées en France entre 1970 et 2019, noms en -sk*",
       x = "",
       y = "",
       caption = "source: Insee, état civil\ntraitement et erreurs : @Re_Mi_La"
  ) +
  theme_void()+
  theme(text = element_text(colour="white"),
        panel.grid = element_blank(),
        axis.ticks.y = element_line(),
        plot.margin =(margin(1,3,1,1, unit="cm")),
        plot.background = element_rect(fill="gray40",
                                       color="gray40"),
        panel.background = element_rect(fill="gray40",
                                        color="gray40"),
        plot.caption = element_text(size=8,face="italic" ),
        plot.caption.position = "plot" ) +
  guides(fill = guide_legend(reverse = TRUE))
