library(tidyverse)
library(lubridate)
library(extrafont)
loadfonts(device="win")

ecarts_8.5 <-  read.delim(paste0(here::here("30DayChartChallenge","data","strips"),"/ecarts_rcp85.txt"),sep=";")
ecarts_4.5 <-  read.delim(paste0(here::here("30DayChartChallenge","data","strips"),"/ecarts_rcp45.txt"),sep=";") %>% 
  filter(Période=="H3")

ano_chaud <- ecarts_8.5 %>% count(Contexte,ATXND) %>% mutate(pct=n/sum(n)*100) %>% 
  bind_rows(
    ecarts_4.5 %>% count(Contexte,ATXND) %>% mutate(pct=n/sum(n)*100)
  ) %>% 
  arrange(Contexte,ATXND)

base <- ano_chaud %>%
  bind_cols(
    ano_chaud %>% group_by(Contexte) %>% summarise(cumul=100-cumsum(pct)+pct) %>% ungroup()%>% select(cumul)) %>% 
  select(Contexte,cumul,ATXND) %>% 
  arrange(Contexte,cumul) %>% 
  mutate(Contexte=fct_relevel(Contexte,c("RCP8.5","RCP4.5")))

ggplot()+
  geom_ribbon(data=base,aes(x=cumul,ymax=ATXND,ymin=0,fill=Contexte),
              alpha=.7,stat = 'identity',color=NA)+
  scale_fill_manual(values = c("tomato3","darkgoldenrod2"))+
  scale_x_continuous(breaks = c(seq(0,100,10)))+
  labs(title = "D'ici 2100, les journées anormalement chaudes vont se multiplier",
       subtitle = "Une journée est anormalement chaude si la température maximale dépasse de 5°C la température maximale moyenne de cette journée pour la période 1971-2005",
       y="Nombre minimal de journées\nanormalement chaudes en 2071-2100",
       x="% du territoire métropolitain",
       fill="Scénario d'évolution\ndes émissions de GES",
       caption="Source : Météo-France, Drias 2020, médianes\nwww.drias-climat.fr\nTraitements et erreurs : @Re_Mi_La")+
  theme_minimal()+
  theme(
    panel.ontop = TRUE,
    panel.grid = element_line(linetype="dotted"),
    text=element_text(family = "Calibri"),
    plot.title = element_text(size=16,face="bold",color="tomato3",hjust=0),
    plot.caption.position = "panel",
    legend.position = "bottom",
    plot.caption=element_text(size=6, face = "italic", colour = "grey40"),
    plot.subtitle=element_text(size=7,face="italic",hjust=0)
  )

ggsave(paste0(here::here("30DayChartChallenge","img"),"/strips.jpg"),
       width=20,height =12 ,units = "cm",dpi=300)
