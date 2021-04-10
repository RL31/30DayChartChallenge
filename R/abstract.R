library(tidyverse)
library(ggfx)
library(extrafont)
loadfonts(device="win")

oignons <- read.csv(paste0(here::here("30DayChartChallenge","data","abstract"),"/oignons.csv"))

# cercles <- tibble(x=c(47,47,47,47,47,47,47),
#                   y=c(1,1,1,1,1,1,1),
#                   size=c(1,2,3,4,5,6,7))

oignons %>% 
  ggplot()+
  with_shadow(
    geom_violin(aes(X50,y=1),fill="sandybrown",color=NA),
    colour="gray60")+
  # with_blur(geom_point(data=cercles,aes(x=x,y=y,size=size),alpha=.5,shape=1,color="sienna4",show.legend = FALSE ),
  #           sigma = 2)+
  scale_size_continuous(range = c(1,75))+
    coord_flip()+
  labs(title="Oignon(s)",
       caption = "Source: mon potager\nTraitement et erreurs : @Re_Mi_La")+
  theme_void()+
  theme(text=element_text(family="Courier New"),
        plot.title = element_text(hjust=0.5,size=30,
                                  face="bold"),
        plot.title.position = "plot",
        plot.caption = element_text(size=8,face="italic" ),
        plot.background = element_rect(color="beige",fill="beige")
  )

ggsave(paste0(here::here("30DayChartChallenge","img"),"/abstract.jpg"),
       width=10,height =15 ,units = "cm",dpi=150)
