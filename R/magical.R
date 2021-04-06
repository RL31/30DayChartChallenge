library(tidyverse)
library(tidytext)
library(extrafont)
library(ggwordcloud)
library(viridis)
loadfonts(device="win")

data <- here::here("30DayChartChallenge","data","hp")

liste_sortileges <- read.csv(paste0(data,"/sortileges.csv"),header = FALSE) %>% 
  set_names("sortilege") %>% 
  mutate(sortilege=trimws(str_to_lower(sortilege)),
         id=row_number()) %>% 
  pivot_wider(names_from="id",values_from="sortilege") %>%
  as.list() %>% 
  paste0(collapse = "|")

liste_fichiers <- c("Book 1 - The Philosopher's Stone.txt",
           "Book 2 - The Chamber of Secrets.txt",
           "Book 3 - The Prisoner of Azkaban.txt",
           "Book 4 - The Goblet of Fire.txt",
           "Book 5 - The Order of the Phoenix.txt",
           "Book 6 - The Half Blood Prince.txt",
           "Book 7 - The Deathly Hallows.txt"
           ) %>% as.matrix()

sortileges <- function(TITRE){
  book <- readLines(paste0(data,"/",TITRE),
                             encoding = "UTF-8"         ) %>% 
    tibble()  %>% 
    set_names("text") %>% 
    unnest_ngrams(word,text,n=2) %>% 
    filter(str_detect(word,pattern=liste_sortileges)) %>% 
    mutate(sort=str_extract_all(word,pattern=liste_sortileges)) %>% 
    as.data.frame() %>% 
    count(sort) %>% 
    mutate(livre=as.factor(substr(TITRE,10,nchar(TITRE)-4)))
  
  return(book)
}

resultat <- map(liste_fichiers,~sortileges(.)) %>% 
  reduce(bind_rows)

resultat %>% 
  arrange(desc(n)) %>% 
    ggplot(aes(label = sort, size=n)) +
  geom_text_wordcloud_area(mask=png::readPNG(paste0(data,"/bolt-solid2.png")),
                           aes(color=livre),
                           rm_outside = TRUE,
                           shape="square",
                           show.legend = TRUE
)+
  scale_size_area(max_size=10)+
  scale_color_viridis(name="",discrete=TRUE,option = "viridis")+
 # facet_wrap(~livre)+
  labs( title="Harry Potter: florilège des sortilèges",
        subtitle="Sortilèges invoqués dans chaque tome\n  \n  ",
        caption = "Source: J.K. Rowling, saga Harry Potter\nTraitement et erreurs : @Re_Mi_La"
  )+
  theme_void()+
  theme(
    text = element_text(family = "Courier New", color="mediumpurple4"),
    panel.grid= element_blank(),
    plot.background = element_rect(color="snow",fill="snow"),
    legend.position = "bottom",
    plot.caption = element_text(size=8,face="italic" ),
    plot.title = element_text(hjust=.5,face="bold",size=22),
    plot.subtitle = element_text(hjust=.5)
    
      )+
  guides(size="none",
         color=guide_legend(override.aes = list(face="bold",label="???",
                                                family="FontAwesome" ) ))
