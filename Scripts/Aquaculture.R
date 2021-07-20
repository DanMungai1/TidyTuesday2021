library(tidyverse)
library(rKenyaCensus)
library(tmap) # World map shapes
library(sf) # Plot maps with ggplot
library(geogrid) # Convert shapefile into hexagon shapefile
library(stringr) # String operations (replace characters, remove characters)
library(patchwork)
#Add fonts from google
sysfonts::font_add_google(name = "Fresca","Fresca")
showtext::showtext_auto()

livestock <- rKenyaCensus::V4_T2.24
df <- livestock %>% mutate(FishPonds = replace_na(FishPonds, 0),
                           FishCages = replace_na(FishCages, 0),
                           Aquaculture = FishPonds + FishCages) %>% 
  pivot_longer(cols = 5:20, names_to = "Livestock", 
                                 values_to = "Counts") %>% 
  filter(!SubCounty == "KENYA",
         !Livestock %in% c("FishCages, FishPonds")) %>% select(-c(2:4)) %>% 
  mutate(Counts = replace_na(Counts, 0)) %>% group_by(County, Livestock) %>% 
  summarise(Animal_pop = sum(Counts)) %>% 
  filter(Livestock == "Aquaculture") %>% arrange(desc(Animal_pop))
kenya_shapes %>%
  left_join(df,by=c("County_for_join"="County"))%>% 
  ggplot()+geom_sf(aes(fill= Animal_pop),col=NA)+
  scale_fill_gradient(low="grey80",high="darkorange")+
  theme_void() + labs(title = "Aquaculture In Kenya",
                      subtitle = "The distribution of Cages and ponds",
                      caption = "Dan_Mungai courtesy of @Shel_Kariuki",
                      fill = "Cages & Ponds") +
  theme(plot.title = element_text(hjust = .5, size = 18),
        plot.subtitle = element_text(hjust = .5, size = 15))

