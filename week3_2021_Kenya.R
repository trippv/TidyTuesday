#Tidytuesday
# week 4, 2021
#Miguel Tripp Valdez
#Kenya Census

#devtools::install_github("Shelmith-Kariuki/rKenyaCensus")

library(tidyverse)
library(rKenyaCensus)

# 1. Load data ------------------------------------------------------------


school <- V4_T2.2 %>% 
  as_tibble(.)

# Distribution of Population Aged 3 Years and Above by School Attendance Status, Sex and Special Age Groups

# mean data by all countries

school_c <- school %>% 
  select(Gender,SubCounty, contains("_Perc")) %>% 
  drop_na() %>% 
  mutate(Total_Perc = rowSums(select(., contains("Perc")), na.rm = TRUE)) %>% 
  #filter only female and male
  filter(Gender %in% c("Male", "Female")) %>% 
  #Add a new column with the sum of left school before and after
  mutate(LeftSchool_Perc = rowSums(select(., contains("Left")), na.rm = TRUE)) %>% 
  select(Gender, SubCounty, LeftSchool_Perc, contains("Never")) %>% 
  #Some counties are duplicated. Calculate mean value
  group_by(Gender, SubCounty) %>% 
  transmute(LeftSchool_Perc1 = mean(LeftSchool_Perc),
         NeverbeentoSchool_Perc1 = mean(NeverbeentoSchool_Perc)) %>% 

  ungroup() %>% 
  pivot_longer(-c(Gender,SubCounty), names_to = "SchoolStatus") %>% 
  group_by(Gender, SubCounty) %>% 
  slice(1:2) %>% 
  mutate(count = n()) %>% 
  mutate(NoSchool = sum(value)) %>% 
  arrange(-NoSchool)
  
  


# plot
ggplot(school_c, aes(x = reorder(SubCounty, -NoSchool), y = value, fill = SchoolStatus))+
  geom_bar(stat = "identity", alpha = 0.6, size = 1.5)+
  facet_wrap(~Gender)+
  coord_polar()+
  geom_hline(yintercept = c(0, 25, 50 , 75), linetype = "dashed", col = "grey45", size = 1)+
  geom_text(aes(x = 0, y = 25+2, label = "25%"), col = "grey45")+
  geom_text(aes(x = 0, y = 50+2, label = "50%"), col = "grey45")+
  geom_text(aes(x = 0, y = 75+2, label = "75%"), col = "grey45")+
  scale_fill_manual(labels = c("Left school", "Never been to school"), values = c("#8B0000", "#68838B"))+
  labs(title = "Kenya Census",
       subtitle = "\nDistribution of Population Aged 3 Years and Above that \nleft school or never been to school",
       caption = "Source: rKenyaCensus by Shelmith Kariuki \n Visualization: @MiguelTripp")+
  theme_void()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.text = element_text(size = 14, vjust = 0),
        plot.title = element_text(size = 26, hjust = 0.5, vjust = 0, margin = margin(15,0,15,0)),
        plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(20,0,20,0)),
        plot.caption = element_text(size = 12, hjust = 1, margin = margin(20,0,10,0)),
        panel.background = element_rect(fill = "#e1d2c0"),
        plot.background = element_rect(fill = "grey95", colour = "#0f0d0d", size = 1)
        )

ggsave(filename = "KenyaSchool.png", width = 6, height = 8, scale = 1.15)
