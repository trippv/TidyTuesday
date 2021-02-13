#Tidytuesday
# week 3, 2021
#Miguel Tripp Valdez
#TATE

library(tidyverse)
library(ggwordcloud)
library(hrbrthemes)
library(patchwork)
library(ggtext)


# 1. Data import -------------------------------------------------------------

artwork <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv')
artists <- readr::read_csv("https://github.com/tategallery/collection/raw/master/artist_data.csv")


#1 . Get female artist
artists_tidy <- artists %>% 
  rename(artistId = id) %>% 
  
  #get the place of birth of artist
  separate(placeOfBirth, into = c("BirthCity", "BirthCountry"), sep = ",") %>% 
  
  #If the country is missing, use the city column
  mutate(BirthCountry = ifelse(!is.na(BirthCity) & is.na(BirthCountry), BirthCity, BirthCountry )) %>% 
  
  #join with paiting and acquisition date
  inner_join(select(artwork, acquisitionYear, artist, artistId,id, year,width, height), 
             by = c("artistId"))


  
#Filter only female artist
  female_artist <- artists_tidy %>% 
    filter(gender == "Female") 


# 2. Main (known) country of birth of female artist
  female_ct <- female_artist %>% 
    mutate(BirthCountry = str_squish(BirthCountry)) %>% 
    drop_na() %>% 
    group_by(BirthCountry) %>% 
    summarise(count = n()) %>% 
    arrange(-count) 

  
 
# 3. Proportion of female with respect to male

 female_prop <- artists_tidy %>% 
   select(name, artist, year,acquisitionYear, gender) %>% 
   drop_na() %>% 
   group_by(acquisitionYear) %>% 
   mutate(counts_total = n()) %>% 
   ungroup() %>% 
   group_by(acquisitionYear, gender) %>% 
   mutate(counts_gender = n(),
          female_prop = counts_gender/counts_total) %>% 
   ungroup() %>% 
   filter(gender == "Female") %>% 
   group_by(acquisitionYear) %>% 
   summarise(female_prop = first(female_prop))

# 4. Plots

 a_color <- "#68838B"
 b_color <- "#8B0000"

# 4.1 Artwork by year
female_art <- ggplot()+
  #acuisition year
  geom_area(data = female_artist %>% 
              group_by(acquisitionYear) %>% 
              summarise(count = n()), 
            aes(x = acquisitionYear, y = count),
            fill = b_color, alpha = 0.4, col = b_color, size =1)+
  #year of artwork
  geom_area(data = female_artist %>% 
         group_by(year) %>% 
         summarise(count = n()), 
         aes(x = year, y = count),
         fill = a_color, col = a_color, alpha = 0.4, size = 1)+
  labs(y = "Number of artworks", x = "Year",
       subtitle  = glue::glue("The number of artwork by <span style='color:{a_color}'>creation date</span> and <br />
                              <span style='color:{b_color}'>acquisition date</span> increased after 1900s..."))



# proportion of female artist by acquisition year
female_prop_plot <- ggplot(female_prop %>% 
         mutate(female_prop = female_prop * 100),
       aes( x = acquisitionYear, y = female_prop))+
  geom_point(size = 2, col = a_color)+
  geom_smooth(se = FALSE, size = 2, col = a_color)+
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  labs(x = "Acquisition year", y = "Percent of female artist",
       caption   = "...but is only after the 1970s that an increaing % of female artist \nis observed...")


# wordcloud

word_cloud <- ggplot(female_ct, aes(label = BirthCountry, size = count, color =count)) +
  geom_text_wordcloud(area_corr = FALSE, eccentricity = 1)  +
  scale_size_area(max_size = 15) +
  scale_color_gradient(low = a_color, high = b_color)+
  theme_minimal()+
  labs(subtitle = "...most of them from the United Kingdom")
  


# all plot

female_plot <- female_art + female_prop_plot / word_cloud & theme_ipsum_pub()
female_plot <- female_plot + plot_annotation(title = "Women of TATE",
                                             caption = "Data Source: Tate Art Museum (github.com/tategallery/collection)|Visualization: @MiguelTripp" ) & 
  theme(plot.title = element_markdown(color = a_color, size = 30, 
                                            margin = margin(1, 0, 1, 0, unit = "line")),
        plot.subtitle = element_markdown(size = 14, lineheight = 1,
                                         margin = margin(0, 0, 0, 4, unit = "line")),
        plot.caption = element_markdown(size = 14, lineheight = 1,
                                         margin = margin(0, 0, 0, 8, unit = "line")))
female_plot
             
ggsave("WomenOfTATE.png", height = 9, width = 14, units = "in") 
