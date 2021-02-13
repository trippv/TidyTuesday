
#Tidytuesday
# week 2, 2021
#Miguel Tripp Valdez

library(tidyverse)
library(countrycode)
library(ggtext)


# 1. Data import -------------------------------------------------------------


transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')

str(transit_cost)




# 2. Country labels (from countrycode) -------------------------------------------------------


df_countries <- codelist %>%
  select(c("ecb", "country.name.en", 
           "continent"))


# 3. Data cleaning -----------------------------------------------------------

transit_cost <- transit_cost %>%
        mutate(tunnel_per = as.numeric(gsub("\\%", "", tunnel_per)),
         start_year = as.numeric(start_year),
         end_year   = as.numeric(end_year),
         real_cost  = as.numeric(real_cost),
         country = ifelse(country == "UK", "GB", country))




# 3.1 binding country with long name

transit_country <- df_countries %>% 
  left_join(transit_cost, ., by = c("country" = "ecb")) %>%
  drop_na(.) %>% 
  group_by(country.name.en) %>% 
  #get the sum of all costs
  summarise(mean_cost_km = mean(cost_km_millions),
            mean_length = mean(length),
            mean_total_cost = mean_cost_km * mean_length,
            n_line = n(),
            sum_real_cost = sum(real_cost),
            sum_real_cost_bll = sum_real_cost / 1000,
            max_project = max(real_cost/1000)) %>% 
  ungroup() %>% 
  #arrange(., -sum_real_cost) %>% 
  slice_max(sum_real_cost, n = 15) %>% 
  mutate(country.name.en = fct_reorder(country.name.en, sum_real_cost))




#highlighting color for text
subtitle_highlight <- "#fc7f03"


# Plot --------------------------------------------------------------------


ggplot(transit_country, aes(x = sum_real_cost_bll, y = country.name.en, color = mean_cost_km))+
  
  #Add big circlers
  geom_point(aes(x = sum_real_cost_bll + 10, y = country.name.en, col = mean_cost_km), 
             size = 9) +
  
  #Add small circle
  geom_point(data = transit_country, aes(x = sum_real_cost_bll + 10, y = country.name.en),
             inherit.aes = FALSE, size = 6, color = "white")+
  
  #Add big segment
  geom_segment(aes(x = 0, xend = sum_real_cost_bll, y = country.name.en, yend = country.name.en), size = 4) + 
  
  #Add small segment
  
  geom_segment(data = transit_country, aes(x = 1.5, xend = sum_real_cost_bll, 
                                           y = country.name.en, yend = country.name.en,
                                           ),color = "white", inherit.aes = FALSE, size = 1.5)+
  
  #guides(color = TRUE)+
  
  scale_fill_viridis_c(option = "cividis")+
  scale_color_viridis_c(option= "cividis")+
  
  geom_text(aes(x = sum_real_cost_bll + 10, label=n_line), col = "black", size = 4)+
  

  theme_classic()+
  
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 12),
    axis.text = element_text(size = 12),
    plot.title = element_markdown(color = subtitle_highlight, size = 20, 
                                  margin = margin(1, 0, 1, 0, unit = "line")),
    plot.title.position = "plot",
    panel.grid.major   = element_blank(),
    plot.background = element_rect(fill = "gray95"),
    plot.caption = element_text(hjust = 1, vjust = 1),
    plot.subtitle = element_markdown(size = 12, lineheight = 1,
                                     margin = margin(0, 2, 0, 0, unit = "line")),
    legend.position = c(.95, 0.95),
    legend.justification = c("right", "top"),
    legend.box.just = "center",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  ) +
  coord_cartesian(clip = "off")+
  scale_x_continuous(expand = c(0,50)) +
  labs(caption = "Data Source:  Transit Costs Project (https://transitcosts.com)",
       x = "Cost in Billions of USD",
       color = "Average cost of project per KM \n (in millions of USD)",
       title = "Transit Costs Project",
       subtitle = glue::glue("This database spans more than 50 countries and more than 11,000 km of urban rail built since the late 1990s. <br /> 
                             Here I show the <span style='color:{subtitle_highlight}'>Top 35 </span> countries with highest investment in railroads.
                             The number of lines is indicated inside <br /> circles. <br />
                             <br />
                             Mainland China has invested more than 700 billion USD in 169 lines with relatively low cost per KM. On the <br/> other hand,the United States has allocated 13 projects only, with the highest cost per KM <br />" ))
         

ggsave("transit-costs_V2.png", width = 8.5, height = 11)                        
        