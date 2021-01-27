library(tidyverse)
library(tidytuesdayR)
library(maps)
library(patchwork)
library(hrbrthemes)
library(ggtext)

tt <- tt_load(2021, week = 5)

plastics <- tt$plastics


# Change some labels

plastics <- plastics %>% 
  mutate(parent_company = ifelse(parent_company == "Pepsico", "PepsiCo", parent_company)) %>% 
  mutate(parent_company = ifelse(str_detect(parent_company, "Nestl"), "Nestlé", parent_company)) %>% 
  mutate(country = ifelse(country == "United States of America", "USA", country)) %>% 
  mutate(country = recode(country, "United Kingdom of Great Britain & Northern Ireland" = "UK"))


# main polluter companies

main_polluters <- plastics %>% 
  filter(!parent_company %in% c("Grand Total", "Unbranded", "null", "Null", "NULL")) %>% 
  group_by(year, parent_company) %>% 
  summarise(sum_total = sum(grand_total, na.rm = TRUE),
            N_country = n(), 
            events = sum(num_events)) %>% 
  ungroup() %>%
  #re-group to stimate the number of year
  group_by(parent_company) %>% 
  mutate(N_year = n(),
         total_sum = sum(sum_total)) %>% 
  ungroup() %>% 
  #filter companies with less than two years
  filter(N_year == 2) %>% 
  arrange(-year, -N_country) %>% 
  #keep the highest in 2020
  group_by(year) %>% 
  top_n(6) %>% 
  mutate(year = factor(year),
         parent_company = fct_reorder(parent_company, - sum_total))



# creating a summarised data frame where we have the number of volunteers per country
volunteers_summary <- plastics %>%
  filter(!parent_company %in% c("Grand Total", "Unbranded", "null", "Null", "NULL")) %>% 
  group_by(country, year) %>%
  summarise(total_volunteers = mean(volunteers)) 


volunteers_year <- volunteers_summary %>%
  pivot_wider(names_from = year, values_from = total_volunteers) %>% # pivoting the data to have a column for each year
  #na.exclude() %>% # excluding the countries with data only for one year
  mutate(diff = `2019` - `2020`) # computing the difference between both years


world_map <- map_data("world") # coordinates data frame

world_map_volunteers <- left_join(world_map, volunteers_year, by = c('region' = 'country')) %>% 
  filter(region != "Antarctica")

#get volunteers range
range(volunteers_year$`2019`)
range(volunteers_year$`2020`)





# plots -------------------------------------------------------------------


volunteer_map_2019 <- ggplot() +
  geom_polygon(data = world_map_volunteers, aes(x = long, y = lat, group = group, fill = `2019`)) +
  scale_fill_viridis_b(option = "inferno", na.value = "grey50", limits = c(1,7000))+
  theme_minimal() +
  theme_void()+
  labs(fill = "Number of \nvolunteers", caption = "2019")+
  theme(legend.position = "left",
        plot.background = element_rect(fill = "#ADD8E6", color = "#ADD8E6"),
        plot.caption = element_text(size = 14, hjust = 0.75))



volunteer_map_2020 <- ggplot() +
  geom_polygon(data = world_map_volunteers, aes(x = long, y = lat, group = group, fill = `2020`)) +
  scale_fill_viridis_b(option = "inferno", na.value = "grey50", limits = c(1, 7000)) +
  theme_minimal() +
  theme_void() +
  labs(fill = "Number of volunteers", caption = "2020")+
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#ADD8E6", color = "#ADD8E6"),
        plot.caption = element_text(size = 14, hjust = 0.75))


# main polluters

colors <- viridis::inferno(2)

main_polluters_plot <- ggplot(main_polluters, aes(y = parent_company, x = sum_total, color = year, size = N_country)) +
  geom_path(aes(sum_total, parent_company, group = parent_company), color = "grey50", size = 0.75)+
  geom_point()+
  scale_color_manual(values = colors)+
  #remove color legend
  guides(size = guide_legend(), color = FALSE)+
  theme_minimal() +
  #add thousands separator in axis
  scale_x_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE),
                     limits = c(0,20000))+
  labs(x = "Number of plastic pieces", size = "Number of \ncountries",
       subtitle = glue::glue("Most polluter companies in <br /> <span style='color:{colors[1]}'>2019</span> and <span style='color:{colors[2]}'>2020</span>"))+
  theme(
    legend.position = "right",
    # plot.margin = margin(40, 20, 40, 20, unit = "pt"),
    axis.text.y = element_text(size = 12),
    axis.title = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey70", size = 0.3),
    panel.grid.minor.x = element_line(color = "grey80", size = 0.15),
    plot.subtitle = element_markdown(size = 14))


# join the two maps with patchwork


join_plot <- volunteer_map_2019 / volunteer_map_2020 | main_polluters_plot 
join_plot <- join_plot + plot_layout(nrow = 1, ncol = 2, widths = c(2,1))

join_plot +
  plot_annotation(
    title = "BREAK FREE FROM PLASTIC",
    subtitle = glue::glue("the group Break Free From Plastic organized over 70,000 volunteers in 51 countries to collect and identify plastic waste"),
    caption = "Data:#breakfreefromplastic| Visualization: @MiguelTripp")+
  theme(plot.background = element_rect(fill = "#ADD8E6", color = "#ADD8E6"),
        plot.title = element_markdown(hjust = 0.5, vjust = 0, size = 30),
        plot.caption = element_text(size = 16))

ggsave(filename = "BFP_pollution.png", width = 15, height = 10, dpi = 300)
  