# Codigo para analizar la base de datos de Netflix

# analizar los valores en el rating de IMBd con el tiempo

library(tidyverse)
library(here)
library(scales)
library(viridis)
library(patchwork)

## parametros graficos
# paletas de colores
netflix_pal <- c("#df0707", "#ba0c0c", "#980a0a", "#780909", "#0b0000" )
apple_pal <- c("#f5f5f7", "#c7c7cc", "#8e8e93", "#48484a", "#1c1c1e")
prime_pal <- c("#00A8E1", "#0077B6", "#023E8A", "#001845", "#000814")

# thema
common_theme <- list(
  scale_y_continuous(limits = c(0, 10)),  # Force y-axis from 0 to 10
  theme_minimal(),
  theme(legend.position = "top",
        plot.margin = margin(10, 10, 10, 10))
)



# read databases
netflix <- read_csv(here("netflix/netflix.csv"))
apple <- read_csv(here("netflix/apple.csv"))
prime <- read_csv(here("netflix/prime.csv"))


platform_data <- c("netflix", "apple", "prime") %>% 
  set_names() %>% 
  map(~ read_csv(here("netflix", paste0(.x, ".csv"))))


# Improved editing function with additional checks
edit_tibble <- function(df) {
  df %>% 
    filter(type == "movie") %>% 
    rename(imdb = imdbAverageRating,
           votes = imdbNumVotes, 
           release = releaseYear) %>% 
    filter(votes >= 10) %>% 
    # Remove rows with NA in critical columns
    drop_na(imdb, release)
}

# Apply editing function to all datasets at once
edited_data <- map(platform_data, edit_tibble)

# Calculate averages for all platforms at once
avg_data <- edited_data %>% 
  map(~ .x %>% 
        group_by(release) %>% 
        summarise(promedio = mean(imdb, na.rm = TRUE), 
                  .groups = "drop"))

# Calculate counts for all platforms at once
count_data <- edited_data %>% 
  map(~ .x %>% 
        count(release, imdb, name = "count"))



# graficar
netflix_plot <- ggplot()+
  geom_tile(data = count_data$netflix, 
    aes(x = release, y = imdb, fill = count))+
   geom_smooth(data = avg_data$netflix, 
     aes(x = release, y = promedio), 
     se = FALSE, color = "darkred")+
  scale_fill_gradientn(colors = rev(netflix_pal))+
  common_theme



apple_plot <- ggplot()+
  geom_tile(data = count_data$apple, 
    aes(x = release, y = imdb, fill = count))+
   geom_smooth(data = avg_data$apple, 
     aes(x = release, y = promedio), 
     se = FALSE, color = "darkblue")+
  scale_fill_gradientn(colors = rev(apple_pal))+
  common_theme


prime_plot  <- ggplot()+
  geom_tile(data = count_data$prime, 
    aes(x = release, y = imdb, fill = count))+
   geom_smooth(data = avg_data$prime, 
     aes(x = release, y = promedio), 
     se = FALSE, color = "darkblue")+
  scale_fill_gradientn(colors = rev(prime_pal)) +
  common_theme




combined_plot <- netflix_plot + apple_plot + prime_plot
#combined_plot <- combined_plot 
combined_plot


ggsave(plot = combined_plot, filename = here("netflix/streming.png"), 
height = 90, width = 180, units = "mm", dpi = 300)
