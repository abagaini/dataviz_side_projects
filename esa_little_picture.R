# create a plot showing change in rainforest cover (Brazil)


# Author: Alexandra Bagaini

library(tidyverse)

# read data
dat <- read_csv("https://raw.githubusercontent.com/littlepictures/datasets/main/lc-2/rainforest_regions.csv")


dt <- dat %>%
    # select on Brazil column
    select(Brazil, year) %>%
    # rename the Brazil column to rainforest_area
    rename("rainforest_area" = "Brazil") %>%
    # convert to 1000 km2 (and round off value)
    mutate(rainforest_area = round(rainforest_area / 1000)) %>%
    filter(year %in% c(1992, 2015))


plot_dt <- tibble(
    km_num = 1:dt$rainforest_area[dt$year == 1992],
    land = rep(1, dt$rainforest_area[dt$year == 1992])
)


# Number of rows and columns in the grid
# (63 * 64 = 4032 == dt$rainforest_area[dt$year == 1992])
num_rows <- 63
num_cols <- 64

# Total number of dots
total_dots <- dt$rainforest_area[dt$year == 1992]

# Calculate the number of cells in the grid
total_cells <- num_rows * num_cols

    # Create a matrix to store x and y coordinates
    coordinates <- matrix(NA, ncol = 2, nrow = total_dots)

    # Assign x and y coordinates to each dot
    counter <- 1
    for (i in 1:num_rows) {
        for (j in 1:num_cols) {
            if (counter <= total_dots) {
                coordinates[counter, 1] <- j # x-coordinate
                coordinates[counter, 2] <- i # y-coordinate
                counter <- counter + 1
            }
        }
    }


plot_dt <- bind_cols(plot_dt, coordinates %>% as_tibble() %>% rename(x = V1, y = V2))

plot_dt <- plot_dt %>%
    # isolate 345 circles (15*23)
    mutate(is_bottom_right = (x > (num_cols - 15)) & (y > (num_rows - 23))) %>% 
      mutate(fill_col = case_when(
    is_bottom_right == 0 ~ "#34623f",
TRUE ~ "#f3e9d2"
), col_col = case_when(
    is_bottom_right == 0 ~ "#34623f",
    TRUE ~ "#34623f"
)) 

legend_dt <- tibble(x = 70, y = 55)
lb1 <- bquote("= 1'000 "~km^2)


p <- plot_dt %>% ggplot() +
    geom_point(aes(x = y, y = -x, color = col_col, fill = fill_col), size = 5.5,
               alpha = 1, shape = 22) +
    geom_point(data = legend_dt, aes(x = y, y = -x), size = 5, stroke = 1
               ,color = "#34623f",
               alpha = 1, shape = 22) +
    theme_void() +
    scale_color_identity() +
    scale_fill_identity() +
    annotate("text",
                     x = 64, y = -69.75, label=lb1, color = "#34623f", size = 6,
                     hjust = 1, vjust = .5, alpha = 1,
                     family = "Helvetica Bold") +
    scale_x_continuous(limits = c(1,65))+
scale_y_continuous(limits = c(-70,-1)) +
    theme(plot.background = element_rect(fill = "#f3e9d2"))

p

ggsave(plot = p, filename = "little_picture.png", 
       width = 30, height = 32, units = "cm", dpi  = 600)


