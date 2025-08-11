# _________________________________________________________________________
# 03-gifxx.R
# _________________________________________________________________________
# August 2025
# Creating gif from daily discrete percentile and daily extreme heatmaps
# _________________________________________________________________________

# Discrete percentile daily GIF --------------------------------------------
discrete_gif_output_folder <- "C:/Users/morga/OneDrive - The University of Waikato/Directed Study/Heat Maps/Discrete"

# Save each discrete daily plot
for (year in sort(unique(max_data_summer$summer_year[max_data_summer$summer_year != 2023]))) {
  p <- plot_percentile_heatmap_discrete_daily(max_data_summer, year)
  ggsave(
    filename = file.path(discrete_gif_output_folder,
                         paste0("percentile_discrete_daily_", year, ".png")),
    plot = p,
    width = 16, height = 5, dpi = 300, bg = "white"
  )
  message("Saved discrete frame for year: ", year)
}

# List and animate discrete daily PNGs
frames_discrete <- list.files(discrete_gif_output_folder,
                              pattern = "percentile_discrete_daily_.*\\.png$",
                              full.names = TRUE)
frames_discrete <- sort(frames_discrete)

img_list_discrete <- image_read(frames_discrete)
animation_discrete <- image_animate(image_join(img_list_discrete), fps = 2)

# Save discrete daily GIF
image_write(animation_discrete,
            path = file.path(discrete_gif_output_folder,
                             "percentile_discrete_daily.gif"))

# Extreme daily GIF --------------------------------------------------------
extreme_gif_output_folder <- "C:/Users/morga/OneDrive - The University of Waikato/Directed Study/Heat Maps/Extreme"

# Save each extreme daily plot
for (year in sort(unique(max_data_summer$summer_year[max_data_summer$summer_year != 2023]))) {
  p <- plot_extreme_heatmap_daily(max_data_summer, year)
  ggsave(
    filename = file.path(extreme_gif_output_folder,
                         paste0("extreme_heatmap_daily_", year, ".png")),
    plot = p,
    width = 16, height = 5, dpi = 300, bg = "white"
  )
  message("Saved extreme frame for year: ", year)
}

# List and animate extreme daily PNGs
frames_extreme <- list.files(extreme_gif_output_folder,
                             pattern = "extreme_heatmap_daily_.*\\.png$",
                             full.names = TRUE)
frames_extreme <- sort(frames_extreme)

img_list_extreme <- image_read(frames_extreme)
animation_extreme <- image_animate(image_join(img_list_extreme), fps = 2)

# Save extreme daily GIF
image_write(animation_extreme,
            path = file.path(extreme_gif_output_folder,
                             "extreme_heatmap_daily.gif"))

