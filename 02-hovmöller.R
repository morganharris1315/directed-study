# _________________________________________________________________________
# 02-hovmöllerxx.R
# _________________________________________________________________________
# August 2025
# Heatmap (Hovmöller) of percentile exceedances and extreme heatmap
# _________________________________________________________________________

# Discrete percentile heatmap function --------------------------------------
plot_percentile_heatmap_discrete_daily <- function(df, target_summer_year) {
  
  all_percentile_levels <- c("0-1", "1-5", "5-10", "10-25", "25-75",
                             "75-90", "90-95", "95-99", "99-100")
  
  summer_subset <- df %>%
    filter(summer_year == target_summer_year) %>%
    mutate(
      plot_date = as.Date("1972-11-30") + dos,
      percentile_band = case_when(
        percentile_daily_temp < 1   ~ "0-1",
        percentile_daily_temp < 5   ~ "1-5",
        percentile_daily_temp < 10  ~ "5-10",
        percentile_daily_temp < 25  ~ "10-25",
        percentile_daily_temp < 75  ~ "25-75",
        percentile_daily_temp < 90  ~ "75-90",
        percentile_daily_temp < 95  ~ "90-95",
        percentile_daily_temp < 99  ~ "95-99",
        percentile_daily_temp <= 100 ~ "99-100",
        TRUE ~ NA_character_
      ),
      percentile_band = factor(percentile_band, levels = all_percentile_levels),
      site = factor(site, levels = df %>% distinct(site, lat) %>% arrange(lat) %>% pull(site))
    )
  
  missing_levels <- setdiff(all_percentile_levels, unique(summer_subset$percentile_band))
  if (length(missing_levels) > 0) {
    dummy_rows <- data.frame(
      plot_date = as.Date(NA),
      site = factor(NA, levels = levels(summer_subset$site)),
      percentile_band = factor(missing_levels, levels = all_percentile_levels),
      stringsAsFactors = FALSE
    )
    summer_subset <- bind_rows(summer_subset, dummy_rows)
  }
  
  ggplot(summer_subset, aes(x = plot_date, y = site, fill = percentile_band)) +
    geom_tile(color = "white") +
    scale_fill_manual(
      values = c(
        "0-1" = "#08306b",
        "1-5" = "#2171b5",
        "5-10" = "#6baed6",
        "10-25" = "#c6dbef",
        "25-75" = "#f0f0f0",
        "75-90" = "#fee5d9",
        "90-95" = "#fcae91",
        "95-99" = "#fb6a4a",
        "99-100" = "#cb181d"
      ),
      name = "Percentile Band",
      na.translate = FALSE,
      drop = FALSE,
      guide = guide_legend(reverse = TRUE)
    ) +
    coord_fixed(ratio = 1.2, clip = "off") +  
    labs(
      title = paste("Summer", target_summer_year - 1, "/", target_summer_year),
      x = "Date",
      y = "Site"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.margin = margin(5, 5, 5, 5),  
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      axis.text.y = element_text(size = 7),
      panel.grid = element_blank(),
      plot.title = element_text(margin = margin(b = 6)) 
    )
}
plot_percentile_heatmap_discrete_daily(max_data_summer, 1973)

# Extreme heatmap function (daily temp above 95th) --------------------------
plot_extreme_heatmap_daily <- function(df, target_summer_year) {
  
  breaks_vec <- c(0, 2, 4, 6, 8, 10, Inf)
  labels_vec <- c("0-2°C", "2-4°C", "4-6°C", "6-8°C", "8-10°C", ">10°C")
  red_palette <- c(
    "0-2°C"   = "#fee5d9",
    "2-4°C"   = "#fcae91",
    "4-6°C"   = "#fb6a4a",
    "6-8°C"   = "#de2d26",
    "8-10°C"  = "#a50f15",
    ">10°C"   = "#67000d"
  )
  
  summer_subset <- df %>%
    filter(summer_year == target_summer_year) %>%
    mutate(
      plot_date = as.Date("1972-11-30") + dos,
      p95_temp = map_dbl(ref_temp_values, ~ if (!is.null(.x)) quantile(.x, 0.95, na.rm = TRUE) else NA_real_),
      temp_above_95th = if_else(!is.na(p95_temp) & temperature >= p95_temp,
                                temperature - p95_temp, NA_real_),
      excess_bin = cut(
        temp_above_95th,
        breaks = breaks_vec,
        labels = labels_vec,
        right = FALSE
      ),
      excess_bin = factor(excess_bin, levels = labels_vec),
      site = factor(site, levels = df %>% distinct(site, lat) %>% arrange(lat) %>% pull(site))
    )
  
  missing_levels <- setdiff(labels_vec, unique(summer_subset$excess_bin))
  if (length(missing_levels) > 0) {
    dummy_rows <- data.frame(
      plot_date = as.Date(NA),  
      site = factor(NA, levels = levels(summer_subset$site)),
      excess_bin = factor(missing_levels, levels = labels_vec),
      stringsAsFactors = FALSE
    )
    summer_subset <- bind_rows(summer_subset, dummy_rows)
  }
  
  ggplot(summer_subset, aes(x = plot_date, y = site)) +
    geom_tile(data = filter(summer_subset, is.na(excess_bin)), 
              fill = "#f0f0f0", color = "white") +
    geom_tile(data = filter(summer_subset, !is.na(excess_bin)), 
              aes(fill = excess_bin), color = "white") +
    scale_fill_manual(
      values = red_palette,
      drop = FALSE,
      name = "°C Above 95th",
      guide = guide_legend(reverse = TRUE)
    ) +
    coord_fixed(ratio = 1.2) +
    labs(
      title = paste("Summer", target_summer_year - 1, "/", target_summer_year),
      x = "Date",
      y = "Site"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.margin = margin(5, 5, 5, 5),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      axis.text.y = element_text(size = 7),
      panel.grid = element_blank(),
      plot.title = element_text(margin = margin(b = 6))
    )
}
plot_extreme_heatmap_daily(max_data_summer, 1973)

# Generate and save plots for all summers -----------------------------------
years <- sort(unique(max_data_summer$summer_year))

for (year in years) {
  # Discrete percentile map
  p1 <- plot_percentile_heatmap_discrete_daily(max_data_summer, year)
  ggsave(paste0("percentile_heatmap_discrete_daily_", year, ".png"),
         plot = p1, width = 16, height = 5, dpi = 300, bg = "white")
  
  # Extreme map
  p2 <- plot_extreme_heatmap_daily(max_data_summer, year)
  ggsave(paste0("extreme_heatmap_daily_", year, ".png"),
         plot = p2, width = 16, height = 5, dpi = 300, bg = "white")
  
  message("Saved both plots for year: ", year)
}
