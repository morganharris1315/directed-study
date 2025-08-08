# _________________________________________________________________________
# 01-import-data.R
# _________________________________________________________________________
# August 2025
# Setup script: Read and combine temperature data from CSV parts, clean data,
# _________________________________________________________________________

# reading in files --------------------------------------------------------
part1 <- read.csv("daily-temperature-for-30-sites-to-2022-part1.csv")
part2 <- read.csv("daily-temperature-for-30-sites-to-2022-part2.csv")
part3 <- read.csv("daily-temperature-for-30-sites-to-2022-part3.csv")
raw_data <- bind_rows(part1, part2, part3)

# converting date to date format ------------------------------------------
raw_data <- raw_data %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

# raw data checks ---------------------------------------------------------
summary(raw_data)
str(raw_data)
sapply(raw_data, function(x) length(unique(x)))

# filtering raw data to max data ------------------------------------------
max_data <- raw_data %>%
  filter(tolower(statistic) == "maximum") %>%
  mutate(
    y = year(date),
    m = month(date),
    d = day(date),
    summer_year = case_when(
      m == 12 ~ y + 1,       # December belongs to next summer year
      m %in% 1:3 ~ y,        # Jan–Mar in current year
      TRUE ~ NA_integer_      # otherwise NA
    ),
    summer_start = case_when(
      !is.na(summer_year) ~ as.Date(paste0(summer_year - 1, "-12-01")),
      TRUE ~ as.Date(NA)
    ),
    dos = as.integer(date - summer_start) + 1  # day of summer (1-based)
  )

# max data checks ---------------------------------------------------------
head(max_data %>% select(date, y, m, d, summer_year, summer_start, dos))
summary(max_data$summer_year)
summary(max_data$dos)
range(max_data$summer_start, na.rm = TRUE)

# adding 15 day sliding window --------------------------------------------
max_data <- max_data %>%
  arrange(site, date) %>%
  group_by(site) %>%
  mutate(
    mean_15day_temp = slide_dbl(
      temperature,
      .before = 7,
      .after = 7,
      .f = ~ mean(.x, na.rm = TRUE)
    )
  ) %>%
  ungroup()

# rolling window checks ---------------------------------------------------
head(max_data %>% select(site, summer_year, date, temperature, mean_15day_temp))


# filtering to summer -----------------------------------------------------
max_data_summer <- max_data %>%
  filter(!is.na(dos) & !is.na(summer_year))

# building reference distribution -----------------------------------------
reference_years <- 1981:2020
reference_distribution <- max_data_summer %>%
  filter(summer_year %in% reference_years) %>%
  group_by(site, dos) %>%
  summarise(
    ref_values = list(mean_15day_temp),
    .groups = "drop"
  )

# joining reference distribution ------------------------------------------
max_data_summer <- max_data_summer %>%
  left_join(reference_distribution, by = c("site", "dos"))


# creating percentile rank ------------------------------------------------
max_data_summer <- max_data_summer %>%
  mutate(percentile_against_ref = map2_dbl(mean_15day_temp, ref_values, function(val, ref) {
    if (is.na(val) || length(ref) < 10) return(NA_real_)  
      ecdf(ref)(val) * 100
    })
  ) 

# percentile checks -------------------------------------------------------
summary(max_data_summer$percentile_against_ref) #Min, max and quartiles are in the right places
hist(max_data_summer$percentile_against_ref, breaks = 30,
     main = "Histogram of Percentiles Against Reference",
     xlab = "Percentile")
sum(is.na(max_data_summer$percentile_against_ref)) #0 Nas

  #spot check
row_example <- max_data_summer %>% filter(!is.na(percentile_against_ref)) %>% slice(1)
val <- row_example$mean_15day_temp
ref <- row_example$ref_values[[1]]
manual_percentile <- ecdf(ref)(val) * 100
print(manual_percentile)
print(row_example$percentile_against_ref)
  #manual and percentile_against_ref are the same. 

  #ploting percentiles
ggplot(max_data_summer %>% filter(!is.na(percentile_against_ref)), 
       aes(x = mean_15day_temp, y = percentile_against_ref)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  labs(title = "Temperature vs Percentile Against Reference",
       x = "15-day mean temperature",
       y = "Percentile against reference") +
  theme_minimal()

  #could try validating with known events or extremes
known_extremes <- tibble(
  site = c("SiteA", "SiteB", "SiteC"),
  date = as.Date(c("2009-02-15", "2017-01-25", "1998-12-20"))
)
  #joining 
extreme_events_data <- known_extremes %>%
  left_join(summer_max_data, by = c("site", "date"))

  #inspecting 
extreme_events_data %>%
  select(site, date, mean_15day_temp, percentile_against_ref)
