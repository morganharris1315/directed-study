# _________________________________________________________________________
# 01-import-dataxx.R
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
reference_distribution_temp <- max_data %>%
  filter(!is.na(dos), !is.na(summer_year), summer_year %in% reference_years) %>%
  group_by(site, dos) %>%
  summarise(
    ref_temp_values = list(temperature),
    .groups = "drop"
  )


# joining reference distribution ------------------------------------------
max_data_summer <- max_data %>%
  filter(!is.na(dos) & !is.na(summer_year)) %>%
  left_join(reference_distribution_temp, by = c("site", "dos")) %>%
  mutate(
    percentile_daily_temp = map2_dbl(temperature, ref_temp_values, function(val, ref) {
      if (is.na(val) || length(ref[!is.na(ref)]) < 10) return(NA_real_)
      ecdf(ref)(val) * 100
    })
  )

# reference distribution checks -------------------------------------------
summary(max_data_summer$percentile_daily_temp)
sum(is.na(max_data_summer$percentile_daily_temp))

