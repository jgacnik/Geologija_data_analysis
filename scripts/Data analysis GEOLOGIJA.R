###############################################################################
# DATA ANALYSIS AND VISUALIZATION for Gačnik et al., Geologija (2026)
# CITATION: Gačnik J., Štrok, M., Žagar, K., Vreča P. (under reivew): Isotopic composition of hydrogen and oxygen in precipitation at the station Portoroz (Reaktor), Slovenia: period 2011–2024
# CODE AUTHOR: JAN GAČNIK, October 2025
# R version: 4.4.2
###############################################################################
# DO THIS STEP ONLY THE FIRST TIME! Installation of needed packages
install.packages(c("rnaturalearth", "sf", "slider", "ggcorrplot", "forecast", "tidyverse", "readxl"))

# Paths relative to the Geologija_data_analysis.Rproj project root
path_scripts <- "scripts"
path_data <- "data"
path_figures <- "figures"
path_tables <- "tables"

###############################################################################
# CODE BELOW DOES NOT NEED ALTERING
###############################################################################
Portoroz_filename <- "Portoroz_2011-2024.xlsx"
Portoroz_filename_metdata <- "Portoroz_daily_metdata.xlsx"
figure_type <- "png"
dpi_set <- 500

# Load required packages for the current session
lapply(c("rnaturalearth", "sf", "slider", "ggcorrplot", "forecast", "tidyverse", "readxl"), require, character.only = TRUE)

# Calling needed functions for PWRMA. Located in separate script "Functions_MA&RMA.R"
source(file.path(path_scripts, "Functions_MA&RMA.R"))

###############################################################################
# Data reading, adjusting, and filtering
###############################################################################
Portoroz_meteo_data <- readxl::read_xlsx(path = file.path(path_data, Portoroz_filename_metdata), skip = 4) %>% 
  janitor::clean_names() %>% 
  dplyr::select("Date_full" = x1,
         "T_average" = povp_dnevna_t_c,
         "RH" = povp_rel_vla_percent,
         "P_amount" = kolicina_padavin_mm,
         "Rain" = dez,
         "Snow" = sneg,
         "Rain_with_snow" = dez_s_snegom) %>% 
  dplyr::mutate(Date_full = as.Date(Date_full),
                Date = floor_date(Date_full, unit = "month")) %>% 
  dplyr::group_by(Date) %>% 
  dplyr::summarise(T_mean = mean(T_average, na.rm = TRUE),
                   P_sum = sum(P_amount, na.rm = TRUE)) %>% 
  dplyr::mutate(Year = year(Date),
                Month = month(Date)) %>% 
  dplyr::filter(Year >= 2011)

Portoroz_meteo_data_long <- Portoroz_meteo_data %>% 
  pivot_longer(cols = c("P_sum", "T_mean"), names_to = "Variable", values_to = "Value") %>% 
  mutate(Facet = case_when(
    grepl("P_", Variable) == TRUE ~ "Precipitation",
    grepl("T_", Variable) == TRUE ~ "Temperature",
    TRUE ~ NA
  ))

Portoroz_meteo_data_rolling <- Portoroz_meteo_data %>% 
  mutate(sum_2 = slide_dbl(P_sum, ~sum(.x, na.rm = TRUE), .before = 1),
         sum_3 = slide_dbl(P_sum, ~sum(.x, na.rm = TRUE), .before = 2))

Portoroz_meteo_data_monthly <- Portoroz_meteo_data_long %>% 
  mutate(Month = month(Date)) %>% 
  group_by(Facet, Month) %>% 
  summarise(Mean = mean(Value, na.rm = TRUE),
            StDev = sd(Value, na.rm = TRUE))

Portoroz_meteo_data_yearly <- Portoroz_meteo_data_long %>% 
  group_by(Facet, Year) %>% 
  summarise(Total = sum(Value, na.rm = TRUE),
            Mean = mean(Value, na.rm = TRUE))

# Save yearly meteorological data into a .csv table
write.csv(Portoroz_meteo_data_yearly, file = file.path(path_tables, "Portoroz_yearly_meteorological_statistics.csv"), row.names = FALSE)

Portoroz_data <- read_xlsx(sheet = "Data Portorož airport", path = file.path(path_data, Portoroz_filename), trim_ws = TRUE) %>%
  dplyr::select(Sample_ID, Station_ID, Name, Year, Month, P, T, RH, δ18O, δ2H, d, "3H (TU)") %>%
  rename("3H" = "3H (TU)") %>% 
  mutate(Season = case_when(
    Month %in% c(12, 1, 2)  ~ "winter",
    Month %in% c(3, 4, 5)  ~ "spring",
    Month %in% c(6, 7, 8)  ~ "summer",
    Month %in% c(9, 10, 11)  ~ "autumn",
    TRUE ~ NA)) %>%
  mutate(Season = factor(Season, levels = c("winter", "spring", "summer", "autumn"))) %>%
  mutate(P_δ18O_δ2H = case_when(
    is.na(δ18O) | is.na(δ2H) ~ NA,
    TRUE ~ P)) %>%
  mutate(P_3H = case_when(
    is.na(`3H`) ~ NA,
    TRUE ~ P)) %>% 
  mutate(Date = as.Date(make_date(Year, Month)))

Portoroz_data_cor <- Portoroz_data %>% 
  dplyr::select(δ18O, δ2H, d, "3H", T, P, RH) %>%
  cor(use = "pairwise.complete.obs", method = "pearson")

Portoroz_data_cor_p <- Portoroz_data %>% 
  dplyr::select(δ18O, δ2H, d, "3H", T, P, RH) %>%
  cor_pmat(method = "pearson") 
# %>% 
#   {
#     p <- .
#     p[] <- ifelse(!is.na(p) & p < 0.05, 0.049, 1)
#     diag(p) <- 1
#     p
#   }

Portoroz_data_longer <-  Portoroz_data %>% 
  pivot_longer(cols = c(δ18O, δ2H, d, "3H", T, P, RH), names_to = "Variable", values_to = "Value") %>%
  mutate(Variable_color = Variable) %>%
  mutate(Variable = case_when(
    Variable == "δ18O" ~ "italic(delta)^18*O",
    Variable == "δ2H" ~ "italic(delta)^2*H",
    Variable == "d" ~ "d-excess",
    Variable == "3H" ~ "A^3*H",
    TRUE ~ Variable
  )) %>%
  mutate(Variable = factor(Variable, levels = c("italic(delta)^18*O", "italic(delta)^2*H", "d-excess", "A^3*H", "T", "P", "RH")))

Portoroz_data_longer_sigma <-  Portoroz_data %>% 
  pivot_longer(cols = c("T", "P"), names_to = "Variable_size", values_to = "Value_size") %>%
  mutate(Variable_size = factor(Variable_size, levels = c("T", "P")))

Portoroz_data_longer_month <- Portoroz_data %>%
  group_by(Month) %>%
  summarise(mean_T = mean(T, na.rm = TRUE),
            mean_P = mean(P, na.rm = TRUE),
            mean_RH = mean(RH, na.rm = TRUE),
            weighted_mean_δ18O = sum(δ18O * ((P_δ18O_δ2H/sum(P_δ18O_δ2H, na.rm = TRUE))), na.rm = TRUE),
            weighted_mean_δ2H = sum(δ2H * ((P_δ18O_δ2H/sum(P_δ18O_δ2H, na.rm = TRUE))), na.rm = TRUE),
            weighted_mean_d = sum(d * ((P_δ18O_δ2H/sum(P_δ18O_δ2H, na.rm = TRUE))), na.rm = TRUE),
            weighted_mean_3H = sum(`3H` * ((P_3H/sum(P_3H, na.rm = TRUE))), na.rm = TRUE)) %>%
  pivot_longer(cols = c(weighted_mean_δ18O, weighted_mean_δ2H, weighted_mean_d, weighted_mean_3H, mean_T, mean_P, mean_RH), names_to = "Variable", values_to = "Value") %>%
  mutate(Variable = case_when(
    grepl("_δ18O", Variable) ~ "italic(delta)^18*O",
    grepl("_δ2H", Variable) ~ "italic(delta)^2*H",
    grepl("_d", Variable) ~ "d-excess",
    grepl("_3H", Variable) ~ "A^3*H",
    grepl("_T", Variable) ~ "T",
    grepl("_P", Variable) ~ "P",
    grepl("_RH", Variable) ~ "RH",
    TRUE ~ Variable)) %>%
  mutate(Variable = factor(Variable, levels = c("italic(delta)^18*O", "italic(delta)^2*H", "d-excess", "A^3*H", "T", "P", "RH")))

Portoroz_data_longer_season <- Portoroz_data %>%
  group_by(Season) %>%
  summarise(mean_T = mean(T, na.rm = TRUE),
            mean_P = mean(P, na.rm = TRUE),
            mean_RH = mean(RH, na.rm = TRUE),
            weighted_mean_δ18O = sum(δ18O * ((P_δ18O_δ2H/sum(P_δ18O_δ2H, na.rm = TRUE))), na.rm = TRUE),
            weighted_mean_δ2H = sum(δ2H * ((P_δ18O_δ2H/sum(P_δ18O_δ2H, na.rm = TRUE))), na.rm = TRUE),
            weighted_mean_d = sum(d * ((P_δ18O_δ2H/sum(P_δ18O_δ2H, na.rm = TRUE))), na.rm = TRUE),
            weighted_mean_3H = sum(`3H` * ((P_3H/sum(P_3H, na.rm = TRUE))), na.rm = TRUE)) %>% 
  pivot_longer(cols = c(weighted_mean_δ18O, weighted_mean_δ2H, weighted_mean_d, weighted_mean_3H, mean_T, mean_P, mean_RH), names_to = "Variable", values_to = "Value") %>%
  mutate(Variable = case_when(
    grepl("_δ18O", Variable) ~ "italic(delta)^18*O",
    grepl("_δ2H", Variable) ~ "italic(delta)^2*H",
    grepl("_d", Variable) ~ "d-excess",
    grepl("_3H", Variable) ~ "A^3*H",
    grepl("_T", Variable) ~ "T",
    grepl("_P", Variable) ~ "P",
    grepl("_RH", Variable) ~ "RH",
    TRUE ~ Variable)) %>%
  mutate(Variable = factor(Variable, levels = c("italic(delta)^18*O", "italic(delta)^2*H", "d-excess", "A^3*H", "T", "P", "RH")))

Portoroz_data_longer_year <- Portoroz_data %>%
  group_by(Year) %>%
  summarise(weighted_mean_δ18O = sum(δ18O * ((P_δ18O_δ2H/sum(P_δ18O_δ2H, na.rm = TRUE))), na.rm = TRUE),
            weighted_mean_δ2H = sum(δ2H * ((P_δ18O_δ2H/sum(P_δ18O_δ2H, na.rm = TRUE))), na.rm = TRUE),
            weighted_mean_d = sum(d * ((P_δ18O_δ2H/sum(P_δ18O_δ2H, na.rm = TRUE))), na.rm = TRUE),
            weighted_mean_3H = sum(`3H` * ((P_3H/sum(P_3H, na.rm = TRUE))), na.rm = TRUE)) %>%
  pivot_longer(cols = c(weighted_mean_δ18O, weighted_mean_δ2H, weighted_mean_d, weighted_mean_3H), 
               names_to = "Variable", values_to = "Value")

Portoroz_data_period_statistics <- Portoroz_data %>% 
  mutate(P_weighting = P) %>%
  pivot_longer(cols = c(δ18O, δ2H, d, "3H", T, P), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  reframe(Mean = mean(Value, na.rm = TRUE),
          "Weighted mean" = sum(Value * ((P_weighting /sum(P_weighting , na.rm = TRUE))), na.rm = TRUE),
          StDev = sd(Value, na.rm = TRUE),
          Median = median(Value, na.rm = TRUE),
          Q1 = quantile(Value, 0.25, na.rm = TRUE),
          Q3 = quantile(Value, 0.75, na.rm = TRUE),
          Min = min(Value, na.rm = TRUE),
          Max = max (Value, na.rm = TRUE),
          n = sum(!is.na(Value))) %>% 
  mutate(Variable = case_when(
    grepl("δ18O", Variable) ~ "italic(delta)^18*O",
    grepl("δ2H", Variable) ~ "italic(delta)^2*H",
    grepl("d", Variable) ~ "d-excess",
    Variable == "3H" ~ "A^3*H",
    grepl("T", Variable) ~ "T",
    grepl("P", Variable) ~ "P",
    TRUE ~ Variable)) %>% 
  mutate(Variable = factor(Variable, levels = c("italic(delta)^18*O", "italic(delta)^2*H", "d-excess", "A^3*H", "T", "P")))

# Save period statistics in a table
write.csv(Portoroz_data_period_statistics, file = file.path(path_tables, "Portoroz_period_statistics.csv"), row.names = FALSE)

Portoroz_data_month_statistics <- Portoroz_data %>% 
  mutate(P_weighting = P) %>%
  pivot_longer(cols = c(δ18O, δ2H, d, "3H", T, P, RH), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable, Month) %>%
  reframe(Mean = mean(Value, na.rm = TRUE),
          "Weighted mean" = sum(Value * ((P_weighting /sum(P_weighting , na.rm = TRUE))), na.rm = TRUE),
          StDev = sd(Value, na.rm = TRUE),
          Median = median(Value, na.rm = TRUE),
          Q1 = quantile(Value, 0.25, na.rm = TRUE),
          Q3 = quantile(Value, 0.75, na.rm = TRUE),
          Min = min(Value, na.rm = TRUE),
          Max = max (Value, na.rm = TRUE),
          n = sum(!is.na(Value))) %>%
  mutate(across(where(is.numeric), ~ round(.x, digits = 1))) %>% 
  mutate(Variable = case_when(
    grepl("δ18O", Variable) ~ "italic(delta)^18*O",
    grepl("δ2H", Variable) ~ "italic(delta)^2*H",
    grepl("d", Variable) ~ "d-excess",
    Variable == "3H" ~ "A^3*H",
    grepl("T", Variable) ~ "T",
    grepl("P", Variable) ~ "P",
    grepl("RH", Variable) ~ "RH",
    TRUE ~ Variable)) %>% 
  mutate(Variable = factor(Variable, levels = c("italic(delta)^18*O", "italic(delta)^2*H", "d-excess", "A^3*H", "T", "P", "RH")))

# Save monthly statistics in a table
write.csv(Portoroz_data_month_statistics, file = file.path(path_tables, "Portoroz_month_statistics.csv"), row.names = FALSE)

Portoroz_data_season_statistics <- Portoroz_data %>% 
  mutate(P_weighting = P) %>%
  pivot_longer(cols = c(δ18O, δ2H, d, "3H", T, P, RH), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable, Season) %>%
  reframe(Mean = mean(Value, na.rm = TRUE),
          "Weighted mean" = sum(Value * ((P_weighting /sum(P_weighting , na.rm = TRUE))), na.rm = TRUE),
          StDev = sd(Value, na.rm = TRUE),
          Median = median(Value, na.rm = TRUE),
          Q1 = quantile(Value, 0.25, na.rm = TRUE),
          Q3 = quantile(Value, 0.75, na.rm = TRUE),
          Min = min(Value, na.rm = TRUE),
          Max = max (Value, na.rm = TRUE),
          n = sum(!is.na(Value))) %>%
  mutate(across(where(is.numeric), ~ round(.x, digits = 1))) %>% 
  mutate(Variable = case_when(
    grepl("δ18O", Variable) ~ "italic(delta)^18*O",
    grepl("δ2H", Variable) ~ "italic(delta)^2*H",
    grepl("d", Variable) ~ "d-excess",
    Variable == "3H" ~ "A^3*H",
    grepl("T", Variable) ~ "T",
    grepl("P", Variable) ~ "P",
    grepl("RH", Variable) ~ "RH",
    TRUE ~ Variable)) %>% 
  mutate(Variable = factor(Variable, levels = c("italic(delta)^18*O", "italic(delta)^2*H", "d-excess", "A^3*H", "T", "P", "RH")))

# Save seasonal statistics in a table
write.csv(Portoroz_data_season_statistics, file = file.path(path_tables, "Portoroz_seasonal_statistics.csv"), row.names = FALSE)

Portoroz_data_year_slopes_2011_2024 <- Portoroz_data %>%
  dplyr::filter(!is.na(δ18O) & !is.na(δ2H)) %>%
  group_by(Year) %>%
  summarise(slope_RMA = RMA_weighted(δ18O, δ2H, P)$slopes$RMA,
            slope_error_RMA =  RMA_weighted(δ18O, δ2H, P)$errors_slopes$RMA,
            slope_RMA_weighted = RMA_weighted(δ18O, δ2H, P)$slopes$RMA_weighted,
            slope_error_RMA_weighted = RMA_weighted(δ18O, δ2H, P)$errors_slopes$RMA_weighted,
            intercept_RMA = RMA_weighted(δ18O, δ2H, P)$intercepts$RMA,
            intercept_error_RMA = RMA_weighted(δ18O, δ2H, P)$errors_intercepts$RMA,
            intercept_RMA_weighted = RMA_weighted(δ18O, δ2H, P)$intercepts$RMA_weighted,
            intercept_error_RMA_weighted = RMA_weighted(δ18O, δ2H, P)$errors_intercepts$RMA_weighted,
            oct_nov = sum(P[month(Date) %in% c(10, 11)], na.rm = TRUE),
            may_jun = sum(P[month(Date) %in% c(5, 6)], na.rm = TRUE),
            total_P = sum(P, na.rm = TRUE),
            mediterranean_index = ((oct_nov - may_jun)/total_P) * 100) %>%
  mutate(across(everything(), ~round(., digits = 2))) %>% 
  mutate(LMWL_RMA = paste0("d2H = (", slope_RMA, " ± ", slope_error_RMA, ") d18O + (", intercept_RMA, " ± ", intercept_error_RMA, ")"),
         LMWL_PWRMA = paste0("d2H = (", slope_RMA_weighted," ± ", slope_error_RMA_weighted, ") d18O + (", intercept_RMA_weighted,  " ± ", intercept_error_RMA_weighted, ")"))

# Save yearly RMA/MA regression statistics in a table
write.csv(Portoroz_data_year_slopes_2011_2024, file = file.path(path_tables, "Portoroz_yearly_regression_statistics.csv"), row.names = FALSE)

Portoroz_data_slopes_period <- Portoroz_data %>%
  dplyr::filter(!is.na(δ18O) & !is.na(δ2H)) %>%
  summarise(slope_RMA = RMA_weighted(δ18O, δ2H, P)$slopes$RMA,
            slope_error_RMA =  RMA_weighted(δ18O, δ2H, P)$errors_slopes$RMA,
            slope_RMA_weighted = RMA_weighted(δ18O, δ2H, P)$slopes$RMA_weighted,
            slope_error_RMA_weighted = RMA_weighted(δ18O, δ2H, P)$errors_slopes$RMA_weighted,
            intercept_RMA = RMA_weighted(δ18O, δ2H, P)$intercepts$RMA,
            intercept_error_RMA = RMA_weighted(δ18O, δ2H, P)$errors_intercepts$RMA,
            intercept_RMA_weighted = RMA_weighted(δ18O, δ2H, P)$intercepts$RMA_weighted,
            intercept_error_RMA_weighted = RMA_weighted(δ18O, δ2H, P)$errors_intercepts$RMA_weighted) %>%
  mutate(across(everything(), ~round(., digits = 2))) %>% 
  mutate(LMWL_RMA = paste0("d2H = (", slope_RMA, " ± ", slope_error_RMA, ") d18O + (", intercept_RMA, " ± ", intercept_error_RMA, ")"),
         LMWL_PWRMA = paste0("d2H = (", slope_RMA_weighted," ± ", slope_error_RMA_weighted, ") d18O + (", intercept_RMA_weighted,  " ± ", intercept_error_RMA_weighted, ")"))

# Save yearly RMA/MA regression statistics in a table
write.csv(Portoroz_data_slopes_period, file = file.path(path_tables, "Portoroz_period_regression_statistics.csv"), row.names = FALSE)


# d18O/d2H versus T regressions and correlations, STATS
Portoroz_T_regression <- Portoroz_data %>% 
  dplyr::filter(!is.na(δ18O) & !is.na(δ2H))

a <- lm(`δ18O` ~ T, data =  Portoroz_T_regression) 
summary(a)
cor(x = Portoroz_data$T, y = Portoroz_data$δ18O, use = "pairwise.complete.obs")
b <- lm(`δ2H` ~ T, data =  Portoroz_data)
summary(b)
cor(x = Portoroz_data$T, y = Portoroz_data$δ2H, use = "pairwise.complete.obs")

###############################################################################
# PLOTTING
###############################################################################
# FIGURE 1
###############################################################################
# Map Slovenia
country_borders <- ne_countries(scale = 10, continent = "Europe")
country_names <- data.frame(
  Station_name = c("SVN", "AUT", "HRV", "ITA", "HUN"),
  lon = c(14.9, 15, 15.35, 12.5, 17.2),
  lat = c(46.3, 47.5, 45, 46.1, 46.8)
) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

station_Portoroz <- st_as_sf(data.frame(
    Station_name = c("Portorož"),
    lon = c(13.61607703),
    lat = c(45.47532273)
  ), coords = c("lon", "lat"), crs = 4326)

ggplot() +
  geom_sf(data = country_borders, fill = NA) +
  geom_sf(data = dplyr::filter(country_borders, sov_a3 == "SVN"), fill = "grey85") +
  geom_text(data = country_names, aes(x = st_coordinates(geometry)[,1],
                                      y = st_coordinates(geometry)[,2],
                                      label = Station_name),
            size = 1.8) +
  geom_sf(data = station_Portoroz, fill = "black", size = 0.6, shape = 21, stroke = 0.4) + 
  geom_text(data = station_Portoroz, aes(x = st_coordinates(geometry)[,1],
                                          y = st_coordinates(geometry)[,2],
                                          label = Station_name),
            color = "black", size = 1.6, fontface = "bold", nudge_y = -0.15) +
  coord_sf(xlim = c(12, 17.5), ylim = c(44.5, 47.5)) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

ggsave(filename = paste("Map_slovenia",".",figure_type, sep = ""), path = path_figures, device = figure_type, dpi = 1500, width = 60, height = 50, units = "mm")
###############################################################################
# FIGURE 2
###############################################################################
# Meteorology all
ggplot(data = Portoroz_meteo_data_long) +
  geom_line(aes(x = lubridate::floor_date(Date, "month"), y = Value), linewidth = 0.3) +
  labs(y = expression("Temperature [°C], precipitation [mm]")) +
  scale_linetype_manual(values = c(1, 3, 2)) + 
  scale_x_date(
    limits = as.Date(c("2011-01-01", "2025-01-01")),
    breaks = seq(as.Date("2011-01-01"), as.Date("2025-01-01"), by = "1 year"),
    date_labels = "%Y",
    expand = expansion(mult = 0.01, add = 0)) + # %b for month
  scale_y_continuous(labels = ~sub("-", "\u2212", .x)) +
  facet_wrap(~ Facet, scales = "free_y", ncol = 1, labeller = label_parsed) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, colour = "black", angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 8, colour = "black"),
        axis.title.y = element_text(size = 9, colour = "black"),
        strip.text = element_text(size = 9, colour = "black", margin = margin(0.02, 0, 0.02, 0, unit = "cm")),
        legend.position = "inside",
        legend.position.inside = c(0.2, 0.92),
        legend.box.spacing = margin(t = 0, r = 0, b = 0, l = 0),
        legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0),
        legend.key.spacing.x = unit(0.7, "cm"),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        legend.text = element_text(size = 9),
        legend.title = element_blank(),
        legend.key.size = unit(0.4, "cm"),
        panel.grid.minor.y = element_blank())

ggsave(filename = paste("Portoroz_meteo_all",".",figure_type, sep = ""), path = path_figures, device = figure_type, dpi = 1500, width = 95, height = 100, units = "mm")

# Meteorology monthly
ggplot(data = Portoroz_meteo_data_monthly) +
  geom_col(aes(x = Month, y = Mean), width = 0.7, position = "dodge") +
  labs(y = expression("Temperature [°C], precipitation [mm]")) +
  scale_fill_manual(values = c("black", "grey75", "grey40")) +
  scale_x_continuous(breaks = seq(1, 12, 1),
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                     expand = expansion(mult = 0.01, add = 0)) +
  scale_y_continuous(labels = ~sub("-", "\u2212", .x)) +
  facet_wrap(~ Facet, scales = "free_y", ncol = 1, labeller = label_parsed) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, colour = "black", angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 8, colour = "black"),
        axis.title.y = element_text(size = 9, colour = "black"),
        strip.text = element_text(size = 9, colour = "black", margin = margin(0.02, 0, 0.02, 0, unit = "cm")),
        legend.position = "inside",
        legend.position.inside = c(0.19, 0.92),
        legend.box.spacing = margin(t = 0, r = 0, b = 0, l = 0),
        legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0),
        legend.key.spacing.x = unit(0.7, "cm"),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        legend.text = element_text(size = 9),
        legend.title = element_blank(),
        legend.key.size = unit(0.4, "cm"),
        panel.grid.minor.y = element_blank())

ggsave(filename = paste("Portoroz_meteo_monthly",".",figure_type, sep = ""), path = path_figures, device = figure_type, dpi = 1500, width = 75, height = 100, units = "mm")

###############################################################################
# FIGURE 3
###############################################################################
# Isotopes time series, all
ggplot(data = dplyr::filter(Portoroz_data_longer, Year > 2010 & !(Variable %in% c("T", "P", "RH"))), aes(x = Date, y = Value)) +
  geom_line(linewidth = 0.3) +
  geom_text(data = dplyr::filter(Portoroz_data_period_statistics, !(Variable %in% c("T", "P", "RH"))),
            aes(x = as.Date("2025-01-01"), y = Max, label = paste0("n = ", n)), # Use y = 0 or another fixed value to position text inside plot
            inherit.aes = FALSE, vjust = 1, hjust = 1, size = 3.2) +
  labs(y = expression("\u03B4"^"18"*"O, \u03B4"^"2"*"H, and d-excess [\u2030]; A ("^"3"*"H) [TU]")) +
  scale_x_date(
    limits = as.Date(c("2011-01-01", "2025-01-01")),
    breaks = seq(as.Date("2011-01-01"), as.Date("2025-01-01"), by = "1 year"),
    date_labels = "%Y",
    expand = expansion(mult = 0.01, add = 0)) + # %b for month
  scale_y_continuous(labels = ~sub("-", "\u2212", .x)) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 1, labeller = label_parsed) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, colour = "black", angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 8, colour = "black"),
        axis.title.y = element_text(size = 9, colour = "black"),
        strip.text = element_text(size = 9, colour = "black", margin = margin(0.02, 0, 0.02, 0, unit = "cm")),
        legend.position = "top",
        legend.box.spacing = margin(t = 0, r = 0, b = 0, l = 0),
        legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0),
        legend.key.spacing.x = unit(0.7, "cm"),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        legend.text = element_text(size = 9),
        legend.title = element_blank(),
        panel.grid.minor.y = element_blank())

ggsave(filename = paste("Isotopes_trend",".",figure_type, sep = ""), path = path_figures, device = figure_type, dpi = 1500, width = 160, height = 130, units = "mm")

###############################################################################
# FIGURE 4
###############################################################################
Portoroz_data_longer |>
  dplyr::filter(is.finite(Value), !is.na(Variable)) |>
  dplyr::group_by(Variable, Month) |>
  dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
  dplyr::filter(n < 2)

Portoroz_data_longer |>
  dplyr::filter(is.finite(Value), !is.na(Variable)) |>
  dplyr::group_by(Variable, Season) |>
  dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
  dplyr::filter(n < 2)


# Monthly plot
ggplot(data = dplyr::filter(Portoroz_data_longer, is.na(Value) == FALSE),
       aes(x = Month, y = Value, group = Month)) +
  geom_point(data = Portoroz_data_longer_month,
             aes(x = Month, y = Value, group = Month), color = "black", size = 0.7, na.rm = TRUE) +
  geom_violin(data = dplyr::filter(Portoroz_data_longer, is.na(Value) == FALSE & Variable != "P") ,
              trim = FALSE, scale = "area", alpha = 0.5, linewidth = 0.3, na.rm = TRUE) +  # Trim = FALSE to avoid cutting off the tails
  geom_violin(data = dplyr::filter(Portoroz_data_longer, is.na(Value) == FALSE & Variable == "P") ,
              trim = TRUE, scale = "area", alpha = 0.5, linewidth = 0.3, na.rm = TRUE) +  # Trim = TRUE to cut the tails for P amount
  geom_text(data = Portoroz_data_month_statistics,
            aes(x = Month, y = Min, label = n), # Use y = 0 or another fixed value to position text inside plot
            inherit.aes = FALSE, vjust = 2.2, size = 3, check_overlap = TRUE) +
  labs(y = expression("\u03B4"^"18"*"O, \u03B4"^"2"*"H, and d-excess [\u2030]; A ("^"3"*"H) [TU]; T [°C], P [mm], RH [%]")) +
  scale_x_continuous(breaks = seq(1, 12, 1),
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                     expand = expansion(mult = 0.01, add = 0)) +
  scale_y_continuous(labels = ~sub("-", "\u2212", .x), expand = expansion(mult = c(0.4, 0.1))) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 1, labeller = label_parsed) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, colour = "black"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 8, colour = "black"),
        axis.title.y = element_text(size = 8, colour = "black"),
        strip.text = element_text(size = 8, colour = "black", margin = margin(0.02, 0, 0.02, 0, unit = "cm")),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave(filename = paste("Portoroz_monthly_violin",".",figure_type, sep = ""), path = path_figures, device = figure_type, dpi = 1500, width = 95, height = 180, units = "mm")

# Seasonal plot
ggplot(data = dplyr::filter(Portoroz_data_longer, is.na(Value) == FALSE),
       aes(x = Season, y = Value, group = Season)) +
  geom_point(data = Portoroz_data_longer_season,
             aes(x = Season, y = Value, group = Season), color = "black", size = 0.7, na.rm = TRUE) +
  geom_violin(data = dplyr::filter(Portoroz_data_longer, is.na(Value) == FALSE & Value != "P"),
              trim = FALSE, scale = "area", alpha = 0.5, linewidth = 0.3, na.rm = TRUE) +  # Trim = FALSE to avoid cutting off the tails
  geom_violin(data = dplyr::filter(Portoroz_data_longer, is.na(Value) == FALSE & Value == "P"),
              trim = TRUE, scale = "area", alpha = 0.5, linewidth = 0.3, na.rm = TRUE) +  # Trim = TRUE to cutting off the tails for P
  geom_text(data = Portoroz_data_season_statistics,
            aes(x = Season, y = Min, label = n), # Use y = 0 or another fixed value to position text inside plot
            inherit.aes = FALSE, vjust = 2.2, size = 3, check_overlap = TRUE) +
  scale_y_continuous(labels = ~sub("-", "\u2212", .x), expand = expansion(mult = c(0.4, 0.1))) +
  labs(y = expression("\u03B4"^"18"*"O, \u03B4"^"2"*"H, and d-excess [\u2030]; A ("^"3"*"H) [TU]; T [°C], P [mm], RH [%]")) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 1, labeller = label_parsed) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, colour = "black"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 8, colour = "black"),
        axis.title.y = element_text(size = 8, colour = "black"),
        strip.text = element_text(size = 8, colour = "black", margin = margin(0.02, 0, 0.02, 0, unit = "cm")),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave(filename = paste("Portoroz_seasonal_violin",".",figure_type, sep = ""), path = path_figures, device = figure_type, dpi = 1500, width = 70, height = 180, units = "mm")

###############################################################################
# FIGURE 5
###############################################################################
# Sigma plot & T
ggplot(data = dplyr::filter(Portoroz_data_longer_sigma, Variable_size == "T" & Value_size > 0),
       aes(x = δ18O, y = δ2H, size =  Value_size)) +
  geom_point(fill = "grey", color = "black", shape = 21, na.rm = TRUE) +
  geom_abline(slope = Portoroz_data_slopes_period$slope_RMA_weighted,
              intercept = Portoroz_data_slopes_period$intercept_RMA_weighted) +
  annotate("text", x = -8, y = -4,
           label = paste0("δ2H = (",
                          Portoroz_data_slopes_period$slope_RMA_weighted,
                          "±",
                          Portoroz_data_slopes_period$slope_error_RMA_weighted,
                          ")δ18O + (",
                          Portoroz_data_slopes_period$intercept_RMA_weighted,
                          "±",
                          Portoroz_data_slopes_period$intercept_error_RMA_weighted,
                          ")"),
           size = 2.7) +
  scale_x_continuous(labels = ~sub("-", "\u2212", .x)) +
  scale_y_continuous(labels = ~sub("-", "\u2212", .x)) +
  scale_radius() +  # breaks = scales::breaks_extended(n = 8)
  labs(y = expression("\u03B4"^"2"*"H [\u2030]"),
       x = expression("\u03B4"^"18"*"O [\u2030]"),
       size = "Temperature [°C]") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, colour = "black"),
        axis.title.x = element_text(size = 8, colour = "black"),
        axis.text.y = element_text(size = 8, colour = "black"),
        axis.title.y = element_text(size = 8, colour = "black"),
        legend.title = element_text(size = 8, colour = "black"),
        legend.text = element_text(size = 7, colour = "black"),
        legend.position = "inside",
        legend.position.inside = c(0.80, 0.25),
        legend.key.spacing.y = unit(1, "mm"),
        legend.spacing = unit(0, "mm"),
        legend.key.size = unit(2, "mm"),
        legend.background = element_blank(),
        panel.grid.minor = element_blank())

ggsave(filename = paste("Portoroz_sigma_plot_T",".",figure_type, sep = ""), path = path_figures, device = figure_type, dpi = 1500, width = 80, height = 80, units = "mm")

# Sigma plot & P
ggplot(data = dplyr::filter(Portoroz_data_longer_sigma, Variable_size == "P" & Value_size > 0),
       aes(x = δ18O, y = δ2H, size =  Value_size)) +
  geom_point(fill = "grey", color = "black", shape = 21, na.rm = TRUE) +
  geom_abline(slope = Portoroz_data_slopes_period$slope_RMA_weighted,
              intercept = Portoroz_data_slopes_period$intercept_RMA_weighted) +
  annotate("text", x = -8, y = -4,
           label = paste0("δ2H = (",
                          Portoroz_data_slopes_period$slope_RMA_weighted,
                          "±",
                          Portoroz_data_slopes_period$slope_error_RMA_weighted,
                          ")δ18O + (",
                          Portoroz_data_slopes_period$intercept_RMA_weighted,
                          "±",
                          Portoroz_data_slopes_period$intercept_error_RMA_weighted,
                          ")"),
           size = 2.7) +
  scale_x_continuous(labels = ~sub("-", "\u2212", .x)) +
  scale_y_continuous(labels = ~sub("-", "\u2212", .x)) +
  scale_radius(breaks = scales::breaks_extended(n = 6)) +  # breaks = scales::breaks_extended(n = 8)
  labs(y = expression("\u03B4"^"2"*"H [\u2030]"),
       x = expression("\u03B4"^"18"*"O [\u2030]"),
       size = "Precipitation [mm]") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, colour = "black"),
        axis.title.x = element_text(size = 8, colour = "black"),
        axis.text.y = element_text(size = 8, colour = "black"),
        axis.title.y = element_text(size = 8, colour = "black"),
        legend.title = element_text(size = 8, colour = "black"),
        legend.text = element_text(size = 7, colour = "black"),
        legend.position = "inside",
        legend.position.inside = c(0.80, 0.25),
        legend.key.spacing.y = unit(1, "mm"),
        legend.spacing = unit(0, "mm"),
        legend.key.size = unit(2, "mm"),
        legend.background = element_blank(),
        panel.grid.minor = element_blank())

ggsave(filename = paste("Portoroz_sigma_plot_P",".",figure_type, sep = ""), path = path_figures, device = figure_type, dpi = 1500, width = 80, height = 80, units = "mm")

###############################################################################
# FIGURE 6
###############################################################################
# Correlation plot
cor_labels <- as.character(Portoroz_data_cor)
cor_labels <- gsub("−", "-", cor_labels)
variable_order <- c(
  "3H", "T", "δ18O", "δ2H", "P", "d", "RH"
)
Portoroz_data_cor_ordered <- Portoroz_data_cor[
  variable_order,
  variable_order
]

Portoroz_data_cor_p_ordered <- Portoroz_data_cor_p[
  variable_order,
  variable_order
]
cor_label_expression <- c(
  "δ18O" = "italic(delta)^{18}*O",
  "δ2H"  = "italic(delta)^{2}*H",
  "3H"   = "{}^{3}*H",
  "d"    = "italic(d)-excess",
  "T"    = "T",
  "P"    = "P",
  "RH"   = "RH"
)

parse_cor_labels <- function(x) {
  labels <- unname(cor_label_expression[x])
  
  # Retain any names not included in the lookup vector
  labels[is.na(labels)] <- x[is.na(labels)]
  
  parse(text = labels)
}
minus_labels <- function(x, digits = 1) {
  labels <- formatC(
    x,
    format = "f",
    digits = digits
  )
  
  gsub("-", "\u2212", labels, fixed = TRUE)
}

correlation_plot <- ggcorrplot(Portoroz_data_cor_ordered,
                               p.mat = Portoroz_data_cor_p_ordered,
                               insig = "stars",
                               type = "lower",
                               hc.order = FALSE,
                               lab = TRUE,
                               legend.title = "",
                               lab_size = 2.7,
                               tl.cex = 8) +
  scale_x_discrete(labels = parse_cor_labels) +
  scale_y_discrete(labels = parse_cor_labels) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-1, 1),
    labels = minus_labels,
    name = ""
  ) +
  theme(
    text = element_text(color = "black"),
    legend.text = element_text(size = 8)
  )

for (i in seq_along(correlation_plot$layers)) {
  labels <- correlation_plot$layers[[i]]$aes_params$label
  
  if (!is.null(labels)) {
    correlation_plot$layers[[i]]$aes_params$label <-
      minus_labels(labels)
  }
}

correlation_plot

ggsave(filename = paste("Portoroz_correlation_plot",".",figure_type, sep = ""), path = path_figures, device = figure_type, dpi = 1500, width = 120, height = 100, units = "mm")

