###############################################################################
# DATA ANALYSIS AND VISUALIZATION for Gačnik et al., Geologija (2026)
# CITATION: Gačnik J., Štrok, M., Žagar, K., Vreča P. (under reivew): Isotopic composition of hydrogen and oxygen in precipitation at the station Ljubljana (Reaktor), Slovenia: period 2011–2024
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
Ljubljana_filename <- "Ljubljana_2011-2024.xlsx"
Ljubljana_filename_metdata <- "Ljubljana_precipitation&temperature_2011-2024.xlsx"
figure_type <- "png"
dpi_set <- 500

# Load required packages for the current session
lapply(c("rnaturalearth", "sf", "slider", "ggcorrplot", "forecast", "tidyverse", "readxl"), require, character.only = TRUE)

# Calling needed functions for PWRMA. Located in separate script "Functions_MA&RMA.R"
source(file.path(path_scripts, "Functions_MA&RMA.R"))

###############################################################################
# Data reading, adjusting, and filtering
###############################################################################
Ljubljana_meteo_data <- read_xlsx(path = file.path(path_data, Ljubljana_filename_metdata), skip = 5,
                                  col_names = c("Date", "P_reaktor", "P_bezigrad", "T_bezigrad", "T_hrastje", "RH", "n_day_w_precip", "n_day_w_rain", "n_day_w_rain_and_precip", "n_day_w_snow", "n_dat_w_snow_cover")) %>%
  mutate(T_hrastje = gsub("[^0-9.-]", "", T_hrastje),
         T_hrastje = as.numeric(T_hrastje)) %>% 
  mutate(Date = as.Date(Date)) %>% 
  mutate(Year = year(Date),
         Month = month(Date)) 

Ljubljana_meteo_data_long <- Ljubljana_meteo_data %>% 
  pivot_longer(cols = c("P_bezigrad", "P_reaktor", "T_bezigrad", "T_hrastje"), names_to = "Variable", values_to = "Value") %>% 
  mutate(Facet = case_when(
    grepl("P_", Variable) == TRUE ~ "Precipitation",
    grepl("P_", Variable) == FALSE ~ "Temperature",
    TRUE ~ NA
  )) %>% 
  mutate(Location = case_when(
    Variable == "T_bezigrad" ~ "Bežigrad",
    Variable == "T_hrastje" ~ "Hrastje",
    Variable == "P_bezigrad" ~ "Bežigrad",
    Variable == "P_reaktor" ~ "Reaktor",
    TRUE ~ NA
  ))

Ljubljana_meteo_data_rolling <- Ljubljana_meteo_data %>% 
  mutate(sum_2 = slide_dbl(P_bezigrad, ~sum(.x, na.rm = TRUE), .before = 1),
         sum_3 = slide_dbl(P_bezigrad, ~sum(.x, na.rm = TRUE), .before = 2))

Ljubljana_meteo_data_monthly <- Ljubljana_meteo_data_long %>% 
  mutate(Month = month(Date)) %>% 
  group_by(Location, Facet, Month) %>% 
  summarise(Mean = mean(Value, na.rm = TRUE),
            StDev = sd(Value, na.rm = TRUE))

Ljubljana_meteo_data_yearly <- Ljubljana_meteo_data_long %>% 
  mutate(Year = year(Date)) %>% 
  group_by(Variable, Year) %>% 
  summarise(Total = sum(Value, na.rm = TRUE),
            Mean = mean(Value, na.rm = TRUE)) %>% 
  mutate(Facet = case_when(
    grepl("Temperature", Variable) == FALSE ~ "Precipitation",
    grepl("Temperature", Variable) == TRUE ~ "Temperature",
    TRUE ~ NA
  ))

# Save yearly meteorological data into a .csv table
write.csv(Ljubljana_meteo_data_yearly, file = file.path(path_tables, "Ljubljana_yearly_meteorological_statistics.csv"), row.names = FALSE)

Ljubljana_data <- read_xlsx(sheet = "Data Ljubljana", path = file.path(path_data, Ljubljana_filename), trim_ws = TRUE) %>%
  left_join(Ljubljana_meteo_data %>% dplyr::select(Year, Month, P_reaktor), by = c("Year", "Month")) %>% 
  dplyr::select(Sample_ID, Station_ID, Name, Year, Month, P, P_reaktor, T, RH, δ18O, δ2H, d, "3H (TU)") %>%
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
  mutate(P_RE_δ18O_δ2H = case_when(
    is.na(δ18O) | is.na(δ2H) ~ NA,
    TRUE ~ P_reaktor)) %>%
  mutate(P_RE_3H = case_when(
    is.na(`3H`) ~ NA,
    TRUE ~ P_reaktor)) %>%
  mutate(Date = as.Date(make_date(Year, Month)))

Ljubljana_data_cor <- Ljubljana_data %>% 
  dplyr::select(δ18O, δ2H, d, "3H", T, P, RH) %>%
  cor(use = "pairwise.complete.obs", method = "pearson")

Ljubljana_data_longer <-  Ljubljana_data %>% 
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

Ljubljana_data_longer_sigma <-  Ljubljana_data %>% 
  pivot_longer(cols = c("T", "P"), names_to = "Variable_size", values_to = "Value_size") %>%
  mutate(Variable_size = factor(Variable_size, levels = c("T", "P")))

Ljubljana_data_longer_month <- Ljubljana_data %>%
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

Ljubljana_data_longer_season <- Ljubljana_data %>%
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

Ljubljana_data_longer_year <- Ljubljana_data %>%
  group_by(Year) %>%
  summarise(weighted_mean_δ18O_BE = sum(δ18O * ((P_δ18O_δ2H/sum(P_δ18O_δ2H, na.rm = TRUE))), na.rm = TRUE),
            weighted_mean_δ2H_BE = sum(δ2H * ((P_δ18O_δ2H/sum(P_δ18O_δ2H, na.rm = TRUE))), na.rm = TRUE),
            weighted_mean_d_BE = sum(d * ((P_δ18O_δ2H/sum(P_δ18O_δ2H, na.rm = TRUE))), na.rm = TRUE),
            weighted_mean_3H_BE = sum(`3H` * ((P_3H/sum(P_3H, na.rm = TRUE))), na.rm = TRUE),
            weighted_mean_δ18O_RE = sum(δ18O * ((P_RE_δ18O_δ2H/sum(P_RE_δ18O_δ2H, na.rm = TRUE))), na.rm = TRUE),
            weighted_mean_δ2H_RE = sum(δ2H * ((P_RE_δ18O_δ2H/sum(P_RE_δ18O_δ2H, na.rm = TRUE))), na.rm = TRUE),
            weighted_mean_d_RE = sum(d * ((P_RE_δ18O_δ2H/sum(P_RE_δ18O_δ2H, na.rm = TRUE))), na.rm = TRUE),
            weighted_mean_3H_RE = sum(`3H` * ((P_RE_3H/sum(P_RE_3H, na.rm = TRUE))), na.rm = TRUE)) %>%
  pivot_longer(cols = c(weighted_mean_δ18O_BE, weighted_mean_δ2H_BE, weighted_mean_d_BE, weighted_mean_3H_BE, 
                        weighted_mean_δ18O_RE, weighted_mean_δ2H_RE, weighted_mean_d_RE, weighted_mean_3H_RE), names_to = "Variable", values_to = "Value") %>% 
  mutate(Site = case_when(
    grepl("_BE", Variable) ~ "Bežigrad",
    grepl("_RE", Variable) ~ "Reaktor"
  )) %>%
  mutate(Site = ifelse(grepl("_BE", Variable), "Bežigrad", "Reaktor")) %>%
  pivot_wider(names_from = c("Site", "Variable"), values_from = "Value", names_sep = "_") 

# Stat tests for Ljubljana-Bežigrad and Ljubljana - Reaktor precipitation-weighted yearly means of isotopic variables
# Diebold-Mariano test ("forecast" package)
dm.test(Ljubljana_data_longer_year$Bežigrad_weighted_mean_δ18O_BE, 
        Ljubljana_data_longer_year$Reaktor_weighted_mean_δ18O_RE, h = 1, power = 2)
dm.test(Ljubljana_data_longer_year$Bežigrad_weighted_mean_δ2H_BE, 
        Ljubljana_data_longer_year$Reaktor_weighted_mean_δ2H_RE, h = 1, power = 2)
dm.test(Ljubljana_data_longer_year$Bežigrad_weighted_mean_d_BE, 
        Ljubljana_data_longer_year$Reaktor_weighted_mean_d_RE, h = 1, power = 2)
dm.test(Ljubljana_data_longer_year$Bežigrad_weighted_mean_3H_BE, 
        Ljubljana_data_longer_year$Reaktor_weighted_mean_3H_RE, h = 1, power = 2)

Ljubljana_data_period_statistics <- Ljubljana_data %>% 
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
write.csv(Ljubljana_data_period_statistics, file = file.path(path_tables, "Ljubljana_period_statistics.csv"), row.names = FALSE)

Ljubljana_data_month_statistics <- Ljubljana_data %>% 
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
write.csv(Ljubljana_data_month_statistics, file = file.path(path_tables, "Ljubljana_month_statistics.csv"), row.names = FALSE)

Ljubljana_data_season_statistics <- Ljubljana_data %>% 
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
write.csv(Ljubljana_data_season_statistics, file = file.path(path_tables, "Ljubljana_seasonal_statistics.csv"), row.names = FALSE)

Ljubljana_data_year_slopes_2011_2024 <- Ljubljana_data %>%
  dplyr::filter(!is.na(δ18O) & !is.na(δ2H)) %>%
  rename("P_bezigrad" = P) %>% 
  pivot_longer(cols = c("P_bezigrad", "P_reaktor"), names_to = "P_type", values_to = "P_amount") %>% 
  group_by(P_type, Year) %>%
  summarise(slope_RMA = RMA_weighted(δ18O, δ2H, P_amount)$slopes$RMA,
            slope_error_RMA =  RMA_weighted(δ18O, δ2H, P_amount)$errors_slopes$RMA,
            slope_RMA_weighted = RMA_weighted(δ18O, δ2H, P_amount)$slopes$RMA_weighted,
            slope_error_RMA_weighted = RMA_weighted(δ18O, δ2H, P_amount)$errors_slopes$RMA_weighted,
            intercept_RMA = RMA_weighted(δ18O, δ2H, P_amount)$intercepts$RMA,
            intercept_error_RMA = RMA_weighted(δ18O, δ2H, P_amount)$errors_intercepts$RMA,
            intercept_RMA_weighted = RMA_weighted(δ18O, δ2H, P_amount)$intercepts$RMA_weighted,
            intercept_error_RMA_weighted = RMA_weighted(δ18O, δ2H, P_amount)$errors_intercepts$RMA_weighted,
            oct_nov = sum(P_amount[month(Date) %in% c(10, 11)], na.rm = TRUE),
            may_jun = sum(P_amount[month(Date) %in% c(5, 6)], na.rm = TRUE),
            total_P = sum(P_amount, na.rm = TRUE),
            mediterranean_index = ((oct_nov - may_jun)/total_P) * 100) %>%
  mutate(across(everything(), ~round(., digits = 2))) %>% 
  mutate(LMWL_RMA = paste0("d2H = (", slope_RMA, " ± ", slope_error_RMA, ") d18O + (", intercept_RMA, " ± ", intercept_error_RMA, ")"),
         LMWL_PWRMA = paste0("d2H = (", slope_RMA_weighted," ± ", slope_error_RMA_weighted, ") d18O + (", intercept_RMA_weighted,  " ± ", intercept_error_RMA_weighted, ")"))

# Save yearly RMA/MA regression statistics in a table
write.csv(Ljubljana_data_year_slopes_2011_2024, file = file.path(path_tables, "Ljubljana_yearly_regression_statistics.csv"), row.names = FALSE)

# d18O/d2H versus T regressions and correlations, STATS
Ljubljana_T_regression <- Ljubljana_data %>% 
  dplyr::filter(!is.na(δ18O) & !is.na(δ2H))

a <- lm(`δ18O` ~ T, data =  Ljubljana_T_regression) 
summary(a)
cor(x = Ljubljana_data$T, y = Ljubljana_data$δ18O, use = "pairwise.complete.obs")
b <- lm(`δ2H` ~ T, data =  Ljubljana_data)
summary(b)
cor(x = Ljubljana_data$T, y = Ljubljana_data$δ2H, use = "pairwise.complete.obs")

Ljubljana_T_regression_hrastje <- Ljubljana_data %>% 
  dplyr::filter(!is.na(δ18O) & !is.na(δ2H)) %>% 
  left_join(dplyr::select(Ljubljana_meteo_data, c("Date", "T_hrastje")), by = "Date")

a_hrastje <- lm(`δ18O` ~ T_hrastje, data =  Ljubljana_T_regression_hrastje)
summary(a_hrastje)
cor(x = Ljubljana_meteo_data$T_hrastje, y = Ljubljana_data$δ18O, use = "pairwise.complete.obs")
b_hrastje <- lm(`δ2H` ~ T_hrastje, data =  Ljubljana_T_regression_hrastje)
summary(b_hrastje)
cor(x = Ljubljana_meteo_data$T_hrastje, y = Ljubljana_data$δ2H, use = "pairwise.complete.obs")

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

station_Ljubljana <- st_as_sf(data.frame(
    Station_name = c("Ljubljana"),
    lon = c(14.512352),
    lat = c(46.065507)
  ), coords = c("lon", "lat"), crs = 4326)

ggplot() +
  geom_sf(data = country_borders, fill = NA) +
  geom_sf(data = dplyr::filter(country_borders, sov_a3 == "SVN"), fill = "grey85") +
  geom_text(data = country_names, aes(x = st_coordinates(geometry)[,1],
                                      y = st_coordinates(geometry)[,2],
                                      label = Station_name),
            size = 1.8) +
  geom_sf(data = station_Ljubljana, fill = "black", size = 0.6, shape = 21, stroke = 0.4) + 
  geom_text(data = station_Ljubljana, aes(x = st_coordinates(geometry)[,1],
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
ggplot(data = Ljubljana_meteo_data_long) +
  geom_line(aes(x = lubridate::floor_date(Date, "month"), y = Value, group = Location, linetype = Location), linewidth = 0.3) +
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

ggsave(filename = paste("Ljubljana_meteo_all",".",figure_type, sep = ""), path = path_figures, device = figure_type, dpi = 1500, width = 95, height = 100, units = "mm")

# Meteorology monthly
ggplot(data = Ljubljana_meteo_data_monthly) +
  geom_col(aes(x = Month, y = Mean, fill = Location), width = 0.7, position = "dodge") +
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

ggsave(filename = paste("Ljubljana_meteo_monthly",".",figure_type, sep = ""), path = path_figures, device = figure_type, dpi = 1500, width = 75, height = 100, units = "mm")

###############################################################################
# FIGURE 3
###############################################################################
# Isotopes time series, all
ggplot(data = dplyr::filter(Ljubljana_data_longer, Year > 2010 & !(Variable %in% c("T", "P", "RH"))), aes(x = Date, y = Value)) +
  geom_line(linewidth = 0.3) +
  geom_text(data = dplyr::filter(Ljubljana_data_period_statistics, !(Variable %in% c("T", "P", "RH"))),
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
# Monthly plot
ggplot(data = dplyr::filter(Ljubljana_data_longer, is.na(Variable) == FALSE),
       aes(x = Month, y = Value, group = Month)) +
  geom_point(data = Ljubljana_data_longer_month,
             aes(x = Month, y = Value, group = Month), color = "black", size = 0.7, na.rm = TRUE) +
  geom_violin(data = dplyr::filter(Ljubljana_data_longer, is.na(Variable) == FALSE & Variable != "P") ,
              trim = FALSE, scale = "area", alpha = 0.5, linewidth = 0.3, na.rm = TRUE) +  # Trim = FALSE to avoid cutting off the tails
  geom_violin(data = dplyr::filter(Ljubljana_data_longer, is.na(Variable) == FALSE & Variable == "P") ,
              trim = TRUE, scale = "area", alpha = 0.5, linewidth = 0.3, na.rm = TRUE) +  # Trim = TRUE to cut the tails for P amont
  geom_text(data = Ljubljana_data_month_statistics,
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

ggsave(filename = paste("Ljubljana_monthly_violin",".",figure_type, sep = ""), path = path_figures, device = figure_type, dpi = 1500, width = 95, height = 180, units = "mm")

# Seasonal plot
ggplot(data = dplyr::filter(Ljubljana_data_longer, is.na(Variable) == FALSE),
       aes(x = Season, y = Value, group = Season)) +
  geom_point(data = Ljubljana_data_longer_season,
             aes(x = Season, y = Value, group = Season), color = "black", size = 0.7, na.rm = TRUE) +
  geom_violin(data = dplyr::filter(Ljubljana_data_longer, is.na(Variable) == FALSE & Variable != "P"),
              trim = FALSE, scale = "area", alpha = 0.5, linewidth = 0.3, na.rm = TRUE) +  # Trim = FALSE to avoid cutting off the tails
  geom_violin(data = dplyr::filter(Ljubljana_data_longer, is.na(Variable) == FALSE & Variable == "P"),
              trim = TRUE, scale = "area", alpha = 0.5, linewidth = 0.3, na.rm = TRUE) +  # Trim = TRUE to cutting off the tails for P
  geom_text(data = Ljubljana_data_season_statistics,
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

ggsave(filename = paste("Ljubljana_seasonal_violin",".",figure_type, sep = ""), path = path_figures, device = figure_type, dpi = 1500, width = 70, height = 180, units = "mm")

###############################################################################
# FIGURE 5
###############################################################################
# Sigma plot & T
ggplot(data = dplyr::filter(Ljubljana_data_longer_sigma, Variable_size == "T" & Value_size > 0),
       aes(x = δ18O, y = δ2H, size =  Value_size)) +
  geom_point(fill = "grey", color = "black", shape = 21, na.rm = TRUE) +
  geom_abline(slope = 7.76, intercept = 9.91) +
  annotate("text", x = -10, y = -10,
           label = "δ2H = (7.76±0.07)δ18O + (9.11±0.65)", size = 2.7) +
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
        legend.position.inside = c(0.80, 0.2),
        legend.key.spacing.y = unit(1, "mm"),
        legend.spacing = unit(0, "mm"),
        legend.key.size = unit(2, "mm"),
        legend.background = element_blank(),
        panel.grid.minor = element_blank())

ggsave(filename = paste("Ljubljana_sigma_plot_T",".",figure_type, sep = ""), path = path_figures, device = figure_type, dpi = 1500, width = 80, height = 80, units = "mm")

# Sigma plot & P
ggplot(data = dplyr::filter(Ljubljana_data_longer_sigma, Variable_size == "P" & Value_size > 0),
       aes(x = δ18O, y = δ2H, size =  Value_size)) +
  geom_point(fill = "grey", color = "black", shape = 21, na.rm = TRUE) +
  geom_abline(slope = 7.76, intercept = 9.91) +
  annotate("text", x = -10, y = -10,
           label = "δ2H = (7.76±0.07)δ18O + (9.11±0.65)", size = 2.7) +
  scale_x_continuous(labels = ~sub("-", "\u2212", .x)) +
  scale_y_continuous(labels = ~sub("-", "\u2212", .x)) +
  scale_radius() +  # breaks = scales::breaks_extended(n = 8)
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
        legend.position.inside = c(0.80, 0.2),
        legend.key.spacing.y = unit(1, "mm"),
        legend.spacing = unit(0, "mm"),
        legend.key.size = unit(2, "mm"),
        legend.background = element_blank(),
        panel.grid.minor = element_blank())

ggsave(filename = paste("Ljubljana_sigma_plot_P",".",figure_type, sep = ""), path = path_figures, device = figure_type, dpi = 1500, width = 80, height = 80, units = "mm")

###############################################################################
# FIGURE 6
###############################################################################
# Correlation plot
cor_labels <- as.character(Ljubljana_data_cor)

# Replace hyphens (−) with minus signs (-) in the labels in post-processing of the figure
cor_labels <- gsub("−", "-", cor_labels)
ggcorrplot(Ljubljana_data_cor, hc.order = TRUE, type = "lower", lab = TRUE,
           legend.title = "",
           lab_size = 2.7,
           tl.cex = 8) +
  theme(
    legend.text = element_text(size = 8)
  )
ggsave(filename = paste("Ljubljana_correlation_plot",".",figure_type, sep = ""), path = path_figures, device = figure_type, dpi = 1500, width = 120, height = 100, units = "mm")
