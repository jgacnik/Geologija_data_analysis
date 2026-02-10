###############################################################################
# DATA ANALYSIS AND VISUALIZATION for Gačnik et al., Geologija (2026)
# CITATION: Gačnik J., Štrok, M., Žagar, K., Vreča P. (under reivew): Isotopic composition of hydrogen and oxygen in precipitation at the station Murska (Reaktor), Slovenia: period 2011–2024
# CODE AUTHOR: JAN GAČNIK, October 2025
# R version: 4.4.2
###############################################################################
# DO THIS STEP ONLY THE FIRST TIME! Installation of needed packages
install.packages(c("rnaturalearth", "sf", "slider", "ggcorrplot", "forecast", "tidyverse", "readxl"))

# CHANGE THE FOLLOWING PARAMETERS:
path_scripts <- "C:/Users/gacnik/OneDrive - ijs.si/Jan 2023+/Voda izotopi Polona/Murska Sobota/scripts" # Change to your directory path where the R scripts are located, use "/" and not "\"
path_data <- "C:/Users/gacnik/OneDrive - ijs.si/Jan 2023+/Voda izotopi Polona/Murska Sobota/data" # Change to your directory path where the data is located, use "/" and not "\"
path_figures <- "C:/Users/gacnik/OneDrive - ijs.si/Jan 2023+/Voda izotopi Polona/Murska Sobota/figures" # Change to your directory path where you want to save figures, use "/" and not "\"

###############################################################################
# CODE BELOW DOES NOT NEED ALTERING
###############################################################################
Murska_filename <- "Murska_sobota_2016-2024.xlsx"
figure_type <- "png"
dpi_set <- 500

# Load required packages for the current session
setwd(path_scripts)
lapply(c("rnaturalearth", "sf", "slider", "ggcorrplot", "forecast", "mblm", "tidyverse", "readxl"), require, character.only = TRUE)

# Calling needed functions
source("Functions_MA&RMA.R")

###############################################################################
# Data reading, adjusting, and filtering
###############################################################################
Murska_data <- read_xlsx(sheet = "Data MS", path = file.path(path_data, Murska_filename), trim_ws = TRUE) %>%
  dplyr::select(Sample_ID, Station_ID, Name, Year, Month, P, T, RH, δ18O, δ2H, d, "3H (TU)") %>%
  rename("3H" = "3H (TU)") %>% 
  mutate(Site = "Murska Sobota") %>%
  mutate(Season = "") %>%
  mutate(Season = case_when(
    Month %in% c(12, 1, 2)  ~ "winter",
    Month %in% c(3, 4, 5)  ~ "spring",
    Month %in% c(6, 7, 8)  ~ "summer",
    Month %in% c(9, 10, 11)  ~ "autumn",
    TRUE ~ Season)) %>%
  mutate(Season = factor(Season, levels = c("winter", "spring", "summer", "autumn"))) %>%
  mutate(P_δ18O_δ2H = P) %>%
  mutate(P_δ18O_δ2H = case_when(
    is.na(δ18O) | is.na(δ2H) ~ NA,
    TRUE ~ P_δ18O_δ2H)) %>%
  mutate(P_3H = case_when(
    is.na(`3H`) ~ NA,
    TRUE ~ P)) %>%
  mutate(Date = as.POSIXct(make_date(Year, Month)),
         time_since_start = interval(min(Date), Date) %/% months(1),
         Date = as.Date(make_date(Year, Month)))

Murska_data_cor <- Murska_data %>% 
  dplyr::select(δ18O, δ2H, d, "3H", T, P, RH) %>%
  cor(use = "pairwise.complete.obs", method = "pearson")


Murska_data_longer <-  Murska_data %>% 
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

Murska_data_longer_sigma <-  Murska_data %>% 
  pivot_longer(cols = c("T", "P"), names_to = "Variable_size", values_to = "Value_size") %>%
  mutate(Variable_size = factor(Variable_size, levels = c("T", "P")))

Murska_data_longer_month <- Murska_data %>%
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

Murska_data_longer_season <- Murska_data %>%
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

Murska_data_longer_year <- Murska_data %>%
  group_by(Year) %>%
  summarise(weighted_mean_δ18O = sum(δ18O * ((P_δ18O_δ2H/sum(P_δ18O_δ2H, na.rm = TRUE))), na.rm = TRUE),
            weighted_mean_δ2H = sum(δ2H * ((P_δ18O_δ2H/sum(P_δ18O_δ2H, na.rm = TRUE))), na.rm = TRUE),
            weighted_mean_d = sum(d * ((P_δ18O_δ2H/sum(P_δ18O_δ2H, na.rm = TRUE))), na.rm = TRUE),
            weighted_mean_3H = sum(`3H` * ((P_3H/sum(P_3H, na.rm = TRUE))), na.rm = TRUE),
            mean_T = mean(T),
            mean_P = mean(P)) %>%
  pivot_longer(cols = c(weighted_mean_δ18O, weighted_mean_δ2H, weighted_mean_d, weighted_mean_3H, mean_T, mean_P), names_to = "Variable", values_to = "Value") %>% 
  pivot_wider(names_from = c("Variable"), values_from = "Value", names_sep = "_") 

#write.csv(Murska_data_longer_year, "Murska_year_statistics.csv", row.names = FALSE)

Murska_data_period_statistics <- Murska_data %>% 
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

#write.csv(Murska_data_period_statistics, "Murska_period_statistics.csv", row.names = FALSE)

Murska_data_month_statistics <- Murska_data %>% 
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

#write.csv(Murska_data_month_statistics, "Murska_month_statistics.csv", row.names = FALSE)

Murska_data_season_statistics <- Murska_data %>% 
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

#write.csv(Murska_data_season_statistics, "Murska_seasonal_statistics.csv", row.names = FALSE)

Murska_data_year_slopes_2016_2024 <- Murska_data %>%
  dplyr::filter(!is.na(δ18O) & !is.na(δ2H) & !is.na(P)) %>% 
  group_by(Year) %>% 
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

#write.csv(Murska_data_year_slopes_2016_2024, "Murska_yearly_regression_statistics.csv", row.names = FALSE)

# d18O/d2H versus T regresssions and correlations, STATS
Murska_T_regression <- Murska_data %>% 
  dplyr::filter(!is.na(δ18O) & !is.na(δ2H))

a <- lm(`δ18O` ~ T, data =  Murska_T_regression) 
summary(a)
cor(x = Murska_data$T, y = Murska_data$δ18O, use = "pairwise.complete.obs")
b <- lm(`δ2H` ~ T, data =  Murska_data)
summary(b)
cor(x = Murska_data$T, y = Murska_data$δ2H, use = "pairwise.complete.obs")

###############################################################################
# PLOTTING
###############################################################################
# FIGURE 1
###############################################################################
# Map Slovenia
shapefile_border_SLO <- ne_countries(scale = 10, continent = "Europe")
shapefile_names_SLO <- data.frame(
  Station_name = c("SVN", "AUT", "HRV", "ITA", "HUN"),
  lon = c(14.9, 15, 15.35, 12.5, 17.2),
  lat = c(46.3, 47.5, 45, 46.1, 46.8)
) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

stations_sf <- data.frame(
  Station_name = c("Murska–Bežigrad", "Murska–JSI", "Murska–Reaktor"),
  lon = c(14.512352, 14.487778, 14.597046),
  lat = c(46.065507, 46.041944, 46.094612)
) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)
Murska_sf <- stations_sf %>%
  dplyr::filter(Station_name == "Murska–Bežigrad") %>%
  mutate(Station_name = "Murska") %>%
  rbind(st_as_sf(data.frame(
    Station_name = c("Vienna", "Portorož"),
    lon = c(16.37250, 13.5799694),
    lat = c(48.20833, 45.5166333)
  ), coords = c("lon", "lat"), crs = 4326))

ggplot() +
  geom_sf(data = shapefile_border_SLO, fill = NA) +
  geom_sf(data = dplyr::filter(shapefile_border_SLO, sov_a3 == "SVN"), fill = "grey85") +
  geom_text(data = shapefile_names_SLO, aes(x = st_coordinates(geometry)[,1], 
                                            y = st_coordinates(geometry)[,2], 
                                            label = Station_name),
            size = 1.8) +
  geom_sf(data = dplyr::filter(Murska_sf, Station_name == "Murska"), fill = "black", size = 0.6, shape = 21, stroke = 0.4) + 
  geom_text(data = dplyr::filter(Murska_sf, Station_name == "Murska"), aes(x = st_coordinates(geometry)[,1], 
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
# FIGURE 3
###############################################################################
# Isotopes timeseries, all
ggplot(data = dplyr::filter(Murska_data_longer, Year > 2010 & !(Variable %in% c("T", "P", "RH"))), aes(x = Date, y = Value)) +
  geom_line(linewidth = 0.3) +
  geom_text(data = dplyr::filter(Murska_data_period_statistics, !(Variable %in% c("T", "P", "RH"))),
            aes(x = as.Date("2025-01-01"), y = Max, label = paste0("n = ", n)), # Use y = 0 or another fixed value to position text inside plot
            inherit.aes = FALSE, vjust = 1, hjust = 1, size = 3.2) +
  labs(y = expression("\u03B4"^"18"*"O, \u03B4"^"2"*"H, and d-excess [\u2030]; A ("^"3"*"H) [TU]")) +
  scale_x_date(
    limits = as.Date(c("2016-01-01", "2025-01-01")),
    breaks = seq(as.Date("2016-01-01"), as.Date("2025-01-01"), by = "1 year"),
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
ggplot(data = dplyr::filter(Murska_data_longer, is.na(Variable) == FALSE),
       aes(x = Month, y = Value, group = Month)) +
  geom_point(data = Murska_data_longer_month,
             aes(x = Month, y = Value, group = Month), color = "black", size = 0.7) +
  geom_violin(trim = FALSE, scale = "area", alpha = 0.5, linewidth = 0.3) +  # Trim = FALSE to avoid cutting off the tails
  geom_text(data = Murska_data_month_statistics,
            aes(x = Month, y = Min, label = n), # Use y = 0 or another fixed value to position text inside plot
            inherit.aes = FALSE, vjust = 2.2, size = 3, check_overlap = TRUE) +
  labs(y = expression("\u03B4"^"18"*"O, \u03B4"^"2"*"H, and d-excess [\u2030]; A ("^"3"*"H) [TU]; T [°C], P [mm], RH [%]")) +
  scale_x_continuous(breaks = seq(1, 12, 1),
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                     expand = expansion(mult = 0.01, add = 0)) +
  scale_y_continuous(labels = ~sub("-", "\u2212", .x), expand = expansion(mult = c(0.3, 0.1))) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 1, labeller = label_parsed) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, colour = "black"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 8, colour = "black"),
        axis.title.y = element_text(size = 8, colour = "black"),
        strip.text = element_text(size = 8, colour = "black", margin = margin(0.02, 0, 0.02, 0, unit = "cm")),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave(filename = paste("Murska_monthly_violin",".",figure_type, sep = ""), path = path_figures, device = figure_type, dpi = 1500, width = 95, height = 180, units = "mm")

# Seasonal plot
ggplot(data = dplyr::filter(Murska_data_longer, Year > 2010 & is.na(Variable) == FALSE),
       aes(x = Season, y = Value, group = Season)) +
  geom_point(data = Murska_data_longer_season,
             aes(x = Season, y = Value, group = Season), color = "black", size = 0.7) +
  geom_violin(trim = FALSE, scale = "area", alpha = 0.5, linewidth = 0.3) +  # Trim = FALSE to avoid cutting off the tails
  geom_text(data = Murska_data_season_statistics,
            aes(x = Season, y = Min, label = n), # Use y = 0 or another fixed value to position text inside plot
            inherit.aes = FALSE, vjust = 2.2, size = 3, check_overlap = TRUE) +
  scale_y_continuous(labels = ~sub("-", "\u2212", .x), expand = expansion(mult = c(0.3, 0.1))) +
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

ggsave(filename = paste("Murska_seasonal_violin",".",figure_type, sep = ""), path = path_figures, device = figure_type, dpi = 1500, width = 70, height = 180, units = "mm")

###############################################################################
# FIGURE 5
###############################################################################
# Sigma plot & T
ggplot(data = dplyr::filter(Murska_data_longer_sigma, Variable_size == "T" & Value_size > 0),
       aes(x = δ18O, y = δ2H, size =  Value_size)) +
  geom_point(fill = "grey", color = "black", shape = 21) +
  geom_abline(slope = 7.76, intercept = 9.91) +
  annotate("text", x = -10, y = -10,
           label = "δ2H = (7.77±0.08)δ18O + (7.61±0.71)", size = 2.7) +
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

ggsave(filename = paste("Murska_sigma_plot_T",".",figure_type, sep = ""), path = path_figures, device = figure_type, dpi = 1500, width = 80, height = 80, units = "mm")

# Sigma plot & P
ggplot(data = dplyr::filter(Murska_data_longer_sigma, Variable_size == "P" & Value_size > 0),
       aes(x = δ18O, y = δ2H, size =  Value_size)) +
  geom_point(fill = "grey", color = "black", shape = 21) +
  geom_abline(slope = 7.76, intercept = 9.91) +
  annotate("text", x = -10, y = -10,
           label = "δ2H = (7.77±0.08)δ18O + (7.61±0.71)", size = 2.7) +
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

ggsave(filename = paste("Murska_sigma_plot_P",".",figure_type, sep = ""), path = path_figures, device = figure_type, dpi = 1500, width = 80, height = 80, units = "mm")

###############################################################################
# FIGURE 6
###############################################################################
# Correlation plot
# Replace hyphens (−) with minus signs (-) in the labels in post-processing of the figure
cor_labels <- as.character(Murska_data_cor)
cor_labels <- gsub("−", "-", cor_labels)
p_matrix <- cor_pmat(Murska_data_cor)

ggcorrplot(Murska_data_cor, hc.order = TRUE, type = "lower", lab = TRUE,
           legend.title = "",
           lab_size = 2.7,
           tl.cex = 8) +
  theme(
    legend.text = element_text(size = 8)
  )
ggsave(filename = paste("Murska_correlation_plot",".",figure_type, sep = ""), path = path_figures, device = figure_type, dpi = 1500, width = 120, height = 100, units = "mm")
