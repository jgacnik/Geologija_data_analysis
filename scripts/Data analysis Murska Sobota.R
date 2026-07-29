###############################################################################
# DATA ANALYSIS AND VISUALIZATION for Gačnik et al., Geologija (2026)
# CITATION: Gačnik J., Štrok, M., Žagar, K., Vreča P. (under reivew): Isotopic composition of hydrogen and oxygen in precipitation at the station Murska (Reaktor), Slovenia: period 2011–2024
# CODE AUTHOR: JAN GAČNIK, October 2025
# R version: 4.4.2
###############################################################################
# DO THIS STEP ONLY THE FIRST TIME! Installation of needed packages
install.packages(c("rnaturalearth", "sf", "slider", "deming", "ggcorrplot", "forecast", "mblm", "tidyverse", "readxl"))

# Paths relative to the Data_analysis_MS.Rproj project root
path_scripts <- "scripts"
path_data <- "data"
path_figures <- "figures"
path_tables <- "tables"

###############################################################################
# CODE BELOW DOES NOT NEED ALTERING
###############################################################################
Murska_filename <- "Murska_Sobota_2016-2024.xlsx"
Hungary_meteo_filename <- "Torokkoppany_meteo_2016-2024.xlsx"
Graz_filename <- "Graz_2016-2024.xlsx"
Hrascica_filename <- "Hrascica.xlsx"
figure_type <- "png"
dpi_set <- 500

# Load required packages for the current session
lapply(c("rnaturalearth", "sf", "slider", "deming", "ggcorrplot", "forecast", "mblm", "tidyverse", "readxl"), require, character.only = TRUE)

# Calling needed functions
source(file.path(path_scripts, "Functions_MA&RMA.R"))

###############################################################################
# Data reading, adjusting, and filtering
###############################################################################
# Murska Sobota data
Murska_data <- read_xlsx(sheet = "Data MS", path = file.path(path_data, Murska_filename), trim_ws = TRUE) %>%
  dplyr::select(Sample_ID, Station_ID, Name, Year, Month, P, T, RH, δ18O, δ2H, d, "3H (TU)") %>%
  rename("3H" = "3H (TU)") %>% 
  mutate(Site = "Murska Sobota") %>%
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

Murska_data_monthly_summary <- Murska_data %>%
  group_by(Month) %>%
  summarise(weighted_mean_d2H = round(sum(δ2H * ((P_δ18O_δ2H/sum(P_δ18O_δ2H, na.rm = TRUE))), na.rm = TRUE), digits = 1),
            weighted_mean_d18O = round(sum(δ18O * ((P_δ18O_δ2H/sum(P_δ18O_δ2H, na.rm = TRUE))), na.rm = TRUE), digits = 2),
            weighted_mean_d = round(sum(d * ((P_δ18O_δ2H/sum(P_δ18O_δ2H, na.rm = TRUE))), na.rm = TRUE), digits = 1),
            weighted_mean_3H = round(sum(`3H` * ((P_3H/sum(P_3H, na.rm = TRUE))), na.rm = TRUE), digits = 1),
            mean_T = round(mean(T, na.rm = TRUE), digits = 1),
            mean_P = round(mean(P, na.rm = TRUE), digits = 0)) %>%
  left_join(read_xlsx(path = file.path(path_data, Hungary_meteo_filename), trim_ws = TRUE) %>%
              mutate(Month = month(date)) %>%
              group_by(Month) %>%
              summarise(mean_T_KO = round(mean(t, na.rm = TRUE), digits = 1),
                        sum_P_KO = round(mean(p, na.rm = TRUE), digits = 0)),
           by = "Month")

# Save monthly statistics into a .csv table
#write.csv(Murska_data_monthly_summary, file = file.path(path_tables, "Murska_monthly_statistics.csv"), row.names = FALSE)

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

Murska_data_seasonal_summary <- Murska_data %>%
  group_by(Season) %>%
  summarise(weighted_mean_d2H = round(sum(δ2H * ((P_δ18O_δ2H/sum(P_δ18O_δ2H, na.rm = TRUE))), na.rm = TRUE), digits = 1),
            weighted_mean_d18O = round(sum(δ18O * ((P_δ18O_δ2H/sum(P_δ18O_δ2H, na.rm = TRUE))), na.rm = TRUE), digits = 2),
            weighted_mean_d = round(sum(d * ((P_δ18O_δ2H/sum(P_δ18O_δ2H, na.rm = TRUE))), na.rm = TRUE), digits = 1),
            weighted_mean_3H = round(sum(`3H` * ((P_3H/sum(P_3H, na.rm = TRUE))), na.rm = TRUE), digits = 1),
            mean_T = round(mean(T, na.rm = TRUE), digits = 1)) %>%
  left_join(Murska_data %>%
              group_by(Year, Season) %>%
              summarise(sum_P = sum(P, na.rm = TRUE), .groups = "drop") %>%
              group_by(Season) %>%
              summarise(mean_sum_P = round(mean(sum_P, na.rm = TRUE), digits = 0), .groups = "drop"),
            by = "Season") %>%
  left_join(read_xlsx(path = file.path(path_data, Hungary_meteo_filename), trim_ws = TRUE) %>%
              mutate(Year = year(date),
                     Month = month(date)) %>%
              mutate(Season = case_when(
                Month %in% c(12, 1, 2)  ~ "winter",
                Month %in% c(3, 4, 5)  ~ "spring",
                Month %in% c(6, 7, 8)  ~ "summer",
                Month %in% c(9, 10, 11)  ~ "autumn",
                TRUE ~ NA)) %>%
              group_by(Season) %>%
              summarise(mean_T_KO = round(mean(t, na.rm = TRUE), digits = 1)),
            by = "Season") %>%
  left_join(read_xlsx(path = file.path(path_data, Hungary_meteo_filename), trim_ws = TRUE) %>%
              mutate(Year = year(date),
                     Month = month(date)) %>%
              mutate(Season = case_when(
                Month %in% c(12, 1, 2)  ~ "winter",
                Month %in% c(3, 4, 5)  ~ "spring",
                Month %in% c(6, 7, 8)  ~ "summer",
                Month %in% c(9, 10, 11)  ~ "autumn",
                TRUE ~ NA)) %>%
              group_by(Year, Season) %>%
              summarise(sum_P = sum(p, na.rm = TRUE), .groups = "drop") %>%
              group_by(Season) %>%
              summarise(mean_sum_P_KO = round(mean(sum_P, na.rm = TRUE), digits = 0), .groups = "drop"),
            by = "Season")

# Save seasonal statistics into a .csv table
#write.csv(Murska_data_seasonal_summary, file = file.path(path_tables, "Murska_seasonal_statistics.csv"), row.names = FALSE)

Murska_data_longer_year <- Murska_data %>%
  group_by(Year) %>%
  summarise(weighted_mean_δ18O = sum(δ18O * ((P_δ18O_δ2H/sum(P_δ18O_δ2H, na.rm = TRUE))), na.rm = TRUE),
            weighted_mean_δ2H = sum(δ2H * ((P_δ18O_δ2H/sum(P_δ18O_δ2H, na.rm = TRUE))), na.rm = TRUE),
            weighted_mean_d = sum(d * ((P_δ18O_δ2H/sum(P_δ18O_δ2H, na.rm = TRUE))), na.rm = TRUE),
            weighted_mean_3H = sum(`3H` * ((P_3H/sum(P_3H, na.rm = TRUE))), na.rm = TRUE),
            mean_T = mean(T),
            sum_P = sum(P)) %>%
  pivot_longer(cols = c(weighted_mean_δ18O, weighted_mean_δ2H, weighted_mean_d, weighted_mean_3H, mean_T, sum_P), names_to = "Variable", values_to = "Value") %>%
  pivot_wider(names_from = c("Variable"), values_from = "Value", names_sep = "_") 

Murska_data_yearly_summary <- Murska_data %>%
  group_by(Year) %>%
  summarise(weighted_mean_d2H = round(sum(δ2H * ((P_δ18O_δ2H/sum(P_δ18O_δ2H, na.rm = TRUE))), na.rm = TRUE), digits = 1),
            weighted_mean_d18O = round(sum(δ18O * ((P_δ18O_δ2H/sum(P_δ18O_δ2H, na.rm = TRUE))), na.rm = TRUE), digits = 2),
            weighted_mean_d = round(sum(d * ((P_δ18O_δ2H/sum(P_δ18O_δ2H, na.rm = TRUE))), na.rm = TRUE), digits = 1),
            weighted_mean_3H = round(sum(`3H` * ((P_3H/sum(P_3H, na.rm = TRUE))), na.rm = TRUE), digits = 1),
            mean_T = round(mean(T, na.rm = TRUE), digits = 1),
            sum_P = round(sum(P, na.rm = TRUE), digits = 0)) %>%
  left_join(read_xlsx(path = file.path(path_data, Hungary_meteo_filename), trim_ws = TRUE) %>%
          mutate(Year = year(date)) %>%
          group_by(Year) %>%
          summarise(mean_T_KO = round(mean(t, na.rm = TRUE), digits = 1),
                    sum_P_KO = round(sum(p, na.rm = TRUE), digits = 0)),
          by = "Year")

# Save yearly statistics into a .csv table
#write.csv(Murska_data_yearly_summary, file = file.path(path_tables, "Murska_yearly_statistics.csv"), row.names = FALSE)

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


Murska_data_period_summary <- Murska_data %>%
  summarise(weighted_mean_d2H = round(sum(δ2H * ((P_δ18O_δ2H/sum(P_δ18O_δ2H, na.rm = TRUE))), na.rm = TRUE), digits = 1),
            weighted_mean_d18O = round(sum(δ18O * ((P_δ18O_δ2H/sum(P_δ18O_δ2H, na.rm = TRUE))), na.rm = TRUE), digits = 2),
            weighted_mean_d = round(sum(d * ((P_δ18O_δ2H/sum(P_δ18O_δ2H, na.rm = TRUE))), na.rm = TRUE), digits = 1),
            weighted_mean_3H = round(sum(`3H` * ((P_3H/sum(P_3H, na.rm = TRUE))), na.rm = TRUE), digits = 1),
            mean_T = round(mean(T, na.rm = TRUE), digits = 1)) %>%
  cbind(Murska_data %>%
          group_by(Year) %>%
          summarise(sum_P = sum(P, na.rm = TRUE), .groups = "drop") %>%
          summarise(mean_sum_P = round(mean(sum_P, na.rm = TRUE), digits = 0), .groups = "drop")) %>%
  cbind(read_xlsx(path = file.path(path_data, Hungary_meteo_filename), trim_ws = TRUE) %>%
        summarise(mean_T_KO = round(mean(t, na.rm = TRUE), digits = 1))) %>%
  cbind(read_xlsx(path = file.path(path_data, Hungary_meteo_filename), trim_ws = TRUE) %>%
          mutate(Year = year(date)) %>%
          group_by(Year) %>%
          summarise(sum_P = sum(p, na.rm = TRUE), .groups = "drop") %>%
          summarise(mean_sum_P_KO = round(mean(sum_P, na.rm = TRUE), digits = 0), .groups = "drop"))

# Save whole period statistics into a .csv table
#write.csv(Murska_data_period_summary, file = file.path(path_tables, "Murska_period_statistics.csv"), row.names = FALSE)

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
            intercept_error_RMA_weighted = RMA_weighted(δ18O, δ2H, P)$errors_intercepts$RMA_weighted,
            EIV_fit = list(
              deming(δ2H ~ δ18O, ystd = rep(2, n()), xstd = rep(0.1, n()), jackknife = TRUE, conf = 0.95)
            )) %>%
  mutate(intercept_EIV = vapply(EIV_fit, \(fit) unname(fit$coefficient[1]), numeric(1)),
         slope_EIV = vapply(EIV_fit, \(fit) unname(fit$coefficient[2]), numeric(1)),
         intercept_error_EIV = vapply(EIV_fit, \(fit) sqrt(diag(fit$variance))[1], numeric(1)),
         slope_error_EIV = vapply(EIV_fit, \(fit) sqrt(diag(fit$variance))[2], numeric(1)),
         intercept_lower_0.95 = vapply(EIV_fit, \(fit) unname(fit$ci[1]), numeric(1)),
         intercept_higher_0.95 = vapply(EIV_fit, \(fit) unname(fit$ci[3]), numeric(1)),
         slope_lower_0.95 = vapply(EIV_fit, \(fit) unname(fit$ci[2]), numeric(1)),
         slope_higher_0.95 = vapply(EIV_fit, \(fit) unname(fit$ci[4]), numeric(1)),
         SSWR = vapply(EIV_fit, \(fit) fit$sigma^2 * (fit$n - 2), numeric(1))
         ) %>%
  mutate(across(where(is.numeric), ~round(., digits = 2))) %>%
  mutate(LMWL_RMA = paste0("d2H = (", slope_RMA, " ± ", slope_error_RMA, ") d18O + (", intercept_RMA, " ± ", intercept_error_RMA, ")"),
         LMWL_PWRMA = paste0("d2H = (", slope_RMA_weighted," ± ", slope_error_RMA_weighted, ") d18O + (", intercept_RMA_weighted,  " ± ", intercept_error_RMA_weighted, ")"),
         LMWL_EIV = sprintf("d2H = (%.2f ± %.2f) d18O + (%.2f ± %.2f)", slope_EIV, slope_error_EIV, intercept_EIV, intercept_error_EIV)) %>%
  dplyr::select(-EIV_fit)

# Save yearly RMA/MA regression statistics into a .csv table
#write.csv(Murska_data_year_slopes_2016_2024, file = file.path(path_tables, "Murska_yearly_regression_statistics.csv"), row.names = FALSE)

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
# Hungarian data together
Hungarian_data <- read_xlsx(sheet = "Data all", path = file.path(path_data, Murska_filename), trim_ws = TRUE) %>%
  dplyr::select(Name, Year, Month, P, T, δ18O, δ2H, d) %>%
  mutate(Season = case_when(
    Month %in% c(12, 1, 2)  ~ "winter",
    Month %in% c(3, 4, 5)  ~ "spring",
    Month %in% c(6, 7, 8)  ~ "summer",
    Month %in% c(9, 10, 11)  ~ "autumn",
    TRUE ~ NA)) %>%
  mutate(Season = factor(Season, levels = c("winter", "spring", "summer", "autumn"))) %>%
  mutate(Date = as.Date(make_date(Year, Month)))


Hungarian_data_longer <-  Hungarian_data %>%
  dplyr::filter(Name != "Murska Sobota - Rakičan") %>%
  pivot_longer(cols = c(δ18O, δ2H, d, T, P), names_to = "Variable", values_to = "Value") %>%
  mutate(Variable_color = Variable) %>%
  mutate(Variable = case_when(
    Variable == "δ18O" ~ "italic(delta)^18*O",
    Variable == "δ2H" ~ "italic(delta)^2*H",
    Variable == "d" ~ "d-excess",
    TRUE ~ Variable
  )) %>%
  mutate(Variable = factor(Variable, levels = c("italic(delta)^18*O", "italic(delta)^2*H", "d-excess", "T", "P")))

Hungarian_data_longer_sigma <- Hungarian_data %>%
  pivot_longer(cols = c("T", "P"), names_to = "Variable_size", values_to = "Value_size") %>%
  mutate(Variable_size = factor(Variable_size, levels = c("T", "P"))) %>%
  dplyr::filter(!(Name %in% c("Törökkoppány", "Tamási"))) %>%
  mutate(Name = case_when(
    Name == "Murska Sobota - Rakičan" ~ "Ledava watershed",
    TRUE ~ Name
  ))


Hungarian_data_period_summary <- Hungarian_data %>%
  group_by(Name) %>%
  summarise(weighted_mean_d2H = round(sum(δ2H * ((P/sum(P, na.rm = TRUE))), na.rm = TRUE), digits = 1),
            weighted_mean_d18O = round(sum(δ18O * ((P/sum(P, na.rm = TRUE))), na.rm = TRUE), digits = 2),
            weighted_mean_d = round(sum(d * ((P/sum(P, na.rm = TRUE))), na.rm = TRUE), digits = 1),
            mean_T = round(mean(T, na.rm = TRUE), digits = 1)) %>%
  left_join(Hungarian_data %>%
              group_by(Name, Year) %>%
              summarise(sum_P = sum(P, na.rm = TRUE), .groups = "drop") %>%
              group_by(Name) %>%
              summarise(mean_sum_P = round(mean(sum_P, na.rm = TRUE), digits = 0), .groups = "drop"),
            by = "Name")

Hungarian_data_longer_month <- Hungarian_data %>%
  dplyr::filter(!(Name %in% c("Murska Sobota - Rakičan", "Koppány watershed"))) %>%
  group_by(Month) %>%
  summarise(mean_T = mean(T, na.rm = TRUE),
            mean_P = mean(P, na.rm = TRUE),
            weighted_mean_δ18O = sum(δ18O * ((P/sum(P, na.rm = TRUE))), na.rm = TRUE),
            weighted_mean_δ2H = sum(δ2H * ((P/sum(P, na.rm = TRUE))), na.rm = TRUE),
            weighted_mean_d = sum(d * ((P/sum(P, na.rm = TRUE))), na.rm = TRUE)) %>%
  pivot_longer(cols = c(weighted_mean_δ18O, weighted_mean_δ2H, weighted_mean_d, mean_T, mean_P), names_to = "Variable", values_to = "Value") %>%
  mutate(Variable = case_when(
    grepl("_δ18O", Variable) ~ "italic(delta)^18*O",
    grepl("_δ2H", Variable) ~ "italic(delta)^2*H",
    grepl("_d", Variable) ~ "d-excess",
    grepl("_T", Variable) ~ "T",
    grepl("_P", Variable) ~ "P",
    TRUE ~ Variable)) %>%
  mutate(Variable = factor(Variable, levels = c("italic(delta)^18*O", "italic(delta)^2*H", "d-excess", "T", "P")))

Hungarian_data_longer_season <- Hungarian_data %>%
  dplyr::filter(!(Name %in% c("Murska Sobota - Rakičan", "Koppány watershed"))) %>%
  group_by(Season) %>%
  summarise(mean_T = mean(T, na.rm = TRUE),
            mean_P = mean(P, na.rm = TRUE),
            weighted_mean_δ18O = sum(δ18O * ((P/sum(P, na.rm = TRUE))), na.rm = TRUE),
            weighted_mean_δ2H = sum(δ2H * ((P/sum(P, na.rm = TRUE))), na.rm = TRUE),
            weighted_mean_d = sum(d * ((P/sum(P, na.rm = TRUE))), na.rm = TRUE)) %>%
  pivot_longer(cols = c(weighted_mean_δ18O, weighted_mean_δ2H, weighted_mean_d, mean_T, mean_P), names_to = "Variable", values_to = "Value") %>%
  mutate(Variable = case_when(
    grepl("_δ18O", Variable) ~ "italic(delta)^18*O",
    grepl("_δ2H", Variable) ~ "italic(delta)^2*H",
    grepl("_d", Variable) ~ "d-excess",
    grepl("_T", Variable) ~ "T",
    grepl("_P", Variable) ~ "P",
    TRUE ~ Variable)) %>%
  mutate(Variable = factor(Variable, levels = c("italic(delta)^18*O", "italic(delta)^2*H", "d-excess", "T", "P")))

Hungarian_data_month_statistics <- Hungarian_data %>%
  dplyr::filter(!(Name %in% c("Murska Sobota - Rakičan", "Koppány watershed"))) %>%
  mutate(P_weighting = P) %>%
  pivot_longer(cols = c(δ18O, δ2H, d, T, P), names_to = "Variable", values_to = "Value") %>%
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
    grepl("T", Variable) ~ "T",
    grepl("P", Variable) ~ "P",
    TRUE ~ Variable)) %>%
  mutate(Variable = factor(Variable, levels = c("italic(delta)^18*O", "italic(delta)^2*H", "d-excess", "T", "P")))

Hungarian_data_season_statistics <- Hungarian_data %>%
  dplyr::filter(!(Name %in% c("Murska Sobota - Rakičan", "Koppány watershed"))) %>%
  mutate(P_weighting = P) %>%
  pivot_longer(cols = c(δ18O, δ2H, d,  T, P), names_to = "Variable", values_to = "Value") %>%
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
    grepl("T", Variable) ~ "T",
    grepl("P", Variable) ~ "P",
    TRUE ~ Variable)) %>%
  mutate(Variable = factor(Variable, levels = c("italic(delta)^18*O", "italic(delta)^2*H", "d-excess", "T", "P")))

Hungarian_data_season_summary <- Hungarian_data %>%
  dplyr::filter(Name != "Tamási") %>%
  group_by(Name, Season) %>%
  summarise(weighted_mean_d2H = round(sum(δ2H * ((P/sum(P, na.rm = TRUE))), na.rm = TRUE), digits = 1),
            weighted_mean_d18O = round(sum(δ18O * ((P/sum(P, na.rm = TRUE))), na.rm = TRUE), digits = 2),
            weighted_mean_d = round(sum(d * ((P/sum(P, na.rm = TRUE))), na.rm = TRUE), digits = 1),
            mean_T = round(mean(T, na.rm = TRUE), digits = 1),
            sum_P = round(sum(P, na.rm = TRUE), digits = 0)) %>%
  rbind(Hungarian_data %>%
          group_by(Season) %>%
          summarise(weighted_mean_d2H = round(sum(δ2H * ((P/sum(P, na.rm = TRUE))), na.rm = TRUE), digits = 1),
                    weighted_mean_d18O = round(sum(δ18O * ((P/sum(P, na.rm = TRUE))), na.rm = TRUE), digits = 2),
                    weighted_mean_d = round(sum(d * ((P/sum(P, na.rm = TRUE))), na.rm = TRUE), digits = 1),
                    mean_T = round(mean(T, na.rm = TRUE), digits = 1),
                    sum_P = round(sum(P, na.rm = TRUE), digits = 0)) %>%
          mutate(Name = "All data"))

# Save seasonal statistics into a .csv table
write.csv(Hungarian_data_season_summary, file = file.path(path_tables, "Hungarian_seasonal_statistics.csv"), row.names = FALSE)

Hungarian_data_period_summary <- Hungarian_data %>%
  group_by(Name) %>%
  summarise(weighted_mean_d2H = round(sum(δ2H * ((P/sum(P, na.rm = TRUE))), na.rm = TRUE), digits = 1),
            weighted_mean_d18O = round(sum(δ18O * ((P/sum(P, na.rm = TRUE))), na.rm = TRUE), digits = 2),
            weighted_mean_d = round(sum(d * ((P/sum(P, na.rm = TRUE))), na.rm = TRUE), digits = 1),
            mean_T = round(mean(T, na.rm = TRUE), digits = 1),
            sum_P = round(sum(P, na.rm = TRUE), digits = 0)) %>%
  rbind(Hungarian_data %>%
          summarise(weighted_mean_d2H = round(sum(δ2H * ((P/sum(P, na.rm = TRUE))), na.rm = TRUE), digits = 1),
                    weighted_mean_d18O = round(sum(δ18O * ((P/sum(P, na.rm = TRUE))), na.rm = TRUE), digits = 2),
                    weighted_mean_d = round(sum(d * ((P/sum(P, na.rm = TRUE))), na.rm = TRUE), digits = 1),
                    mean_T = round(mean(T, na.rm = TRUE), digits = 1),
                    sum_P = round(sum(P, na.rm = TRUE), digits = 0)) %>%
          mutate(Name = "All data"))

# Save whole period statistics into a .csv table
write.csv(Hungarian_data_period_summary, file = file.path(path_tables, "Hungarian_period_statistics.csv"), row.names = FALSE)

Hungarian_data_slopes <- Hungarian_data %>%
  dplyr::filter(!is.na(δ18O) & !is.na(δ2H) & !is.na(P)) %>%
  group_by(Name) %>%
  summarise(slope_RMA = RMA_weighted(δ18O, δ2H, P)$slopes$RMA,
            slope_error_RMA =  RMA_weighted(δ18O, δ2H, P)$errors_slopes$RMA,
            slope_RMA_weighted = RMA_weighted(δ18O, δ2H, P)$slopes$RMA_weighted,
            slope_error_RMA_weighted = RMA_weighted(δ18O, δ2H, P)$errors_slopes$RMA_weighted,
            intercept_RMA = RMA_weighted(δ18O, δ2H, P)$intercepts$RMA,
            intercept_error_RMA = RMA_weighted(δ18O, δ2H, P)$errors_intercepts$RMA,
            intercept_RMA_weighted = RMA_weighted(δ18O, δ2H, P)$intercepts$RMA_weighted,
            intercept_error_RMA_weighted = RMA_weighted(δ18O, δ2H, P)$errors_intercepts$RMA_weighted) %>%
  rbind(Hungarian_data %>%
          dplyr::filter(!is.na(δ18O) & !is.na(δ2H) & !is.na(P)) %>%
          dplyr::filter(Name != "Koppány watershed") %>%
          summarise(slope_RMA = RMA_weighted(δ18O, δ2H, P)$slopes$RMA,
                    slope_error_RMA =  RMA_weighted(δ18O, δ2H, P)$errors_slopes$RMA,
                    slope_RMA_weighted = RMA_weighted(δ18O, δ2H, P)$slopes$RMA_weighted,
                    slope_error_RMA_weighted = RMA_weighted(δ18O, δ2H, P)$errors_slopes$RMA_weighted,
                    intercept_RMA = RMA_weighted(δ18O, δ2H, P)$intercepts$RMA,
                    intercept_error_RMA = RMA_weighted(δ18O, δ2H, P)$errors_intercepts$RMA,
                    intercept_RMA_weighted = RMA_weighted(δ18O, δ2H, P)$intercepts$RMA_weighted,
                    intercept_error_RMA_weighted = RMA_weighted(δ18O, δ2H, P)$errors_intercepts$RMA_weighted) %>%
          mutate(Name = "All data")) %>%
  mutate(across(where(is.numeric), ~round(., digits = 2))) %>%
  mutate(LMWL_RMA = paste0("d2H = (", slope_RMA, " ± ", slope_error_RMA, ") d18O + (", intercept_RMA, " ± ", intercept_error_RMA, ")"),
         LMWL_PWRMA = paste0("d2H = (", slope_RMA_weighted," ± ", slope_error_RMA_weighted, ") d18O + (", intercept_RMA_weighted,  " ± ", intercept_error_RMA_weighted, ")"))

# Save LMWL regression statistics into a .csv table
write.csv(dplyr::select(Hungarian_data_slopes, Name, LMWL_RMA, LMWL_PWRMA), file = file.path(path_tables, "Hungarian_regression_statistics.csv"), row.names = FALSE)

RMA_slope_MS <- Hungarian_data_slopes$slope_RMA[Hungarian_data_slopes$Name == "Murska Sobota - Rakičan"]
RMA_error_MS <- Hungarian_data_slopes$slope_error_RMA[Hungarian_data_slopes$Name == "Murska Sobota - Rakičan"]
RMA_slope_T <- Hungarian_data_slopes$slope_RMA[Hungarian_data_slopes$Name == "Tamási"]
RMA_error_T <- Hungarian_data_slopes$slope_error_RMA[Hungarian_data_slopes$Name == "Tamási"]
RMA_slope_KO <- Hungarian_data_slopes$slope_RMA[Hungarian_data_slopes$Name == "Törökkoppány"]
RMA_error_KO <- Hungarian_data_slopes$slope_error_RMA[Hungarian_data_slopes$Name == "Törökkoppány"]
RMA_slope_T_KO <- Hungarian_data_slopes$slope_RMA[Hungarian_data_slopes$Name == "Koppány watershed"]
RMA_error_T_KO <- Hungarian_data_slopes$slope_error_RMA[Hungarian_data_slopes$Name == "Koppány watershed"]

# Comparison 1: Tamási vs Murska Sobota - Rakičan
t_stat_T_MS <- (RMA_slope_T - RMA_slope_MS) / sqrt(RMA_error_T^2 + RMA_error_MS^2)

# Comparison 2: Törökkoppány vs Murska Sobota - Rakičan
t_stat_KO_MS <- (RMA_slope_KO - RMA_slope_MS) / sqrt(RMA_error_KO^2 + RMA_error_MS^2)

# Comparison 3: Törökkoppány & Tamási vs Murska Sobota - Rakičan
t_stat_T_KO_MS <- (RMA_slope_T_KO - RMA_slope_MS) / sqrt(RMA_error_T_KO^2 + RMA_error_MS^2)

# Comparison 4: Tamási vs Törökkoppány
t_stat_T_KO <- (RMA_slope_T - RMA_slope_KO) / sqrt(RMA_error_T^2 + RMA_error_KO^2)

# Comparison 5: Tamási vs Törökkoppány & Tamási
t_stat_T_T_KO <- (RMA_slope_T - RMA_slope_T_KO) / sqrt(RMA_error_T^2 + RMA_error_T_KO^2)

# Comparison 6: Törökkoppány vs Törökkoppány & Tamási
t_stat_KO_T_KO <- (RMA_slope_KO - RMA_slope_T_KO) / sqrt(RMA_error_KO^2 + RMA_error_T_KO^2)

# Print the t-statistics for each pair
cat("t-statistic for Tamási vs Murska Sobota - Rakičan: ", t_stat_T_MS, "\n")
cat("t-statistic for Törökkoppány vs Murska Sobota - Rakičan: ", t_stat_KO_MS, "\n")
cat("t-statistic for Törökkoppány & Tamási vs Murska Sobota - Rakičan: ", t_stat_T_KO_MS, "\n")
cat("t-statistic for Tamási vs Törökkoppány: ", t_stat_T_KO, "\n")
cat("t-statistic for Tamási vs Törökkoppány & Tamási: ", t_stat_T_T_KO, "\n")
cat("t-statistic for Törökkoppány vs Törökkoppány & Tamási: ", t_stat_KO_T_KO, "\n")

# Graz data
Graz_data <- read_xlsx(sheet = "Data Graz", path = file.path(path_data, Graz_filename), trim_ws = TRUE) %>%
  dplyr::select(Station_ID, Year, Month, P, T, RH, δ18O, δ2H, d) %>%
  mutate(Site = "Graz") %>%
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
  mutate(Date = as.Date(make_date(Year, Month)))

Graz_data_year_slopes_2016_2024 <- Graz_data %>%
  dplyr::filter(!is.na(δ18O) & !is.na(δ2H) & !is.na(P)) %>%
  summarise(slope_RMA = RMA_weighted(δ18O, δ2H, P)$slopes$RMA,
            slope_error_RMA =  RMA_weighted(δ18O, δ2H, P)$errors_slopes$RMA,
            slope_RMA_weighted = RMA_weighted(δ18O, δ2H, P)$slopes$RMA_weighted,
            slope_error_RMA_weighted = RMA_weighted(δ18O, δ2H, P)$errors_slopes$RMA_weighted,
            intercept_RMA = RMA_weighted(δ18O, δ2H, P)$intercepts$RMA,
            intercept_error_RMA = RMA_weighted(δ18O, δ2H, P)$errors_intercepts$RMA,
            intercept_RMA_weighted = RMA_weighted(δ18O, δ2H, P)$intercepts$RMA_weighted,
            intercept_error_RMA_weighted = RMA_weighted(δ18O, δ2H, P)$errors_intercepts$RMA_weighted) %>%
  mutate(across(where(is.numeric), ~round(., digits = 2))) %>%
  mutate(LMWL_RMA = paste0("d2H = (", slope_RMA, " ± ", slope_error_RMA, ") d18O + (", intercept_RMA, " ± ", intercept_error_RMA, ")"),
         LMWL_PWRMA = paste0("d2H = (", slope_RMA_weighted," ± ", slope_error_RMA_weighted, ") d18O + (", intercept_RMA_weighted,  " ± ", intercept_error_RMA_weighted, ")"))

# Hrascica data
Hrascica_data <- read_xlsx( path = file.path(path_data, Hrascica_filename), trim_ws = TRUE) %>%
  dplyr::select(Year, Month, Precipitation, `Air Temperature`, O18, H2) %>%
  rename("P" = Precipitation, "T" = `Air Temperature`, δ18O = O18, δ2H = H2) %>%
  mutate(Site = "Hrascica",
         d = δ2H - 8 * δ18O) %>%
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
  mutate(Date = as.Date(make_date(Year, Month)))

Hrascica_data_year_slopes_2016_2024 <- Hrascica_data %>%
  dplyr::filter(!is.na(δ18O) & !is.na(δ2H) & !is.na(P)) %>%
  summarise(slope_RMA = RMA_weighted(δ18O, δ2H, P)$slopes$RMA,
            slope_error_RMA =  RMA_weighted(δ18O, δ2H, P)$errors_slopes$RMA,
            slope_RMA_weighted = RMA_weighted(δ18O, δ2H, P)$slopes$RMA_weighted,
            slope_error_RMA_weighted = RMA_weighted(δ18O, δ2H, P)$errors_slopes$RMA_weighted,
            intercept_RMA = RMA_weighted(δ18O, δ2H, P)$intercepts$RMA,
            intercept_error_RMA = RMA_weighted(δ18O, δ2H, P)$errors_intercepts$RMA,
            intercept_RMA_weighted = RMA_weighted(δ18O, δ2H, P)$intercepts$RMA_weighted,
            intercept_error_RMA_weighted = RMA_weighted(δ18O, δ2H, P)$errors_intercepts$RMA_weighted) %>%
  mutate(across(where(is.numeric), ~round(., digits = 2))) %>%
  mutate(LMWL_RMA = paste0("d2H = (", slope_RMA, " ± ", slope_error_RMA, ") d18O + (", intercept_RMA, " ± ", intercept_error_RMA, ")"),
         LMWL_PWRMA = paste0("d2H = (", slope_RMA_weighted," ± ", slope_error_RMA_weighted, ") d18O + (", intercept_RMA_weighted,  " ± ", intercept_error_RMA_weighted, ")"))


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
  Station_name = c("Murska"),
  lon = c(16.191278),
  lat = c(46.652078)
) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

ggplot() +
  geom_sf(data = shapefile_border_SLO, fill = NA) +
  geom_sf(data = dplyr::filter(shapefile_border_SLO, sov_a3 == "SVN"), fill = "grey85") +
  geom_text(data = shapefile_names_SLO, aes(x = st_coordinates(geometry)[,1], 
                                            y = st_coordinates(geometry)[,2], 
                                            label = Station_name),
            size = 1.8) +
  geom_sf(data = dplyr::filter(stations_sf, Station_name == "Murska"), fill = "black", size = 0.6, shape = 21, stroke = 0.4) +
  geom_text(data = dplyr::filter(stations_sf, Station_name == "Murska"), aes(x = st_coordinates(geometry)[,1],
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
ggplot(data = dplyr::filter(Murska_data_longer, !(Variable %in% c("T", "P", "RH"))), aes(x = Date, y = Value)) +
  geom_line(linewidth = 0.3, na.rm = TRUE) +
  geom_text(data = dplyr::filter(Murska_data_period_statistics, !(Variable %in% c("T", "P", "RH"))),
            aes(x = as.Date("2025-01-01"), y = Max, label = paste0("n = ", n)), # Use y = 0 or another fixed value to position text inside plot
            inherit.aes = FALSE, vjust = 1, hjust = 1, size = 3.2, na.rm = TRUE) +
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
# Murska Sobota
# Monthly plot
ggplot(data = Murska_data_longer, aes(x = Month, y = Value, group = Month)) +
  geom_point(data = Murska_data_longer_month,
             aes(x = Month, y = Value, group = Month), color = "black", size = 0.7, na.rm = TRUE) +
  geom_violin(data = dplyr::filter(Murska_data_longer, Variable != "P"),
              trim = FALSE, scale = "area", alpha = 0.5, linewidth = 0.3, na.rm = TRUE) +  # Trim = FALSE to avoid cutting off the tails
  geom_violin(data = dplyr::filter(Murska_data_longer, Variable == "P"),
              trim = FALSE, bounds = c(0, Inf), scale = "area", alpha = 0.5, linewidth = 0.3, na.rm = TRUE) +
  geom_text(data = Murska_data_month_statistics,
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

ggsave(filename = paste("Murska_monthly_violin",".",figure_type, sep = ""), path = path_figures, device = figure_type, dpi = 1500, width = 95, height = 180, units = "mm")

# Seasonal plot
ggplot(data = Murska_data_longer,
       aes(x = Season, y = Value, group = Season)) +
  geom_point(data = Murska_data_longer_season,
             aes(x = Season, y = Value, group = Season), color = "black", size = 0.7, na.rm = TRUE) +
  geom_violin(data = dplyr::filter(Murska_data_longer, Variable != "P"),
              trim = FALSE, scale = "area", alpha = 0.5, linewidth = 0.3, na.rm = TRUE) +  # Trim = FALSE to avoid cutting off the tails
  geom_violin(data = dplyr::filter(Murska_data_longer, Variable == "P"),
              trim = FALSE, bounds = c(0, Inf), scale = "area", alpha = 0.5, linewidth = 0.3, na.rm = TRUE) +
  geom_text(data = Murska_data_season_statistics,
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

ggsave(filename = paste("Murska_seasonal_violin",".",figure_type, sep = ""), path = path_figures, device = figure_type, dpi = 1500, width = 70, height = 180, units = "mm")

# Hungary
# Monthly plot
ggplot(data = dplyr::filter(Hungarian_data_longer, Name != "Koppány watershed"),
       aes(x = Month, y = Value, group = Month)) +
  geom_point(data = Hungarian_data_longer_month,
             aes(x = Month, y = Value, group = Month), color = "black", size = 0.7, na.rm = TRUE) +
  geom_violin(trim = FALSE, scale = "area", alpha = 0.5, linewidth = 0.3, na.rm = TRUE) +  # Trim = FALSE to avoid cutting off the tails
  geom_text(data = Hungarian_data_month_statistics,
            aes(x = Month, y = Min, label = n), # Use y = 0 or another fixed value to position text inside plot
            inherit.aes = FALSE, vjust = 2.2, size = 3, check_overlap = TRUE) +
  labs(y = expression("\u03B4"^"18"*"O, \u03B4"^"2"*"H, and d-excess [\u2030]; T [°C], P [mm]")) +
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

ggsave(filename = paste("Hungarian_monthly_violin",".",figure_type, sep = ""), path = path_figures, device = figure_type, dpi = 1500, width = 95, height = 180, units = "mm")

# Seasonal plot
ggplot(data = dplyr::filter(Hungarian_data_longer, Name != "Koppány watershed"),
       aes(x = Season, y = Value, group = Season)) +
  geom_point(data = Hungarian_data_longer_season,
             aes(x = Season, y = Value, group = Season), color = "black", size = 0.7, na.rm = TRUE) +
  geom_violin(data = dplyr::filter(Hungarian_data_longer, Variable != "P"),
              trim = FALSE, scale = "area", alpha = 0.5, linewidth = 0.3, na.rm = TRUE) +  # Trim = FALSE to avoid cutting off the tails
  geom_violin(data = dplyr::filter(Hungarian_data_longer, Variable == "P"),
              trim = FALSE, bounds = c(0, Inf), scale = "area", alpha = 0.5, linewidth = 0.3, na.rm = TRUE) +  # Trim = FALSE to avoid cutting off the tails
  geom_text(data = Hungarian_data_season_statistics,
            aes(x = Season, y = Min, label = n), # Use y = 0 or another fixed value to position text inside plot
            inherit.aes = FALSE, vjust = 4, size = 3, check_overlap = TRUE) +
  scale_y_continuous(labels = ~sub("-", "\u2212", .x), expand = expansion(mult = c(0.5, 0.1))) +
  labs(y = expression("\u03B4"^"18"*"O, \u03B4"^"2"*"H, and d-excess [\u2030]; T [°C], P [mm]")) +
  facet_wrap(~ Variable, scales = "free_y", nrow = 2, labeller = label_parsed) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, colour = "black"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 8, colour = "black"),
        axis.title.y = element_text(size = 8, colour = "black"),
        strip.text = element_text(size = 8, colour = "black", margin = margin(0.02, 0, 0.02, 0, unit = "cm")),
        panel.grid.minor = element_blank(),
        legend.position = "none")

ggsave(filename = paste("Hungarian_seasonal_violin",".",figure_type, sep = ""), path = path_figures, device = figure_type, dpi = 1500, width = 180, height = 90, units = "mm")

###############################################################################
# FIGURE 5
###############################################################################
# Sigma plot & T
ggplot(data = dplyr::filter(Murska_data_longer_sigma, Variable_size == "T" & Value_size > 0),
       aes(x = δ18O, y = δ2H, size =  Value_size)) +
  geom_point(fill = "grey", color = "black", shape = 21, na.rm = TRUE) +
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
  geom_point(fill = "grey", color = "black", shape = 21, na.rm = TRUE) +
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

# Sigma plot HUNGARIAN
Hungarian_data_with_all <- dplyr::bind_rows(Hungarian_data,
                                            data.frame(δ18O = NA, δ2H = NA, Name = "All data",
                                                       shape = NA, color = "black"))

ggplot() +
  geom_abline(slope = 8, intercept = 10, color = "black", linewidth = 0.3) +
  geom_abline(data = dplyr::filter(Hungarian_data_slopes, !(Name %in%  c("Törökkoppány", "Tamási"))),
              aes(slope = slope_RMA_weighted, intercept = intercept_RMA_weighted, color = Name),
              linewidth = 0.3) +
  geom_point(data = dplyr::filter(Hungarian_data, !(Name %in%  c("Törökkoppány", "Tamási"))),
             aes(x = δ18O, y = δ2H, shape = Name, color = Name),
             size = 1.2, na.rm = TRUE) +
  geom_point(data = dplyr::filter(Hungarian_data_with_all, Name == "All data"),
             aes(x = δ18O, y = δ2H, shape = Name, color = Name),
             size = 1.2, na.rm = TRUE, show.legend = FALSE) +
  geom_text(data = dplyr::filter(Hungarian_data_slopes, !(Name %in%  c("Törökkoppány", "Tamási"))),
            aes(x = c(-10, -10, -10), y = c(-10, -17, -24), label = LMWL_PWRMA, color = Name),
            size = 2.5, show.legend = FALSE) +
  scale_shape_manual(values = c(3, 16, 17)) +
  scale_x_continuous(labels = ~sub("-", "\u2212", .x)) +
  scale_y_continuous(labels = ~sub("-", "\u2212", .x)) +
  labs(y = expression("\u03B4"^"2"*"H [\u2030]"),
       x = expression("\u03B4"^"18"*"O [\u2030]")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 7, colour = "black"),
        axis.title.x = element_text(size = 7, colour = "black"),
        axis.text.y = element_text(size = 7, colour = "black"),
        axis.title.y = element_text(size = 7, colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 6, colour = "black"),
        legend.position = "inside",
        legend.position.inside = c(0.80, 0.2),
        legend.key.spacing.y = unit(1, "mm"),
        legend.spacing = unit(0, "mm"),
        legend.key.size = unit(2, "mm"),
        legend.background = element_blank(),
        panel.grid.minor = element_blank())

ggsave(filename = paste("Hungarian_sigma_plot",".",figure_type, sep = ""), path = path_figures, device = figure_type, dpi = 1500, width = 90, height = 90, units = "mm")

# Sigma plot & T HUNGARIAN
ggplot(data = dplyr::filter(Hungarian_data_longer_sigma, Variable_size == "T" & Value_size > 0),
       aes(x = δ18O, y = δ2H, size =  Value_size)) +
  geom_point(aes(shape = Name), fill = "grey", color = "black", na.rm = TRUE) +
  geom_abline(data = dplyr::filter(Hungarian_data_slopes %>% mutate(Name = ifelse(Name == "Murska Sobota - Rakičan", "Ledava watershed", Name)),
                                   !(Name %in%  c("Törökkoppány", "Tamási", "All data"))),
              aes(slope = slope_RMA_weighted, intercept = intercept_RMA_weighted, linetype = Name),
              linewidth = 0.3) +
  scale_shape_manual(values = c(21,1)) +
  scale_x_continuous(labels = ~sub("-", "\u2212", .x)) +
  scale_y_continuous(labels = ~sub("-", "\u2212", .x)) +
  scale_radius() +  # breaks = scales::breaks_extended(n = 8)
  labs(y = expression("\u03B4"^"2"*"H [\u2030]"),
       x = expression("\u03B4"^"18"*"O [\u2030]"),
       size = "Temperature [°C]",
       shape = "",
       linetype = "") +
  guides(shape = guide_legend(position = "top"),
         linetype = guide_legend(position = "top")) +
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

ggsave(filename = paste("Hungarian_sigma_plot_T",".",figure_type, sep = ""), path = path_figures, device = figure_type, dpi = 1500, width = 80, height = 85, units = "mm")

# Sigma plot & P HUNGARIAN
ggplot(data = dplyr::filter(Hungarian_data_longer_sigma, Variable_size == "P" & Value_size > 0),
       aes(x = δ18O, y = δ2H, size =  Value_size)) +
  geom_point(aes(shape = Name), fill = "grey", color = "black", na.rm = TRUE) +
  geom_abline(data = dplyr::filter(Hungarian_data_slopes %>% mutate(Name = ifelse(Name == "Murska Sobota - Rakičan", "Ledava watershed", Name)),
                                   !(Name %in%  c("Törökkoppány", "Tamási", "All data"))),
              aes(slope = slope_RMA_weighted, intercept = intercept_RMA_weighted, linetype = Name),
              linewidth = 0.3) +
  scale_shape_manual(values = c(21,1)) +
  scale_x_continuous(labels = ~sub("-", "\u2212", .x)) +
  scale_y_continuous(labels = ~sub("-", "\u2212", .x)) +
  scale_radius() +  # breaks = scales::breaks_extended(n = 8)
  labs(y = expression("\u03B4"^"2"*"H [\u2030]"),
       x = expression("\u03B4"^"18"*"O [\u2030]"),
       size = "Precipitation [mm]",
       shape = "",
       linetype = "") +
  guides(shape = guide_legend(position = "top"),
         linetype = guide_legend(position = "top")) +
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

ggsave(filename = paste("Hungarian_sigma_plot_P",".",figure_type, sep = ""), path = path_figures, device = figure_type, dpi = 1500, width = 80, height = 85, units = "mm")

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
