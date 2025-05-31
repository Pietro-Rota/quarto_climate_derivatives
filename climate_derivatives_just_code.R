
#* Weather Derivatives Temperature Options ----

## setup ----
source("C:/Users/pietr/OneDrive/Desktop/formula.main.R")
Sys.setlocale("LC_TIME", "English") # set output language in English
theme_set(theme_minimal())
knitr::opts_chunk$set(fig.align = 'center')
library(forecast)
library(nlstools)

# conflicted::conflicts_prefer(dpylr::select)
# conflicted::conflicts_prefer(dplyr::filter)
# conflicted::conflicts_prefer(e1071::skewness)
# conflicted::conflicts_prefer(e1071::kurtosis)
# conflicted::conflicts_prefer(plotly::layout)

select <- dplyr::select
filter <- dplyr::filter
skewness <- e1071::skewness
kurtosis <- e1071::kurtosis
layout <- plotly::layout

## session_info ----
dff <- sessionInfo()
dff$R.version$version.string
dff$platform
rm(dff)

## functions_loaded ----
file <- "C:\\Users\\pietr\\OneDrive\\Desktop\\angolo in alto a destra\\quarto_climate_derivatives\\quarto_climate_derivatives.qmd"
when_rendering(functions_loaded(file))

## required_packages ----
when_rendering(required_packages(file))

## required_functions ----
when_rendering(required_functions(file))

## github file ----
extract_r_code(file, "climate_derivatives_just_code.R")

## time info ----
cat("time of creation", "\n")
print(file.info(file)$ctime, "\n")
cat("LAST MODIFICATION", "\n")
print(file.info(file)$mtime, "\n")
cat("Last Access", "\n")
print(file.info(file)$mtime, "\n")

## MAP ----
library(leaflet, quietly = TRUE, warn.conflicts = FALSE)
map_data <- data.frame(
  name = "Location",
  lat = 28.3688,
  lon = -81.5614
)
# Create a leaflet map
if (knitr::is_html_output()) {
  leaflet(map_data) %>%
    addTiles() %>% # Add default OpenStreetMap tiles
    addMarkers(~lon, ~lat, label = ~name, ) %>%
    addCircleMarkers(~lon, ~lat, radius = 10, color = "red", fillOpacity = 0.8) %>%
    setView(lng = map_data$lon, lat = map_data$lat, zoom = 9)
} else {
  print("interactive map of disneyworld in the html version")
}

## Dataset download ----
ORIGINAL_DATASET <- read.csv("DISNEY DATA.csv", skip = 10)

DATASET <- ORIGINAL_DATASET %>% 
  mutate(T_MAX=remove_outliers(T2M_MAX, fill = "NA") %>% na.approx) %>% 
  mutate(T_MIN=remove_outliers(T2M_MIN, fill = "NA") %>% na.approx) %>% 
  mutate(DAY = as.Date(ORIGINAL_DATASET$DOY - 1, origin = paste0(ORIGINAL_DATASET$YEAR, "-01-01"))) %>%
  mutate(Month=month(DAY)) %>% 
  dplyr::select(DAY, YEAR, Month, DOY, T_MAX, T_MIN)
  

DATASET$T_AVG <- apply(DATASET[c("T_MAX", "T_MIN")], 1, mean)

ORIGINAL_DATASET[ifelse(find_outliers(ORIGINAL_DATASET$T2M_MAX)==0,FALSE,TRUE),] %>% 
  group_by(YEAR) %>% 
  summarise("Days" = length(DOY)) %>% 
  ggplot( aes(x = YEAR, y = Days))+
    geom_bar(stat = "identity") + 
    labs(x= NULL, y =NULL, title = "Days where the sensor malfunctioned", 
        subtitle = "Identified by remove outliers")

if (knitr::is_html_output()) {
DATASET %>% select(DAY, T_MAX, T_MIN, T_AVG) %>% 
  smart_round(digits = 3) %>% 
  datatable() %>% 
  formatStyle("T_MAX", 
  background = styleColorBar(range(DATASET$T_MAX), "indianred3"),
  backgroundSize = "100% 80%", 
  backgroundRepeat = "no-repeat"
  )
  } else {
  print("interactive table of the data in the html version")
}

#* Initial data visualization ----

## Initial Viz ----
cleandataset <- DATASET %>% 
  select(T_MAX, T_MIN, T_AVG) %>% 
  xts(order.by = DATASET$DAY)

desc_df(cleandataset)

plot1 <- cleandataset %>% 
  as.data.frame() %>% 
  mutate(year = index(cleandataset)) %>% 
  ggplot(aes(x=year))+
  geom_line(aes(y=T_MAX, color = "T_MAX"))+
  geom_line(aes(y=T_MIN, color = "T_MIN"))+
  geom_line(aes(y=T_AVG, color = "T_AVG"))+
  labs(title = "Last 43 years of recorded data", y="Temperature", x=NULL)+
  scale_color_manual(name = "Temps", 
                    values = c(T_MAX = "indianred3",T_MIN = "lightblue",T_AVG = "lightgreen"))

plot1

## last yrs ----
NDAYS <- nrow(cleandataset)
lookback <- 365*10

tail(cleandataset, lookback) %>% 
  as.data.frame() %>% 
  mutate(year = index(tail(cleandataset, lookback))) %>% 
  ggplot(aes(x = year))+
  geom_line(aes(y=T_MAX, color = "T_MAX"))+
  geom_line(aes(y=T_MIN, color = "T_MIN"))+
  geom_line(aes(y=T_AVG, color = "T_AVG"))+
  labs(title = "Last 10 years of recorded data", y="Temperature", x=NULL)+
  scale_color_manual(name = "Temps", 
                    values = c(T_MAX = "indianred3",T_MIN = "lightblue",T_AVG = "lightgreen"))

## gganimate ----
library(gganimate)

months365 <- c()  # initialize empty vector

for (i in month.abb) {
  months365 <- c(months365, i, rep(" ", 4))
}
monthplot <- months365[-52][-47][-40][-33][-22][-13][-8]

plot_data <- cleandataset %>%
  as.data.frame() %>%
  mutate(
    date = index(cleandataset),
    year = year(date),
    month = month(date),
    week = week(date)
  ) %>%
  group_by(year, week) %>%
  summarise(T_AVG = mean(T_AVG), .groups = 'drop') %>%
  arrange(year, week)

if (knitr::is_html_output()) {
funny_plot <- ggplot(plot_data, aes(x = factor(week), y = T_AVG, group = year, color = T_AVG)) +
  geom_line(linewidth = 1.2, show.legend = FALSE) +
  coord_polar() +
  scale_x_discrete(breaks = 1:53, labels = monthplot) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(subtitle = "Year: {frame_time}", x = NULL, y = NULL) +
  transition_reveal(year)

animate(funny_plot, fps = 30, duration = 15, end_pause =120)
} else {
print("animated plot in html")
}

## unnamed chunk ----
library(tidyverse)

pivot_df <- DATASET %>%
  mutate(t_diff = c(NA, diff(T_AVG))) %>% 
  select(Month, YEAR, t_diff) %>%
  pivot_wider(names_from = YEAR, values_from = t_diff, values_fn = median) %>% 
  mutate(Month = month.abb) %>% 
  melt(id.vars = "Month", variable.name = "Year") %>% 
  arrange(Year) %>% 
  drop_na() %>% 
  mutate(Year = as.integer(Year)+1980)
  
last_dec <- pivot_df %>%
  filter(Month == "Dec") %>% 
  mutate(Year = Year - 1,
  Month = "last_Dec")

next_jan <- pivot_df %>%
  filter(Month == "Jan") %>% 
  mutate(Year = Year + 1,
  Month = "Next_jan")

t_data <- bind_rows(pivot_df, next_jan) %>%
  mutate(Month = factor(Month, levels = c(month.abb, "next_Jan")),
         Month_number = as.numeric(Month)) %>% drop_na()

annotation <- t_data %>%
  slice_max(Year-1) %>%
  slice_max(Month_number)

temp_lines <- tibble(
  x = 12,
  y = c(1.5, 2.0),
  labels = c("1.5\u00B0C", "2.0\u00B0C")
)

month_labels <- tibble(
  x = 1:12,
  labels = month.abb,
  y = 2.7
)

t_data %>% 
  ggplot(aes(x=Month_number, y=value, group=Year, color=Year)) +
  geom_hline(yintercept = c(1, 2.0), color="red") +
  geom_line() +
  scale_x_continuous(breaks=1:12,
                    labels=month.abb, expand = c(0,0),
                    sec.axis = dup_axis(name = NULL, labels=NULL)) +
  scale_y_continuous(breaks = seq(-2, 2, 0.2),
                    limits = c(-2, 2.7), expand = c(0, -0.7), 
                    sec.axis = dup_axis(name = NULL, labels=NULL)) + 
  coord_polar(start = 2*pi/12) +
  labs(x = NULL,
      y = NULL,
      title = "Global temperature change (1981-2025)")


#* Seasonal analysis ----

## Distribution of season ----
DATASET_seas <- DATASET %>%
  group_by(Month < 4|Month>9) %>%
  rename("Season"="Month < 4 | Month > 9") %>%
  mutate(Season = ifelse(Season, "Winter", "Summer"))
  
DATASET_seas[-1] %>% 
  xts(order.by = DATASET$DAY)%>% 
  show_df() %>% 
  gt() %>%
  tab_header(title = "Temperature Overview") %>%
  opt_stylize(style = 5, add_row_striping = TRUE) %>%
  cols_align(align = "center") %>%
  sub_missing(columns = everything(), missing_text = "⋮")


winter_dataset <- DATASET_seas[DATASET_seas$Season=="Winter",]
summer_dataset <- DATASET_seas[DATASET_seas$Season=="Summer",]

cat(paste0("The difference in number of rows is approximately: ", round(nrow(summer_dataset) / nrow(winter_dataset) - 1, 4)*100,"%", ", or ", round((nrow(summer_dataset) / nrow(winter_dataset) - 1)*NDAYS, 1), " days"))

grid.arrange(ncol=2,
ggplot(winter_dataset)+
  geom_histogram(aes(x = T_MAX, fill = "T_MAX"), alpha=0.8, bins = 80)+
  geom_histogram(aes(x = T_MIN, fill = "T_MIN"), alpha=0.8, bins = 80)+
  geom_histogram(aes(x = T_AVG, fill = "T_AVG"), alpha=0.8, bins = 80)+
  labs(title = "Distribution charts of winter",x=NULL)+
  scale_fill_manual(name = NULL, 
                    values = c(T_MAX = "indianred3",T_MIN = "lightblue",T_AVG = "lightgreen"))+
  theme(legend.position = "bottom")

,

ggplot(summer_dataset)+
  geom_histogram(aes(x = T_MAX, fill = "T_MAX"), alpha=0.8, bins = 80)+
  geom_histogram(aes(x = T_MIN, fill = "T_MIN"), alpha=0.8, bins = 80)+
  geom_histogram(aes(x = T_AVG, fill = "T_AVG"), alpha=0.8, bins = 80)+
  labs(title = "Distribution charts of summer",x=NULL)+
  scale_fill_manual(name = NULL, 
                    values = c(T_MAX = "indianred3",T_MIN = "lightblue",T_AVG = "lightgreen"))+
  theme(legend.position = "bottom")

)

## SUM vs WIN ----
ggplot()+
  geom_histogram(data = winter_dataset, aes(x = T_AVG, fill = "Winter"), alpha=0.8, bins = 80)+
  geom_histogram(data = summer_dataset, aes(x = T_AVG, fill = "Summer"), alpha=0.8, bins = 80)+
  labs(title = "Distribution charts of the 2 averages",x=NULL)+
    scale_fill_manual(name = NULL, 
                    values = c(Winter="steelblue", Summer="orange"))+
  theme(legend.position = "bottom")

## hottest/coldest months ----
DATASET_seas %>% 
  group_by(Month) %>% 
  summarise(T_min_min = min(T_MIN),
            T_min_max = max(T_MIN),
            T_max_min = min(T_MAX),
            T_max_max = max(T_MAX)) %>% 
  round(3) %>%
  mutate(Month = month.abb) %>% 
  gt() %>% 
  opt_stylize(5) %>% 
  tab_header("Data exploration", "what is the highest and lowest in my database") %>% 
  cols_label(T_min_min = "Min",
              T_min_max = "Max",
              T_max_min = "Min",
              T_max_max = "Max") %>% 
  tab_spanner(label = "T_MIN", columns = 2:3) %>%  
  tab_spanner(label = "T_MAX", columns = 4:5)

#* Long term trends ----

## LT-trends ----
SMA(DATASET$T_AVG, lookback) %>% quickplot(show_legend = F, title = "Long term trend")
runSD(DATASET$T_AVG, lookback) %>% quickplot(show_legend = F, title = "Long term Standard deviation")

#* Seasonal decomposition ----

## SEAS DEC + FOURIER ----
temps <- DATASET
apply_convolution <- function(x, kernel) {
  # Use filter() from stats package to apply convolution
  filtered <- stats::filter(x, kernel, sides = 2)  # Use sides = 2 for symmetric filter
  return(filtered)
}

# DENOISED - convolution with a window of 90 days
kernel <- dnorm(-3:3)
data.frame("Gaussian_Kernel" = round(kernel, 10))

temps$Denoised <- apply_convolution(temps$T_AVG, kernel)
temps$Denoised <- na.fill(temps$Denoised, mean(temps$Denoised, na.rm = TRUE))

temps$Trend <- SMA(temps$Denoised, n = lookback)

library(nlme, quietly = T, warn.conflicts = F)

# Define the model
sin_component <- function(t, a, b, alpha, theta) {
  omega <- 2 * pi / 365.25
  a + b * t + alpha * sin(omega * t + theta)
}
omega <- 2 * pi / 365.25

# Fit model using non linear squares
temps$NUM_DAY <- 1:nrow(temps)

fit <- nls(Denoised ~ sin_component(NUM_DAY, a, b, alpha, theta),
          data = temps,
          start = list(a = 1, b = 0, alpha = 1, theta = 0))


# Get coefficients and confidence intervals for the model
params <- coef(fit)
confint_fit <- suppressMessages(confint(fit))

temps$SEAS <- params["alpha"] * sin(omega * temps$NUM_DAY + params["theta"])
temps$TREND <- params["a"] + params["b"] * temps$NUM_DAY
temps$BAR <- temps$TREND + temps$SEAS
temps$RESID <-  temps$T_AVG - temps$TREND - temps$SEAS

## model_stats ----
check_acc(temps$BAR, fitted(fit),15, title = "T_BAR VS fitted from the non linear squares")

check_acc(temps$RESID, residuals(fit),15, title = "T_BAR VS fitted from the non linear squares")

#* Model performance ----

## performance ----
# Print Model 
for (i in 1:length(params)) {
  cat(names(params)[i], ": ", round(params[i], 3), 
      " CI ~normally [", round(confint_fit[i, 1], 3), ",", round(confint_fit[i, 2], 3), "]\n")
  }

# Model performance
cat("  RSS model sine curve:", round(RSS(temps$T_AVG, temps$BAR), 2), "\n")
cat("  MAE model fit:", round(MAE(temps$BAR, temps$T_AVG), 2), "\n")

# fix the trend by using the linear trend
temps$Trend <- temps$Trend %>% na.fill(params["a"] + params["b"] * 1:lookback)

#* Visualization of results ----

## FOURIER VIZ ----
# plot denoised 
ggplot(tail(temps, lookback), aes(x=DAY))+
  geom_point(aes(y = T_AVG, color = "Average"), size = 1)+
  geom_point(aes(y = Denoised, color = "Denoised"), size = 1)+
  scale_color_manual(name=NULL, values = c(Average = "royalblue", Denoised = "#ffcc00"))+
  labs(title = "Average temperature", x = "Date", y = "Temperature", 
      subtitle = "Before and after the gaussian convolution filter")

temps_xts <- temps %>%
  select(T_AVG, Denoised, TREND, SEAS, RESID) %>%
  xts(order.by = temps$DAY) %>%
  tail(lookback)

# Plot seasonal decomposition from Avg to residuals
grid.arrange(nrow=5, top = paste0("Classical decomposition - last ",  lookback/365, " years"), 
            
  temps_xts$T_AVG %>% 
    quickplot(subtitle = "Average Temperature", show_legend = F, xlab = NULL, ylab = "Temps", 
    type = geom_point, show_x = F),

  temps_xts$Denoised %>% 
    quickplot(subtitle = "Denoised", show_legend = F, xlab = NULL, ylab = "Temps", 
    type = geom_point, show_x = F),

  temps_xts$TREND %>% 
    quickplot(subtitle = "Trend", show_legend = F, xlab = NULL, ylab = "Temps" , show_x = F),

  temps_xts$SEAS %>% 
    quickplot(subtitle = "Seasonal", show_legend = F, xlab = NULL, ylab = "Temps" , show_x = F), 

  temps_xts$RESID %>% 
    quickplot(subtitle = "Residuals", show_legend = F, xlab = NULL, ylab = "Temps"))

# Plot original vs. fitted data
ggplot(temps, aes(x = DAY)) +
  geom_point(aes(y = T_AVG), color = 'royalblue', size = 0.5) +
  geom_line(aes(y = BAR), color = 'orange', linewidth=2) +
  labs(title = "Temperature Model Fit (all Observations)", y = "Temperature (deg C)")

#* Check for possible model degradation ----

## Time degrad ----
grid.arrange(nrow = 2, ncol = 2,
ggplot(temps %>% head(lookback), aes(x = DAY)) +
  geom_point(aes(y = T_AVG), color = 'royalblue', size = 0.5) +
  geom_line(aes(y = BAR), color = 'orange', linewidth=2) +
  labs(title = paste0("Temperature Model Fit (First ", lookback/365, " years)"),x=NULL, y = NULL),

ggplot(temps %>% head(lookback), aes(x = DAY)) +
  geom_line(aes(y = RESID), color = 'black', linewidth=0.5) +
  labs(title = paste0("Residuals (First ", lookback/365, " years)"),x=NULL, y = NULL),

ggplot(temps %>% tail(lookback), aes(x = DAY)) +
  geom_point(aes(y = T_AVG), color = 'royalblue', size = 0.5) +
  geom_line(aes(y = BAR), color = 'orange', linewidth=2) +
  labs(title = paste0("Temperature Model Fit (Last ", lookback/365, " years)"), x=NULL, y = NULL),

ggplot(temps %>% tail(lookback), aes(x = DAY)) +
  geom_line(aes(y = RESID), color = 'black', linewidth=0.5) +
  labs(title = paste0("Residuals (Last ", lookback/365, " years)"),x=NULL, y = NULL)
)

#* Residuals analysis and diagnostics ----

## Resid analysis ----
grid.arrange(nrow = 2, 
# ACF 
ggAcf(temps$RESID, lag.max = 100)+
  labs(title = "ACF of Residuals", x = NULL, y = NULL),

# PACF
ggPacf(temps$RESID, lag.max = 100)+
  labs(title = "PACF of Residuals", x = NULL, y = NULL),

## Check normality of residuals using QQ plot
ggplot(temps, aes(sample = RESID)) +
  stat_qq(color="royalblue")+
  stat_qq_line(color = "black", linewidth = 0.4)+
  labs(title = "QQ plot", x="Theoretical Quantiles", y= "Observed Quantiles"),

## Check for heteroskedasticity or any pattern in residuals
ggplot(temps %>% tail(lookback), aes(x=BAR, y = RESID))+
  geom_point(size = 0.4)+
  geom_smooth(method = "lm")+
  labs(title = paste0("Residuals vs Fitted - last ",  lookback/365, " years"), x= "Fitted Values", y = "Residuals"))

# Histogram with bell curve and kurtosis
ggplot(temps, aes(x = RESID)) +
  geom_histogram(aes(y = after_stat(density)), fill = "lightblue", bins = 30) +
  stat_function(fun = dnorm, args = list(mean = mean(temps$RESID), sd = sd(temps$RESID)), 
                color = "red", linewidth = 1.2) +
  # geom_vline(xintercept = skewness(temps$RESID)[1], linetype = "dashed", linewidth=1, color = "darkred")+
  labs(title = "Histogram of Residuals with Normal Curve", x = "Residuals", y = "Density", 
        # subtitle = "Vertical line is kurtosis"
        )

#* Ornstein-Uhlenbeck (OU) process ----

## OU2 ----
temps_OU <- temps
# Define parameters for the OU process
kappa <- 1-arima(temps_OU$RESID, order = c(1,0,0))$coef[1]  # Mean-reversion rate
sigma <- 0.1                                                # Volatility of the process
dt <- 1                                                     # Time step (daily data)

cat("Kappa is estimated as:", round(kappa,4))

# Initialize variables for simulation
n <- nrow(temps_OU)                      # Number of time points
T_simulated <- numeric(n)                # Simulated temperature vector
T_simulated[1] <- temps_OU$Denoised[1]   # Set initial temperature to the first observed value

# Simulate the seasonal mean as a time-varying mean (trend + seasonal component)
T_bar <- temps_OU$BAR

# Simulate the modified OU process
for (i in 2:n) {
  # Rate of change of the seasonal mean
  dT_bar_dt <- (T_bar[i] - T_bar[i - 1]) / dt
  
  # Brownian motion increment
  dWt <- rnorm(1, mean = 0, sd = sqrt(dt))
  
  T_simulated[i] <- T_simulated[i - 1] + 
                    (dT_bar_dt + kappa * (T_bar[i] - T_simulated[i - 1])) * dt + 
                    sigma * dWt
}

temps_OU$OU <- T_simulated

DATE <- tail(temps$DAY, lookback)

grid.arrange(layout_matrix = matrix(data = c(1,1,1,2), ncol = 1), left = "Temperature",
temps_OU %>%
  select(Denoised, OU, BAR) %>%
  tail(lookback) %>%
  ggplot(aes(x = DATE)) +
  geom_point(aes(y = Denoised, color = "Denoised"), size = 0.5) +
  geom_line(aes(y = OU, color = "Simulated"), linewidth = 0.8) +
  scale_color_manual(name = NULL, values = c(Denoised = "#ffcc00", Simulated = "darkgreen")) +
  labs(title = "Simulated Ornstein-Uhlenbeck Process for Temperature - Last 10 years", x = NULL, y = NULL)+
  theme(axis)+ 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
,
temps_OU %>%
  select(Denoised, OU, BAR) %>%
  tail(lookback) %>%
  ggplot(aes(x = DATE)) +
  geom_line(aes(y = Denoised-OU, color = "Difference"), linewidth = 0.5) +
  scale_color_manual(name = NULL, values = c(Difference = "mediumspringgreen")) +
  labs(x = "Day", y = NULL)
)

#* Modeling volatility ----

#* Polynomial Regression: ----

## vol modeling ----
# Create a dataframe with temperature and date components
temp_t <- data.frame(
  Date = temps$DAY,  # Assuming the index is a proper Date object
  T = temps$Denoised,
  day = yday(temps$DAY),
  month = month(temps$DAY)
)

# 2. Calculate monthly volatility statistics
vol1 <- temps %>%
  group_by(YEAR, Month) %>%
  summarise(
    mean_temp = mean(Denoised, na.rm = TRUE),
    std_temp = sd(Denoised, na.rm = TRUE)
  ) %>%
  ungroup()

# 5. Volatility Analysis
    vol <- temps %>%
      select(DAY, T_AVG) %>%
      mutate(day = yday(DAY),
            month = month(DAY)) %>%
      group_by(day) %>%
      summarise(mean = mean(T_AVG, na.rm = TRUE),
                std = sd(T_AVG, na.rm = TRUE))

#* B-splines ----

## B-splines ----
library(splines)
x <- 1:366
y <- vol$std

# Define the number of knots
knots <- c(1, 3, 5, 10, 15, 20, 30, 50, 80)
x <- 1:366
# Function to create a spline model and plot
create_spline_plot <- function(knots, x, y) {
  # Fit the spline model
  spline_model <- lm(y ~ bs(x, knots = knots))
  yfit <- predict(spline_model, data.frame(x = x))
  
  # Calculate Residual Sum of Squares (RSS)
  rss <- sum((y - yfit)^2)
  aic <- length(x) * log(rss / length(x)) + 2 * knots

  # Create the plot
  plots <- ggplot() +
    geom_point(aes(x, y), color = 'cornflowerblue', size = 1.5) +
    geom_line(aes(x, yfit), color = 'black', linewidth = 1) +
    ggtitle(paste("Knots #", knots, "\nRSS:", round(rss, 2), "AIC:", round(aic,2))) +
    labs(x = NULL, y = NULL)
    theme_classic()+
    theme(plot.title = element_text(size = 10, face = "bold"))
  
  return(plots)
}

# Generate all the plots
plots <- lapply(knots, create_spline_plot, x = x, y = y)

# Arrange and display the plots in a 2x3 grid
grid.arrange(grobs = plots, nrow = 3, ncol = 3, left = "Standard deviation of temperatures", bottom = "Day of the year", top = "Different B-splines models")

#* Polynomial models to fit volatility ----

## polynomial ----
# Function to create polynomial model and plot
x <- 1:366
create_poly_plot <- function(degree, x, y) {
  # Fit polynomial model
  model <- lm(y ~ poly(x, degree, raw = TRUE))
  yfit <- predict(model, data.frame(x = x))
  
  # Calculate metrics
  rss <- sum(resid(model)^2)
  aic <- AIC(model)
  
  # Create plot
  ggplot() +
    geom_point(aes(x, y), color = '#00962B', size = 1) +
    geom_line(aes(x, yfit), color = 'black', linewidth = 1) +
    ggtitle(paste("Degree:", degree, "\nRSS:", round(rss, 2), "AIC:", round(aic, 2))) +
    labs(y = NULL, x = NULL)+
    theme_classic()+
    theme(plot.title = element_text(size = 10, face = "bold"))
  }

# Generate plots for degrees 1 through 9
degrees <- 1:9
plots <- lapply(degrees, create_poly_plot, x = x, y = y)

# Arrange in 3x3 grid
grid.arrange(grobs = plots, nrow = 3, ncol = 3, left = "Standard deviation of temperatures", bottom = "Day of the year", top = "Different polynomial models")

#* Fourier transforms ----

## fourier int ----
fourier_series <- function(x, n_terms, period = 365.25) {
  omega <- 2 * pi / period
  terms <- list(a0 = 1)

  for (i in 1:n_terms) {
    terms[[paste0("a", i)]] <- cos(i * omega * x)
    terms[[paste0("b", i)]] <- sin(i * omega * x)
  }
  return(terms)
}

fourier_series(1:366, 3) %>%
  data.frame("actual" = normalize(y, 1)) %>%
  quickplot(
    title = "Fourier transformation different components with actual volatility",
    subtitle = "Order 3 so 7 components",
    xlab = "Day of the year"
  )


## fourier plot ----
create_fourier_plot <- function(n_terms, x, y) {
  # Fit Fourier series
  fourier_terms <- fourier_series(1:366, n_terms = n_terms)
  model_data <- data.frame(y = y, fourier_terms)

  fourier_fit <- lm(y ~ ., data = model_data)
  fourier <- predict(fourier_fit, newdata = model_data)

  # Calculate metrics
  rss <- sum(resid(fourier_fit)^2)
  aic <- AIC(fourier_fit)

  # Create plot
  ggplot(data = data.frame(), aes(x = x)) +
    geom_point(aes(y = y), color = 'orangered3', size = 1) +
    geom_line(aes(y = fourier), color = 'black', linewidth = 1) +
    ggtitle(paste("Order:", n_terms, "\nRSS:", round(rss, 2), "AIC:", round(aic, 2))) +
    labs(y = NULL, x = NULL) +
    theme_classic() +
    theme(plot.title = element_text(size = 10, face = "bold"))
}

# Generate plots for degrees 1 through 9
order <- c(0:8)
plots <- lapply(order, create_fourier_plot, x = x, y = y)

# Arrange in 3x3 grid
grid.arrange(grobs = plots, nrow = 3, ncol = 3,
  left = "Standard deviation of temperatures",
  bottom = "Day of the year",
  top = "Different Fourier Series models")

#* Regime switch ----

## Regime switch ----
library(changepoint, quietly = T, warn.conflicts = F)
create_changepoint_plot <- function(n_breaks, x, y) {
  # Detect changepoints in variance with fixed number of breaks
  cp <- cpt.var(y, method = "PELT", Q = n_breaks)

  # Get regime-wise standard deviation
  regime_sd <- rep(NA, length(y))
  segs <- c(1, cpts(cp), length(y))
  for (i in 1:(length(segs) - 1)) {
    idx <- segs[i]:(segs[i + 1] - 1)
    regime_sd[idx] <- sd(y[idx])
  }

  # RSS (within segment variance sum)
  rss <- sum((y - na.omit(regime_sd))^2)

  # Build plot
  data_plot <- data.frame(x = x, y = y, sd_fit = regime_sd)
  ggplot(data_plot, aes(x = x)) +
    geom_point(aes(y = y), color = 'purple4', size = 1) +
    geom_line(aes(y = sd_fit), color = 'black', linewidth = 1) +
    ggtitle(paste("Breaks:", n_breaks, "\nRSS:", round(rss, 2))) +
    theme_classic() +
    labs(y = NULL, x = NULL) +
    theme(plot.title = element_text(size = 10, face = "bold"))
}
# Create 9 plots for rhe breaks
breaks <- c(1, 2, 4, 8, 10, 12, 20, 30, 40)
plots <- suppressWarnings(lapply(breaks, create_changepoint_plot, x = x, y = y))

suppressWarnings(grid.arrange(grobs = plots, nrow = 3, ncol = 3,
  left = "Standard deviation of temperatures",
  bottom = "Day of the year",
  top = "Variance Regime Switching Models"))

## AR on vol_DOY ----
# 3. Print long-term volatility metrics
cat("Trend or long term volatility is easy: ~", round(mean(vol1$std_temp, na.rm = TRUE), 3), "\n")
cat("Gamma is:", round(sd(vol1$std_temp, na.rm = TRUE), 3), "\n")

# 4. Fit AR(1) model for mean reversion rate
ar_model <- arima(vol1$std_temp, order = c(1, 0, 0), include.mean = FALSE)
coef <- ar_model$coef
residuals <- ar_model$residuals

cat("Rate of mean reversion of volatility process is:", round(coef["ar1"], 3), "\n")
summary(ar_model)

#* Montecarlo simulations ----

## MNC ----
a <- params[1]
b <- params[2]
theta <- atan2(params[3], params[4])
alpha <- sqrt(params[3]^2 + params[4]^2)
kappa <- as.double(kappa)

data.frame(a = a, b = b, theta = theta, alpha = alpha, kappa = kappa)

# 1. Temperature Model Functions
T_model <- function(x, a, b, alpha, theta) {
  omega <- 2 * pi / 365.25
  a + b * x + alpha * sin(omega * x + theta)
}
dT_model <- function(x, a, b, alpha, theta) {
  omega <- 2 * pi / 365.25
  b + alpha * omega * cos(omega * x + theta)
}

# 2. Prepare Data
if (inherits(temps$DAY, "Date")) {
  first_ord <- as.numeric(temps$DAY[1])
  temp_t$ordinal <- as.numeric(temps$DAY)
} else {
  temp_t$Date <- as.Date(temps$DAY)
  first_ord <- as.numeric(temps$DAY[1])
  temp_t$ordinal <- as.numeric(temps$DAY)
}

# 3. Apply Model with Given Parameters
Tbar_params <- list(a = a, b = b, alpha = alpha, theta = theta)

temps$model_fit <- T_model(
  temp_t$ordinal - first_ord,
  Tbar_params$a,
  Tbar_params$b,
  Tbar_params$alpha,
  Tbar_params$theta
)

grid.arrange(
  nrow = 2,
  ncol = 2,
  ggplot(temps %>% head(lookback), aes(x = DAY)) +
    geom_point(aes(y = T_AVG), color = 'royalblue', size = 0.5) +
    geom_line(aes(y = model_fit), color = 'orange', linewidth = 2) +
    labs(
      title = paste0(
        "Temperature Model Fit (First ",
        lookback / 365,
        " years)"
      ),
      x = NULL,
      y = NULL
    ),

  ggplot(temps %>% head(lookback), aes(x = DAY)) +
    geom_line(aes(y = T_AVG - model_fit), color = 'black', linewidth = 0.5) +
    labs(
      title = paste0("Residuals (First ", lookback / 365, " years)"),
      x = NULL,
      y = NULL
    ),

  ggplot(temps %>% tail(lookback), aes(x = DAY)) +
    geom_point(aes(y = T_AVG), color = 'royalblue', size = 0.5) +
    geom_line(aes(y = model_fit), color = 'orange', linewidth = 2) +
    labs(
      title = paste0("Temperature Model Fit (Last ", lookback / 365, " years)"),
      x = NULL,
      y = NULL
    ),

  ggplot(temps %>% tail(lookback), aes(x = DAY)) +
    geom_line(aes(y = T_AVG - model_fit), color = 'black', linewidth = 0.5) +
    labs(
      title = paste0("Residuals (Last ", lookback / 365, " years)"),
      x = NULL,
      y = NULL
    )
)


# 6. Spline Fit for Volatility
spline_fit <- function(knots, x, y) {
  x_new <- seq(0, 1, length.out = knots + 2)[2:(knots + 1)]
  knots_pos <- quantile(x, probs = x_new)
  bspline <- lm(y ~ bs(x, knots = knots_pos, degree = 15))
  predict(bspline, newdata = data.frame(x = x))
}

volatility <- spline_fit(15, vol$day, vol$std)

# Plot Volatility
ggplot(vol, aes(x = day)) +
  geom_point(aes(y = std, color = "Observed Volatility")) +
  geom_line(aes(y = volatility, color = "Spline Fit"), linewidth = 1) +
  scale_color_manual(
    values = c("Observed Volatility" = "blue", "Spline Fit" = "black")
  ) +
  labs(
    title = "Temperature Volatility by Day of Year",
    y = "Std Dev (°C)",
    x = "Day of Year"
  ) +
  theme_minimal()

# 7. Monte Carlo Simulation Functions
euler_step <- function(row, kappa, M) {
  T_i <- ifelse(is.na(row$Tbar_shift), row$Tbar, row$Tbar_shift)
  T_det <- T_i + row$dTbar
  T_mrev <- kappa * (row$Tbar - T_i)
  sigma <- row$vol * rnorm(M)
  T_det + T_mrev + sigma
}

monte_carlo_temp <- function(trading_dates, Tbar_params, vol_model, 
                            first_ord, M = 1, kappa = 0.2430516) {
  # Convert dates to numeric if needed
  if (inherits(trading_dates, "Date")) {
    trading_numeric <- as.numeric(trading_dates)
  } else {
    trading_dates <- as.Date(trading_dates)
    trading_numeric <- as.numeric(trading_dates)
  }

  # Calculate Tbar and dTbar
  x_vals <- trading_numeric - first_ord
  Tbars <- T_model(x_vals, Tbar_params$a, Tbar_params$b, 
                    Tbar_params$alpha, Tbar_params$theta)

  dTbars <- dT_model(x_vals, Tbar_params$a, Tbar_params$b, 
                    Tbar_params$alpha, Tbar_params$theta)

  # Create simulation dataframe
  mc_temps <- data.frame(
    Date = trading_dates,
    Tbar = Tbars,
    dTbar = dTbars,
    day = yday(trading_dates),
    vol = vol_model[yday(trading_dates)] # Directly add volatility
  )

  # Add lagged Tbar
  mc_temps$Tbar_shift <- dplyr::lag(mc_temps$Tbar)

  # Run simulations - modified apply call
  simulations <- sapply(1:nrow(mc_temps), function(i) {
    row <- mc_temps[i, ]
    euler_step(row, kappa, M)
  })

  # Transpose and format results
  simulations <- t(simulations)
  colnames(simulations) <- paste0("Sim", 1:M)

  list(
    mc_temps = mc_temps,
    mc_sims = cbind(Date = trading_dates, as.data.frame(simulations))
  )
}

# 8. Run Simulation
trading_dates <- seq(as.Date("2022-09-01"), as.Date("2025-08-31"), by = "day")
sim_results <- monte_carlo_temp(
  trading_dates,
  Tbar_params,
  volatility,
  first_ord,
  M = 5
)

# 9. Plot Simulation Results
sim_results$mc_sims %>%
  pivot_longer(-Date, names_to = "Simulation", values_to = "Temperature") %>%
  ggplot(aes(x = Date, y = Temperature, color = Simulation)) +
  geom_point(alpha = 0.7) +
  labs(title = "Monte Carlo Temperature Simulations", y = "Temperature (°C)") +
  theme_minimal() +
  theme(legend.position = "bottom")

## MNC sym ----
# Set number of simulations
no_sims <- 100000

# Define winter and summer dates (Southern Hemisphere)
trading_dates_winter <- as.Date("2025-10-01")
trading_dates_summer <- as.Date("2025-04-01")

# Run simulations
sim_results_winter <- monte_carlo_temp(trading_dates_winter, Tbar_params, volatility, first_ord, M = no_sims)
sim_results_summer <- monte_carlo_temp(trading_dates_summer, Tbar_params, volatility, first_ord, M = no_sims)


# Extract results
mc_sims_winter <- sim_results_winter$mc_sims %>% select(-Date)
mc_sims_summer <- sim_results_summer$mc_sims %>% select(-Date)

Tbar_summer <- sim_results_summer$mc_temps$Tbar[1]
Tbar_winter <- sim_results_winter$mc_temps$Tbar[1]

# Create combined data frame for plotting
plot_data <- bind_rows(
  data.frame(Temperature = unlist(mc_sims_summer), Season = "Summer"),
  data.frame(Temperature = unlist(mc_sims_winter), Season = "Winter")
)

# Create the plot
ggplot(plot_data, aes(x = Temperature, fill = Season)) +
  geom_histogram(position = "identity", alpha = 0.8, bins = 80) +
  geom_vline(aes(xintercept = Tbar_winter), color = "darkblue",
              linewidth = 1.5, linetype = "solid") +
  geom_vline(aes(xintercept = Tbar_summer), color = "darkorange",
              linewidth = 1.5, linetype = "solid") +
  scale_fill_manual(values = c(Winter="steelblue", Summer="orange")) +
  labs(title = "Winter vs Summer Temperature MC Sims",
        x = "Temperature (°C)", 
        y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "bottom")

#* Sidequests ----

#* Sidequest: overlap of the dataset ----

## each year overlapped ----
pivot_df <- DATASET %>%
  select(DOY, YEAR, T_AVG) %>%
  pivot_wider(names_from = YEAR, values_from = T_AVG)


MAX_pivot_df <- DATASET %>%
  select(DOY, YEAR, T_MAX) %>%
  pivot_wider(names_from = YEAR, values_from = T_MAX)

MIN_pivot_df <- DATASET %>%
  select(DOY, YEAR, T_MIN) %>%
  pivot_wider(names_from = YEAR, values_from = T_MIN)


MEAN <- apply(pivot_df[-1], 1, mean, na.rm=TRUE)
MEDIAN <- apply(pivot_df[-1], 1, median, na.rm=TRUE)
IQR <- apply(pivot_df[-1], 1, IQR, na.rm=TRUE)
SD <- apply(pivot_df[-1], 1, sd, na.rm=TRUE)

data.frame(MEAN = MEAN,
          MEDIAN = MEDIAN) %>% 
  quickplot(title = "Mean and median across the year", xlab = "Day of the year", ylab = "Temperature")

data.frame(IQR = SMA(IQR, n = 7),
          SD = SD) %>% 
  quickplot(title = "Standard deviation and Inter-quartile-range across the year",
            subtitle = "IQR MA(7)", xlab = "Day of the year", ylab = "Temperature")

data.frame(MAX = apply(pivot_df[-1], 1, max, na.rm=TRUE), 
          MIN = apply(pivot_df[-1], 1, min, na.rm=TRUE)) %>%
  mutate(AVG = (MAX + MIN)/2) %>% 
  mutate(RANGE = MAX - MIN) %>% 
  quickplot(title = "Range across the year", show_legend = T, xlab = "Day of the year", ylab = "Temperature")


melt(pivot_df[-1], id.vars = NULL) %>% 
  ggplot(aes(x = variable, y = value)) +
    geom_boxplot(fill = "gray") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
    labs(title = "Boxplot for all years", x = NULL, y = NULL)

DATASET %>%
  select(Month, T_AVG) %>%
  group_by(Month) %>% 
  ggplot(aes(x = Month, y = T_AVG, group = Month)) +
    geom_boxplot(fill = "gray") +
    scale_x_continuous(breaks = seq(1, 12, by = 1), labels = month.abb) +
    labs(title = "Boxplot for all months across the years", x = NULL, y = NULL)

#* How is this year compared to the rest ----

## avg hottness ----
ggplot(DATASET, aes(x = DOY, y = T_AVG, group = YEAR, color = YEAR)) +
  geom_line() +
  labs(title = "All years overlapped", x = "Day of the year", y = "Index level of returns")


THISYEAR <- data.frame(
  c(DATASET[DATASET$YEAR == 2024, ]["T_MAX"]),
  c(DATASET[DATASET$YEAR == 2024, ]["T_AVG"]),
  c(DATASET[DATASET$YEAR == 2024, ]["T_MIN"])
)

cbind(pivot_df,
      "MIN" = apply(pivot_df[-1], 1, min, na.rm = TRUE),
      "MAX" = apply(pivot_df[-1], 1, max, na.rm = TRUE),
      "MEAN" = apply(pivot_df[-1], 1, mean, na.rm = TRUE)) %>%
  round(2) %>%
  ggplot(aes(x = DOY)) +
    geom_ribbon(aes(ymin = MIN, ymax = MAX), alpha = 0.2) +
    geom_line(aes(y = MEAN, color = "MEAN"), linewidth = 1) +
    geom_line(aes(y = pivot_df$`2025`, color = "AVG_Current"), linewidth = 1.3) +
    geom_line(aes(y = MAX_pivot_df$`2025`, color = "MAX_Current"), linewidth = 0.6, alpha = 1) +
    geom_line(aes(y = MIN_pivot_df$`2025`, color = "MIN_Current"), linewidth = 0.6, alpha = 1) +
  scale_color_manual(name = NULL, values = c(MEAN = "orange2", AVG_Current = "olivedrab", 
                                                MIN_Current = "lightblue", MAX_Current = "indianred")) +
  labs(title = "Is this year hotter on average?", y = NULL, x = NULL)+
  theme(legend.position = "bottom")

#* Sidequest: predicting temperatures in the future ----

## forecast compared to real ----
model_formula <- function(t, a, b, a1, b1) {
  omega <- 2 * pi / 365.25
  a + b * t + a1 * cos(omega * t) + b1 * sin(omega * t)
}

forecast_N <- 30
new_t <- max(temps$NUM_DAY) + forecast_N  # forecast_N days after last observation
predicted_T <- model_formula(new_t, params["a"], params["b"], params["alpha"], params["theta"])
cat("predicted temperature for", as.character(max(temps$DAY)+forecast_N), "=", predicted_T)

arima_model <- Arima(temps$RESID, order = c(2, 0, 0), include.mean = FALSE)
future_residuals <- forecast(arima_model, h = forecast_N)  # forecast_N steps ahead

first_date <- min(temps$DAY)
future_dates <- seq(max(temps$DAY), by = "day", length.out = forecast_N)
future_t <- as.numeric(difftime(future_dates, first_date, units = "days"))

# Deterministic part
deterministic_part <- model_formula(future_t, params["a"], params["b"], params["alpha"], params["theta"])

# Stochastic part (residuals)
stochastic_part <- future_residuals$mean

# Final prediction
future_T <- deterministic_part + stochastic_part

forecast_df <- data.frame(
  Date = future_dates,
  Temperature = future_T,
  Lower = future_T - 1.96 * future_residuals$mean,  # 95% CI
  Upper = future_T + 1.96 * future_residuals$mean
)

ggplot() +
  geom_line(data = tail(temps, 100), aes(x = DAY, y = T_AVG)) +
  geom_line(data = forecast_df, aes(x = Date, y = Temperature), color = "red") +
  geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lower, ymax = Upper), alpha = 0.2) +
  labs(title = "Temperature Forecast with 95% CI", y = "Temperature (°C)", x = "Date") 

#* forecast seasonal plots ----

## ggforecast ----
ts(data = temps$T_AVG, frequency = 12, start = temps$DAY[1]) %>% tail(365*2+ last(temps$DOY)) %>% forecast::ggseasonplot()
ts(data = temps$T_AVG, frequency = 365, start = temps$DAY[1]) %>% tail(365*2+ last(temps$DOY)) %>% forecast::gglagplot()
ts(data = temps$T_AVG, frequency = 365.25, start = temps$DAY[1]) %>% tail(365*2+ last(temps$DOY)) %>% forecast::gglagchull()

ts(data = temps$T_AVG, frequency = 365, start = temps$DAY[1]) %>% tail(365*10+ last(temps$DOY)) %>% forecast::ggtsdisplay(plot.type = "scatter", points = F, smooth = T, lag.max = 20, theme=theme_minimal())


## beep ----
beepr::beep(sound = 4)

## unnamed chunk ----
# Assuming trading_dates and sim_length already defined
x_vals <- as.numeric(trading_dates) - first_ord
Tbar_vals <- T_model(x_vals, a, b, alpha, theta)
dTbar_vals <- dT_model(x_vals, a, b, alpha, theta)
sigma_vals <- volatility[yday(trading_dates)]

data.frame(
  x_vals = x_vals,
  Tbar_vals = Tbar_vals,
  dTbar_vals = dTbar_vals,
  sigma_vals = sigma_vals
) %>% show_df()

data.frame(
  x_vals = x_vals,
  Tbar_vals = Tbar_vals,
  dTbar_vals = dTbar_vals,
  sigma_vals = sigma_vals
) %>% apply(2, normalize) %>% quickplot()
