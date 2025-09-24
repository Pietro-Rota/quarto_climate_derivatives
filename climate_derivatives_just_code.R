#* AUTO-GENERATED STANDALONE R SCRIPT ----
#* Generated from R Markdown file using extract_r_code()
#* Source file: quarto_climate_derivatives.qmd
#* Generated on: 2025-09-24 13:14:58.250909

#* REQUIRED PACKAGES ----
#? If you don't have these packages, run: install.packages(c("caret", "DT", "forecast", "gt", "leaflet", "MASS", "nlstools", "purrr", "rugarch", "tibble", "timeSeries", "zoo", "colorspace", "e1071", "gganimate", "gtExtras", "lubridate", "mgcv", "PerformanceAnalytics", "quantmod", "splines", "tidyr", "TTR", " ", "dplyr", "fBasics", "ggplot2", "knitr", "magrittr", "nlme", "plotly", "reshape2", "stats4", "timeDate", "xts", " "))
# Load required packages
library(caret)
library(DT)
library(forecast)
library(gt)
library(leaflet)
library(MASS)
library(nlstools)
library(purrr)
library(rugarch)
library(tibble)
library(timeSeries)
library(zoo)
library(colorspace)
library(e1071)
library(gganimate)
library(gtExtras)
library(lubridate)
library(mgcv)
library(PerformanceAnalytics)
library(quantmod)
library(splines)
library(tidyr)
library(TTR)
library(dplyr)
library(fBasics)
library(ggplot2)
library(knitr)
library(magrittr)
library(nlme)
library(plotly)
library(reshape2)
library(stats4)
library(timeDate)
library(xts)

#* CUSTOM FUNCTIONS ----
# Custom functions from main file
bprint <- function (obj) 
{
    for (var in names(obj)) {
        cat(var, "=", as.numeric(obj[var]), "\n")
    }
}

check_acc <- function (data1, data2, n = 10, title = NULL, visual = c(TRUE, FALSE, "both")) 
{
    stopifnot(length(data1) == length(data2), is.numeric(data1), is.numeric(data2))
    accuracy <- data.frame()
    for (i in 0:n) {
        new <- mean(round(data1, i) == round(data2, i), na.rm = TRUE)
        accuracy <- rbind(accuracy, new)
    }
    colnames(accuracy) <- "values"
    cor_val <- cor(data1, data2, use = "complete.obs")
    mae_val <- mean(abs(data1 - data2), na.rm = TRUE)
    mse_val <- mean((data1 - data2)^2, na.rm = TRUE)
    diff_data <- data.frame(data1 = data1, data2 = data2) %>% head(5000) %>% mutate(index = index(.), diff = data1 - data2)
    diff_range <- range(diff_data$diff, na.rm = TRUE)
    diff_span <- diff_range[2] - diff_range[1]
    y_limits <- if (diff_span < 1e-06) {
        c(-1e-06, 1e-06)
    }
    else {
        NULL
    }
    diff_plot <- ggplot(diff_data, aes(x = index, y = diff)) + geom_point(color = "darkred", size = 0.7) + labs(subtitle = "Difference", x = NULL, y = NULL) + coord_cartesian(ylim = y_limits)
    accplot <- ggplot(accuracy, aes(x = 0:n, y = values)) + geom_line(linewidth = 1, color = "dodgerblue2") + geom_point(size = 2, color = "dodgerblue3") + scale_x_continuous(breaks = seq(0, n, by = 1)) + ylim(0, 1) + scale_y_continuous(labels = label_percent(), limits = c(0, 1)) + labs(subtitle = "Percentage of accuracy", y = NULL, x = "Rounding decimals")
    plot <- data.frame(data1 = data1, data2 = data2) %>% head(5000) %>% ggplot(aes(x = index(data1))) + geom_point(aes(y = data1, color = "Data1"), size = 2) + geom_point(aes(y = data2, color = "Data2"), size = 2) + scale_color_manual(values = c(Data1 = "purple3", Data2 = "mediumseagreen"), name = NULL) + labs(title = "Visual inspection", y = NULL, x = NULL) + theme(legend.position = "bottom")
    sum_df <- summary(data.frame(data1 = data1, data2 = data2))
    full_df <- data.frame(accuracy[accuracy != 0] * 100) %>% round(2) %>% data.frame() %>% set_colnames("Values !=0 %")
    if (visual[1] == TRUE) {
        return(marrangeGrob(list(plot, diff_plot, accplot), layout_matrix = matrix(c(3, 3, 2, 2, 1, 1, 1, 1), nrow = 4, ncol = 2), top = title))
    }
    else if (visual[1] == FALSE) {
        return(list(distance = data.frame(COR = cor_val, MAE = mae_val, MSE = mse_val), df = full_df, sum_df = sum_df))
    }
    else if (visual[1] == "both") {
        return(list(plots = marrangeGrob(list(plot, diff_plot, accplot), layout_matrix = matrix(c(3, 3, 2, 2, 1, 1, 1, 1), nrow = 4, ncol = 2), top = title), df = full_df, sum_df = sum_df))
    }
}

compare_cdf <- function (simulated, observed, title = "CDF Comparison", color = "steelblue") 
{
    ecdf_sim <- ecdf(simulated)
    ecdf_obs <- ecdf(observed)
    n <- min(length(simulated), length(observed))
    x_vals <- seq(from = min(c(simulated, observed)), to = max(c(simulated, observed)), length.out = n)
    df <- data.frame(Simulated = ecdf_sim(x_vals), Observed = ecdf_obs(x_vals))
    ggplot(df, aes(x = Simulated, y = Observed)) + geom_line(color = color, size = 1) + labs(title = title, x = "Simulated CDF", y = "Observed CDF")
}

desc_df <- function (data, quantiles = c(0.01, 0.25, 0.75, 0.99), digits = 4) 
{
    summary_stats <- function(x) {
        n <- sum(!is.na(x))
        mean <- mean(x, na.rm = TRUE)
        sd <- sd(x, na.rm = TRUE)
        median <- median(x, na.rm = TRUE)
        trimmed <- mean(x, trim = 0.1, na.rm = TRUE)
        min <- min(x, na.rm = TRUE)
        max <- max(x, na.rm = TRUE)
        range <- max - min
        skew <- sum((x - mean)^3, na.rm = TRUE)/(n * sd^3)
        kurtosis <- sum((x - mean)^4, na.rm = TRUE)/(n * sd^4) - 3
        se <- sd/sqrt(n)
        percent_missing <- sum(is.na(x))/length(x) * 100
        quantiles_values <- quantile(x, probs = quantiles, na.rm = TRUE)
        c(n = n, mean = mean, sd = sd, median = median, trimmed = trimmed, min = min, max = max, range = range, skew = skew, kurtosis = kurtosis, se = se, `%NA` = percent_missing, Q = quantiles_values[1], Q = quantiles_values[2], Q = quantiles_values[3], Q = quantiles_values[4])
    }
    stats <- sapply(data, function(col) {
        if (is.numeric(col)) 
            summary_stats(col)
        else rep(NA, length(summary_stats(0)))
    })
    as.data.frame(round(t(stats), digits = digits))
}

extract_r_code <- function (input_file, output_file, include_main = TRUE, source_path = "C:/Users/pietr/OneDrive/Desktop/formula.main.R") 
{
    lines <- readLines(input_file)
    in_chunk <- FALSE
    code_lines <- c()
    in_self_function <- FALSE
    self_function_start <- "^extract_r_code_complex\\s*<-\\s*function\\b"
    if (include_main) {
        code_lines <- c(code_lines, "#* AUTO-GENERATED STANDALONE R SCRIPT ----", "#* Generated from R Markdown file using extract_r_code()", paste0("#* Source file: ", basename(input_file)), paste0("#* Generated on: ", Sys.time()), "")
        tryCatch({
            required_pkgs <- required_packages(input_file)
            required_pkgs <- required_pkgs[!is.na(required_pkgs) & nzchar(trimws(required_pkgs)) & trimws(required_pkgs) != ""]
            if (length(required_pkgs) > 0) {
                code_lines <- c(code_lines, "#* REQUIRED PACKAGES ----", paste0("#? If you don't have these packages, run: install.packages(c(\"", paste(required_pkgs, collapse = "\", \""), "\"))"), "# Load required packages")
                for (pkg in required_pkgs) {
                  if (!is.na(pkg) && nzchar(trimws(pkg)) && trimws(pkg) != "") {
                    code_lines <- c(code_lines, paste0("library(", trimws(pkg), ")"))
                  }
                }
                code_lines <- c(code_lines, "")
            }
        }, error = function(e) {
            code_lines <<- c(code_lines, "# Warning: Could not automatically detect required packages", "# Please manually add library() calls as needed", "")
        })
        tryCatch({
            if (!exists("functions_loaded", mode = "function")) {
                stop("functions_loaded() function not found. Please source your main R file first.")
            }
            used_functions <- functions_loaded(input_file, dataframe = FALSE)
            cat("DEBUG: functions_loaded returned:", class(used_functions), "\n")
            cat("DEBUG: functions_loaded returned:", class(used_functions), "\n")
            if (is.null(used_functions)) {
                cat("DEBUG: No custom functions detected in the file\n")
            }
            else if (length(used_functions) > 0) {
                cat("DEBUG: Found", length(used_functions), "custom functions:", paste(used_functions, collapse = ", "), "\n")
                code_lines <- c(code_lines, "#* CUSTOM FUNCTIONS ----", "# Custom functions from main file")
                if (!file.exists(source_path)) {
                  stop("Main R file not found at: ", source_path)
                }
                mainEnv <- new.env()
                cat("DEBUG: Sourcing main file...\n")
                source(source_path, local = mainEnv)
                all_main_functions <- ls(envir = mainEnv)
                all_main_functions <- all_main_functions[sapply(all_main_functions, function(x) is.function(get(x, envir = mainEnv)))]
                cat("DEBUG: Functions available in main file:", paste(all_main_functions, collapse = ", "), "\n")
                functions_added <- 0
                for (func_name in used_functions) {
                  cat("DEBUG: Processing function:", func_name, "\n")
                  if (exists(func_name, envir = mainEnv)) {
                    func_obj <- get(func_name, envir = mainEnv)
                    if (!is.function(func_obj)) {
                      cat("DEBUG: Warning -", func_name, "is not a function\n")
                      next
                    }
                    func_text <- deparse(func_obj, width.cutoff = 500)
                    code_lines <- c(code_lines, paste0(func_name, " <- ", paste(func_text, collapse = "\n")), "")
                    functions_added <- functions_added + 1
                    cat("DEBUG: Successfully added function:", func_name, "\n")
                  }
                  else {
                    cat("DEBUG: Warning - function", func_name, "not found in main environment\n")
                  }
                }
                cat("DEBUG: Total functions added:", functions_added, "\n")
            }
        }, error = function(e) {
            cat("ERROR in custom function extraction:", e$message, "\n")
            code_lines <<- c(code_lines, paste0("# Warning: Could not automatically extract custom functions"), paste0("# Error: ", e$message), "# Please manually add function definitions as needed", "")
        })
        code_lines <- c(code_lines, "#* MAIN CODE ----", "")
    }
    for (line in lines) {
        if (grepl(self_function_start, line)) {
            in_self_function <- TRUE
        }
        if (in_self_function && grepl("^\\s*}\\s*$", line)) {
            in_self_function <- FALSE
            next
        }
        if (in_self_function) {
            next
        }
        if (include_main && (grepl("^\\s*(library|require)\\s*\\(", line) || grepl("^\\s*source\\s*\\(", line) || grepl("extract_r_code.*\\(", line))) {
            next
        }
        if (!in_chunk && grepl("^#{1,6} ", line)) {
            heading <- sub("^#+\\s+", "", line)
            code_lines <- c(code_lines, "", paste0("#* ", heading, " ----"))
        }
        else if (grepl("^```\\{r", line)) {
            in_chunk <- TRUE
            chunk_label <- sub("^```\\{r\\s*([^,}]*)?.*", "\\1", line)
            chunk_label <- trimws(chunk_label)
            label_line <- if (nzchar(chunk_label)) {
                paste0("## ", chunk_label, " ----")
            }
            else {
                "## unnamed chunk ----"
            }
            code_lines <- c(code_lines, "", label_line)
        }
        else if (grepl("^```", line) && in_chunk) {
            in_chunk <- FALSE
        }
        else if (in_chunk) {
            if (!grepl("^#\\|", line)) {
                code_lines <- c(code_lines, line)
            }
        }
    }
    writeLines(code_lines, output_file)
    cat("Standalone R script created successfully!\n")
    cat("Output file:", output_file, "\n")
    if (include_main) {
        cat("Dependencies automatically included.\n")
    }
}

find_outliers <- function (x, yes = 1, no = 0) 
{
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 2 * IQR
    upper_bound <- Q3 + 2 * IQR
    df <- numeric(length(x))
    df <- data.frame(ifelse(x < lower_bound | x > upper_bound, yes = yes, no = no)) %>% na.fill(fill = 0)
    return(df)
}

quickbarplot <- function (data, title = NULL, plot_engine = c("ggplot", "plotly"), xlab = "Category", ylab = "Value", show_legend = FALSE, subtitle = NULL, caption = NULL, bar_width = 0.8, legend_name = "Variable", legend_position = c("right", "left", "bottom", "top"), alpha = 1, facet_wrap = FALSE, show_x = TRUE, palette = c("custom", "gradiant", "none", "any_color")) 
{
    custom_palette <- rep(c("firebrick", "darkblue", "#006400", "gray30", "#457575", "#6100a8", "orange2", "brown", "#483D8B", "#556B2F", "#8B008B", "#5F9EA0", "#6B8E23", "#9932CC"), 1000)
    my_data_long <- melt_any(data)
    plot <- ggplot(my_data_long, aes(x = variable, y = value, fill = factor(variable), text = paste0("Name: ", variable, "<br>Value: ", value))) + geom_col(width = min(bar_width, 1), alpha = alpha) + labs(title = title, subtitle = subtitle, caption = caption, x = xlab, y = ylab) + scale_fill_manual(name = legend_name, values = custom_palette) + theme(legend.position = legend_position[1], plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
    if (nrow(my_data_long) > 20) {
        plot <- plot + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    if (!show_legend) {
        plot <- plot + theme(legend.position = "none")
    }
    if (!show_x) {
        plot <- plot + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
    }
    if (facet_wrap) {
        if ("group" %in% colnames(my_data_long)) {
            plot <- plot + facet_wrap(~group, scales = "free_x")
        }
        else {
            plot <- plot + facet_wrap(~variable, scales = "free_x")
        }
    }
    final_plot <- switch(plot_engine[1], ggplot = plot, plotly = ggplotly(plot, tooltip = "text") %>% layout(xaxis = list(title = xlab), yaxis = list(title = ylab, fixedrange = FALSE), dragmode = "zoom"))
    return(final_plot)
}

quickplot <- function (data, title = NULL, plot_engine = c("ggplot", "plotly"), xlab = "Date", ylab = "Value", show_legend = TRUE, subtitle = NULL, caption = NULL, linewidth = 0.4, legend_name = "Variable", legend_position = c("right", "left", "bottom", "top"), alpha = 1, type = geom_line, facet_wrap = FALSE, x_size = 1, x_start = 1, x_step = 1, show_x = TRUE) 
{
    plot_data <- data.frame(Date = index(data), data)
    custom_palette <- rep(c("firebrick", "darkblue", "#006400", "gray30", "#457575", "#6100a8", "orange2", "brown", "#483D8B", "#556B2F", "#8B008B", "#5F9EA0", "#6B8E23", "#9932CC"), 1000)
    my_data_long <- pivot_longer(data = plot_data, cols = -Date, names_to = "Variable", values_to = "Value")
    if (class(data)[1] != "xts") {
        my_data_long$Date <- my_data_long$Date/x_size
    }
    if (x_start != 1) {
        my_data_long$Date <- (my_data_long$Date * x_step) + x_start
    }
    plot <- ggplot(my_data_long, aes(x = Date, y = Value, color = Variable)) + type(linewidth = linewidth, alpha = alpha) + labs(title = title, subtitle = subtitle, caption = caption, x = xlab, y = ylab) + scale_color_manual(name = legend_name, values = custom_palette) + theme(legend.position = legend_position[1], plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
    if (!show_legend) {
        plot <- plot + theme(legend.position = "none")
    }
    if (!show_x) {
        plot <- plot + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
    }
    if (facet_wrap) {
        plot <- plot + facet_wrap(~Variable)
        return(plot)
    }
    final_plot <- switch(plot_engine[1], ggplot = plot, plotly = ggplotly(plot) %>% layout(xaxis = list(rangeslider = list(visible = TRUE, thickness = 0.08)), yaxis = list(fixedrange = FALSE), dragmode = "zoom"))
    return(final_plot)
}

remove_outliers <- function (x, fill = c("mean", "median", "NA", "zero"), min = 0.25, max = 0.75) 
{
    Q1 <- quantile(x, min, na.rm = TRUE)
    Q3 <- quantile(x, max, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 2 * IQR
    upper_bound <- Q3 + 2 * IQR
    rep <- switch(fill[1], mean = mean(x, na.rm = TRUE), median = median(x, na.rm = TRUE), `NA` = NA, zero = 0)
    x[x < lower_bound | x > upper_bound] <- rep
    return(x)
}

RSS <- function (y, y_pred) 
{
    sum((y - y_pred)^2)
}

show_df <- function (prices, n = 5, rounding = Inf, name_first_col = "DATE") 
{
    price_date <- cbind(index(prices), smart_round(data.frame(prices), rounding))
    colnames(price_date) <- (c(name_first_col, colnames(prices)))
    rownames(price_date) <- (1:length(index(prices)))
    first_rows <- head(price_date, n)
    last_rows <- tail(price_date, n)
    separator <- matrix(NA, nrow = 1, ncol = ncol(price_date))
    colnames(separator) <- (c(name_first_col, colnames(prices)))
    summary_table <- bind_rows(first_rows, as.data.frame(separator), last_rows)
    return(summary_table)
}

smart_round <- function (df, digits = 2) 
{
    mutate(df, across(where(is.numeric), round, digits = digits))
}

#* MAIN CODE ----


#* Weather Derivatives Temperature Options ----

## setup ----
Sys.setlocale("LC_TIME", "English") # set output language in English
theme_set(theme_minimal())
knitr::opts_chunk$set(fig.align = 'center')

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
session_info <- sessionInfo()
session_info$R.version$version.string
session_info$platform
rm(session_info)

## functions_loaded ----
file <- "C:\\Users\\pietr\\OneDrive\\Desktop\\angolo in alto a destra\\quarto_climate_derivatives - Copia\\quarto_climate_derivatives.qmd"
when_rendering(functions_loaded(file))

## required_packages ----
when_rendering(required_packages(file))

## required_functions ----
when_rendering(required_functions(file))
when_rendering(dump_functions(file))

## github file ----

## time info ----
cat("time of creation", "\n")
print(file.info(file)$ctime, "\n")
cat("LAST MODIFICATION", "\n")
print(file.info(file)$mtime, "\n")
cat("Last Access", "\n")
print(file.info(file)$mtime, "\n")
cat("Last Render", "\n")
print(Sys.time(), "\n")

#* Climate data download ----

## MAP ----
map_data <- data.frame(
  name = "Location",
  lat = 51.5221,
  lon = -0.0903 
)
# Create a leaflet map
if (knitr::is_html_output()) {
  leaflet(map_data) %>%
    addTiles() %>% # Add default OpenStreetMap tiles
    addCircleMarkers(~lon, ~lat, radius = 5, color = "red", fillOpacity = 0.5) %>%
    setView(lng = map_data$lon, lat = map_data$lat, zoom = 13)
} else {
  print("interactive map of disneyworld in the html version")
}

## Dataset download ----
ORIGINAL_DATASET <- read.csv("London_DATA.csv")

DATASET <- ORIGINAL_DATASET %>% 
  mutate(T_MAX=remove_outliers(T2M_MAX, fill = "NA") %>% na.approx) %>% 
  mutate(T_MIN=remove_outliers(T2M_MIN, fill = "NA") %>% na.approx) %>% 
  mutate(DAY = as.Date(ORIGINAL_DATASET$DOY - 1, origin = paste0(ORIGINAL_DATASET$YEAR, "-01-01"))) %>%
  mutate(Month=month(DAY)) %>% 
  dplyr::select(DAY, YEAR, Month, DOY, T_MAX, T_MIN)
  

DATASET$T_AVG <- apply(DATASET[c("T_MAX", "T_MIN")], 1, mean)
sens_misfunctions <- sum(ifelse(find_outliers(ORIGINAL_DATASET$T2M_MAX)==0,FALSE,TRUE))

if (sens_misfunctions == 0) {
  print("No days where the sensors misfunctioned")
} else{
  ORIGINAL_DATASET[ifelse(find_outliers(ORIGINAL_DATASET$T2M_MAX)==0,FALSE,TRUE),] %>% 
  group_by(YEAR) %>% 
  summarise("Days" = length(DOY)) %>% 
  ggplot( aes(x = YEAR, y = Days))+
    geom_bar(stat = "identity") + 
    labs(x= NULL, y =NULL, title = "Days where the sensor malfunctioned", 
        subtitle = "Identified by remove outliers")
}

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

## polar viz ----
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


#* Overlap of the dataset ----

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
  c(DATASET[DATASET$YEAR == 2025, ]["T_MAX"]),
  c(DATASET[DATASET$YEAR == 2025, ]["T_AVG"]),
  c(DATASET[DATASET$YEAR == 2025, ]["T_MIN"])
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

## unnamed chunk ----
quickplot(SMA(DATASET$T_AVG, lookback), show_legend = F, title = "Long term Rolling average",
  x_start = min(DATASET$YEAR),x_step = 1 / 365.25)

quickplot(
  runSD(DATASET$T_AVG, lookback), show_legend = F, title = "Long term Standard deviation",
  x_start = min(DATASET$YEAR), x_step = 1 / 365.25)

#* Seasonal decomposition ----

## SEAS DEC + FOURIER ----
temps <- DATASET
apply_convolution <- function(x, kernel) {
  # Use filter() from stats package to apply convolution
  filtered <- stats::filter(x, kernel, sides = 2)  # Use sides = 2 for symmetric filter
  return(filtered)
}

kernel <- dnorm(-3:3)
data.frame("Gaussian_Kernel" = round(kernel, 10))

temps$Denoised <- apply_convolution(temps$T_AVG, kernel)
temps$Denoised <- na.fill(temps$Denoised, mean(temps$Denoised, na.rm = TRUE))

temps$Trend <- SMA(temps$Denoised, n = lookback)


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
MOD_params <- coef(fit)
confint_fit <- suppressMessages(confint(fit))

temps$SEAS <- MOD_params["alpha"] * sin(omega * temps$NUM_DAY + MOD_params["theta"])
temps$TREND <- MOD_params["a"] + MOD_params["b"] * temps$NUM_DAY
temps$BAR <- temps$TREND + temps$SEAS
temps$RESID <-  temps$T_AVG - temps$TREND - temps$SEAS

## model_stats ----
check_acc(temps$BAR, fitted(fit),15, title = "T_BAR VS fitted from the non linear squares")

check_acc(temps$RESID, residuals(fit),15, title = "T_BAR VS fitted from the non linear squares")

#* Model performance ----

## performance ----
# Print Model 
for (i in 1:length(MOD_params)) {
  cat(names(MOD_params)[i], ": ", round(MOD_params[i], 3), 
      " CI ~normally [", round(confint_fit[i, 1], 3), ",", round(confint_fit[i, 2], 3), "]\n")
  }

# Model performance
cat("  RSS model sine curve:", round(RSS(temps$T_AVG, temps$BAR), 2), "\n")
cat("  MAE model fit:", round(MAE(temps$BAR, temps$T_AVG), 2), "\n")

# fix the trend by using the linear trend
temps$Trend <- temps$Trend %>% na.fill(MOD_params["a"] + MOD_params["b"] * 1:lookback)

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
sigma <- sd(temps$RESID, na.rm = TRUE)                      # Volatility of the process
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

## vol modeling ----
# Create a dataframe with temperature and date components
temp_t <- data.frame(
  Date = temps$DAY,  # Assuming the index is a proper Date object
  T = temps$Denoised,
  day = yday(temps$DAY),
  month = month(temps$DAY)
)

vol_df <- temps %>%
  select(DAY, T_AVG) %>%
  mutate(day = yday(DAY),
        month = month(DAY)) %>%
  group_by(day) %>%
  summarise(mean = mean(T_AVG, na.rm = TRUE),
            std = sd(T_AVG, na.rm = TRUE))

# just a shorthand for reference 
x <- 1:366
y <- vol_df$std

quickplot(vol_df$std, type = geom_point, show_legend = F, xlab = "Day of the year", ylab = "Temperature", title = "Temperature volatility")

#* Polynomial Regression: ----

## polynomial ----
create_poly_plot <- function(degree, x, y) {
  # Fit polynomial model
  model <- lm(y ~ poly(x, degree, raw = TRUE))
  yfit <- predict(model, data.frame(x = x))
  
  rss <- sum(resid(model)^2)
  n <- 366
  aic <- n*log(rss/n)+2*degree

  plot <- ggplot() +
    geom_point(aes(x, y), color = '#00962B', size = 1) +
    geom_line(aes(x, yfit), color = 'black', linewidth = 1) +
    ggtitle(paste("Degree:", degree, "\nRSS:", round(rss, 2), "AIC:", round(aic, 2))) +
    labs(y = NULL, x = NULL) +
    theme_classic() +
    theme(plot.title = element_text(size = 10, face = "bold"))
  
  return(list(
    plot = plot,
    degree = degree,
    rss = rss,
    aic = aic
  ))
}

degrees <- 1:9
results <- lapply(degrees, create_poly_plot, x = x, y = y)

# Extract plots
plots <- lapply(results, function(x) x$plot)

grid.arrange(grobs = plots, nrow = 3, ncol = 3, left = "Standard deviation of temperatures", bottom = "Day of the year", top = "Different polynomial models")

# Extract metrics into a data frame
poly_metrics_df <- data.frame(
  parameter = sapply(results, function(x) x$degree),
  rss = sapply(results, function(x) x$rss),
  aic = sapply(results, function(x) x$aic)) %>% 
    set_rownames(paste0("Polynomial",1:9))

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
  data.frame() %>% 
    quickplot(title = "Fourier transformation different components",
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

  rss <- sum(resid(fourier_fit)^2)
  n <- 366
  aic <- n*log(rss/n)+2*n_terms

  plot <- ggplot(data = data.frame(), aes(x = x)) +
    geom_point(aes(y = y), color = 'orangered3', size = 1) +
    geom_line(aes(y = fourier), color = 'black', linewidth = 1) +
    ggtitle(paste("Order:", n_terms, "\nRSS:", round(rss, 2), "AIC:", round(aic, 2))) +
    labs(y = NULL, x = NULL) +
    theme_classic() 

    # Return both plot and metrics
  return(list(
    plot = plot,
    order = n_terms,
    rss = rss,
    aic = aic
  ))
}

order <- c(0:8)

results <- lapply(order, create_fourier_plot, x = x, y = y)

plots <- lapply(results, function(x) x$plot)

grid.arrange(grobs = plots, nrow = 3, ncol = 3,
  left = "Standard deviation of temperatures",
  bottom = "Day of the year",
  top = "Different Fourier Series models")

# Extract metrics into a data frame
fourier_metrics_df <- data.frame(
  parameter = sapply(results, function(x) x$order),
  rss = sapply(results, function(x) x$rss),
  aic = sapply(results, function(x) x$aic)) %>% 
    set_rownames(paste0("Fourier",1:9))

#* B-splines ----

## B-splines ----
create_spline_plot <- function(knots, x, y) {
  # Fit the spline model
  spline_model <- lm(y ~ bs(x, knots = knots))
  yfit <- predict(spline_model, data.frame(x = x))
  
  rss <- sum((y - yfit)^2)
  aic <- length(x) * log(rss / length(x)) + 2 * knots

  plot <- ggplot() +
    geom_point(aes(x, y), color = 'cornflowerblue', size = 1.5) +
    geom_line(aes(x, yfit), color = 'black', linewidth = 1) +
    ggtitle(paste("Knots #", knots, "\nRSS:", round(rss, 2), "AIC:", round(aic,2))) +
    labs(x = NULL, y = NULL) +
    theme_classic()

  # Return both plot and metrics
  return(list(
    plot = plot,
    knots = knots,
    rss = rss,
    aic = aic
  ))
}

knots <- c(1, 3, 5, 10, 15, 20, 30, 50, 80)
results <- lapply(knots, create_spline_plot, x = x, y = y)

plots <- lapply(results, function(x) x$plot)

grid.arrange(grobs = plots, nrow = 3, ncol = 3, left = "Standard deviation of temperatures", bottom = "Day of the year", top = "Different B-splines models")

spline_metrics_df <- data.frame(
  parameter = sapply(results, function(x) x$knots),
  rss = sapply(results, function(x) x$rss),
  aic = sapply(results, function(x) x$aic)
) %>% 
    set_rownames(paste0("B-Spline",1:9))

#* Kernel smoothing ----

## kernel smooth ----
kernel_smooth <- function(x_eval, x_data, y_data, h_val=10) {
    weights <- exp(-0.5 * ((x_eval - x_data) / h_val)^2)
    weights <- weights / sum(weights)
    return(sum(weights * y_data))
}

create_kernel_plot <- function(x, y, bandwidth) {  
  y_pred <- sapply(x, function(xi) kernel_smooth(xi, x, y, bandwidth))
  
  rss <- sum((y - y_pred)^2)
  n <- length(y)
  aic <- n * log(rss/n) + 2*bandwidth
  
  data_plot <- data.frame(x = x, y = y, y_pred = y_pred)
  
  plot <- ggplot(data_plot, aes(x = x)) +
    geom_point(aes(y = y), color = 'darkolivegreen4', size = 1, alpha = 0.7) +
    geom_line(aes(y = y_pred), color = 'black', linewidth = 1.2) +
    ggtitle(paste("Bandwidth:", bandwidth, "\nRSS:", round(rss, 2), "AIC:", round(aic, 2))) +
    labs(y = NULL, x = NULL) +
    theme_classic()   
  return(list(plot = plot, rss = rss, aic = aic, bandwidth = bandwidth))
}

width <- floor(seq(5, 50, length.out = 9))
results <- lapply(width, create_kernel_plot, x = x, y = y)

# Extract plots
plots <- lapply(results, function(x) x$plot)

grid.arrange(grobs = plots, nrow = 3, ncol = 3,
            left = "Standard deviation of temperatures",
            bottom = "Day of the year",
            top = "Different KERNEL smoothing models")

# Extract metrics into data frame
kernel_metrics_df <- data.frame(
  parameter = sapply(results, function(x) x$bandwidth),
  rss = sapply(results, function(x) x$rss),
  aic = sapply(results, function(x) x$aic)) %>% 
    set_rownames(paste0("Kernel-smoothing",1:9))

#* Loess modelling ----

## loess ----
create_loess_plot <- function(span_val, x, y) {
  # Fit LOESS model
  loess_fit <- loess(y ~ x, span = span_val)
  loess_pred <- predict(loess_fit, newdata = data.frame(x = x))
  
  rss <- sum((y - loess_pred)^2)
  
  # Effective degrees of freedom from loess
  df <- loess_fit$enp  # "equivalent number of parameters"
  n <- 366
  aic <- n * log(rss/n) + 2 * df
  
  plot <- ggplot(data = data.frame(x = x, y = y, fit = loess_pred), aes(x = x)) +
    geom_point(aes(y = y), color = 'violetred4', size = 1) +
    geom_line(aes(y = fit), color = 'black', linewidth = 1) +
    ggtitle(paste("Span:", span_val, "\nRSS:", round(rss, 2), "AIC:", round(aic, 2))) +
    labs(y = NULL, x = NULL) +
    theme_classic()
  
  # Return both plot and metrics
  return(list(
    plot = plot,
    span = span_val,
    rss = rss,
    aic = aic
  ))
}

spans <- seq(0.1, 0.9, by = 0.1)
results <- lapply(spans, create_loess_plot, x = x, y = y)

# Extract plots
plots <- lapply(results, function(x) x$plot)

grid.arrange(grobs = plots, nrow = 3, ncol = 3,
            left = "Standard deviation of temperatures",
            bottom = "Day of the year",
            top = "Different LOESS smoothing models")

# Extract metrics into data frame
loess_metrics_df <- data.frame(
  parameter = sapply(results, function(x) x$span),
  rss = sapply(results, function(x) x$rss),
  aic = sapply(results, function(x) x$aic)) %>% 
    set_rownames(paste0("LOESS",1:9))


#* GAM ----

## GAM ----

create_gam_plot <- function(x, y, k_value) {
  # Fit GAM with cyclic smooth for seasonal data
  gam_model <- gam(y ~ s(x, bs = "cc", k = k_value), weights = rep(1, length(y)))
  y_pred <- predict(gam_model)
  
  rss <- sum((y - y_pred)^2)
  n <- 366
  edf <- sum(gam_model$edf)  # effective degrees of freedom
  aic <- n * log(rss/n) + 2 * edf
  
  data_plot <- data.frame(x = x, y = y, y_pred = y_pred)
  
  plot <- ggplot(data_plot, aes(x = x)) +
    geom_point(aes(y = y), color = 'purple4', size = 1, alpha = 0.7) +
    geom_line(aes(y = y_pred), color = 'black', linewidth = 1.2) +
    ggtitle(paste("K:", k_value, "\nRSS:", round(rss, 2), 
                  "AIC:", round(aic, 2))) +
    theme_classic() +
    labs(y = NULL, x = NULL)
  
  return(list(plot = plot, k_value = k_value, rss = rss, aic = aic))
}

k_values <- floor(seq(4, 20, length.out = 9))
results <- lapply(k_values, create_gam_plot, x = x, y = y)

# Extract plots
plots <- lapply(results, function(x) x$plot)

grid.arrange(grobs = plots, nrow = 3, ncol = 3,
            left = "Standard deviation of temperatures",
            bottom = "Day of the year",
            top = "Different GAM smoothing models")

# Extract metrics into data frame
gam_metrics_df <- data.frame(
  parameter = sapply(results, function(x) x$k_value),
  rss = sapply(results, function(x) x$rss),
  aic = sapply(results, function(x) x$aic)) %>% 
    set_rownames(paste0("GAM",1:9))

#* Figuring out which is the best model ----

## best model ----
comparison_df <- rbind(
  poly_metrics_df,
  fourier_metrics_df,
  spline_metrics_df,
  kernel_metrics_df,
  loess_metrics_df,
  gam_metrics_df
) %>%
  set_colnames(c("Parameter", "RSS", "AIC"))


summary_comparison_df <- data.frame()

for (i in 1:6) {
  summary_comparison_df <- rbind(
    summary_comparison_df,
    (apply(comparison_df[seq((i - 1) * 9 + 1, 9 * i), ], 2, mean))
  )
}
summary_comparison_df[, 1] <- c(
  "Polynomial",
  "Fourier",
  "B-spline",
  "Kernel",
  "Loess",
  "GAM"
)
colnames(summary_comparison_df) <- c("Model", "RSS", "AIC")
summary_comparison_df$AIC <- abs(summary_comparison_df$AIC)


summary_comparison_df$RSS %>%
  set_names(summary_comparison_df$Model) %>%
  quickbarplot(title = "RSS value of all models", palette = "navyblue") +
  theme(plot.title = element_text(size = 20)) +
  coord_cartesian(
    ylim = c(
      min(summary_comparison_df$RSS) * 0.8,
      max(summary_comparison_df$RSS) * 1.1
    )
  )

summary_comparison_df$AIC %>%
  set_names(summary_comparison_df$Model) %>%
  quickbarplot(title = "AIC value of all models", palette = "darkred") +
  theme(plot.title = element_text(size = 20)) +
  coord_cartesian(
    ylim = c(
      min(summary_comparison_df$AIC) * 0.9,
      max(summary_comparison_df$AIC) * 1.05
    )
  )

head(comparison_df[order(comparison_df$AIC), ]) %>%
  gt(rownames_to_stub = T) %>%
  opt_stylize(5, color = "blue") %>%
  tab_header("Best model according to AIC")
head(comparison_df[order(comparison_df$RSS), ]) %>%
  gt(rownames_to_stub = T) %>%
  opt_stylize(5, color = "green") %>%
  tab_header("Best model according to RSS")


best_model <- comparison_df[which.min(comparison_df$AIC), ]
best_model_name <- gsub("[0-9]", "", (rownames(best_model)))
parameter <- switch(best_model_name,
  "Polynomial" = "degrees",
  "Fourier" = "order",
  "B-Spline" = "knots",
  "Kernel-smoothing" = "bandwidth",
  "LOESS" = "span",
  "GAM" = "k values",
  stop("Unknown model: ", best_model_name) # error
)

## AR on vol_DOY ----
# 3. Print long-term volatility metrics
cat("Trend or long term volatility ~", round(mean(vol_df$std, na.rm = TRUE), 3), "\n")
cat("Gamma is ~", round(sd(vol_df$std, na.rm = TRUE), 3), "\n")

# 4. Fit AR(1) model for mean reversion rate
ar_model <- arima(vol_df$std, order = c(1, 0, 0), include.mean = FALSE)
coef <- ar_model$coef
residuals <- ar_model$residuals

cat("Rate of mean reversion of volatility process is:", round(coef["ar1"], 3), "\n")
summary(ar_model)

#* Montecarlo simulations ----

## Montecarlo model ----
a <- MOD_params[1]
b <- MOD_params[2]
theta <- atan2(MOD_params[3], MOD_params[4])
alpha <- sqrt(MOD_params[3]^2 + MOD_params[4]^2)
kappa <- as.double(kappa)

data.frame(a = a, b = b, theta = theta, alpha = alpha, kappa = kappa) %>%
  set_rownames("value")

# Temperature Model Functions
T_model <- function(x, a, b, alpha, theta) {
  omega <- 2 * pi / 365.25
  a + b * x + alpha * sin(omega * x + theta)
}
dT_model <- function(x, a, b, alpha, theta) {
  omega <- 2 * pi / 365.25
  b + alpha * omega * cos(omega * x + theta)
}

# Apply Model with Given Parameters
Tbar_params <- list(a = a, b = b, alpha = alpha, theta = theta)
temp_t$ordinal <- as.numeric(temps$DAY) - as.numeric(temps$DAY[1]) # x values
first_ordinal_val <- as.numeric(temps$DAY[1])

temps$model_fit <- T_model(
  temp_t$ordinal,
  Tbar_params$a,
  Tbar_params$b,
  Tbar_params$alpha,
  Tbar_params$theta
)

## final volatility ----
# Big switch function to get the correct model VALUES
fit_volatility <- switch(
  best_model_name,
  "Polynomial" = {
    yfit <- predict(lm(y ~ poly(x, parameter, raw = TRUE)), data.frame(x = x))
  },
  "Fourier" = {
    model_data <- data.frame(y = y, fourier_series(1:366, n_terms = parameter))
    fourier_fit <- lm(y ~ ., data = model_data)
    yfit <- predict(fourier_fit, newdata = model_data)
  },
  "B-Spline" = {
    yfit <- predict(lm(y ~ bs(x, knots = parameter)), data.frame(x = x))
  },
  "Kernel-smoothing" = {
    yfit <- sapply(x, function(xi) kernel_smooth(xi, x, y, ))
  },
  "LOESS" = {
    loess_fit <-
      yfit <- predict(
        loess(y ~ x, span = parameter),
        newdata = data.frame(x = x)
      )
  },
  "GAM" = {
    yfit <- predict(gam(
      y ~ s(x, bs = "cc", k = parameter),
      weights = rep(1, length(y))
    ))
  },
  # Default: error if unknown type
  stop("Unknown model type: ", best_model_name)
)
vol_df$fvol <- yfit

# Plot Volatility
ggplot(vol_df, aes(x = day)) +
  geom_point(aes(y = std, color = "Observed Volatility"), size = 2) +
  geom_line(aes(y = fvol, color = "Estimate volatility"), size = 2) +
  scale_color_manual(
    NULL,
    values = c("Observed Volatility" = "blue", "Estimate volatility" = "black")
  ) +
  labs(
    title = paste(
      "Temperature Volatility final using",
      best_model_name,
      best_model$Parameter
    ),
    y = "Std Dev (°C)",
    x = "Day of Year"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

## functions ----
# REAL Monte Carlo Simulation Functions
euler_step <- function(row, kappa, M) {
  T_i <- ifelse(is.na(row$Tbar_shift), row$Tbar, row$Tbar_shift)
  T_det <- T_i + row$dTbar
  T_mrev <- kappa * (row$Tbar - T_i)
  sigma <- row$vol * rnorm(M)
  T_det + T_mrev + sigma
}

monte_carlo_temp <- function(trading_dates, Tbar_params, vol_df, 
                            first_ordinal, M = 1, kappa = 0.226139) {
  # Convert dates to numeric if needed
  trading_numeric <- as.numeric(trading_dates)
  kappa_val <- as.double(kappa)

  # Calculate Tbar and dTbar
  x_vals <- trading_numeric - first_ordinal_val
  Tbars <- T_model(
    x_vals,
    Tbar_params$a,
    Tbar_params$b,
    Tbar_params$alpha,
    Tbar_params$theta
  )

  dTbars <- dT_model(
    x_vals,
    Tbar_params$a,
    Tbar_params$b,
    Tbar_params$alpha,
    Tbar_params$theta
  )

  # Create simulation dataframe
  mc_temps <- data.frame(
    Date = trading_dates,
    Tbar = Tbars,
    dTbar = dTbars,
    day = yday(trading_dates),
    vol = data.frame(vol_df)$fvol[yday(trading_dates)] # Directly add volatility
  )

  # Add lagged Tbar
  mc_temps$Tbar_shift <- dplyr::lag(mc_temps$Tbar)

  # Run simulations - modified apply call
  simulations <- sapply(1:nrow(mc_temps), function(i) {
    row <- mc_temps[i, ]
    euler_step(row, kappa_val, M)
  })

  # Transpose and format results
  simulations <- t(simulations)
  colnames(simulations) <- paste0("Sim", 1:M)

  list(
    mc_temps = mc_temps,
    mc_sims = cbind(Date = trading_dates, as.data.frame(simulations))
  )
}

## 1sim ----
# Run Simulation
one_year_trading_dates <- seq(as.Date("2024-12-31"), as.Date("2025-12-31"), by = "day")
sim_results <- monte_carlo_temp(
  one_year_trading_dates,
  Tbar_params,
  vol_df,
  first_ordinal_val,
  M = 5
)

piv_sim <- pivot_longer(sim_results$mc_sims, -Date, names_to = "Simulation", values_to = "Temperature")

MIN_MAX <- data.frame(
      "MIN" = apply_convolution(apply(pivot_df[-1], 1, quantile, 0.05 , na.rm = TRUE),dnorm(-3:3)),
      "MAX" = apply_convolution(apply(pivot_df[-1], 1, quantile, 0.95, na.rm = TRUE),dnorm(-3:3))
    )

suppressWarnings(
  ggplot() +
  geom_ribbon(data = MIN_MAX, aes(x = one_year_trading_dates, ymin = MIN, ymax = MAX), , alpha = 0.2) +
  geom_ribbon(data = MIN_MAX, aes(x = one_year_trading_dates, ymin = MIN-2*vol_df$fvol, ymax = MAX+2*vol_df$fvol), alpha = 0, color = "red") +
  geom_point(data = piv_sim, aes(x = Date, y = Temperature, color = Simulation), alpha = 0.7) +
labs(title = "Monte Carlo Temperature Simulations", y = "Temperature (°C)", x = "Day of the year") +
  guides(fill = "none") + 
  theme(legend.position = "bottom"))

## plot of model components ----
trading_dates_year <- seq(as.Date("2024-12-31"), as.Date("2025-12-31"), by = "day")

x_vals <- as.numeric(trading_dates_year) - as.numeric(trading_dates_year)[1]
Tbar_vals <- T_model(x_vals, a, b, alpha, theta)
dTbar_vals <- dT_model(x_vals, a, b, alpha, theta)
sigma_vals <- data.frame(vol_df)$fvol[yday(trading_dates_year)]

data.frame(
  x_vals = x_vals,
  Tbar_vals = Tbar_vals,
  dTbar_vals = dTbar_vals,
  sigma_vals = sigma_vals
) %>%
  show_df()

grid.arrange(top = "Components of the Montecarlo simulation divided", ncol = 1,
  quickplot(Tbar_vals, subtitle = "Deterministic Tbar", show_legend = F, linewidth = 1, show_x = F, xlab = NULL, ylab = NULL),
  quickplot(dTbar_vals, subtitle = "Deterministic Tbar shift", show_legend = F, linewidth = 1, show_x = F, xlab = NULL, ylab = NULL),
  quickplot(sigma_vals, subtitle = "Volatility model", show_legend = F, linewidth = 1, show_x = F, xlab = NULL, ylab = NULL),
  quickplot(monte_carlo_temp(trading_dates_year, Tbar_params, vol_df, first_ordinal_val, M = 2)$mc_sims[2], subtitle = "Montecarlo result of 1 simulaiton", show_legend = F, linewidth = 1, show_x = F, xlab = F, ylab = NULL))

## MNC sym ----
# Set number of simulations
no_sims <- 100000

# Define winter and summer dates
trading_dates_winter <- as.Date("2025-04-01")
trading_dates_summer <- as.Date("2025-10-01")

# Run simulations
sim_results_winter <- monte_carlo_temp(trading_dates_winter, Tbar_params, vol_df, first_ordinal_val, M = no_sims)
sim_results_summer <- monte_carlo_temp(trading_dates_summer, Tbar_params, vol_df, first_ordinal_val, M = no_sims)


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


# Create the plot
ggplot(plot_data, aes(x = Temperature, fill = Season)) +
  geom_histogram(position = "identity", alpha = 0.9, bins = 80) +
  geom_freqpoly(data=winter_dataset, aes(x = T_AVG, y = after_stat(count*10), color = "Hist Winter"),  bins = 80, linewidth = 2)+
  geom_freqpoly(data=summer_dataset, aes(x = T_AVG, y = after_stat(count*10), color = "Hist Summer"),  bins = 80, linewidth = 2)+
  geom_vline(aes(xintercept = Tbar_winter), color = darken("steelblue", 0.3), linewidth = 2, linetype = "solid") +
  geom_vline(aes(xintercept = Tbar_summer), color = darken("orange", 0.3), linewidth = 2, linetype = "solid") +
  scale_fill_manual(values = c(Winter = "steelblue", Summer = "orange")) +
  scale_color_manual(name = NULL, values = c("Hist Winter" = "navy", "Hist Summer" = "darkorange")) +
  labs(title = "Comparison of both historical and simulated", x = "Temperature (°C)", y = "Simulated Frequency") +
  theme(legend.position = "bottom") +
  scale_y_continuous(name = "Frequency", sec.axis = sec_axis(~ . * 0.07, name = "Historical Frequency"))


grid.arrange( top = "Cumulative Density: Simulated vs Observed", nrow = 1,
suppressWarnings(compare_cdf(unlist(mc_sims_summer), summer_dataset$T_AVG, "Summer dataset", "darkorange")),
suppressWarnings(compare_cdf(unlist(mc_sims_winter), winter_dataset$T_AVG, "Winter dataset", "steelblue")))

#* Risk neutral pricing ----

## coldest summer ----
temps_checked <- 5:28
prob_no_payout_df <- data.frame(matrix(numeric(length(temps_checked)*3), ncol = 3)) %>% 
  set_colnames(c("temps", "summer", "winter"))
prob_no_payout_df[, 1] <- temps_checked

prob_sim_summer <- as.numeric(sim_results_summer$mc_sims[-1])
counter <- 1
for (n in temps_checked) {
  payoffs <- ifelse(prob_sim_summer >= n, 0, n - prob_sim_summer)
  prob_no_payout <- mean(payoffs == 0) * 100
  prob_no_payout_df[counter,2] <- prob_no_payout
  cat(paste0("Probability P(max(",n,"-Tn, 0) = 0): ", prob_no_payout, "%","\n"))
  counter <- counter+1
}

## hottest winter ----
prob_sim_winter <- as.numeric(sim_results_winter$mc_sims[-1])

counter <- 1
for (n in temps_checked) {
    payoffs <- ifelse(prob_sim_winter <= n, 0, n - prob_sim_winter)
  prob_no_payout <- mean(payoffs == 0) * 100
  cat(paste0("Probability P(max(",n,"-Tn, 0) = 0): ", prob_no_payout, "%","\n"))
  prob_no_payout_df[counter,3] <- prob_no_payout
  counter <- counter+1
}

ggplot(prob_no_payout_df, aes(x = temps))+
  geom_line(aes(y = summer, color = "Summer"))+
  geom_line(aes(y = winter, color = "Winter"))

#* Market model ----

## final model ----
temperature_option <- function(trading_dates, no_sims, Tbar_params, vol_df, 
r = 0.05, alpha = 2500, K = 300, T_maturity = 1, first_ordinal = first_ordinal_val, option_type = "call", ref_temperature = 18) {
  T_maturity_mod <- (max(as.numeric(trading_dates))-min(as.numeric(trading_dates)))/365.25
  if (T_maturity==1) {
    T_maturity <- T_maturity_mod
  }
  # Run Monte Carlo temperature simulation (assumes your R version exists)
  mc_res <- monte_carlo_temp(trading_dates, Tbar_params, vol_df, first_ordinal, no_sims)
  mc_sims <- mc_res$mc_sims   # matrix/dataframe of simulated temps
  
  M <- ncol(mc_sims)  # number of simulations
  N <- nrow(mc_sims)  # number of time steps (not directly needed)
  
  mc_arr <- as.matrix(mc_sims[,-1])
  
  # Cooling degree days (CDD) or similar metric
  DD <- colSums(pmax(ref_temperature - mc_arr, 0))
  
  # Option payoff
  if (option_type == "call") {
    Payoff <- alpha * pmax(DD - K, 0)
  } else {
    Payoff <- alpha * pmax(K - DD, 0)
  }
  
  # Discounted expected payoff
  C0 <- exp(-r * T_maturity) * mean(Payoff)
  
  # Standard error of estimator
  sigma <- sqrt(sum((exp(-r * T_maturity) * Payoff - C0)^2) / (M - 1))
  SE <- sigma / sqrt(M)
  
  return(list(price = C0, se = SE))
}

trading_dates <- seq(as.Date("2024-12-01"), as.Date("2025-03-01"), by = "day")

temperature_option(trading_dates, 10000, Tbar_params, vol_df, r = 0.05, alpha = 2500, K = 300, T_maturity = 1, first_ordinal, option_type = "put", ref_temperature = 18) %>% bprint()

temperature_option(trading_dates, 10000, Tbar_params, vol_df, r = 0.05, alpha = 2500, K = 300, T_maturity = 1, first_ordinal, option_type = "call", ref_temperature = 18) %>% bprint()

## loop to get graph ----
cat("maturity =", (max(as.numeric(trading_dates))-min(as.numeric(trading_dates)))/365.25, "years")
strikes <- seq(450,650, length.out = 20)
ref_t <- 10
nsim <- 10000
alpha <- 5000
prices_options <- data.frame(matrix(numeric(length(strikes)*4),ncol = 4)) %>% 
  set_colnames(c("put", "call", "SE_put", "SE_call"))
counter <- 1
for (i in strikes) {
 PUT <- temperature_option(trading_dates, nsim, Tbar_params, vol_df, ref_temperature = ref_t, K = i, option_type = "put", alpha = alpha)
  CALL <- temperature_option(trading_dates, nsim, Tbar_params, vol_df, ref_temperature = ref_t, K = i, option_type = "call", alpha = alpha)

  prices_options[counter,1] <- PUT$price
  prices_options[counter,2] <- CALL$price
  prices_options[counter,3] <- PUT$se
  prices_options[counter,4] <- CALL$se
counter <- counter+1
}

ggplot(cbind(prices_options,strikes), aes(x = strikes))+ 
  geom_line(aes(y = put,  color = "put")) + 
    geom_point(aes(y = put,  color = "put")) + 
  geom_line(aes(y = call, color = "call")) +
    geom_point(aes(y = call, color = "call")) +
    labs(title = "Price charts of the Montecarlo options", y = "price")

grid.arrange(top = "Standard error chart of the option at different strike prices",
  ggplot(cbind(prices_options,strikes), aes(x = strikes))+ 
    geom_col(aes(y = SE_put,  fill = "put")) + 
    scale_fill_manual(NULL, values = "darkred") + 
    ylab(NULL) + 
    theme(legend.position = "bottom"),

  ggplot(cbind(prices_options,strikes), aes(x = strikes))+ 
    geom_col(aes(y = SE_call, fill = "call"))+
    ylab(NULL) + 
    scale_fill_manual(NULL, values = "darkblue")+ 
    theme(legend.position = "bottom")
)

# Add this debug code:
mc_res <- monte_carlo_temp(trading_dates, Tbar_params, vol_df, first_ordinal, 100)
mc_sims <- mc_res$mc_sims   # matrix/dataframe of simulated temps
M <- ncol(mc_sims)  # number of simulations
N <- nrow(mc_sims)  # number of time steps (not directly needed)
mc_arr <- as.matrix(mc_sims[,-1])

dd_sample <- colSums(pmax(ref_t - mc_arr, 0))
cat("DD stats: mean =", mean(dd_sample), ", range =", range(dd_sample), "\n")
cat("Strikes in range?", any(strikes > min(dd_sample) & strikes < max(dd_sample)), "\n")

## unnamed chunk ----
quickplot(mc_res$mc_sims[, -1][1:20], show_legend = F,
  title = "Temperature Simulation Paths with the deterministic path") +
  geom_line(
    data = data.frame(x = 1:length(trading_dates), y = mc_res$mc_temps$Tbar),
    aes(x = x, y = y),
    color = "black",
    linewidth = 2
  )

temp_diffs <- apply(mc_arr, 2, function(x) x - mc_res$mc_temps$Tbar)[, 1:20]
daily_vol <- apply(mc_arr, 1, sd)

grid.arrange(
  quickplot(temp_diffs, title = "Difference from the deterministic path", show_legend = F),
  quickplot(daily_vol, title = "Daily Volatility Across Simulations", show_legend = F)
)
cat("Mean deviation from seasonal average:", mean(abs(temp_diffs)), "\n")

cat("Volatility range:", range(daily_vol), "\n")

#* Greek Numerical Approximations ----

## unnamed chunk ----
shock <- 0.1
params_up <- Tbar_params
params_down <- Tbar_params
params_up$a <- Tbar_params$a + shock
params_down$a <- Tbar_params$a - shock

price_up   <- temperature_option(trading_dates, 1000, params_up, vol_df, option_type = "call"  , alpha = 1)$price
price_down <- temperature_option(trading_dates, 1000, params_down, vol_df, option_type = "call", alpha = 1)$price

delta <- (price_up - price_down) / (2 * shock)

data.frame(price_up, price_down, delta) %>% 
  gt() %>% cols_label(price_up = "Price up", price_down = "Price down", delta = "Delta") %>% 
  opt_stylize(5) %>% tab_header(paste("Delta with shock =",shock))

## unnamed chunk ----
shock <- 0.1
vol_up <- vol_df
vol_down <- vol_df
vol_up$fvol <- vol_df$fvol + shock
vol_down$fvol <- vol_df$fvol - shock

price_up   <- temperature_option(trading_dates, 1000, params_up, vol_up, option_type = "call"  , alpha = 1)$price
price_down <- temperature_option(trading_dates, 1000, params_down, vol_down, option_type = "call", alpha = 1)$price

vega <- (price_up - price_down) / (2 * shock)

data.frame(price_up, price_down, vega) %>% 
  gt() %>% cols_label(price_up = "Price up", price_down = "Price down", vega = "Vega") %>% 
  opt_stylize(5) %>% tab_header(paste("Vega with shock =",shock))

## unnamed chunk ----
shock <- 0.1
params_up <- Tbar_params
params_down <- Tbar_params
params_up$alpha <- Tbar_params$alpha + shock
params_down$alpha <- Tbar_params$alpha - shock

price_up   <- temperature_option(trading_dates, 1000, params_up, vol_df, option_type = "call"  , alpha = 1)$price
price_down <- temperature_option(trading_dates, 1000, params_down, vol_df, option_type = "call", alpha = 1)$price

alpha_g <- (price_up - price_down) / (2 * shock)

data.frame(price_up, price_down, alpha_g) %>% 
  gt() %>% cols_label(price_up = "Price up", price_down = "Price down", alpha_g = "Alpha greek") %>% 
  opt_stylize(5) %>% tab_header(paste("Seasonality greek with shock =",shock))

## unnamed chunk ----
shock <- 3
price_up   <- temperature_option(trading_dates, 1000, Tbar_params, vol_df, option_type = "call", alpha = 1, ref_temperature = 18+shock)$price
price_down <- temperature_option(trading_dates, 1000, Tbar_params, vol_df, option_type = "call", alpha = 1, ref_temperature = 18-shock)$price

temp_g <- (price_up - price_down) / (2 * shock)

data.frame(price_up, price_down, temp_g) %>% 
  gt() %>% cols_label(price_up = "Price up", price_down = "Price down", temp_g = "Reference T Greek") %>% 
  opt_stylize(5) %>% tab_header(paste("Reference Temperature Greek with shock =",shock))

## unnamed chunk ----
days_elapsed <- 3

price_t0 <- temperature_option(trading_dates, 1000, Tbar_params, vol_df, option_type = "call", alpha = 1)$price

# Option after some days have passed (shorter remaining period)
remaining_dates <- trading_dates[(days_elapsed):length(trading_dates)]
price_t1 <- temperature_option(remaining_dates, 1000, Tbar_params, vol_df, option_type = "call", alpha = 1)$price

theta <- (price_t1 - price_t0) / days_elapsed

data.frame(price_up, price_down, temp_g) %>% 
  gt() %>% cols_label(price_up = "Price T0", price_down = "Price T1", temp_g = "Reference T Greek") %>% 
  opt_stylize(5) %>% tab_header(paste("Theta with shock =", days_elapsed))

#* Sidequests ----

#* Sidequest: predicting temperatures in the future ----

## forecast compared to real ----
model_formula <- function(t, a, b, a1, b1) {
  omega <- 2 * pi / 365.25
  a + b * t + a1 * cos(omega * t) + b1 * sin(omega * t)
}

forecast_N <- 30
new_t <- max(temps$NUM_DAY) + forecast_N  # forecast_N days after last observation
predicted_T <- model_formula(new_t, MOD_params["a"], MOD_params["b"], MOD_params["alpha"], MOD_params["theta"])
cat("predicted temperature for", as.character(max(temps$DAY)+forecast_N), "=", predicted_T)

arima_model <- Arima(temps$RESID, order = c(2, 0, 0), include.mean = FALSE)
future_residuals <- forecast(arima_model, h = forecast_N)  # forecast_N steps ahead

first_date <- min(temps$DAY)
future_dates <- seq(max(temps$DAY), by = "day", length.out = forecast_N)
future_t <- as.numeric(difftime(future_dates, first_date, units = "days"))

# Deterministic part
deterministic_part <- model_formula(future_t, MOD_params["a"], MOD_params["b"], MOD_params["alpha"], MOD_params["theta"])

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

#* Milan comparison ----

## Milan comparison ----
MILAN_RAW <- read.csv("Milan_DATA.csv", skip = 10)

MIL_DATASET <- MILAN_RAW %>%
  mutate(
    T_MAX = remove_outliers(T2M_MAX, fill = "NA") %>% na.approx(),
    T_MIN = remove_outliers(T2M_MIN, fill = "NA") %>% na.approx(),
    DAY   = as.Date(DOY - 1, origin = paste0(YEAR, "-01-01")),
    Month = month(DAY),
    T_AVG = (T_MAX + T_MIN) / 2
  ) %>%
  select(DAY, YEAR, Month, DOY, T_MAX, T_MIN, T_AVG)

# Pivot just once for Milan (average only)
MIL_pivot_df <- MIL_DATASET %>%
  select(DOY, YEAR, T_AVG) %>%
  pivot_wider(names_from = YEAR, values_from = T_AVG)

# Build comparison dataset (Milan + London summary stats)
FD <- data.frame(
  DOY      = MIL_pivot_df$DOY,
  MIL_MIN  = apply(MIL_pivot_df[-1], 1, min, na.rm = TRUE),
  MIL_MAX  = apply(MIL_pivot_df[-1], 1, max, na.rm = TRUE),
  MIL_MEAN = apply(MIL_pivot_df[-1], 1, mean, na.rm = TRUE),
  LON_MIN  = apply(pivot_df[-1], 1, min, na.rm = TRUE),
  LON_MAX  = apply(pivot_df[-1], 1, max, na.rm = TRUE),
  LON_MEAN = apply(pivot_df[-1], 1, mean, na.rm = TRUE)
)

# Plot comparison
ggplot(FD, aes(x = DOY)) +
  geom_ribbon(aes(ymin = MIL_MIN, ymax = MIL_MAX, fill = "Milan"), alpha = 0.2) +
  geom_ribbon(aes(ymin = LON_MIN, ymax = LON_MAX, fill = "London"), alpha = 0.2) +
  geom_line(aes(y = MIL_MEAN, color = "Milan Avg"), linewidth = 1) +
  geom_line(aes(y = LON_MEAN, color = "London Avg"), linewidth = 1) +
  scale_color_manual(name = NULL, values = c("Milan Avg" = "darkblue", "London Avg" = "orange2")) +
  scale_fill_manual(name = NULL, values = c(Milan = "lightskyblue", London = "darkorange2")) +
  labs(
    title = "Temperature comparison Milan vs London",
    y = "Degrees Celsius",
    x = "Day of the year",
    caption = "NASA POWER - Data Access Viewer (DAV) 1982–2025"
  ) +
  theme(legend.position = "bottom") +
  guides(color = "none")


minn <- min(FD$LON_MIN, FD$MIL_MIN)
maxx <- max(FD$LON_MAX, FD$MIL_MAX)

grid.arrange(
  top = "Boxplot for all months across the years",
  nrow = 1,
  DATASET %>%
    select(Month, T_AVG) %>%
    group_by(Month) %>%
    ggplot(aes(x = Month, y = T_AVG, group = Month)) +
    geom_boxplot(fill = "lightskyblue", outliers = FALSE, staplewidth = 0.8) +
    scale_x_continuous(breaks = seq(1, 12, by = 1), labels = month.abb) +
    labs(title = "London", x = NULL, y = NULL) +
    ylim(minn, maxx)
    ,

  MIL_DATASET %>%
    select(Month, T_AVG) %>%
    group_by(Month) %>%
    ggplot(aes(x = Month, y = T_AVG, group = Month)) +
    geom_boxplot(fill = "darkorange2", outliers = FALSE, staplewidth = 0.8) +
    scale_x_continuous(breaks = seq(1, 12, by = 1), labels = month.abb) +
    labs(title = "Milan", x = NULL, y = NULL) +
    ylim(minn, maxx)
)

## beep ----
beepr::beep(sound = 4)
rsconnect::writeManifest()
