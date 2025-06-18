# Load necessary libraries
library(ggplot2)
library(forecast)
library(tidyverse)
library(lubridate)

# Convert to time series object
ts_ac_c_t_s <- ts(air_care_cat$TOTAL_sales, start = c(2022, 11), frequency = 52)
ts_ac_c_t_eq <- ts(air_care_cat$TOTAL_eq, start = c(2022, 11), frequency = 52)
ts_ac_c_a_s <- ts(air_care_cat$AMAZON_sales, start = c(2022, 11), frequency = 52)
ts_ac_c_a_eq <- ts(air_care_cat$AMAZON_eq, start = c(2022, 11), frequency = 52)
ts_ac_c_twoa_s <- ts(air_care_cat$TOTAL_WO_AMAZON_sales, start = c(2022, 11), frequency = 52)
ts_ac_c_twoa_eq <- ts(air_care_cat$TOTAL_WO_AMAZON_eq, start = c(2022, 11), frequency = 52)

ts_ac_scj_t_s <- ts(air_care_scj$TOTAL_sales, start = c(2022, 11), frequency = 52)
ts_ac_scj_t_eq <- ts(air_care_scj$TOTAL_eq, start = c(2022, 11), frequency = 52)
ts_ac_scj_a_s <- ts(air_care_scj$AMAZON_sales, start = c(2022, 11), frequency = 52)
ts_ac_scj_a_eq <- ts(air_care_scj$AMAZON_eq, start = c(2022, 11), frequency = 52)
ts_ac_scj_twoa_s <- ts(air_care_scj$TOTAL_WO_AMAZON_sales, start = c(2022, 11), frequency = 52)
ts_ac_scj_twoa_eq <- ts(air_care_scj$TOTAL_WO_AMAZON_eq, start = c(2022, 11), frequency = 52)


ts_objs <- c("ts_ac_c_t_s", "ts_ac_c_t_eq", "ts_ac_c_a_s", "ts_ac_c_a_eq",
             "ts_ac_c_twoa_s", "ts_ac_c_twoa_eq",
             "ts_ac_scj_t_s", "ts_ac_scj_t_eq", "ts_ac_scj_a_s", "ts_ac_scj_a_eq",
             "ts_ac_scj_twoa_s", "ts_ac_scj_twoa_eq")

pdf("AC_time_series_plots.pdf", width = 10, height = 5)
map(ts_objs, ~ autoplot(get(.x)) +
      ggtitle(paste("Air Care Plot for", .x)) +
      xlab("Year") + ylab("Sales"))
dev.off()

################################################################

# Convert to named list of actual time series objects
ts_list <- setNames(mget(ts_objs), ts_objs)

# Helper function to convert object name to readable title
make_title <- function(name) {
  parts <- unlist(str_split(name, "_"))
  parts <- parts[parts != "ts"]  # remove "ts" prefix
  
  label_map <- list(
    ac = "Air Care",
    c = "Category",
    scj = "SCJ",
    t = "Total",
    a = "Amazon",
    twoa = "Total w/o Amazon",
    s = "Sales",
    eq = "EQ"
  )
  
  translated <- sapply(parts, function(p) label_map[[p]])
  paste(translated, collapse = " ")
}

# Function to plot decomposition with seasonality strength
plot_decomp_with_strength <- function(ts_obj, ts_name = "Time Series") {
  decomp <- decompose(ts_obj)
  strength <- 1 - var(decomp$random, na.rm = TRUE) / var(decomp$x, na.rm = TRUE)
  strength_label <- paste("Seasonality strength:", round(strength, 2))
  
  autoplot(decomp) +
    ggtitle(paste("Decomposition of", ts_name, "\n", strength_label)) +
    theme_minimal()
}

# Loop through and generate plots
pdf("AC_decompose_plots.pdf", width = 10, height = 5)
imap(ts_list, ~ plot_decomp_with_strength(.x, make_title(.y)))
dev.off()

# Decompose the time series
#decomp_t_s <- decompose(ts_sales)
#autoplot(decomp_ts) +
#  ggtitle("Decomposition of Weekly Total Sales Aircare") 
#seasonality_strength_ts <- 1 - var(decomp_ts$random, na.rm = TRUE) / var(decomp_ts$x, na.rm = TRUE)
#print(paste("Seasonality strength:", round(seasonality_strength_ts, 2)))


# Plot with month indicators
ggplot(air_care_cat, aes(x = period_description, y = TOTAL_sales)) +
  geom_line(color = "steelblue") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(title = "Weekly Total Sales Time Series",
       x = "Month",
       y = "Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Convert to long format for ggplot
air_care_long <- air_care_cat %>%
  pivot_longer(cols = c(TOTAL_sales, AMAZON_sales), names_to = "channel", values_to = "value")

# Plot with month indicators and both lines
ggplot(air_care_long, aes(x = period_description, y = value, color = channel)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b\n%Y") +
  labs(title = "Weekly Sales Time Series",
       x = "Month",
       y = "Sales",
       color = "Channel") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

pdf("AC_Total_Amazon_plots.pdf", width = 10, height = 5)
ggplot(air_care_cat, aes(x = period_description)) +
  geom_line(aes(y = TOTAL_sales, color = "Total Sales"), size = 1) +
  geom_line(aes(y = AMAZON_sales * 10, color = "Amazon Sales"), size = 1, linetype = "dashed") +  # scale Amazon sales for visibility
  scale_y_continuous(
    name = "Total Sales",
    sec.axis = sec_axis(~ . / 10, name = "Amazon Sales")
  ) +
  scale_color_manual(values = c("Total Sales" = "steelblue", "Amazon Sales" = "darkorange")) +
  labs(title = "Weekly Sales with Amazon Re-Scaled Sales (Dual Axis)",
       x = "Date", color = "Channel") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()