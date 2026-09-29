# Week 2: Data visualization in R - Texas housing market, 2000-2015
# Run this file top to bottom from an empty working directory.

# ---- 00_setup ----
# Packages (all on CRAN; install.packages() once if you don't have them)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# txhousing ships with ggplot2, so no download is needed
housing <- txhousing

# --- a small custom theme so every chart looks consistent ---
navy  <- "#1F3A5F"
rust  <- "#C8553D"
sand  <- "#E9E2D0"
grey  <- "#8A8F98"

theme_report <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.title       = element_text(face = "bold", size = 13, colour = navy),
      plot.subtitle    = element_text(colour = "grey30", margin = margin(b = 8)),
      plot.caption     = element_text(colour = grey, size = 8, hjust = 0),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.title       = element_text(colour = "grey25"),
      strip.text       = element_text(face = "bold", colour = navy)
    )
}
src <- "Source: Texas Real Estate Research Center, via the ggplot2 'txhousing' dataset"

# folder for the saved chart images
dir.create("figs", showWarnings = FALSE)

# a helper to save every figure the same way
save_fig <- function(plot, name, w = 7, h = 4.4) {
  ggsave(file.path("figs", paste0(name, ".png")), plot,
         width = w, height = h, dpi = 300, bg = "white")
}

# --- first look at the data ---
glimpse(housing)
cat("\nMissing values per column:\n")
print(colSums(is.na(housing)))
cat("\nCities:", n_distinct(housing$city),
    "| Years:", min(housing$year), "to", max(housing$year),
    "| Last month in 2015:", max(housing$month[housing$year == 2015]), "\n")

cat("\nMarkets with the most months of missing sales data (out of 187):\n")
housing %>%
  group_by(city) %>%
  summarise(missing_months = sum(is.na(sales))) %>%
  arrange(desc(missing_months)) %>%
  slice_head(n = 5) %>%
  as.data.frame() %>%
  print()

# Cities that report a sales figure in every single month.
# Used later so statewide totals are not distorted by cities dropping in and out.
complete_cities <- housing %>%
  group_by(city) %>%
  summarise(missing = sum(is.na(sales))) %>%
  filter(missing == 0) %>%
  pull(city)
cat("\nCities with a complete sales record:", length(complete_cities), "of 46\n")

# ---- 01_hist ----
med_price <- median(housing$median, na.rm = TRUE)

fig1 <- ggplot(housing, aes(x = median)) +
  geom_histogram(binwidth = 10000, boundary = 50000,
                 fill = navy, colour = "white", linewidth = 0.3, na.rm = TRUE) +
  geom_vline(xintercept = med_price, colour = rust, linewidth = 0.8, linetype = "dashed") +
  annotate("text", x = med_price + 6000, y = 1050, hjust = 0, colour = rust,
           size = 3.4, label = paste0("Typical month: ", dollar(med_price))) +
  scale_x_continuous(labels = dollar, breaks = seq(50000, 300000, 50000)) +
  scale_y_continuous(limits = c(0, 1100), expand = c(0, 0)) +
  labs(title = "Half of all city-months fall between $100k and $150k",
       subtitle = "Median sale price for every city and month, 2000-2015 (7,986 observations)",
       x = "Median sale price in that city-month", y = "Number of city-months",
       caption = src) +
  theme_report()

save_fig(fig1, "fig1_price_histogram")

cat("Summary of monthly median price:\n")
print(summary(housing$median))
cat("\nShare of city-months at or above $200k:",
    percent(mean(housing$median >= 200000, na.rm = TRUE), accuracy = 0.1), "\n")
cat("Share of city-months under $100k:",
    percent(mean(housing$median < 100000, na.rm = TRUE), accuracy = 0.1), "\n")

# ---- 02_bar ----
city_totals <- housing %>%
  group_by(city) %>%
  summarise(total_sales = sum(sales, na.rm = TRUE)) %>%
  arrange(desc(total_sales))

top10 <- city_totals %>%
  slice_head(n = 10) %>%
  mutate(city = reorder(city, total_sales),
         big_two = city %in% c("Houston", "Dallas"))

fig2 <- ggplot(top10, aes(x = total_sales, y = city, fill = big_two)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = comma(total_sales)), hjust = -0.1, size = 3.2, colour = "grey20") +
  scale_fill_manual(values = c(`TRUE` = rust, `FALSE` = navy), guide = "none") +
  scale_x_continuous(labels = comma, breaks = seq(0, 1000000, 250000),
                     limits = c(0, 1200000), expand = c(0, 0)) +
  labs(title = "Houston and Dallas dominate the market",
       subtitle = "Total homes sold, 2000 to mid-2015, ten busiest markets",
       x = "Total homes sold", y = NULL, caption = src) +
  theme_report() +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = "grey90"))

save_fig(fig2, "fig2_top_cities")

state_total <- sum(city_totals$total_sales)
cat("Total homes sold across all 46 markets:", comma(state_total), "\n\n")
top10 %>% arrange(desc(total_sales)) %>%
  mutate(share = percent(total_sales / state_total, accuracy = 0.1)) %>%
  select(city, total_sales, share) %>% as.data.frame() %>% print()
cat("\nHouston + Dallas together:",
    percent(sum(city_totals$total_sales[1:2]) / state_total, accuracy = 0.1), "of all sales\n")

# ---- 03_line ----
# Use only the 26 cities that report every month, so the line reflects real
# market movement instead of cities appearing and disappearing.
monthly <- housing %>%
  filter(city %in% complete_cities) %>%
  group_by(year, month) %>%
  summarise(sales = sum(sales), .groups = "drop") %>%
  mutate(date = as.Date(sprintf("%d-%02d-01", year, month))) %>%
  arrange(date)

# 12-month moving average to strip out the seasonal zig-zag
monthly$trend <- as.numeric(stats::filter(monthly$sales, rep(1/12, 12), sides = 2))

low_pt  <- monthly %>% filter(year >= 2008 & year <= 2012) %>% slice_min(trend, n = 1)
high_pt <- monthly %>% filter(year <= 2008) %>% slice_max(trend, n = 1)

fig3 <- ggplot(monthly, aes(x = date)) +
  geom_line(aes(y = sales), colour = "grey70", linewidth = 0.4) +
  geom_line(aes(y = trend), colour = navy, linewidth = 1.1, na.rm = TRUE) +
  geom_point(data = high_pt, aes(y = trend), colour = rust, size = 2.5) +
  geom_point(data = low_pt,  aes(y = trend), colour = rust, size = 2.5) +
  annotate("text", x = as.Date("2003-01-01"), y = 33500, colour = rust, size = 3.3,
           hjust = 0.5, label = "Pre-crash peak\n(2006-07)") +
  annotate("segment", x = as.Date("2004-03-01"), xend = high_pt$date - 40,
           y = 33000, yend = high_pt$trend + 700, colour = rust,
           arrow = arrow(length = unit(0.12, "cm"))) +
  annotate("text", x = low_pt$date, y = 9000, colour = rust, size = 3.3,
           label = "Bottom (2010)") +
  scale_y_continuous(labels = comma, limits = c(8000, 37000)) +
  scale_x_date(breaks = seq(as.Date("2000-01-01"), by = "2 years", length.out = 9),
               date_labels = "%Y") +
  labs(title = "Sales fell 31% from 2006 to 2010, then rebuilt to the old level",
       subtitle = "Monthly sales, 26 cities with complete records (grey) and 12-month average (blue)",
       x = NULL, y = "Homes sold per month", caption = src) +
  theme_report() +
  theme(panel.grid.major.x = element_line(colour = "grey92"))

save_fig(fig3, "fig3_sales_trend")

yearly <- monthly %>% group_by(year) %>% summarise(sales = sum(sales))
cat("Annual sales, 26 complete cities (2015 = Jan-Jul only):\n")
print(as.data.frame(yearly))
peak <- yearly$sales[yearly$year == 2006]; trough <- yearly$sales[yearly$year == 2010]
cat("\n2006 to 2010 change:", percent((trough - peak) / peak, accuracy = 0.1), "\n")
jj <- monthly %>% filter(month <= 7) %>% group_by(year) %>% summarise(s = sum(sales))
cat("Jan-Jul 2015 vs Jan-Jul 2014:",
    percent(jj$s[jj$year == 2015] / jj$s[jj$year == 2014] - 1, accuracy = 0.1), "\n")
cat("Jan-Jul 2015 vs Jan-Jul 2006:",
    percent(jj$s[jj$year == 2015] / jj$s[jj$year == 2006] - 1, accuracy = 0.1), "\n")
cat("12-month average at the 2006-07 peak:", comma(round(high_pt$trend)),
    "sales a month | at the 2010 low:", comma(round(low_pt$trend)), "\n")

# ---- 04_season ----
# Seasonal index: each month's sales divided by that year's monthly average.
# 1.0 = an average month, 1.25 = 25% busier than average.
# Only full years (2000-2014) and the 26 complete cities are used.
season <- housing %>%
  filter(city %in% complete_cities, year <= 2014) %>%
  group_by(year, month) %>%
  summarise(sales = sum(sales), .groups = "drop") %>%
  group_by(year) %>%
  mutate(index = sales / mean(sales)) %>%
  ungroup() %>%
  mutate(month_lab = factor(month, levels = 1:12, labels = month.abb))

month_med <- season %>% group_by(month_lab) %>% summarise(med = median(index))
season <- season %>% left_join(month_med, by = "month_lab")

fig4 <- ggplot(season, aes(x = month_lab, y = index, fill = med > 1)) +
  geom_hline(yintercept = 1, colour = "grey40", linetype = "dashed") +
  geom_boxplot(width = 0.65, colour = "grey25", outlier.colour = rust, outlier.size = 1.6) +
  scale_fill_manual(values = c(`TRUE` = "#E7A58F", `FALSE` = "#A9BCD3"), guide = "none") +
  scale_y_continuous(breaks = seq(0.6, 1.4, 0.2)) +
  labs(title = "The housing market has a strong summer season",
       subtitle = "Sales in each month relative to that year's average month, 2000-2014",
       x = NULL, y = "Seasonal index (1.0 = average)", caption = src) +
  theme_report()

save_fig(fig4, "fig4_seasonality")

cat("Median seasonal index by month:\n")
print(round(setNames(month_med$med, month_med$month_lab), 2))
cat("\nJune vs January (median index):",
    round(month_med$med[6] / month_med$med[1], 2), "times as many sales\n")

# Which individual years sit outside the whiskers?
cat("\nUnusual months (outside the box whiskers):\n")
season %>% group_by(month_lab) %>%
  filter(index > quantile(index, .75) + 1.5 * IQR(index) |
         index < quantile(index, .25) - 1.5 * IQR(index)) %>%
  ungroup() %>% transmute(month = month_lab, year, index = round(index, 2)) %>%
  as.data.frame() %>% print()

# ---- 05_bigfour ----
big4 <- c("Austin", "Dallas", "Houston", "San Antonio")

# Put every city on the same starting line: average price in 2000 = 100
base_2000 <- housing %>%
  filter(city %in% big4, year == 2000) %>%
  group_by(city) %>%
  summarise(base = mean(median))

prices <- housing %>%
  filter(city %in% big4) %>%
  arrange(city, year, month) %>%
  left_join(base_2000, by = "city") %>%
  group_by(city) %>%
  mutate(index  = median / base * 100,
         smooth = as.numeric(stats::filter(index, rep(1/12, 12), sides = 1)),
         date   = as.Date(sprintf("%d-%02d-01", year, month))) %>%
  ungroup()

end_labels <- prices %>% group_by(city) %>% slice_tail(n = 1)
pal <- c(Austin = navy, Dallas = rust, Houston = "#3B8EA5", `San Antonio` = "#8C6BB1")

fig5 <- ggplot(prices, aes(x = date, y = smooth, colour = city)) +
  geom_hline(yintercept = 100, colour = "grey60", linewidth = 0.3) +
  geom_line(linewidth = 1.05, na.rm = TRUE) +
  geom_text(data = end_labels, aes(label = city), hjust = -0.1, size = 3.4,
            fontface = "bold", show.legend = FALSE) +
  scale_colour_manual(values = pal, guide = "none") +
  scale_x_date(breaks = seq(as.Date("2001-01-01"), by = "2 years", length.out = 8),
               date_labels = "%Y", expand = expansion(mult = c(0.02, 0.25))) +
  labs(title = "San Antonio started cheapest and ended up rising the most",
       subtitle = "Median sale price, 12-month average, with each city's 2000 average set to 100",
       x = NULL, y = "Price index (2000 = 100)", caption = src) +
  theme_report() +
  theme(panel.grid.major.x = element_line(colour = "grey92"))

save_fig(fig5, "fig5_big_four_prices")

cat("Price index at latest reading (Jul 2015, 12-mo average; 100 = 2000 level):\n")
print(end_labels %>% ungroup() %>% mutate(index = round(smooth)) %>%
        select(city, index) %>% as.data.frame())
cat("\nMedian sale price, Jan-Jul 2015 (average of monthly medians):\n")
print(housing %>% filter(city %in% big4, year == 2015) %>% group_by(city) %>%
        summarise(price = dollar(mean(median))) %>% as.data.frame())
cat("\nMedian sale price, full-year 2000 (average of monthly medians):\n")
print(base_2000 %>% mutate(base = dollar(base)) %>% as.data.frame())

# ---- 06_scatter1 ----
# One dot per city-month. Both axes on a log scale because city sizes
# range from tiny (dozens of sales) to huge (thousands).
pairs_df <- housing %>% filter(!is.na(sales), !is.na(listings), listings > 0)

fit <- lm(log10(sales) ~ log10(listings), data = pairs_df)

fig6 <- ggplot(pairs_df, aes(x = listings, y = sales)) +
  geom_point(alpha = 0.25, size = 1.2, colour = navy) +
  geom_smooth(method = "lm", formula = y ~ x, colour = rust, fill = rust,
              alpha = 0.15, linewidth = 1) +
  scale_x_log10(labels = comma) +
  scale_y_log10(labels = comma) +
  annotation_logticks(sides = "bl", colour = "grey60", size = 0.3) +
  labs(title = "Cities with more homes listed sell more homes",
       subtitle = "Each dot is one city in one month; both axes use a log scale",
       x = "Active listings (homes on the market)", y = "Homes sold that month",
       caption = src) +
  theme_report() +
  theme(panel.grid.major.x = element_line(colour = "grey92"))

save_fig(fig6, "fig6_listings_vs_sales")

cat("Observations used:", comma(nrow(pairs_df)), "\n")
cat("Correlation (log scale):",
    round(cor(log10(pairs_df$listings), log10(pairs_df$sales)), 3), "\n")
cat("R-squared:", round(summary(fit)$r.squared, 3), "\n")
cat("Slope on log-log scale:", round(coef(fit)[2], 3), "\n")
cat("Example: a city with 1,000 listings is predicted to sell about",
    round(10^predict(fit, data.frame(listings = 1000))), "homes a month;\n",
    "with 10,000 listings, about", round(10^predict(fit, data.frame(listings = 10000))), "\n")

# ---- 07_scatter2 ----
# 'inventory' = months of inventory: how many months it would take to sell
# every listed home at the current pace. Low = sellers' market.
inv_df <- housing %>%
  filter(!is.na(inventory), !is.na(median), inventory > 0) %>%
  mutate(period = cut(year, breaks = c(1999, 2007, 2011, 2015),
                      labels = c("2000-2007: steady growth",
                                 "2008-2011: downturn",
                                 "2012-2015: recovery")),
         spi = city == "South Padre Island")

fig7 <- ggplot(inv_df, aes(x = inventory, y = median)) +
  geom_point(data = filter(inv_df, !spi), alpha = 0.22, size = 1, colour = navy) +
  geom_point(data = filter(inv_df, spi), colour = rust, size = 1.6) +
  geom_smooth(data = filter(inv_df, !spi), method = "lm", formula = y ~ x,
              se = FALSE, colour = "black", linewidth = 0.7) +
  facet_wrap(~ period) +
  scale_x_log10(breaks = c(1, 3, 10, 30), labels = c("1", "3", "10", "30")) +
  scale_y_continuous(labels = dollar_format(scale = 1/1000, suffix = "k")) +
  labs(title = "Slow-selling markets became more clearly the cheaper ones after 2011",
       subtitle = "Months of inventory vs. median price. Red dots = South Padre Island",
       x = "Months of inventory (log scale)", y = "Median sale price", caption = src) +
  theme_report() +
  theme(panel.grid.major.x = element_line(colour = "grey92"),
        plot.subtitle = element_text(colour = "grey30", margin = margin(b = 8)))

save_fig(fig7, "fig7_inventory_vs_price", w = 8.5, h = 4.2)

cat("Correlation between log(inventory) and price, excluding South Padre Island:\n")
inv_df %>% filter(!spi) %>% group_by(period) %>%
  summarise(r = round(cor(log10(inventory), median), 2), n = n()) %>%
  as.data.frame() %>% print()
cat("\nMedian months of inventory by year, all cities:\n")
print(housing %>% group_by(year) %>% summarise(months = median(inventory, na.rm = TRUE)) %>%
        as.data.frame())
cat("\nThe five highest inventory readings in the data:\n")
print(housing %>% arrange(desc(inventory)) %>% slice_head(n = 5) %>%
        select(city, year, month, sales, listings, inventory) %>% as.data.frame())
cat("\nTypical reading (median):", median(housing$inventory, na.rm = TRUE),
    "months | 99th percentile:", quantile(housing$inventory, 0.99, na.rm = TRUE), "months\n")

# ---- 08_lollipop ----
# Compare the same seven months (Jan-Jul) in 2010 and 2015. 2015 only has
# data up to July and prices rise in summer, so using full-year 2010 against
# partial-year 2015 would flatter the growth figure.
window_price <- function(yr) {
  housing %>%
    filter(year == yr, month <= 7) %>%
    group_by(city) %>%
    summarise(price = median(median, na.rm = TRUE),
              n_ok  = sum(!is.na(median)), .groups = "drop") %>%
    filter(n_ok >= 5) %>%
    select(city, price)
}

growth <- inner_join(window_price(2010), window_price(2015),
                     by = "city", suffix = c("_2010", "_2015")) %>%
  mutate(change = price_2015 / price_2010 - 1,
         direction = ifelse(change >= 0, "up", "down")) %>%
  arrange(change) %>%
  mutate(city = factor(city, levels = city))

fig8 <- ggplot(growth, aes(x = change, y = city, colour = direction)) +
  geom_vline(xintercept = 0, colour = "grey50") +
  geom_segment(aes(x = 0, xend = change, yend = city), linewidth = 0.7) +
  geom_point(size = 2.4) +
  scale_colour_manual(values = c(up = navy, down = rust), guide = "none") +
  scale_x_continuous(labels = percent_format(accuracy = 1), breaks = seq(-0.1, 0.6, 0.1),
                     expand = expansion(mult = c(0.03, 0.06))) +
  labs(title = "44 of 46 Texas markets got pricier from 2010 to 2015",
       subtitle = "Change in median sale price, Jan-Jul 2010 to Jan-Jul 2015, by city",
       x = "Price change", y = NULL, caption = src) +
  theme_report() +
  theme(panel.grid.major.y = element_line(colour = "grey94"),
        panel.grid.major.x = element_line(colour = "grey90"),
        axis.text.y = element_text(size = 8))

save_fig(fig8, "fig8_price_growth", w = 7, h = 8)

cat("Cities compared:", nrow(growth), "\n")
cat("Cities where prices rose:", sum(growth$change > 0), "| fell:", sum(growth$change < 0), "\n")
cat("Median change across cities:", percent(median(growth$change), accuracy = 0.1), "\n\n")
cat("Biggest increases:\n")
print(growth %>% arrange(desc(change)) %>% slice_head(n = 5) %>%
        transmute(city = as.character(city), from = dollar(price_2010),
                  to = dollar(price_2015), change = percent(change, accuracy = 0.1)) %>%
        as.data.frame())
cat("\nSmallest changes / declines:\n")
print(growth %>% slice_head(n = 5) %>%
        transmute(city = as.character(city), from = dollar(price_2010),
                  to = dollar(price_2015), change = percent(change, accuracy = 0.1)) %>%
        as.data.frame())

cat("\nThe four biggest markets, same comparison:\n")
print(growth %>% filter(city %in% c("Austin", "Dallas", "Houston", "San Antonio")) %>%
        arrange(desc(change)) %>%
        transmute(city = as.character(city), from = dollar(price_2010),
                  to = dollar(price_2015), change = percent(change, accuracy = 0.1)) %>%
        as.data.frame())

