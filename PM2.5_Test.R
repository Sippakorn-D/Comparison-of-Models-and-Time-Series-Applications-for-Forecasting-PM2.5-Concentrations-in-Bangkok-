# ==============================================================================
# Time Series Analysis for PM 2.5
# Supports file: "PM2.5 เเก้.xlsx" (Monthly Frequency Data)
# ==============================================================================

# 1. Setup & Libraries
# ------------------------------------------------------------------------------
required_packages <- c("nortest", "readxl")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages) > 0) install.packages(new_packages)

library(nortest)
library(readxl)

# 2. Data Import
# ------------------------------------------------------------------------------
cat("Please select the 'PM2.5 เเก้.xlsx' file...
")
file_path <- file.choose()
dat <- read_excel(file_path)

# Rename columns for easy reference in the code (based on Excel file structure)
colnames(dat) <- c("year", "month_name", "pm25")

# Create Time Series Object
# Starts January 2024, frequency 12 (monthly)
n <- nrow(dat)
y_month <- ts(dat$pm25, start = c(2024, 1), frequency = 12)

# 3. Exploratory Data Analysis
# ------------------------------------------------------------------------------
# 3.1 Monthly time series plot
plot(y_month,
     type = "o", col = "blue", lwd = 2, pch = 16,
     xlab = "Time (Year)", ylab = "Average PM2.5 (µg/m³)",
     main = "Monthly PM2.5 in Bangkok (2024-2026)")

# 3.2 Normality Test (Lilliefors Test)
normality <- lillie.test(dat$pm25)
print(normality)

# Histogram and Q-Q Plot
par(mfrow = c(1, 2))
hist(dat$pm25, breaks = 10, col = "lightblue",
     main = "Distribution of PM2.5", xlab = "PM2.5 (µg/m³)")
qqnorm(dat$pm25, main = "Normal Q-Q Plot")
qqline(dat$pm25, col = "red", lwd = 2)
par(mfrow = c(1, 1))

# 3.3 Trend Test (Spearman Rank Correlation)
t <- seq_along(dat$pm25)
trend_test <- cor.test(t, dat$pm25, method = "spearman", exact = FALSE)
print(trend_test)

# Calculate Z-Daniel value
r_s <- unname(trend_test$estimate)
z_daniel <- r_s * sqrt(n - 1)
p_daniel <- 2 * pnorm(-abs(z_daniel))

daniel_test_result <- data.frame(rho = r_s, Z = z_daniel, p_value = p_daniel)
print("Z-Daniel Test Result:")
print(daniel_test_result)

# 4. Seasonality Test & ACF
# ------------------------------------------------------------------------------
# 4.1 Check annual relationship using Kruskal-Wallis Test
# Detrending
dec_temp <- decompose(y_month, type = "additive")
detrended <- as.numeric(y_month - dec_temp$trend)
season_data <- na.omit(data.frame(month = as.factor(dat$month_name), detrended = detrended))

season_test <- kruskal.test(detrended ~ month, data = season_data)
print(season_test)

boxplot(detrended ~ month, data = season_data,
        col = "lightgreen", xlab = "Month", ylab = "Detrended PM2.5",
        main = "Monthly Seasonal Pattern")

# 4.2 ACF Plot
acf(y_month, lag.max = 24,
    main = "ACF of Monthly PM2.5", xlab = "Lag (months)")

# 4.3 Ljung-Box Test (Considering Lag 12 or 1 year)
lb_test <- Box.test(y_month, lag = 12, type = "Ljung-Box", fitdf = 0)
print(lb_test)
