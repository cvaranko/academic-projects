setwd("C:/Users/carot/OneDrive/Desktop/ECON400/Project #2")

library(tidyverse)
library(zoo)  # for na.approx
library(stargazer)
library(car)
library(lmtest)

# Load data and rename each series
mortg    <- read.csv("MORTGAGE30US.csv") %>% rename(mortg = MORTGAGE30US)
fedfunds <- read.csv("FEDFUNDS.csv")     %>% rename(fedfunds = FEDFUNDS)
cpi      <- read.csv("CPILFESL.csv")     %>% rename(cpi = CPILFESL)
gdp      <- read.csv("GDP.csv")          %>% rename(ngdp = GDP)
gs10     <- read.csv("GS10.csv")         %>% rename(gs10 = GS10)
gs20     <- read.csv("GS20.csv")         %>% rename(gs20 = GS20)
hpi      <- read.csv("CSUSHPISA.csv")    %>% rename(hpi = CSUSHPISA)
unrate   <- read.csv("UNRATE.csv")       %>% rename(unrate = UNRATE)
spread   <- read.csv("T10Y2YM.csv")      %>% rename(spread = T10Y2YM)

# Convert mortgage from weekly to monthly
mortg_monthly <- mortg %>%
  mutate(observation_date = as.Date(observation_date),
         observation_date = floor_date(observation_date, "month")) %>%
  group_by(observation_date) %>%
  summarise(mortg = mean(mortg, na.rm = TRUE))

# Convert observation_date to Date type in all dataframes
fedfunds <- fedfunds %>% mutate(observation_date = as.Date(observation_date))
cpi      <- cpi      %>% mutate(observation_date = as.Date(observation_date))
gdp      <- gdp      %>% mutate(observation_date = as.Date(observation_date))
gs10     <- gs10     %>% mutate(observation_date = as.Date(observation_date))
gs20     <- gs20     %>% mutate(observation_date = as.Date(observation_date))
hpi      <- hpi      %>% mutate(observation_date = as.Date(observation_date))
unrate   <- unrate   %>% mutate(observation_date = as.Date(observation_date))
spread   <- spread   %>% mutate(observation_date = as.Date(observation_date))

# Merge all series by date
df <- mortg_monthly %>%
  left_join(fedfunds, by = "observation_date") %>%
  left_join(cpi,      by = "observation_date") %>%
  left_join(gdp,      by = "observation_date") %>%
  left_join(gs10,     by = "observation_date") %>%
  left_join(gs20,     by = "observation_date") %>%
  left_join(hpi,      by = "observation_date") %>%
  left_join(unrate,   by = "observation_date") %>%
  left_join(spread,   by = "observation_date")

df <- df %>% filter(observation_date >= "1996-01-01" & observation_date <= "2026-01-01") # filter date range


head(df)
summary(df)
nrow(df)


# Interpolate missing values
df <- df %>%
  mutate(ngdp   = na.approx(ngdp,   na.rm = FALSE),
         cpi    = na.approx(cpi,    na.rm = FALSE),
         unrate = na.approx(unrate, na.rm = FALSE))

summary(df)


# OLS with all variables
model1 <- lm(mortg ~ fedfunds + cpi + ngdp + gs10 + gs20 + hpi + unrate + spread,
             data = df)

summary(model1)


# Correlation matrix of X variables
cor_matrix <- cor(df[, c("fedfunds", "cpi", "ngdp", "gs10", "gs20", 
                         "hpi", "unrate", "spread")], 
                  use = "complete.obs")
round(cor_matrix, 2)

vif(model1) # VIF


# Reduced model dropping ngdp and gs10
model2 <- lm(mortg ~ fedfunds + cpi + gs20 + hpi + unrate + spread, data = df)
summary(model2)
vif(model2)

# Drop hpi
model3 <- lm(mortg ~ fedfunds + cpi + gs20 + unrate + spread, data = df)
summary(model3)
vif(model3)

# Drop unrate
model4 <- lm(mortg ~ fedfunds + cpi + gs20 + spread, data = df)
summary(model4)
vif(model4)

# Drop fedfunds
model5 <- lm(mortg ~ cpi + gs20 + spread, data = df)
summary(model5)
vif(model5)


# Breusch-Pagan test
bptest(model5)

# White test
bptest(model5, ~ fitted(model5) + I(fitted(model5)^2))

# log transformation on CPI
df$log_cpi <- log(df$cpi)

model6 <- lm(mortg ~ log_cpi + gs20 + spread, data = df)
summary(model6)
vif(model6)


# Comparison of all models
stargazer(model1, model2, model3, model4, model5, model6,
          type = "text",
          column.labels = c("Full", "Drop ngdp/gs10", 
                            "Drop hpi", "Drop unrate",
                            "Drop fedfunds", "Log CPI"),
          keep.stat = c("n", "rsq", "adj.rsq", "f"))


# Final HSK check on model6
bptest(model6)
bptest(model6, ~ fitted(model6) + I(fitted(model6)^2))

# Final residual plots for model6
par(mfrow = c(1, 3))
plot(df$log_cpi, resid(model6), main = "Residuals vs log(CPI)",
     xlab = "log(CPI)", ylab = "Residuals")
abline(h = 0, col = "red")

plot(df$gs20, resid(model6), main = "Residuals vs GS20",
     xlab = "GS20", ylab = "Residuals")
abline(h = 0, col = "red")

plot(df$spread, resid(model6), main = "Residuals vs Spread",
     xlab = "Spread", ylab = "Residuals")
abline(h = 0, col = "red")
