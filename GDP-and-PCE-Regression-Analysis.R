library(readxl)

# Load data, skip row 1
data <- read_excel("BEA data Project 1.xlsx", skip = 1)

# Fix column names
data <- data[, c(1, 3, 4)]
colnames(data) <- c("Year", "GDP", "PCE")

# Convert to numeric 
data$GDP <- as.numeric(data$GDP)
data$PCE <- as.numeric(data$PCE)

# Drop any NA rows
data <- na.omit(data)

head(data)
nrow(data)

# ---- MODEL A ----
modelA <- lm(PCE ~ GDP, data = data)
summary(modelA)

# ---- MODEL B ----
data$ln_GDP <- log(data$GDP)
data$ln_PCE <- log(data$PCE)

modelB <- lm(ln_PCE ~ ln_GDP, data = data)
summary(modelB)