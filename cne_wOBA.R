library(collegebaseball)
library(dplyr)
library(broom)
library(baseballr)

#find cne school ids
ncaa_school_id_lookup(team_name = "Gordon", season = 2024)
gordon_id <- 574618
ncaa_school_id_lookup(team_name = "Endicott", season = 2024)
endicott_id <- 574667
ncaa_school_id_lookup(team_name = "Hartford", season = 2024)
hartford_id <- 574079
ncaa_school_id_lookup(team_name = "Suffolk", season = 2024)
suffolk_id <- 574552
ncaa_school_id_lookup(team_name = "Nichols", season = 2024)
nichols_id = 574493
ncaa_school_id_lookup(team_name = "Curry", season = 2024)
curry_id <- 574775
ncaa_school_id_lookup(team_name = "Wentworth", season = 2024)
wentworth_id <- 574575
ncaa_school_id_lookup(team_name = "Roger", season = 2024)
roger_id <- 574526
ncaa_school_id_lookup(team_name = "Wester", season = 2024)
wne_id <- 574579

#create a df of each cne team's hitting stats
gordon_stats <- data.frame(ncaa_stats(574618, 2024, "batting"))
endicott_stats <- data.frame(ncaa_stats(endicott_id, 2024, "batting"))
hartford_stats <- data.frame(ncaa_stats(hartford_id, 2024, "batting"))
suffolk_stats <- data.frame(ncaa_stats(suffolk_id, 2024, "batting"))
nichols_stats <- data.frame(ncaa_stats(nichols_id, 2024, "batting"))
curry_stats <- data.frame(ncaa_stats(curry_id, 2024, "batting"))
wentworth_stats <- data.frame(ncaa_stats(wentworth_id, 2024, "batting"))
roger_stats <- data.frame(ncaa_stats(roger_id, 2024, "batting"))
wne_stats <- data.frame(ncaa_stats(wne_id, 2024, "batting"))

#combine into one large df, sum runs and hits
cne_stats <- dplyr::bind_rows(gordon_stats, endicott_stats, hartford_stats, suffolk_stats, nichols_stats, curry_stats, wentworth_stats, roger_stats, wne_stats)
total_runs <- sum(cne_stats$R)
total_hits <- sum(cne_stats$H)

#singles is not a defined stat, use this to calculate.
cne_stats <- cne_stats %>%
  mutate(X1B = H - (X2B + X3B + HR))

# Fit regression model
model <- lm(R ~ HBP + BB + X1B + X2B + X3B + HR, data = cne_stats)

# Extract coefficients
coefs <- coef(model)

# Create PA column
cne_stats$PA <- cne_stats$AB + cne_stats$BB + cne_stats$HBP + cne_stats$SF + cne_stats$SH

# Pull regression weights
wHBP <- coefs["HBP"]
wBB  <- coefs["BB"]
w1B  <- coefs["X1B"]
w2B  <- coefs["X2B"]
w3B  <- coefs["X3B"]
wHR  <- coefs["HR"]

# Compute custom wOBA
cne_stats$wOBA_custom <- (
  (wHBP * cne_stats$HBP) +
    (wBB  * cne_stats$BB)  +
    (w1B  * cne_stats$X1B) +
    (w2B  * cne_stats$X2B) +
    (w3B  * cne_stats$X3B) +
    (wHR  * cne_stats$HR)
) / cne_stats$PA

# Compare with Fangraphs wOBA
cne_stats$wOBA_fg <- (
  (0.722 * cne_stats$HBP) +
    (0.690 * cne_stats$BB)  +
    (0.888 * cne_stats$X1B) +
    (1.271 * cne_stats$X2B) +
    (1.616 * cne_stats$X3B) +
    (2.101 * cne_stats$HR)
) / cne_stats$PA

# Scale factor to normalize
scale_factor <- mean(cne_stats$OBP, na.rm = TRUE) / mean(cne_stats$wOBA_custom, na.rm = TRUE)
cne_stats$wOBA_custom <- cne_stats$wOBA_custom * scale_factor

# Filter players with >= 20 PA
cne_stats <- cne_stats |> filter(PA > 20)

View(cne_stats)

