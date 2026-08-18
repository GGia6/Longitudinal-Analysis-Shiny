# ============================================================
# Dummy Merged Survey Data Generator
# 6 waves x 250 respondents = 1,500 observations
# ============================================================

library(haven)

set.seed(42)

# ------------------------------------------------------------
# 1. Basic survey structure
# ------------------------------------------------------------

n_per_wave <- 250

waves <- paste0("D3 Wave ", 1:6)

df <- data.frame(
  RespondentID = 1:(n_per_wave * length(waves)),
  Wave = rep(waves, each = n_per_wave)
)

n <- nrow(df)


# ------------------------------------------------------------
# 2. CATEGORICAL VARIABLES
# ------------------------------------------------------------

# Gender
df$Gender <- sample(
  1:3,
  n,
  replace = TRUE,
  prob = c(0.48, 0.47, 0.05)
)

# Employment status
df$Employment <- sample(
  1:4,
  n,
  replace = TRUE,
  prob = c(0.48, 0.12, 0.25, 0.15)
)

# Satisfaction: ordinal 1-5
df$Satisfaction <- sample(
  1:5,
  n,
  replace = TRUE,
  prob = c(0.08, 0.14, 0.25, 0.32, 0.21)
)

# Direction: ordinal
# 1 = Wrong direction
# 2 = Staying the same
# 3 = Right direction
df$Direction <- sample(
  1:3,
  n,
  replace = TRUE,
  prob = c(0.25, 0.45, 0.30)
)


# ------------------------------------------------------------
# 3. NUMERIC VARIABLES
# ------------------------------------------------------------

# Age
df$Age <- round(
  pmin(
    pmax(rnorm(n, mean = 38, sd = 13), 18),
    80
  )
)

# Annual household income
df$AnnualIncome <- round(
  pmin(
    pmax(
      rlnorm(n, meanlog = log(55000), sdlog = 0.55),
      15000
    ),
    250000
  ),
  -2
)

# Household size
df$HouseholdSize <- sample(
  1:6,
  n,
  replace = TRUE,
  prob = c(0.15, 0.30, 0.25, 0.18, 0.08, 0.04)
)

# Continuous 0-100 trust score
df$TrustScore <- round(
  pmin(
    pmax(rnorm(n, mean = 62, sd = 18), 0),
    100
  ),
  1
)


# ------------------------------------------------------------
# 4. Add survey-style missing values
# ------------------------------------------------------------

# Age: 3% missing
df$Age[
  sample(1:n, size = round(n * 0.03))
] <- NA

# Income: 6% missing
df$AnnualIncome[
  sample(1:n, size = round(n * 0.06))
] <- NA

# Trust score: 4% missing
df$TrustScore[
  sample(1:n, size = round(n * 0.04))
] <- NA

# Satisfaction: 3% missing
df$Satisfaction[
  sample(1:n, size = round(n * 0.03))
] <- NA


# ------------------------------------------------------------
# 5. Add SPSS variable labels
# ------------------------------------------------------------



# ------------------------------------------------------------
# 6. Add SPSS value labels
# ------------------------------------------------------------

df$Gender <- labelled(
  df$Gender,
  labels = c(
    Male = 1,
    Female = 2,
    Other = 3
  )
)

df$Employment <- labelled(
  df$Employment,
  labels = c(
    Employed = 1,
    Unemployed = 2,
    Student = 3,
    Other = 4
  )
)

df$Satisfaction <- labelled(
  df$Satisfaction,
  labels = c(
    "Very dissatisfied" = 1,
    "Dissatisfied" = 2,
    "Neutral" = 3,
    "Satisfied" = 4,
    "Very satisfied" = 5
  )
)

df$Direction <- labelled(
  df$Direction,
  labels = c(
    "Wrong direction" = 1,
    "Staying the same" = 2,
    "Right direction" = 3
  )
)


# ------------------------------------------------------------
# 7. Save as SPSS .sav
# ------------------------------------------------------------

write_sav(
  df,
  "dummy_survey_merged_waves.sav"
)


# ------------------------------------------------------------
# 8. Quick verification
# ------------------------------------------------------------

cat("\n========================================\n")
cat("Dummy survey created successfully!\n")
cat("========================================\n\n")

cat("Rows:", nrow(df), "\n")
cat("Columns:", ncol(df), "\n")
cat("Waves:", length(unique(df$Wave)), "\n\n")

cat("Variables:\n")
print(names(df))

cat("\nStructure:\n")
str(df)

cat("\nWave counts:\n")
print(table(df$Wave))

cat("\nNumeric variable summaries:\n")
print(summary(df[c(
  "Age",
  "AnnualIncome",
  "HouseholdSize",
  "TrustScore"
)]))

cat("\nFile created:\n")
cat("dummy_survey_merged_waves.sav\n")