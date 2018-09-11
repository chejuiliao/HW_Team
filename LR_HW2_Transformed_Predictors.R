setwd("~/IAA/Logistic/HW")
library(haven)

# Read data
df <- haven::read_sas("../data/insurance_t.sas7bdat")

# Helper function, counts unique values in the column
count_uniques <- function(df1, column) {
  nrow(unique(df1[column]))
}

# Convert columns with more than 10 values to continuous, otherwise make them numeric
for (name in names(df)){
  ifelse (count_uniques(df, name) > 10,
          df[[name]] <- as.numeric(df[[name]]),
          df[[name]] <- as.factor(df[[name]])
  )
}

# The bank branch is an exception to this rule
# Convert into factor
df$BRANCH <- as.factor(df$BRANCH)

# Cleaning
library(dplyr)

for (column in names(df)) {
  # Remove columns with more than 1000 missing values
  if( sum(is.na(df[[column]])) > 1000){
    df[[column]] <- NULL
  }
  
  # For each column, check whether the 75th percentile is 0. If so, remove it
  else if( class(df[[column]]) != "factor"){
    if (quantile(df[[column]], 0.75, na.rm=T) == 0) {
      df[[column]] <- NULL  # Remove column
    }
  }
}

# Impute missing values with the mean of the column
# ACCTAGE/CRSCORE
df$ACCTAGE[is.na(df$ACCTAGE)] <- mean(df$ACCTAGE, na.rm=T)
df$CRSCORE[is.na(df$CRSCORE)] <- mean(df$CRSCORE, na.rm=T)

# Identify the numeric columns, and factor columns, make a temp df for each
num_cols <- unlist(lapply(df, is.numeric))
numeric_df = df[,num_cols]

factor_cols <- unlist(lapply(df, is.factor))
factor_df = df[, factor_cols]

# Correlation matrix of numeric variables
cor(numeric_df)

# TELLER, DEP, and CHECKS are highly correlated, and CHECKS has highest variation, so drop the other two
df$TELLER <- NULL
df$DEP <- NULL

# Run logistic regression
log_model <- glm(INS ~ ., data=df, family=binomial(link="logit"))
summary(log_model)

# In order to pare down model, remove the worst predictor pairwise above 0.3 p-value (We know this isn't a great way to do it)
df$MMCRED <- NULL
df$CASHBK <- NULL
df$CRSCORE <- NULL
df$RES <- NULL
df$INAREA <- NULL
df$NSF <- NULL
df$SDB <- NULL

# The final model
log_model <- glm(INS ~ ., data=df, family=binomial(link="logit"))
summary(log_model)

# Compare two individuals and their odds ratio
# The first individual is a "Risky" inidivudal, i.e., they don't have a Money Market,
# CD account, IRA account, or a Mortgage. The Second individual is a risk adverse person.
newdata <- data.frame(ACCTAGE=c(mean(df$ACCTAGE), mean(df$ACCTAGE)),
                      DDA = c('1' , '1'),
                      DDABAL = c(mean(df$DDABAL), mean(df$DDABAL)),
                      DEPAMT = c(mean(df$DEPAMT), mean(df$DEPAMT)),
                      CHECKS = c(mean(df$CHECKS), mean(df$CHECKS)),
                      DIRDEP = c('0', '0'),
                      SAV = c('1', '1'),
                      SAVBAL = c(mean(df$SAVBAL), mean(df$SAVBAL)),
                      ATM = c('1', '1'),
                      ATMAMT = c(mean(df$ATMAMT), mean(df$ATMAMT)),
                      CD = c('0', '1'),  # Individual
                      IRA = c('0', '1'),
                      LOC = c('1', '1'),
                      ILS = c('0', '0'),
                      MM = c('0', '1'),
                      MTG = c('1', '0'),  # 1st ind has mortgage
                      MOVED = c('0', '0')
)

# See the two subjects' difference in odds
exp(diff(
  predict(log_model,
          newdata = newdata)
))




# Transform Vars ----------------------------------------------------------

df_transformed <- df

#Log transform our continuous variables that are very right skewed
hist(log(df$AC4CTAGE))
df_transformed$ACCTAGE <- log(df$ACCTAGE)

# Shift up by 400
hist(log(df$DDABAL))
df_transformed$DDABAL <- log(df$DDABAL + 400)

hist(log(df$DEPAMT))
df_transformed$DEPAMT <- log(df$DEPAMT + 1)

hist(log(df$CHECKS))

hist(df$SAVBAL)
hist(log(df$SAVBAL))
df_transformed$SAVBAL <- log(df$SAVBAL + 1)

min(df$ATMAMT)
hist(df$ATMAMT + 1)
hist(log(df$ATMAMT + 1))
df_transformed$ATMAMT <- log(df$ATMAMT + 1)



# Fit new model -----------------------------------------------------------

updated_model <- glm(INS ~ ., data=df_transformed, family=binomial(link="logit"))
summary(updated_model)





# Model w Interactions ----------------------------------------------------
test_model_df <- df_transformed
test_model_df$LOC <- NULL
test_model_df$MOVED <- NULL
# Base AIC: 9027
# Current AIC: 8954

test_model_df$savings_propensity <- test_model_df$DDABAL / (test_model_df$SAVBAL + 1)
test_model_df$cash_a_lot <- test_model_df$ATMAMT / (test_model_df$DDABAL + 1)


wealth_interaction <- glm(INS ~ . + SAV*MM + SAVBAL*MM  + SAVBAL*ATMAMT,  data=test_model_df, family=binomial(link="logit"))
summary(wealth_interaction)

