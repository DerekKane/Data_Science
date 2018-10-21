---
  title: "XGBoost Rossman Parameter Tuning"
author: "khozzy"
date: "16 October 2015"
output: html_document
---
  
  #Introduction
  The following document can be used to tune-in XGBoost hyper-parameters. It is designed to experiment with different combinations of features, parameters and compare results.

##Analysis info
The following information show brief overview of what was done:
  
  - rows with missing values were removed,
- extracted `Day`, `Month`, `Year` from `Date`,
- `Sales` transformed to `LogSales`,
- `Date`, `Promo2SinceWeek`, `PromoInterval`, `Sales`, `Customers` features dropped,
- missing values in `CompetitionDistance` were filled with their average value

#Loading data

##Read CSV files
Files are read with `readr` package which provides significant speed improvements.
```{r}
library(readr)

train.raw <- read_csv('../input/train.csv', col_types="ncDnncccc")
store.raw <- read_csv('../input/store.csv', col_types='nccnnncnnc', na='')

# Parse features to factors
train.raw$DayOfWeek <- as.factor(train.raw$DayOfWeek)
train.raw$Open <- as.factor(train.raw$Open)
train.raw$Promo <- as.factor(train.raw$Promo)
train.raw$StateHoliday <- as.factor(train.raw$StateHoliday)
train.raw$SchoolHoliday <- as.factor(train.raw$SchoolHoliday)

store.raw$StoreType <- as.factor(store.raw$StoreType)
store.raw$Assortment <- as.factor(store.raw$Assortment)
store.raw$Promo2 <- as.factor(store.raw$Promo2)
```

The data is like:
  ```{r}
str(train.raw)
str(store.raw)
```


##Feature engineering
In this part you generally apply some cleaning and feature extraction methods
```{r, message=FALSE}
require(dplyr)
require(lubridate)

# Join datasets by `Store` column
train.full <- left_join(train.raw, store.raw, by = 'Store')

# Filtering
train.full <- train.full %>%
  filter(Sales > 0) %>%
  filter(Open == 1) %>%
  mutate(Day = lubridate::day(Date)) %>%
  mutate(Month = lubridate::month(Date)) %>%
  mutate(Year = lubridate::year(Date)) %>%
  mutate(LogSales = log(Sales))

# Drop unnecessary columns
train.full$Date <- NULL
train.full$Promo2SinceWeek <- NULL
train.full$PromoInterval <- NULL
train.full$Sales <- NULL
train.full$Customers <- NULL

# Remove columns below due to many NAs
train.full$CompetitionOpenSinceYear <- NULL
train.full$CompetitionOpenSinceMonth <- NULL
train.full$Promo2SinceYear <- NULL

# Fill CompetitionDistance with the average value
meanCompetitionDistance <- mean(train.full$CompetitionDistance, na.rm = TRUE)
train.full[is.na(train.full$CompetitionDistance), c("CompetitionDistance")] <- meanCompetitionDistance
```

#Prediction
##Prepare the data
Before proceeding the data frame will be stored as a *sparse matrix*. The `LogSales` column will be treated as a target variable and therefore removed from the matrix:
  ```{r, message=FALSE}
require(Matrix)

train.full.sparse <- sparse.model.matrix(LogSales~.-1, data=train.full)
```

Data will be stored using `DGMatrix` class, which is a recommended way
```{r, message=FALSE}
require(xgboost)

dtrain <- xgb.DMatrix(
  data=train.full.sparse, 
  label=train.full$LogSales)
```

##Evaluation metric
Notice that the results are evaluated using *Root Mean Square Percentage Error* ([link](https://www.kaggle.com/c/rossmann-store-sales/details/evaluation)). XGBoost allows us to specify custom metric for validating the results:
  ```{r}
rmpse <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  elab <- exp(as.numeric(labels))
  epreds <- exp(as.numeric(preds))
  err <- sqrt(mean((epreds/elab-1)^2))
  return(list(metric = "RMPSE", value = err))
}
```

##Parameters
In this part you can experiment how different parameter can be tuned to acquire better scores
```{r}
param <- list(
  objective="reg:linear",
  booster="gbtree",
  eta=0.8, # Control the learning rate
  max.depth=8, # Maximum depth of the tree
  subsample=0.7, # subsample ratio of the training instance
  colsample_bytree=0.7 # subsample ratio of columns when constructing each tree
)
```

##Training
Training is done using 5-fold CV. You can also tune some parameters here (like the number of trees - `nround`):
  ```{r}
history <- xgb.cv(
  data=dtrain,
  params = param,
  early.stop.round=30, # training with a validation set will stop if the performance keeps getting worse consecutively for k rounds
  nthread=4, # number of CPU threads
  nround=50, # number of trees
  verbose=0, # do not show partial info
  nfold=5, # number of CV folds
  feval=rmpse, # custom evaluation metric
  maximize=FALSE # the lower the evaluation score the better
)
```

#Results
The best score obtained: **`r min(history$test.RMPSE.mean)`**
  
  The process of training the data is visualized on the following plot:
  ```{r, message=FALSE}
require(ggplot2)

history$trees <- as.integer(rownames(history))

ggplot(history, aes(x=trees, y=test.RMPSE.mean)) +
  geom_line() +
  geom_errorbar(
    aes(ymin=test.RMPSE.mean-test.RMPSE.std, ymax=test.RMPSE.mean+test.RMPSE.std), 
    width=.05, 
    color="red") +
  ggtitle("Rossman training RMPSE using 5-fold CV") + xlab("Number of trees") + ylab("RMPSE") +
  annotate("text", 
           x=max(history$trees), 
           y=max(history$test.RMPSE.mean)-0.1, 
           label=paste("Best RMPSE:\n", min(history$test.RMPSE.mean)), 
           alpha=.5, 
           hjust=1) +
  theme_bw()
```

