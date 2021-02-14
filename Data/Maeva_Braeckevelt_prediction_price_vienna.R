################################################################################################
# Data analysis 3:
# Assignment 3 : Prediction of the price for Vienna's hotels
# by Maeva Braeckevelt
###############################################################################################x

# ------------------------------------------------------------------------------------------------------
# CLEAR MEMORY
rm(list=ls())

# Import libraries 
library(tidyverse)
library(stargazer)
library(haven)
library(scales)
library(lspline)
library(pander)
library(estimatr)
library(texreg)
library(ggthemes)
library(tidyverse)
library(geosphere)
library(moments)
library(dplyr)
library(knitr)
library(jtools)
library(ggplot2)
#install.packages("GGally")
library(GGally)


# load theme and functions
source("C:/Users/mbrae/OneDrive/Bureau/CEU/DA3/da_case_studies/ch00-tech-prep/theme_bg.R")
source("C:/Users/mbrae/OneDrive/Bureau/CEU/DA3/da_case_studies/ch00-tech-prep/da_helper_functions.R")
setwd("C:/Users/mbrae/OneDrive/Bureau/CEU/DA3/A1/")
options(digits = 3) 

#####################################################################


# load Vienna db

My_path <- "https://raw.githubusercontent.com/Maeva2408/DA3_A3/master/Data/hotels-vienna.csv"
data <- read_csv(paste0(My_path))
--------------------------------------------------

####SAMPLE SELECTION

# I need data from November on weekday
data$weekend
data$holiday
data$month

# data already good!

#####################################################################
# Label Engeneering

# I want to define my y variable
# It is a prediction of price to find the best deals In Vienna
# Price distribution

summary(data$price)
summary_table <- summary(data$price)
pander(summary_table)

data %>% 
  keep(is.numeric) %>%
  ggplot(aes(x= price))+
  geom_histogram(fill= "orangered4", col= "salmon")+
  theme_bw()+
  scale_fill_wsj()+
  labs(x = "Price in euro",y = "count of hotels")


# I Will filter out the hotels price more than 400 euro

data <- filter( data ,price <= 400)

data %>% 
  keep(is.numeric) %>%
  ggplot(aes(x= price))+
  geom_histogram(fill= "orangered4", col= "salmon")+
  theme_bw()+
  scale_fill_wsj()+
  labs(x = "Price in euro",y = "count of hotels")

summary(data$price)
summary_table <- summary(data$price)
pander(summary_table)

# better distribution, I will not transform my y variable, it will be easier for my residuals
# without transform it again

yvar <- "price"


#####################################################################
# Feature Engeneering
#
# detail of the variable

#### hotel_id - Hotel ID -	

# No need as a  variable

#### accommodation_type - Type of accomodation - factor

# I want only hotels so I will only keep hotels

data <- filter( data ,data$accommodation_type == 'Hotel')

#### country	- Country- string

# No need as a  variable

#### city - City based on search	- string
# not needed as variable

#### city_actual	- City actual of hotel - string

data$city_actual

# I will only used Vienna, So I will filter the other one

data <- filter( data ,data$city_actual == 'Vienna')

#### neighbourhood	- Neighburhood -string

data %>% 
  keep(is.numeric) %>%
  ggplot(aes(x= data$neighbourhood))+
  geom_histogram(fill= "orangered4", col= "salmon", stat="count")+
  theme_bw()+
  scale_fill_wsj()+
  labs(x = "Neighbourhood",y = "count of hotels")

# check if NA
sum(is.na(data$neighbourhood))
#No NA good

# I will put as Small_neighbourhood the neighbourhood that have less than 5 hotels

lessthan5 <- data %>% 
  group_by(neighbourhood) %>% 
  summarise(
    n = n()
  )

lessthan5 <- lessthan5 %>% 
  filter(n < 5)

data <- data %>% 
  mutate(
    neighbourhood = ifelse(data$neighbourhood %in% lessthan5$neighbourhood, "Small_neighbourhood", data$neighbourhood)
  )

data$f_neighbourhood <- as.factor(data$neighbourhood)

#### center1label - Centre 1 - name of location for distance -	string

# No need as a variable

# distance	Distance - from main city center -	numeric

data %>% 
  keep(is.numeric) %>%
  ggplot(aes(x= data$distance))+
  geom_histogram(fill= "orangered4", col= "salmon", stat="count")+
  theme_bw()+
  scale_fill_wsj()+
  labs(x = "Distance from city center1",y = "count of hotels")

# I will decide to only keep the one that are maximum Five mile from the city center
data <- filter( data ,data$distance <= 5)

data %>% 
  keep(is.numeric) %>%
  ggplot(aes(x= data$distance))+
  geom_histogram(fill= "orangered4", col= "salmon", stat="count")+
  theme_bw()+
  scale_fill_wsj()+
  labs(x = "Distance from city center1",y = "count of hotels")

#### center2label -	Centre 2 - name of location for distance_alter -	string

# No need as a variable

#### distance_alter -	Distance - alternative - from Centre 2 - numeric

data %>% 
  keep(is.numeric) %>%
  ggplot(aes(x= data$distance_alter))+
  geom_histogram(fill= "orangered4", col= "salmon", stat="count")+
  theme_bw()+
  scale_fill_wsj()+
  labs(x = "Distance from city center2",y = "count of hotels")

# I will decide to only keep everything
data %>% 
  keep(is.numeric) %>%
  ggplot(aes(x= data$distance_alter))+
  geom_histogram(fill= "orangered4", col= "salmon", stat="count")+
  theme_bw()+
  scale_fill_wsj()+
  labs(x = "Distance from city center",y = "count of hotels")

#### stars -	Number of stars -	numeric

data$stars

# No NA and already in good format

#### rating -	User rating average -	numeric
data$rating
min(data$rating)

# I will replace the NA with 0 no need for a flag because there are no other 0
data <- data %>% 
mutate(
    rating = ifelse(is.na(data$rating) == TRUE, 0, data$rating))

#### rating_count -	Number of user ratings -	numeric
data$rating_count
sum(is.na(data$rating_count))
# I will replace the NA with 0 no need for a flag because there are no other 0data <- data %>% 
data <- data %>% 
mutate(
    rating_count = ifelse(is.na(data$rating_count) == TRUE, 0, data$rating_count) )

#### ratingta	User - rating average (tripadvisor)	- numeric
data$ratingta
sum(is.na(data$ratingta))

# I will replace the NA with 0 no need for a flag because there are no other 0
data <- data %>% 
  mutate(
    ratingta = ifelse(is.na(data$ratingta) == TRUE, 0, data$ratingta) )

#### ratingta_count -	Number of user ratings (tripadvisor) - numeric
data$ratingta_count
sum(is.na(data$ratingta_count))

# I will replace the NA with 0 no need for a flag because there are no other 0
data <- data %>% 
  mutate(
    ratingta_count = ifelse(is.na(data$ratingta_count) == TRUE, 0, data$ratingta_count))

#### hotel_id -	Hotel ID	- numeric

# No need as a variable

#### year -	Year (YYYY) -	numeric 

# No need as a variable

#### month	- Month (MM) -	numeric  

# No need as a variable

#### weekend -	Flag, if day is a weekend -	binary

# No need as a variable

#### holiday	- Flag, if day is a public holiday -	binary

# No need as a variable


#### nnights	- Number of nights  -	factor
data$nnights

# No need as a variable, all the same values 1 night

#### price -	Pricee in EUR -	numeric

# My y variable

#### scarce_room -Flag, if room was noted as scarce -	binary
data$scarce_room

# ok this way

#### offer -	Flag, if there was an offer available -	binary
data$offer

# ok

#### offer_cat	- Type of offer -	factor
data$offer_cat
sum(is.na(data$offer_cat))
# ok this way


## Now I will put the variable into categories

# f_neighbourhood
# distance	Distance - from main city center -	numeric
# distance_alter -	Distance - alternative - from Centre 2 - numeric
# stars -	Number of stars -	numeric
# rating -	User rating average -	numeric
# rating_count -	Number of user ratings -	numeric
# ratingta	User - rating average (tripadvisor)	- numeric
# ratingta_count -	Number of user ratings (tripadvisor) - numeric
# scarce_room -Flag, if room was noted as scarce -	binary
# offer -	Flag, if there was an offer available -	binary
# offer_cat	- Type of offer -	factor

location <- c("f_neighbourhood", "distance", "distance_alter")

rating <- c("stars", "rating","rating_count", "ratingta",
          "ratingta_count")


value <- c("scarce_room", "offer", "offer_cat")

# I will add some interactions based on common knwoledge

interactions <- c("distance*stars", "distance_alter*stars", "distance*rating", "distance_alter*rating")

# I will choose my models them by adding more and more categories


X1 <- c(location)
X2 <- c(location,rating)
X3 <- c(location,rating, value)
X4 <- c(location,rating, value, interactions)


# Models

model1 <- paste0(" ~ ",paste(X1,collapse = " + "))
model2 <- paste0(" ~ ",paste(X2,collapse = " + "))
model3 <- paste0(" ~ ",paste(X3,collapse = " + "))
model4 <- paste0(" ~ ",paste(X4,collapse = " + "))

#################################
# Separate hold-out set #
#################################

# I will not do an holdhout set because I only have 246 observations and my goal is not to predict price for a live data
# but find the best deals in my actual data


##############################
#      cross validation      #
##############################

## N = 5
n_folds=5
# Create the folds
set.seed(20180123)

folds_i <- sample(rep(1:n_folds, length.out = nrow(data) ))
# Create results
model_results_cv <- list()

## OLS

for (i in (1:4)){
  model_name <-  paste0("model",i)
  model_pretty_name <- paste0("(",i,")")
  
  yvar <- "price"
  xvars <- eval(parse(text = model_name))
  formula <- formula(paste0(yvar,xvars))
  
  # Initialize values
  rmse_train <- c()
  rmse_test <- c()
  
  model_data <- lm(formula,data = data)
  BIC <- BIC(model_data)
  nvars <- model_data$rank -1
  r2 <- summary(model_data)$r.squared
  
  # Do the k-fold estimation
  for (k in 1:n_folds) {
    test_i <- which(folds_i == k)
    # Train sample: all except test_i
    data_train <- data[-test_i, ]
    # Test sample
    data_test <- data[test_i, ]
    # Estimation and prediction
    model <- lm(formula,data = data_train)
    prediction_train <- predict(model, newdata = data_train)
    prediction_test <- predict(model, newdata = data_test)
    
    # Criteria evaluation
    rmse_train[k] <- mse_lev(prediction_train, data_train[,yvar] %>% pull)**(1/2)
    rmse_test[k] <- mse_lev(prediction_test, data_test[,yvar] %>% pull)**(1/2)
    
  }
  
  model_results_cv[[model_name]] <- list(yvar=yvar,xvars=xvars,formula=formula,model_data=model_data,
                                         rmse_train = rmse_train,rmse_test = rmse_test,BIC = BIC,
                                         model_name = model_pretty_name, nvars = nvars, r2 = r2)
}

model <- lm(formula,data = data_train)

t1 <- imap(model_results_cv,  ~{
  as.data.frame(.x[c("rmse_test", "rmse_train")]) %>%
    dplyr::summarise_all(.funs = mean) %>%
    mutate("model_name" = .y , "model_pretty_name" = .x[["model_name"]] ,
           "nvars" = .x[["nvars"]], "r2" = .x[["r2"]], "BIC" = .x[["BIC"]])
}) %>%
  bind_rows()
t1
pander(t1)


# The model 3 has the lowest RMSE and the BIC slightly higher than  the model 2
# I will chose model 3 for cart and RF

# CART

train_control <- trainControl(method = "cv", number = n_folds)

model_cart <- formula(formula(paste0(yvar,model3)))

set.seed(20180123)

system.time({
  cart_model <- train(model_cart,
    data = data,
    method = "rpart",
    tuneLength = 10,
    trControl = train_control
  )
})

cart_model
summary(cart_model)

# It looks like my Rmse is around 41 so it is similar as OLS


## Random forest

# do 5-fold CV
train_control <- trainControl(method = "cv",
                              number = 5,
                              verboseIter = FALSE)


# set tuning
tune_grid <- expand.grid(
  .mtry = c(3, 5, 7),
  .splitrule = "variance",
  .min.node.size = c(5, 10)
)


# Model RF
set.seed(20180123)
system.time({
  rf_model <- train(model_cart,
    data = data,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity"
  )
})
rf_model

# My Rmse is around 36, so it will probably be my best model!


## Compare models

final_models <-
  list("CART" = cart_model,
       "Random_forest" = rf_model)

results <- resamples(final_models) %>% summary()

results

pander(t1)


## So the model with the best RMSE is the Random forest, it also have the best Rsquared.

## I will chose this model for my prediction So I will the Run the prediction on the whole dataset with the best parameter

# set tuning for final model

train_control <- trainControl(method = "none",verboseIter = FALSE)

tune_grid <- expand.grid(
  .mtry = c(5),
  .splitrule = "variance",
  .min.node.size = c(5)
)
set.seed(20180123)
rf_model_final <- train(model_cart,
                        data = data,
                        method = "ranger",
                        trControl = train_control,
                        tuneGrid = tune_grid,
                        importance = "impurity"
)

rf_model_final

# Predictions -------------------------------------------------------------


rf_predicted_probabilities <- predict(rf_model_final, newdata = data, type = "raw")
data$rf_predicted_probabilities <-rf_predicted_probabilities

# Calculate residuals

data$rf_prediction_res <- data$price - data$rf_predicted_probabilities

data %>% select(price, rf_prediction_res, rf_predicted_probabilities)

# Check the ones with the smallest (negative) residuals 

Five5hotels <- data %>% top_n( -5 , rf_prediction_res ) %>% 
  select( hotel_id , price, stars, rf_predicted_probabilities,rf_prediction_res , rf_prediction_res )

# List of 5 best deals
bestdeals <- data %>%
  select(hotel_id, price, ,rf_prediction_res, distance, stars, rating) %>%
  arrange(rf_prediction_res) %>%
  .[1:5,] %>%
  as.data.frame() 

pander(bestdeals)

# best deals from chapter 10

Other5hotels <- data[data$hotel_id %in% c(21912, 21975, 22080, 22184, 22344),]
bestotherdeals <- Other5hotels  %>%
  select(hotel_id, price,rf_prediction_res, distance, stars, rating) %>%
  .[1:5,] %>%
  as.data.frame() 

pander(bestotherdeals)


## All differents hotels because I did not filter only 3 and 4 stars

## I can compare for the 3 and 4 stars

data3and4 <- data %>% filter(stars>=3 & stars<=4) 

bestdeals3and4 <- data3and4 %>%
  select(hotel_id, price,rf_prediction_res, distance, stars, rating) %>%
  arrange(rf_prediction_res) %>%
  .[1:5,] %>%
  as.data.frame() 

pander(bestdeals3and4)

## Now I have two in common!!

# Plot price vs predicted price

level_vs_pred <- ggplot(data = data) +
  geom_point(aes(y=price, x=data$rf_predicted_probabilities), color = "Orangered4", size = 1,
             shape = 16, alpha = 0.7, show.legend=FALSE) +
  geom_segment(aes(x = 0, y = 0, xend = 400, yend = 400), size=0.5, color="salmon", linetype=2) +
  coord_cartesian(xlim = c(0, 400), ylim = c(0, 400)) +
  scale_x_continuous(expand = c(0.01,0.01),limits=c(0, 400), breaks=seq(0, 400, by=50)) +
  scale_y_continuous(expand = c(0.01,0.01),limits=c(0, 400), breaks=seq(0, 400, by=50)) +
  labs(y = "Price (EURO)", x = "Predicted price  (EURO)") +
  theme_bg() 
level_vs_pred 

