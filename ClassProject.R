library(corrplot)

housing <- read.csv("./housing.csv")
head(housing)
#> head(housing)
#  longitude latitude housing_median_age total_rooms total_bedrooms population households
#1   -122.23    37.88                 41         880            129        322        126
#2   -122.22    37.86                 21        7099           1106       2401       1138
#3   -122.24    37.85                 52        1467            190        496        177
#4   -122.25    37.85                 52        1274            235        558        219
#5   -122.25    37.85                 52        1627            280        565        259
#6   -122.25    37.85                 52         919            213        413        193
#median_income median_house_value ocean_proximity
#1        8.3252             452600        NEAR BAY
#2        8.3014             358500        NEAR BAY
#3        7.2574             352100        NEAR BAY
#4        5.6431             341300        NEAR BAY
#5        3.8462             342200        NEAR BAY
#6        4.0368             269700        NEAR BAY

housing$ocean_proximity <- as.factor(housing$ocean_proximity)

levels(housing$ocean_proximity)
#> levels(housing$ocean_proximity)
#[1] "<1H OCEAN"  "INLAND"     "ISLAND"     "NEAR BAY"   "NEAR OCEAN"

head(housing)
#> head(housing)
#  longitude latitude housing_median_age total_rooms total_bedrooms population households
#1   -122.23    37.88                 41         880            129        322        126
#2   -122.22    37.86                 21        7099           1106       2401       1138
#3   -122.24    37.85                 52        1467            190        496        177
#4   -122.25    37.85                 52        1274            235        558        219
#5   -122.25    37.85                 52        1627            280        565        259
#6   -122.25    37.85                 52         919            213        413        193
#  median_income median_house_value ocean_proximity
#1        8.3252             452600        NEAR BAY
#2        8.3014             358500        NEAR BAY
#3        7.2574             352100        NEAR BAY
#4        5.6431             341300        NEAR BAY
#5        3.8462             342200        NEAR BAY
#6        4.0368             269700        NEAR BAY

tail(housing)
#> tail(housing)
#      longitude latitude housing_median_age total_rooms total_bedrooms population households
#20635   -121.56    39.27                 28        2332            395       1041        344
#20636   -121.09    39.48                 25        1665            374        845        330
#20637   -121.21    39.49                 18         697            150        356        114
#20638   -121.22    39.43                 17        2254            485       1007        433
#20639   -121.32    39.43                 18        1860            409        741        349
#20640   -121.24    39.37                 16        2785            616       1387        530
#      median_income median_house_value ocean_proximity
#20635        3.7125             116800          INLAND
#20636        1.5603              78100          INLAND
#20637        2.5568              77100          INLAND
#20638        1.7000              92300          INLAND
#20639        1.8672              84700          INLAND
#20640        2.3886              89400          INLAND

summary(housing)
#> summary(housing)
# longitude         latitude     housing_median_age  total_rooms    total_bedrooms  
#Min.   :-124.3   Min.   :32.54   Min.   : 1.00      Min.   :    2   Min.   :   1.0  
#1st Qu.:-121.8   1st Qu.:33.93   1st Qu.:18.00      1st Qu.: 1448   1st Qu.: 296.0  
#Median :-118.5   Median :34.26   Median :29.00      Median : 2127   Median : 435.0  
#Mean   :-119.6   Mean   :35.63   Mean   :28.64      Mean   : 2636   Mean   : 537.9  
#3rd Qu.:-118.0   3rd Qu.:37.71   3rd Qu.:37.00      3rd Qu.: 3148   3rd Qu.: 647.0  
#Max.   :-114.3   Max.   :41.95   Max.   :52.00      Max.   :39320   Max.   :6445.0  
#                                                                    NA's   :207     
#   population      households     median_income     median_house_value   ocean_proximity
# Min.   :    3   Min.   :   1.0   Min.   : 0.4999   Min.   : 14999     <1H OCEAN :9136  
# 1st Qu.:  787   1st Qu.: 280.0   1st Qu.: 2.5634   1st Qu.:119600     INLAND    :6551  
# Median : 1166   Median : 409.0   Median : 3.5348   Median :179700     ISLAND    :   5  
# Mean   : 1425   Mean   : 499.5   Mean   : 3.8707   Mean   :206856     NEAR BAY  :2290  
# 3rd Qu.: 1725   3rd Qu.: 605.0   3rd Qu.: 4.7432   3rd Qu.:264725     NEAR OCEAN:2658  
# Max.   :35682   Max.   :6082.0   Max.   :15.0001   Max.   :500001 

numeric_vars <- housing[sapply(housing, is.numeric)]
print(numeric_vars)
cor_matrix <- cor(numeric_vars, use = "complete.obs")
cor_matrix
#>cor_matrix
#                     longitude    latitude housing_median_age total_rooms total_bedrooms
#longitude           1.00000000 -0.92461611        -0.10935655  0.04548017     0.06960802
#latitude           -0.92461611  1.00000000         0.01189907 -0.03666681    -0.06698283
#housing_median_age -0.10935655  0.01189907         1.00000000 -0.36062830    -0.32045104
#total_rooms         0.04548017 -0.03666681        -0.36062830  1.00000000     0.93037950
#total_bedrooms      0.06960802 -0.06698283        -0.32045104  0.93037950     1.00000000
#population          0.10027030 -0.10899734        -0.29578730  0.85728125     0.87774674
#households          0.05651277 -0.07177419        -0.30276797  0.91899153     0.97972827
#median_income      -0.01555015 -0.07962632        -0.11827772  0.19788152    -0.00772285
#median_house_value -0.04539822 -0.14463821         0.10643205  0.13329413     0.04968618
#                     population  households median_income median_house_value
#longitude           0.100270301  0.05651277  -0.015550150        -0.04539822
#latitude           -0.108997344 -0.07177419  -0.079626319        -0.14463821
#housing_median_age -0.295787297 -0.30276797  -0.118277723         0.10643205
#total_rooms         0.857281251  0.91899153   0.197881519         0.13329413
#total_bedrooms      0.877746743  0.97972827  -0.007722850         0.04968618
#population          1.000000000  0.90718590   0.005086624        -0.02529973
#households          0.907185900  1.00000000   0.013433892         0.06489355
#median_income       0.005086624  0.01343389   1.000000000         0.68835548
#median_house_value -0.025299732  0.06489355   0.688355475         1.00000000
dev.new(width = 12, height = 12)
par(mfrow = c(3, 3))
for (col in colnames(numeric_vars)) {
  hist(numeric_vars[[col]], main = paste("Histogram of", col), xlab = col, col = "blue")
}

dev.new(width = 12, height = 12)
par(mfrow = c(3, 3)) # Set up the plotting area for multiple plots
for(col in colnames(numeric_vars)) {
  boxplot(numeric_vars[[col]], main = paste("Boxplot of", col), xlab = col, col = "green")
}

dev.new(width = 12, height = 12)
par(mfrow = c(1, 3)) # Set up the plotting area for three plots
boxplot(housing$housing_median_age ~ housing$ocean_proximity, main = "Housing Median Age by Ocean Proximity", xlab = "Ocean Proximity", ylab = "Housing Median Age", col = "red")
boxplot(housing$median_income ~ housing$ocean_proximity, main = "Median Income by Ocean Proximity", xlab = "Ocean Proximity", ylab = "Median Income", col = "red")
boxplot(housing$median_house_value ~ housing$ocean_proximity, main = "Median House Value by Ocean Proximity", xlab = "Ocean Proximity", ylab = "Median House Value", col = "red")

library(e1071)
library(Hmisc)
housing$total_bedrooms[is.na(housing$total_bedrooms)] <- impute(housing$total_bedrooms[is.na(housing$total_bedrooms)], what='median')
housing$total_bedrooms <- impute(housing$total_bedrooms, what = "median")
summary(housing)
#> summary(housing)

#207 values imputed to 435 

# longitude         latitude     housing_median_age  total_rooms    total_bedrooms  
#Min.   :-124.3   Min.   :32.54   Min.   : 1.00      Min.   :    2   Min.   :   1.0  
#1st Qu.:-121.8   1st Qu.:33.93   1st Qu.:18.00      1st Qu.: 1448   1st Qu.: 297.0  
#Median :-118.5   Median :34.26   Median :29.00      Median : 2127   Median : 435.0  
#Mean   :-119.6   Mean   :35.63   Mean   :28.64      Mean   : 2636   Mean   : 536.8  
#3rd Qu.:-118.0   3rd Qu.:37.71   3rd Qu.:37.00      3rd Qu.: 3148   3rd Qu.: 643.2  
#Max.   :-114.3   Max.   :41.95   Max.   :52.00      Max.   :39320   Max.   :6445.0  
#  population      households     median_income     median_house_value   ocean_proximity
#Min.   :    3   Min.   :   1.0   Min.   : 0.4999   Min.   : 14999     <1H OCEAN :9136  
#1st Qu.:  787   1st Qu.: 280.0   1st Qu.: 2.5634   1st Qu.:119600     INLAND    :6551  
#Median : 1166   Median : 409.0   Median : 3.5348   Median :179700     ISLAND    :   5  
#Mean   : 1425   Mean   : 499.5   Mean   : 3.8707   Mean   :206856     NEAR BAY  :2290  
#rd Qu.: 1725   3rd Qu.: 605.0   3rd Qu.: 4.7432   3rd Qu.:264725     NEAR OCEAN:2658  
#Max.   :35682   Max.   :6082.0   Max.   :15.0001   Max.   :500001   


#median_bedrooms <- median(housing$total_bedrooms, na.rm = TRUE)

#housing$total_bedrooms[is.na(housing$total_bedrooms)] <- median_bedrooms

#summary(housing)

#install.packages("dplyr")
library(dplyr)
housing <- housing %>%
  mutate(`NEAR BAY` = ifelse(ocean_proximity == "NEAR BAY", 1, 0),
         `<1H OCEAN` = ifelse(ocean_proximity == "<1H OCEAN", 1, 0),
         `INLAND` = ifelse(ocean_proximity == "INLAND", 1, 0),
         `NEAR OCEAN` = ifelse(ocean_proximity == "NEAR OCEAN", 1, 0),
         `ISLAND` = ifelse(ocean_proximity == "ISLAND", 1, 0))

housing <- housing %>%
  select(-ocean_proximity)

colnames(housing)
#> colnames(housing)
#[1] "longitude"          "latitude"           "housing_median_age" "total_rooms"       
#[5] "total_bedrooms"     "population"         "households"         "median_income"     
#[9] "median_house_value" "NEAR BAY"           "<1H OCEAN"          "INLAND"            
#[13] "NEAR OCEAN"         "ISLAND"  


housing <- housing %>%
  mutate(mean_bedrooms = total_bedrooms / households,
         mean_rooms = total_rooms / households)

housing <- housing %>%
  select(-total_bedrooms, -total_rooms)

colnames(housing)
#> colnames(housing)
#[1] "longitude"          "latitude"           "housing_median_age" "population"        
#[5] "households"         "median_income"      "median_house_value" "NEAR BAY"          
#[9] "<1H OCEAN"          "INLAND"             "NEAR OCEAN"         "ISLAND"            
#[13] "mean_bedrooms"      "mean_rooms"        

numeric_vars <- housing %>%
  select(longitude, latitude, housing_median_age, population, households, median_income, mean_bedrooms, mean_rooms)


scaled_numeric_vars <- as.data.frame(scale(numeric_vars))

cleaned_housing <- cbind(scaled_numeric_vars, housing %>% select(`NEAR BAY`, `<1H OCEAN`, `INLAND`, `NEAR OCEAN`, `ISLAND`, median_house_value))

head(cleaned_housing)
#> head(cleaned_housing)
#  longitude latitude housing_median_age population households median_income mean_bedrooms
#1 -1.327803 1.052523          0.9821189 -0.9744050 -0.9770092    2.34470896  -0.148510661
#2 -1.322812 1.043159         -0.6070042  0.8614180  1.6699206    2.33218146  -0.248535936
#3 -1.332794 1.038478          1.8561366 -0.8207575 -0.8436165    1.78265622  -0.052900657
#4 -1.337785 1.038478          1.8561366 -0.7660095 -0.7337637    0.93294491  -0.053646030
#5 -1.337785 1.038478          1.8561366 -0.7598283 -0.6291419   -0.01288068  -0.038194658
#6 -1.337785 1.038478          1.8561366 -0.8940491 -0.8017678    0.08744452   0.005232996
#  mean_rooms NEAR BAY <1H OCEAN INLAND NEAR OCEAN ISLAND median_house_value
#1  0.6285442        1         0      0          0      0             452600
#2  0.3270334        1         0      0          0      0             358500
#3  1.1555925        1         0      0          0      0             352100
#4  0.1569623        1         0      0          0      0             341300
#5  0.3447024        1         0      0          0      0             342200
#6 -0.2697231        1         0      0          0      0             269700

#head(housing)

set.seed(314) 
sample_index <- sample(1:nrow(cleaned_housing), size = 0.7 * nrow(cleaned_housing))
training_set <- cleaned_housing[sample_index, ]
testing_set <- cleaned_housing[-sample_index, ]
dim(training_set)
#> dim(training_set)
#[1] 14447    14

dim(testing_set)
#> dim(testing_set)
#[1] 6193   14

library(randomForest)
train <- cleaned_housing[sample_index, ]

test <- cleaned_housing[-sample_index, ]
train_x <- train %>% select(-median_house_value)
train_y <- train$median_house_value
rf <- randomForest(x = train_x, y = train_y, ntree = 500, importance = TRUE)
names(rf)

mse <- rf$mse[length(rf$mse)]
rmse_train <- sqrt(mse)
rmse_train

test_x <- test %>% select(-median_house_value)
test_y <- test$median_house_value

predictions <- predict(rf, newdata = test_x)

calc_rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

rmse_test <- calc_rmse(test_y, predictions)
print(paste("Test RMSE: ", rmse_test))

print(paste("Training RMSE: ", rmse_train))
#> print(paste("Training RMSE: ", rmse_train))
#[1] "Training RMSE:  49850.295687348"

print(paste("Test RMSE: ", rmse_test))
#> print(paste("Test RMSE: ", rmse_test))
#[1] "Test RMSE:  48593.2906954222"

varImpPlot(rf)

importance(rf)
#> importance(rf)
#                     %IncMSE IncNodePurity
#longitude          59.328115  2.172981e+13
#latitude           57.728362  1.921735e+13
#housing_median_age 78.161884  8.614781e+12
#population         38.284901  6.756124e+12
#households         40.273566  7.093733e+12
#median_income      91.286594  6.471642e+13
#mean_bedrooms      46.970522  6.856702e+12
#mean_rooms         46.160099  1.953651e+13
#NEAR BAY           12.637580  9.956542e+11
#<1H OCEAN          20.344698  3.730462e+12
#INLAND             49.368558  2.760789e+13
#NEAR OCEAN         17.108986  1.939435e+12
#ISLAND              4.885867  6.650189e+10

