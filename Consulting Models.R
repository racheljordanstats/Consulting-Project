#require tidyverse
require(tidyverse)

#read in data
library(readr)
modeling_data <- read_csv("All Data for Modeling.csv")

#remove factor levels with not enough data
modeling_data_2 <- modeling_data %>% 
  filter(RatingHalfBath != "EX") %>% 
  filter(RatingKitchen != "VP") %>% 
  filter(RatingKitchen != "EX") %>% 
  select(-c("Carport"))

#relevel factors
modeling_data_2$PhysicalCondition <- factor(modeling_data_2$PhysicalCondition,
                                          levels=c("UN","VP","PR","FR","AV",
                                                   "GD","VG","EX"),labels=c(1:8))
modeling_data_2$Quality <- factor(modeling_data_2$Quality, levels=c("E-","E","E+",
                                                                "D-","D","D+",
                                                                "C-","C","C+",
                                                                "B-","B","B+",
                                                                "A-","A","A+",
                                                                "AA-","AA","AA+"),
                                labels=c(1:18))

modeling_data_2$RatingBath <- factor(modeling_data_2$RatingBath,
                                          levels=c("UN","VP","PR","FR","AV",
                                                   "GD","VG","EX"),labels=c(1:8))

modeling_data_2$RatingHalfBath <- factor(modeling_data_2$RatingHalfBath,
                                   levels=c("UN","VP","PR","FR","AV",
                                            "GD","VG"),labels=c(1:7))

modeling_data_2$RatingKitchen <- factor(modeling_data_2$RatingKitchen,
                                   levels=c("UN","PR","FR","AV",
                                            "GD","VG"),labels=c(1:6))

#remove LastSaleDate column
modeling_data_2 <- modeling_data_2[,-18]

#remove factors that don't have enough data
bad_factors_removed_2 <- modeling_data_2 %>% 
  filter(RatingHalfBath != "8" | RatingKitchen != "8") %>% 
  select(-c(BlacktopParking,Patio,DetatchedGarage,FrameGarage...41,AsphaltPaving,
            GunitePool,RecRoom,ChainLinkFence,MasonryGarage,BasementParking,
            Attic))

bad_factors_removed_2 <- bad_factors_removed_2 %>% 
  filter(RatingHalfBath != 8)

bad_factors_removed_2 <- na.omit(bad_factors_removed_2)
#split into train and test sets
library(caTools)
#make this example reproducible
set.seed(2022)
#use 70% of dataset as training set and 30% as test set
sample <- sample.split(bad_factors_removed_2$PropertyID, SplitRatio = 0.7)
train  <- subset(bad_factors_removed_2, sample == TRUE)
test   <- subset(bad_factors_removed_2, sample == FALSE)

#gamma GLM on training data
gamma_fit_1 <- glm(LastSalePrice ~ .,family=Gamma(link="log"),data=train)
summary(gamma_fit_1)

#calculate MSE for test set - gamma GLM with all predictors
data_test_new <- test     # Duplicate test data set
data_test_new$PhysicalCondition[which(!(data_test_new$PhysicalCondition %in% unique(train$PhysicalCondition)))] <- NA  # Replace new levels by NA
data_test_new$RatingBath[which(!(data_test_new$RatingBath %in% unique(train$RatingBath)))] <- NA  # Replace new levels by NA
data_test_new$RatingHalfBath[which(!(data_test_new$RatingHalfBath %in% unique(train$RatingHalfBath)))] <- NA  # Replace new levels by NA
data_test_new$RatingKitchen[which(!(data_test_new$RatingKitchen %in% unique(train$RatingKitchen)))] <- NA  # Replace new levels by NA
data_test_new <- na.omit(data_test_new)
mean((data_test_new$LastSalePrice - predict(gamma_fit_1, data_test_new,na.action = na.exclude))**2)

#try using bestglm because our MSE from above is ridiculous!!! 

#load bestglm package
require(bestglm)
# data_matrix <- modeling_data_2
# data_matrix$PhysicalCondition <- as.factor(data_matrix$PhysicalCondition)
# data_matrix$Quality <- as.factor(data_matrix$Quality)
# data_matrix$PrimaryWall <- as.factor(data_matrix$PrimaryWall)
# data_matrix$SeasonSold <- as.factor(data_matrix$SeasonSold)
# data_matrix$AirConditioned <- as.factor(data_matrix$AirConditioned)
# data_matrix$RatingBath <- as.factor(data_matrix$RatingBath)
# data_matrix$RatingHalfBath <- as.factor(data_matrix$RatingHalfBath)
# data_matrix$RatingKitchen <- as.factor(data_matrix$RatingKitchen)
# best_gamma <- bestglm(data_matrix,family=gamma)

#random forest
require(randomForest)

#split into train and test sets
library(caTools)
#make this example reproducible
set.seed(03030)
#use 70% of dataset as training set and 30% as test set
sample_rf <- sample.split(modeling_data$PropertyID, SplitRatio = 0.7)
train_rf  <- subset(modeling_data, sample_rf == TRUE)
test_rf  <- subset(modeling_data, sample_rf == FALSE)

random_forest_1 <- randomForest(LastSalePrice ~ ., data = train_rf,
                         importance = TRUE, na.action = na.omit,mtry=15)
print(random_forest_1)

check_rf <- function(x) {
  random_forests <- randomForest(LastSalePrice ~ ., data = train_rf,
               importance = TRUE, na.action = na.omit,mtry=x)
  return(random_forests)
}

for(i in 1:56) {
  results <- check_rf(i)
  all_results <- c(results)
}
