# Install missing packages
if (!require(tibble)) install.packages('tibble')
if (!require(readr)) install.packages('readr')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(corrplot)) install.packages('corrplot')
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(animation)) install.packages('animation')
if (!require(RColorBrewer)) install.packages('RColorBrewer')
if (!require(e1071)) install.packages('e1071')
if (!require(caret)) install.packages('caret')
if (!require(randomForest)) install.packages('randomForest')
if (!require(doParallel)) install.packages('doParallel')
if (!require(animation)) install.packages('animation')


# Display the global power plant schema

library(tibble)
library(readr)
library(tidyverse)

gppb <- read.csv("global_power_plant_database.csv", header = TRUE)

gppb1 <- tibble(gppb)

head(gppb1)

# Choose columns wit the most essential data

gppb2 <- gppb1 %>% select(gppd_idnr,capacity_mw,latitude,longitude,primary_fuel,
                          generation_gwh_2013,generation_gwh_2014,generation_gwh_2015,generation_gwh_2016,generation_gwh_2017)

head(gppb2)

# Assume the energy production was zero in the years that it was not recorded
gppb2[is.na(gppb2)] <- 0
head(gppb2)

library(corrplot)

# factorize the character columns
gppb3 <- gppb2
gppb3$gppd_idnr <- as.numeric(as.factor(gppb3$gppd_idnr))
gppb3$primary_fuel <- as.numeric(as.factor(gppb3$primary_fuel))
head(gppb3)

# Plot the correlation figure
corrplot(cor(gppb3),method = "color",type = "upper")

# Average the energy generation columns into one

gppb3$avg_gwh <- rowMeans(gppb3[,c(6:11)], na.rm = TRUE)

gppb4 <- gppb3 %>% select(-c(generation_gwh_2013,generation_gwh_2014,generation_gwh_2015,generation_gwh_2016,generation_gwh_2017,estimated_generation_gwh))
head(gppb4)

# remove rows with zero average energy generation
gppb5 <- gppb4[gppb4$avg_gwh != 0,]

#plot new correlation plot
corrplot(cor(gppb5),method = "color",type = "upper")

library(ggplot2)

# Add a column with string chracters for primary fuel
gppb4$primary_fuel_chr <- add_column(gppb2$primary_fuel)

# Create a plot
gppb4 %>% select(avg_gwh,primary_fuel_chr) %>% group_by(primary_fuel_chr) %>% summarise(mean_gwh = mean(avg_gwh)) %>% ggplot(aes(primary_fuel_chr,mean_gwh)) + geom_bar(stat = "identity", color = "blue", fill = "blue") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Create a plot
gppb4 %>% select(avg_gwh,primary_fuel_chr) %>% group_by(primary_fuel_chr) %>% summarise(total_gwh = sum(avg_gwh)) %>% ggplot(aes(primary_fuel_chr,total_gwh)) + geom_bar(stat = "identity", color = "blue", fill = "blue") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Round off the latitude and longitudes into integers for stratification
gppb6 <- gppb5
gppb6$longitude <- round(gppb6$longitude)
gppb6$latitude <- round(gppb6$latitude)
head(gppb6)

#edit and rescale the dataset
gppb7 <- gppb6 %>% mutate(capacity_mw = scale(capacity_mw),
                          longitude = scale(longitude),
                          primary_fuel = scale(primary_fuel),
                          avg_gwh = scale(avg_gwh)) %>% select(-c(gppd_idnr,latitude))

head(gppb7)

# Make the first animation of latitude and primary fuel
set.seed(2233)
library(animation)
ani.options(nmax = 3)
kmeans.ani(gppb7[c(2,4)],6, pch = 1:6, col = 1:6)

# Analyse the best number of clusters
k.max <- 15
data <- gppb7
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss

# plot total within sum of squares for each cluster count
tab <- cbind.data.frame(1:k.max,wss)
colnames(tab) <- c("k","ws")
tab %>% ggplot(aes(k, ws)) + geom_line() +geom_point()

# Redo kmeans with optimal k
set.seed(2233)
clst <- kmeans(gppb7,6)
clst$size
centr <- clst$centers
centr

# create dataset for reshape
cluster <- c(1: 6)
centered <- data.frame(cluster, centr)

# Reshape the data
reshaped <- gather(centered, feature, value, capacity_mw: avg_gwh)
head(reshaped,10)

library(RColorBrewer)
# Create the palette
heat.palette <-colorRampPalette(rev(brewer.pal(10, 'RdYlGn')),space='Lab')

#make the plot of the heatmap
reshaped %>% ggplot(aes(x = feature, y = cluster, fill = value)) +
  scale_y_continuous(breaks = seq(1, 6, by = 1)) +
  geom_tile() +
  coord_equal() +
  scale_fill_gradientn(colours = heat.palette(90)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

library(caret)
library(e1071)

# Split the data into train and test sets
# Here we use the version of our datasets that contains-
# all important columns with original values
set.seed(2233)
test_ind <- createDataPartition(as.factor(gppb5$primary_fuel),p=0.2, list = FALSE, times = 1)

test_data <- gppb5[test_ind,]
train_data <- gppb5[-test_ind,]

head(test_data)

library(randomForest)
library(doParallel)

set.seed(1558)

# set up parallelism
cores <- 3
clt <- makePSOCKcluster(cores)
registerDoParallel(clt)

# choose parameters for training
trControl <- trainControl(method = "cv",
                          number = 7,
                          search = "grid")

# tune the grid 
tunegrid = expand.grid(.mtry=c(1:20))

# train the model
rf_model <- train(as.factor(primary_fuel)~., sampsize=7000,train_data, method = "parRF", ntree=300, tuneGrid = tunegrid, trControl = trControl)

#the loop
loop_rf <- foreach(ntree=rep(300,cores),
                   .combine = combine,
                   .multicombine = TRUE,
                   .packages = "randomForest") %dopar% 
  randomForest(as.factor(primary_fuel)~., sampsize =7000,train_data, ntree=ntree, tuneGrid = tunegrid)

stopCluster(clt)
print(rf_model)

library(randomForest)
library(doParallel)

set.seed(1558)

# set up parallelism
cores <- 3
clt <- makePSOCKcluster(cores)
registerDoParallel(clt)

# set mtry
tunegrid = expand.grid(.mtry = 3)

# choose parameters for training
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")

# train the model
rf_model1 <- train(as.factor(primary_fuel)~., sampsize=10000,train_data, method = "parRF", ntree=500, tuneGrid = tunegrid, trControl = trControl)

#the loop
loop_rf <- foreach(ntree=rep(500,cores),
                   .combine = combine,
                   .multicombine = TRUE,
                   .packages = "randomForest") %dopar% 
  randomForest(as.factor(primary_fuel)~., sampsize =10000,train_data, ntree=ntree, tuneGrid = tunegrid)

stopCluster(clt)
print(rf_model1)

# make the prediction
pred <- predict(rf_model1,test_data)

# Check the accuracy of the prediction
acc <- confusionMatrix(pred, as.factor(test_data$primary_fuel))
acc$overall

set.seed(1558)
#train the model
svm_model <- svm(as.factor(primary_fuel)~., train_data, type = "C-classification", kernel = "linear")
print(svm_model)

set.seed(1558)
# make the prediction
pred <- predict(svm_model,test_data)

# Check the accuracy of the prediction
acc <- mean(test_data$primary_fuel==pred)
acc

set.seed(1558)
#train the model
svm_model1 <- svm(as.factor(primary_fuel)~., train_data, cost = 20, type = "C-classification", kernel = "linear")


set.seed(1558)
# make the prediction
pred <- predict(svm_model1,test_data)

# Check the accuracy of the prediction
acc <- mean(test_data$primary_fuel==pred)
acc

set.seed(1558)
#train the model
svm_model2 <- svm(as.factor(primary_fuel)~., train_data, type = "C-classification", kernel = "radial")

set.seed(1558)
# make the prediction
pred <- predict(svm_model2,test_data)

# Check the accuracy of the prediction
acc <- mean(test_data$primary_fuel==pred)
acc

set.seed(1558)
#train the model
svm_model3 <- svm(as.factor(primary_fuel)~., train_data,cost = 200, type = "C-classification", kernel = "radial")

set.seed(1558)
# make the prediction
pred <- predict(svm_model3,test_data)

# Check the accuracy of the prediction
acc <- mean(test_data$primary_fuel==pred)
acc