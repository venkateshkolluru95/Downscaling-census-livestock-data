#testing this code with 90:10 split ratio and 10km buffer data for sheep & goat density predictions (provided 100 observations for testing)
# the buffer samples were created in ArcGIS and were loaded into R
#install and load the below required libraries
library(dplyr)
library(caret)
library(mgcv)
library(randomForest)
library(xgboost)
library(caret)
library(readxl)
#load the sample data
sample <- read_excel("sample_code_and_data/buff_10km_kaz_2000.xlsx")
sample2 <- sample[,c(1,2:14)] #load the response and predictor variables
x <- as.matrix(sample2[,c(2:14)]) # convert the dataframe to a matrix
xs <- scale(x,center = TRUE, scale = TRUE) #scale/standardize the data using 'scale' function
y <- sample2$shp_gt_2000#load the response variable into a variable named 'y'
dataset <- data.frame(cbind(y,xs))#combine the scaled x and y variables
summary(dataset)#view the summary of the dataset

################################################################################
#splitting testing and training set
set.seed(30000)
ind <- sample(2, nrow(dataset), replace = TRUE, prob = c(0.9, 0.1))#split the dataset to a train and test split ratio of 90:10
train <- dataset[ind==1,]#load the splitted data into a variable named 'train'
test <- dataset[ind==2,] #load the splitted data into a variable named 'test'
###############################################################################################
# Random Forest Regression 

set.seed(30000)
#use the tuneRF function from randomForest library to identify the best subset of variables under each tree
mtry <- randomForest::tuneRF(train[,2:14],train$y ,  mtryStart = c(1:13),ntreeTry=2000, stepFactor=2, improve=0.05,
                             trace=TRUE, plot=TRUE, doBest=FALSE)
best.m <- mtry[mtry[, 2] == min(mtry[,2]), 1]#load the best subset variable number into the variable 'best.m'
print(mtry)
print(best.m) 
#############################################################################################
#specify the repeated cross validation with number of folds and repeats and hyper parameter optimization technique
control <- trainControl(method = 'repeatedcv',
                        number = 10,
                        repeats = 5,
                        search = 'grid')
#create tunegrid for the RF model
tunegrid <- expand.grid(.mtry = best.m)
modellist <- list()

#train with different ntree parameters, mtry and CV method and save the model into the variable named 'fit'
for (ntree in c(1000,1500,2000,2500,3000)){
  set.seed(1000)
  fit <- train(y ~ .,
               data = train,
               method = 'rf',
               metric = 'RMSE',
               tuneGrid = tunegrid,
               trControl = control,
               ntree = ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}


#load the CV values and results
results <- resamples(modellist)
summary(results) #view summary of fit statistics for different ntree values
##############################################################################################
# use the fitted model to predict for the test data
predicted <- predict(fit,test)
observed<-test[,1]
#load the following libraries to calculate the fit statistics
library(caret)
library(MLmetrics) 
library(Metrics)
#RMSE
sqrt(mean((predicted - observed)^2))
#correlation coefficient
(cor(predicted,observed))^2 
#Correlation
cor(predicted,observed)
#MAPE
MAPE(predicted, observed)
#MAE
caret::MAE(observed, predicted)

##################################################################################################

# use the below code to develop the final map using the predictor variable rasters
library(raster)
setwd("specify the folder where the raster images of predictor variables are available")
img<-list.files(pattern='*.tif$')#load the tiff files
stack<-stack(img)#stack all the loaded images
stack
#check the names of rasters if they are matching with the sample data from excel
names(stack)
#if they are not matching, rename the stack images according to the names from excel
names(stack) <- c("elevation","eudis_setls","lai_2000","n8_l8_2000","npp2000","pop_den_2000","pr2000","r2000","sm2000","snde2000","t2000","vpd2000","w2000")

#############################################################################
# retrieve the names of the raster layers in the raster stack object xs that have been scaled and centered.
rasterNames = names(attr(x = xs,which = "scaled:center"))
# retrieve the center values used for scaling and centering each raster layer in the stack xs.
center = attr(x = xs,which = "scaled:center")
#retrieve the scaling factors used for scaling each raster layer in the stack xs.
scales =  attr(x = xs,which = "scaled:scale")

#The following loop takes each raster layer (stack[[rasterNames[i]]]), subtracts corresponding center value (center[i]), and then divides by corresponding scaling factor (scales[i]). 
# Finally, the updated raster layer is assigned back to the same position in the stack object.
for(i in 1:13){
  stack[[rasterNames[i]]] = (stack[[rasterNames[i]]]-center[i])/scales[i]
  
}

setwd("specify the folder where you want to save the predicted raster")
#apply the developed model on the stack to predict the final raster layer
rf_shpgt <- predict(stack,fit,filename="sr_2000.tif",na.rm=TRUE, progress="text",format="GTiff", overwrite=TRUE)  

