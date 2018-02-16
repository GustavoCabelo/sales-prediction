#Library####
library(readr)
library(lattice)
library(ggplot2)
library(caret)
library(corrplot)
library(datasets)
library(ggplot2)
library(reshape2)
library(base)
library(stats)
library(e1071)
library(magrittr)

#Loading and understanding file structure ####
{
    # Loading
    existing_products <- read.csv(paste0(getwd(),"/existingproductattributes2017.2.csv"), header=TRUE,sep=",")
    View(existing_products)
    
    new_products <- read.csv(paste0(getwd(),"/newproductattributes2017.2.csv"), header=TRUE,sep=",")
    View(new_products)

    # Summary
    summary(existing_products)
    summary(new_products)

    # checking product types
    print(unique(existing_products$ProductType))
    print(unique(new_products$ProductType))

    # Structure
    str(existing_products)    
    str(new_products)

}

#Pre-process
{

#De-duplicate entries (Extended Guarantee)####
  
  existing_products2 <- existing_products[c(34:41),]
  existing_products2 <- aggregate(.~ProductType, data=existing_products2, FUN=mean)
  existing_products <- rbind(existing_products[-c(34:41),],existing_products2)
  rm(existing_products2)

# Dealing with missing attrbiutes
  
  sapply(existing_products, function(x) {sum(is.na(x))})  
  
  # All 15 NAs are placed in the BestSellerRank, this represent 20% of the total observations. For this reason the attribute was removed from the analysis.       
  existing_products$BestSellersRank <- NULL

# Looking for attributes Correlation    
    # dummify the data####
    #     Dummifying those variables set as text (factor, chr) to be able to run a correlation matrix
      {
      newDataframe <- dummyVars(" ~ .", data = existing_products)
      readyData <- data.frame(predict(newDataframe, newdata = existing_products))
      View(readyData)
    
      summary(readyData)
      
      str(readyData)
}
    #Finding Correlation between attributes
      {
        corrData <- cor(readyData)
        corrData
        corrplot(corrData)
      }

#Feature Selection
    # Those features related to Service Reviews and Product Review were detected as the most correlated to predict volume.
    # However, to avoid overfit, the following thresholds were applied:
    #   - independert variables with correlation >0.95 with the dependent variable were removed
    #   - independent variables with strong correlation between themselves (>0.85) were removed, keeping only the variable with strongest correlation with the volume.
    keeps <- c("Volume", "x4StarReviews", "PositiveServiceReview")
    drops <- c("x5StarReviews", "x3StarReviews", "x2StarReviews", "x1StarReviews", "NegativeServiceReview")


#Plots####
    #Bar_plot
    ggplot(existing_products, aes(x=x4StarReviews, y=Volume))+geom_boxplot()
    
    #Point
    ggplot(data = existing_products) +
      geom_smooth(mapping = aes(x=x4StarReviews, y=Volume))+
      geom_point(mapping = aes(x=x4StarReviews, y=Volume, color = ProductType))

#Define and Remove Outliers####

    existing_products <- existing_products[-which(existing_products$Volume > 6000), ] #<--- remove outliers with volume > 6000
    
    producttypes <- c("PC", "Laptop", "Netbook", "Smartphone") #<--- create list with important product types
    
    existing_products <- existing_products[-which(existing_products$x4StarReviews <100 & #<--- remove outliers, except smartphone
                                                    existing_products$Volume > 1000 & 
                                                    as.character(existing_products$ProductType) != producttypes[4]),]
    
    
#Plots after Pre-Processing####
  ggplot(existing_products, aes(x=x4StarReviews, y=Volume))+
      geom_boxplot()
    
  ggplot(existing_products, aes(x=x4StarReviews, y=Volume))+
    geom_jitter()
  
  ggplot(data = existing_products) +
    geom_smooth(mapping = aes(x=x4StarReviews, y=Volume))+
    geom_point(mapping = aes(x=x4StarReviews, y=Volume, color = ProductType))

}


#Modeling####
#     As the dataset is too small, the Cross-validation method was adopted to overcome the data-partition process

  ## Cross Validation method set
  CrossValidation <- trainControl(method = "repeatedcv", number = 4, repeats = 100)

  
    #Training_x_Testing (Cross-validation)
  
      #LinearModelFit
      {
        set.seed(123)
        LMFit2 <- train(Volume~., data = existing_products[keeps], method = "lm", trControl = CrossValidation, preProc = c("center","scale"))
        LMFit2
        
        # RMSE      Rsquared   MAE     
        # 253.6112  0.7714843  121.7871
      }
      #SVMFit
      {
        set.seed(123)
        SVMFit <- train(Volume~., data = existing_products[keeps], method = "svmLinear2", trControl = CrossValidation, preProc = c("center","scale"))
        SVMFit
        
        # cost  RMSE      Rsquared   MAE     
        # 0.25  232.6950  0.7769015  107.3593
        # 0.50  221.5244  0.7887388  103.9160
        # 1.00  215.6266  0.7953591  101.8730
      }
      #RFFit --- Winner --- 
      {
        set.seed(123)
        RFFit <- train(Volume~., data = existing_products[keeps], method = "rf", trControl = CrossValidation, preProc = c("center","scale"))
        RFFit
        
        # RMSE      Rsquared   MAE     
        # 196.6574  0.8470498  98.02289
      }
      #GBFit
      {
        set.seed(123)
        GBFit <- train(Volume~., data = existing_products[keeps], method = "gbm", trControl = CrossValidation, preProc = c("center","scale"))
        GBFit
        
        # interaction.depth  n.trees  RMSE      Rsquared   MAE     
        # 2                  150      330.3107  0.6130552  186.3500
      }
        

#Predicting Volume on New Products
{

  #Remove ProductTypes that are not desired####
  print(new_products$ProductType)
  new_products <- new_products[which(new_products$ProductType %in% producttypes),] #<--- keep only product types that are the scope of this analysis
      }

  ## Volume Prediction
  {
    Predictions <- predict(RFFit, newdata=new_products[keeps])
    new_products$Volume <- round(Predictions)
    View(new_products)
  }
  

  ###Calculate Revenue  
  {
    new_products$Revenue  <- round((new_products$Price*new_products$ProfitMargin*new_products$Volume),digits = 2)

    }


####Save Predictions in CSV
  {
    write.csv(new_products, paste0(getwd(),"/new_products_Prediction.csv"), row.names = FALSE)
    }




}