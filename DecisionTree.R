# @Encode: utf-8
# @Date: July 02 2017
# @Title: Decision Tree
# @Description: 
# - A implementation of Decision Tree algorithm in R.
# - By 'rpart' and 'C50' package
# - Visualization of the result

#### Load Package ####
# Check dependent packages
# If not extends
# Install it
if(library("rpart", logical.return = TRUE) == FALSE){ # Package for Decision Tree algorithm
  install.packages("rpart")
  library("rpart")
}
if(library("partykit", logical.return = TRUE) == FALSE){ # Package for visualizaiotn
  install.packages("partykit")
  library("partykit")
}
if(library("ggplot2", logical.return = TRUE) == FALSE){ # Package for visualizaiotn
  install.packages("ggplot2")
  library("ggplot2")
}
if(library("C50", logical.return = TRUE) == FALSE){ # Package for visualizaiotn
  install.packages("C50")
  library("C50")
}




#### Get Data ####
# Get the data for test
# Choose 'iris' dataset for test
str(iris)
iris <- iris
# Data contain 4 attributes and 1 response variable 'Species'
# All data in 3 classes
# In first step I will use only 2 attributes and 2 class of flowers to bulid a decision tree
# Because 2-dim is convenience for visualization
# Finally, I will use all 4 attributes to bulid decision tree
# to test the performce of decision tree algorithm

#### Modeling ####
# Use 2 attributes and 1 response variable
# in 2 classse: "setosa", "versicolor"
dataModel1 <- iris[dataModel1$Species %in% c("setosa", "versicolor"),
                   c("Petal.Length", "Petal.Width", "Species")] 
# Use 'rpart' function 
model_1 <- rpart(as.factor(Species) ~ Petal.Length + Petal.Width,
                 data = dataModel1,
                 method = "class",
                 parms = list(prior=c(0.5,0.5),
                              split="gini"))
# Visualization the tree
plot(as.party(model_1)) # We can see all sample are correctly classification

# Bulid a function to visualization the regular generates by decision tree algorithm
regularVisible <- function(model, data){
  # Parameters:
    # model: rpart object generates by 'rpart' function
    # data: train set
  
  # Plot train set
  p <- ggplot(data = data, aes(x=Petal.Length, y=Petal.Width))+
        geom_point(aes(color=Species), alpha=0.5, size=2)
  # Add regular
  for(i in 1:(nrow(model$splits)-1)){
    if(rownames(model$splits)[i]=="Petal.Length"){
      p <- p+geom_vline(xintercept = model$splits[i,4], size=1)
    }else if(rownames(model$splits)[i]=="Petal.Width"){
      p <- p+geom_hline(yintercept = model$splits[i,4], size=1)
    }
  }
  print(p)
}

regularVisible(model_1, dataModel1)

# Then I will use all atrributes and all classes of flower to bulid a model
model_2 <- rpart(as.factor(Species) ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                 data = iris,
                 method = "class",
                 parms = list(prior=c(0.3333,0.3333,1-(0.3333*2)),
                              split="gini"))
# Visualization 
plot(as.party(model_2)) 

# visualization the regular
regularVisible(model_2, iris) # We can see no all sample are correctly classification

#### C5.0 algorithm ####

# Create a model based on regulation
# No boost(single tree)
modelRegular <- C5.0(as.factor(Species) ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                     data = iris,
                     rules = TRUE)


