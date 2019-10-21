library(DALEX2)
library(iBreakDown)
library(reticulate)
library(ranger)

titanic_train <- read.csv("titanic_toy_example/titanic_bd.csv")
head(titanic_train)

titanic <- titanic_train[,-1]
titanic$Survived <- factor(titanic$Survived)

# baseline random forest
set.seed(123)
rf <- ranger(Survived~., titanic, probability = TRUE)

predict_function <- function(m, newdata){
  predict(m, data = newdata)$predictions[,2]
}
predict_function(rf, titanic)

# iBreakDown
rf_explain <- explain(rf, data = titanic[ ,-1], predict_function = predict_function,
                      y = titanic$Survived, label = "Random Forest")

passanger <- titanic[274, -1]

# calculate and plot uncertainty of explanation
set.seed(123)
bd <- shap(rf_explain, new_observation = passanger)
plot(bd)
