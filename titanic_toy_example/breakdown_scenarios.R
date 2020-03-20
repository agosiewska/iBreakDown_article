library(DALEX)
library(iBreakDown)
library(reticulate)

set.seed(123)

titanic_train <- read.csv("titanic_toy_example/titanic_bd.csv")
head(titanic_train)

X_train <- titanic_train[,c(3:5)]
Y_train <- titanic_train$Survived

rf <- py_load_object("./titanic_toy_example/rf_classifier.pkl", pickle = "pickle")

predict_function <- function(model, newdata){
  model$predict_proba(as.matrix(newdata))[,2]
}
predict_function(rf,  X_train[274, ])


# iBreakDown
rf_explain1 <- explain(rf, data = X_train,
                      y = Y_train, label = "Scenario 1",
                      predict_function = predict_function)
rf_explain2 <- explain(rf, data = X_train,
                       y = Y_train, label = "Scenario 2",
                       predict_function = predict_function)
rf_explain3 <- explain(rf, data = X_train,
                       y = Y_train, label = "Scenario 3",
                       predict_function = predict_function)

i <- 274
passanger <- X_train[i,]
(exp_1 <- local_attributions(rf_explain1, X_train[274,], order=c(2, 1,3)))
p11 <- plot(exp_1)


(exp_2 <- local_attributions(rf_explain2, X_train[274,], order=c(2, 3,1)))
p12 <- plot(exp_2)


(exp_3 <- local_interactions(rf_explain3, X_train[274,], order = c("Sex", "Age:Pclass")))
p13 <- plot(exp_3)


library(patchwork)
library(ggplot2)

(p11 + p12)

p13 + ggtitle("Scenario 3")

save(p13, file = "bd_sc3.rda")

