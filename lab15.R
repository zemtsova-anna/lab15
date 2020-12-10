# renv::init() # инициализация виртуального окружения
# renv::install("e1071", "ROCR") # установка библиотеки из CRAN
# renv::snapshot() # делаем снимок версий библиотек в нашем виртуальном окружении
# фиксируем этот список в .lock-файле для возможности восстановления
# renv::restore() # команда отктиться к предыдушему удачному обновления библиотек

# ------------------- 
# Лабораторная работа №15:
# Машины опорных векторов (SVM).

# ---------- Задание --------------
# примените SVM к дата-сету Iris для решения задачи классификации цветков разного вида

#прогружаем необходимые пакеты
library(e1071)
library(ROCR)

#изменяем рабочий каталог 
setwd("C:/Users/Acer/Desktop/lab11.)

#читаем csv в iris
iris< - read.csv("iris.csv",header = T, sep = ",")

#Rplot1
for(i in 1:100) { #other
  iris[i,5] = 2
}
for(i in 101:150){ #Virginica
  iris[i,5] = 1
}
iris < - lapply(iris, as.numeric)

x < - data.frame(iris$sepal.length, iris$sepal.width, 
                iris$petal.length, iris$petal.width)
y <- iris$variety

set.seed(1233)

plot(x, col = (73 - y))

#Rplot2
dat = data.frame(x = x, y = as.factor(y))

svm.fit = svm(y ~., data = dat, kernel = "linear",
             cost = 10, scale = F) 

plot(svm.fit, dat,x.iris.petal.width ~ x.iris.petal.length,
     slice = list(x.iris.sepal.width = 3, x.iris.sepal.length = 4))

#Rplot3
svmfit$
summary(svmfit)

set.seed(1)
tune.out = tune(svm, y~., data = dat, kernel = "linear",
                ranges = list(cost = c(0.001, 0.1, 1, 5, 10, 100)))
summary(tune.out)

bestmod = tune.out$best.model
summary(bestmod)

set.seed(1)
train = sample(150, 75)

ypred = predict(bestmod, dat[-train,])
table(predict = ypred, truth = dat[-train,"y"]) 

rocplot = function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
 plot(perf, ...)
}


svmfit.opt = svm(y ~., data = dat[train,], kernel = "radial",
                 gamma = 2, cost = 1, decision.values = T)

fitted = attributes(predict(svmfit.opt, dat[train,], 
                            decision.values = T))$decision.values
par(mfrow = c(1,2))
rocplot(fitted, dat[train, "y"], main = "Training Data")
svmfit.flex = svm(y ~., data = dat[train,], kernel = "radial",
                  gamma = 50, cost = 1, decision.values = T)

fitted = attributes(predict(svmfit.flex, dat[train,], 
                            decision.values = T))$decision.values
rocplot(fitted, dat[train, "y"], main = "Training Data", 
        add = T, col = "green")


fitted = attributes(predict(svmfit.opt, dat[-train,], 
                            decision.values = T))$decision.values
rocplot(fitted, dat[-train,"y"], main = "Test data ")
fitted = attributes(predict(svmfit.flex, dat[-train,], 
                            decision.values = T))$decision.values
rocplot(fitted, dat[-train,"y"], add = T, col = "green")
