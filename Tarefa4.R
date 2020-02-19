# Tarefa 4 - Curso de Verão
# Autor: Rodrigo Moni

library(dplyr)
library (e1071)
library(glmnet)

# Classificação usando Support Vector Machines

elements <- read.csv(file.path("/home/rodrigo/Documentos/Tarefa 4/dadosTarefa3.csv"), sep = ';', stringsAsFactors=FALSE)

elements <- na.omit(elements)

# dados padronizados
scaledElements <- elements  %>% mutate_at(c("tumorsize", "co2", "lungcapacity", "WBC", "RBC", "IL6"), ~(scale(.) %>% as.vector))

firstVarSet <- data.frame(x1 = scaledElements$tumorsize, x2 = scaledElements$lungcapacity, x3 = scaledElements$co2,  y = scaledElements$DID)
y = scaledElements$DID

# kernel linear: u'*v
svmfit.l = svm(y~. , data = firstVarSet , kernel ="linear" , cost =10 , gamma =1)
plot(svmfit.l, firstVarSet, grid = 200, x1~x3)
table(predict(svmfit.l), y)

# gerando dados
set.seed(1)
x = matrix(rnorm(20*2), ncol =2)
x = rbind(x, matrix (rnorm (50*2), ncol =2))
y = c(rep(-1 ,10), rep(1 ,10))
y = c (y, rep (0 ,50))
plot(x, col = as.factor(y))

# separando um pouco os dados
x[y == 0,2] = x[y == 0,2] + 2
plot(x, col = as.factor(y))

dat = data.frame(x2  = x[,2], x1 = x[,1],  y = as.factor(y))

# kernel linear: u'*v
svmfit.l = svm(y~. ,data = dat , kernel ="linear" , cost =10 , gamma =1)
plot(svmfit.l, dat, grid = 200)
table(predict(svmfit.l), y)