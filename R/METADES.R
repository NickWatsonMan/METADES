# Hello, this is the first META-DES package for R
# made by Nikita Volodarskiy


#Libraries needed
library("ipred")
library("rpart")
library("mlbench")
library("FNN")
library("zoo")

#MAIN FUNCTIONS
#The Class of MET-DES
METADES <- setClass("metades", slots=list(
  pool_classifiers = "ANY",
  meta_classifiers = "ANY",
  Hc = "numeric"
))

init <- function(pool_classifiers=NaN, meta_classifiers=NaN, Hc=.5) {
  #initialization of the METDES class
  METADES()
  library("ipred")
  library("rpart")
  library("mlbench")
  library("FNN")
  library("zoo")

  d <<- new("metades", pool_classifiers=pool_classifiers,
            meta_classifiers = meta_classifiers,
            Hc = Hc)
}

#Example data for testing
data(BreastCancer)
BreastCancer$Id <- NULL

train <- BreastCancer[1:50,] #this is t
train$Id <- NULL
train_lambda <- BreastCancer[1:150, 1:10] #this is t_lambda
train_lambda$Id <- NULL

' testing on bankruptcy data
my_data <- read_excel("/Users/nikitavolodarsky/Documents/Data_balanced.xlsx", sheet = 1)
train <- as.data.frame(my_data[1:30, ])
train$B4 <- as.factor(train$B4)
train$B5 <- NULL
train$B6 <- NULL
train_lambda <-as.data.frame(my_data[30:60,]) #this is t_lambda
train_lambda$B4 <- as.factor(train_lambda$B4)
train_lambda$B5 <- NULL
train_lambda$B6 <- NULL'


fit <- function(train, train_lambda) {
  overproduction(train)
  metatraining(train_lambda)
  #generalization()
}

overproduction <- function(data) {
  d@pool_classifiers <<- bagging(B4 ~., data = data, coob = T) #Learn how to use var instead of Class
}

metatraining <- function(data) {
    n <- dim(data)[1]
    first <- 1
    #For each x_j ∈ t_lambda
    print("computing t_lambda_astr")
    for (i in 1:n){
      row <- data[i, ]
      h <- consensus(row) #Calculate consensus coef.
      if (h < 0.6){
        if(first==1){
          t_lambda_astr <- row
          first <- 2
        }
        else{
          t_lambda_astr <- rbind(t_lambda_astr, row)
        }
      }
    }
    t_lambda_astr <<- t_lambda_astr
    #Compute competence region for each element in t_lambda_astr
    #teta_j = {x1, ..., x_k} this is why teta_j is a List
    print("computing competence region")
    teta <<- competence_region(data, t_lambda_astr)
    #Compute output profile
    print("computing output profile")
    fi <<- output_profile(t_lambda_astr)

    #Extracting features
    #Neighbors׳ hard classification:
    #f_1 is a matrix
    print("computing meta-feature 1")
    f_1 <<- get_feature1(teta)

    #Probabilities
    #f_2 is a matrix
    print("computing meta-feature 2")
    f_2 <<- get_feature2(teta)
}

generalization <- function() {

}

get_feature2 <- function(teta){
  n <- length(teta$o)
  #join all elements from teta, cuz' we don't need them separately here.
  teta_elems <- numeric()
  for(i in 1:n){
    el <- teta$o[[i]]
    teta_elems <- rbind(teta_elems, el)
  }

  f2 <- matrix(0, length(d@pool_classifiers$mtrees), dim(teta_elems)[1])
  n <- dim(teta_elems)[1]
  for (i in 1:n){
    el <- teta_elems[i,1:ncol(teta_elems)-1] #element without class
    preds <- get_probability(el)
    n2 <- dim(preds)[1]
    for (j in 1:n2){
      pred <- preds[j,]
      f2[j, i] <- max(pred)
    }
  }
  return(f2)
}

#Neighbors׳ hard classification logics:
get_feature1 <- function(teta){
  n <- length(teta$o)
  #join all elements from teta, cuz' we don't need them separately here.
  teta_elems <- numeric()
  for(i in 1:n){
    el <- teta$o[[i]]
    teta_elems <- rbind(teta_elems, el)
  }
  teta_elems <<- teta_elems
  #Matrix with feature 1. Default all zero:
  f1 <- matrix(0, length(d@pool_classifiers$mtrees), dim(teta_elems)[1])
  #get predictions
  n <- dim(teta_elems)[1]
  for (i in 1:n){
    el <- teta_elems[i,1:ncol(teta_elems)-1] #element without class
    preds <- get_predictions_class(el)
    n2 <- dim(preds)[1]
    for (j in 1:n2){
      cls <- preds[j]
      #If prediction is correct change value to 1:
      if (levels(teta_elems[i, ncol(teta_elems)])[cls] == levels(teta_elems[i, ncol(teta_elems)])[1]){
        f1[j, i] <- 1
      }
    }
  }
  return(f1)
}

#consensus() = max(C_0, C_1, …, C_L) / L.
#Здесь C_i – количество классификаторов,  «проголосовавших» за класс i, L – общее количество классов.
consensus <- function(row){
  n <- length(d@pool_classifiers$mtrees)
  res_final <- numeric()
  #TO-DO: comment
  for(i in 1:n){
    c <- get_tree_n(i)
    res <- unname(predict(c, row, type=c("vector")))
    res_final <- rbind(res_final, res)
  }
  #calculate the sum for each Class in res_final
   res_table <- table(res_final)
   cons_coef <- max(res_table) / length(res_final)
  return(cons_coef)
}

#Compute competence region for each element in t_lambda_astr
#input: t_lambda, t_lambda_astr
competence_region <- function(tl, tla){
  roc <- list(name="Regions of Competence", o = NULL)
  k <- round(sqrt(dim(tl)[1])) #choosing an appropriate 'k' for k-NN
  class_crop <- ncol(tl) - 1
  #compute k nearest neighbors from t_lambda for each element in t_lambda_astr
  n <- dim(tla)[1]
  #to use k-NN no missing values are allowed
  tl[is.na(tl)] <- 2 #think of this a bit better
  tla[is.na(tla)] <- 2 #think of this a bit better

  #format to double structure
  dtl <- t(do.call(rbind, lapply(tl, as.numeric)))
  dtla <- t(do.call(rbind, lapply(tla, as.numeric)))
  labels <- tl[,ncol(tl)]

  k_nn <- knn(dtl[,1:class_crop], dtla[,1:class_crop], labels, k = k, algorithm="cover_tree")
  indices <- attr(k_nn, "nn.index")
  for (i in 1:n){
    el <- tla[i,]
    res_final <- numeric()
    for (j in 1:k){
      res <- tl[indices[i, j],]
      res_final <- rbind(res_final, res[1,])
    }
    roc$o[[i]] <- res_final
  }
  return(roc)
}

#The output profile of the instance  is denoted by x_tilda = {x_tilda_1, ... , x_tilda_m}
#where each x_tilda_i is the decision yielded by the base classifier ci for the sample
#input: t_labmda_astr
output_profile <- function(tla){
  res <- list(name="Output Profile", o = NULL)
  n <- dim(tla)[1]
  for (i in 1:n){
    el <- tla[i, ]
    x_tilda <- get_predictions(el)
    res$o[[i]] <- x_tilda
  }

  return(res)
}

get_predictions <- function(el){
  n <- length(d@pool_classifiers$mtrees)
  res_final <- numeric()
  for(i in 1:n){
    c <- get_tree_n(i)
    res <- predict(c, el, type="vector")
    res_final <- rbind(res_final, res)
  }
  rownames(res_final) <- c(1:n)
  return(res_final)
}

get_predictions_class <- function(el){
  n <- length(d@pool_classifiers$mtrees)
  res_final <- numeric()
  for(i in 1:n){
    c <- get_tree_n(i)
    res <- predict(c, el, type="class")
    res_final <- rbind(res_final, res)
  }
  return(res_final)
}

get_probability <- function(el){
  n <- length(d@pool_classifiers$mtrees)
  res_final <- numeric()
  for(i in 1:n){
    c <- get_tree_n(i)
    res <- predict(c, el, type="prob")
    res_final <- rbind(res_final, res[1,])
  }
  return(res_final)
}

get_tree_n <- function(n){
  #returns the n regression tree from the pool of classificators
  #the pool of classificators is stored in variable "d"
  return(d@pool_classifiers$mtrees[[n]]$btree)
}

#TO-DO take it to another file
#Testing Bagging package
data(BreastCancer)
BreastCancer$Id <- NULL
res <- bagging(Class ~ Cl.thickness + Cell.size
               + Cell.shape + Marg.adhesion
               + Epith.c.size + Bare.nuclei
               + Bl.cromatin + Normal.nucleoli
               + Mitoses, data=BreastCancer, coob=TRUE)

DataSample <- BreastCancer[1:20,]
test <- BreastCancer[1:20, 1:10]
test$Id <- NULL

DataSample$Id <- NULL
mt <- bagging(Class ~ ., DataSample, coob = T)


f1 <- mt$mtrees[[1]]$btree
plot(f1)
text(f1, use.n=F)

par(mfrow = c(1,2), xpd = NA)
f2 <- mt$mtrees[[19]]$btree
plot(f2)
text(f2, use.n=F)

res <- predict(f2, test)
res[1,1:2]

#testing data structure List
v1 <- c(1:4)
v2 <- c(2:5)
o <- list(name = "kek", x = NULL)
o$x[[1]] <- v1
o$x[[2]] <- v2

#TESTING k-NN
#seraching for nearest neighbors test FNN : knn methode
tr <- BreastCancer[1:20,] #this is t
ntr <- t(do.call(rbind, lapply(tr, as.numeric)))
lb <- tr$Class
ts <- BreastCancer[21:25,] #this is t_lambda
ts[is.na(ts)] <- 2
nts<- t(do.call(rbind, lapply(ts, as.numeric)))
cl<- lb


ks <- FNN :: knn(ntr[,1:9], nts[,1:9], cl, k = 5)
ats <- attributes(.Last.value)
indices <- attr(ks, "nn.index")

