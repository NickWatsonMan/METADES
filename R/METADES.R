# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#Libraries needed
library("ipred")
library("rpart")
library("mlbench")

#MAIN FUNCTIONS
#The Class of MET-DES
METDES <- setClass("metades", slots=list(
  pool_classifiers = "ANY",
  meta_classifiers = "ANY",
  Hc = "numeric"
))

init <- function(pool_classifiers=NaN, meta_classifiers=NaN, Hc=.5) {
  #initialization of the METDES class
  METDES()
  library("ipred")
  library("rpart")
  library("mlbench")

  d <<- new("metades", pool_classifiers=pool_classifiers,
            meta_classifiers = meta_classifiers,
            Hc = Hc)
}

#Example data for testing
data(BreastCancer)
BreastCancer$Id <- NULL


train <- BreastCancer[1:20,] #this is t
train$Id <- NULL
train_lambda <- BreastCancer[21:40, 1:10] #this is t_lambda
train_lambda$Id <- NULL
train_lambda$Class <- NULL

fit <- function(train, train_lambda) {
  overproduction(train)
  metatraining(train_lambda)
  #generalization()
}

overproduction <- function(data) {
  d@pool_classifiers <<- bagging(Class ~., data = data, coob = T) #Learn how to use var instead of Class
}

metatraining <- function(data) {
    n <- dim(data)[1]
    first <- 1
    #For each x_j ∈ t_lambda
    for (i in 1:n){
      row <- data[i, ]
      h <- consensus(row) #Calculate consensus coef.
      print(h)
      if (h < 0.9){
        if(first==1){
          t_lambda_astr <<- row
          first <- 2
        }
        else{
          t_lambda_astr <<- rbind(t_lambda_astr, row)
        }
        print("YES!!!")
      }
    }
    #Compute competence region for each element in t_lambda_astr
    #teta_j = {x1, ..., x_k} this is why teta_j is a List
    teta <- competence_region(t_labda_astr)
    #Compute output profile
    fi <- output_profile(t_lambda_astr)
}

#consensus() = max(C_0, C_1, …, C_L) / L.
#Здесь C_i – количество классификаторов,  «проголосовавших» за класс i, L – общее количество классов.
consensus <- function(row){
  n <- length(d@pool_classifiers$mtrees)
  res_final <- numeric()
  #TO-DO: comment
  for(i in 1:n){
    c <- get_tree_n(i)
    res <- predict(c, row)
    res_final <- rbind(res_final, res[1,])
  }
  #calculate the sum for each Class in res_final
  res_final_col_sums <- colSums(res_final)
  major_class_vote <- max(res_final_col_sums)
  cons_coef <- major_class_vote / dim(res_final)[1]
  return(cons_coef)
}

#Compute competence region for each element in t_lambda_astr
competence_region <- function(t){

}

#The output profile of the instance  is denoted by x_tilda = {x_tilda_1, ... , x_tilda_m}
#where each x_tilda_i is the decision yielded by the base classifier ci for the sample
#input: t_labmda_astr
output_profile <- function(t){
  res <- list(name="Output Profile", o = NULL)
  n <- dim(t)[1]
  for (i in 1:n){
    el <- t[i, ]
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
    res <- predict(c, el)
    res_final <- rbind(res_final, res[1,])
  }
  return(res_final)
}

get_tree_n <- function(n){
  #returns the n regression tree from the pool of classificators
  #the pool of classificators is stored in variable "d"
  #TO-DO if clause
  return(d@pool_classifiers$mtrees[[n]]$btree)
}

generalization <- function() {

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
