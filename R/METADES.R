# Hello, this is the first META-DES package for R
# made by Nikita Volodarskiy


#Libraries needed
library("ipred")
library("rpart")
library("mlbench")
library("FNN")
library("zoo")
library("readxl")
library("rpart.plot")
library("RSNNS")
library("minpack.lm")


#MAIN FUNCTIONS
#The Class of MET-DES
METADES <- setClass("metades", slots=list(
  pool_classifiers = "ANY",
  meta_classifiers = "ANY",
  Hc = "numeric"
))

#Example data for testing
'data(BreastCancer)
BreastCancer$Id <- NULL

train <- BreastCancer[1:50,] #this is t
train$Id <- NULL
train_lambda <- BreastCancer[1:150, 1:10] #this is t_lambda
train_lambda$Id <- NULL'

# testing on bankruptcy data

my_data <- read_excel("/Users/nikitavolodarsky/Documents/Data_balanced.xlsx", sheet = 1)
train <- as.data.frame(my_data[1:15, ])
train$B4 <- as.factor(train$B4)
train$B5 <- NULL
train$B6 <- NULL

train_lambda <-as.data.frame(my_data[21:35,]) #this is t_lambda
train_lambda$B4 <- as.factor(train_lambda$B4)
train_lambda$B5 <- NULL
train_lambda$B6 <- NULL

test <- as.data.frame(my_data[36:50, ])
test$B4 <- as.factor(test$B4)
test$B5 <- NULL
test$B6 <- NULL


fit <- function(train, train_lambda, test) {
  #Class initialization
  METADES()

  #Class entity
  d <<- new("metades", pool_classifiers = "ANY")

  overproduction(train)
  metatraining(train_lambda, test)
}

overproduction <- function(data) {
  d@pool_classifiers <<- bagging(B4 ~., data = data, coob = T) #Learn how to use var instead of Class
}

metatraining <- function(data, dtest) {
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

    #Overall local accuracy:
    print("computing meta-feature 3")
    f_3 <<- get_feature3(teta)

    #Output profiles classification:
    print("computing meta-feature 4")
    f_4 <<- get_feature4(fi, t_lambda_astr)

    #Classifier's confidence:
    print("computing meta-feature 5")
    f_5 <<- get_feature5()

    print("calculating classifiers class")
    classifiers_class <<- get_classifiers_class(data)

    print("vector with meta-features v")
    meta_features_vector <<- cbind(f_1, f_2, f_3, f_4)
    #meta_features_vector + classifiers_class
    meta_f_v_and_classifiers_class <<- cbind(f_1, f_2, f_3, f_4, classifiers_class)

    print("building meta-training data set")
    meta_training_data_set <<- get_meta_training_data_set(meta_features_vector, data)

    print("training meta-classifier")
    meta_classifier <<- get_meta_classifier(meta_training_data_set, classifiers_class)

    #GENERALIZATION PHASE
    generalization(dtest, meta_training_data_set, classifiers_class, meta_classifier)
}

generalization <- function(data, dsel, class, m_classifier) {
  #dsel to one dataframe
  dsel_joined <- join_list(dsel)

  ld <- dim(t_lambda_astr)[1]
  #data <- data[1:ld,]
  #Find the region of competence
  print("region of comp")
  g_com_reg <<- competence_region(data, dsel_joined[1:ld,])

  #Find the output profile
  print("output profile")
  g_out_prof <<- output_profile(dsel_joined[1:ld,])

  #f_1 is a matrix
  print("computing meta-feature 1")
  g_f_1 <<- get_feature1(g_com_reg)

  #Probabilities
  #f_2 is a matrix
  print("computing meta-feature 2")
  g_f_2 <<- get_feature2(g_com_reg)

  #Overall local accuracy:
  print("computing meta-feature 3")
  g_f_3 <<- get_feature3(g_com_reg)

  #Output profiles classification:
  print("computing meta-feature 4")
  g_f_4 <<- get_feature4(g_out_prof, data)

  #Classifier's confidence:
  #print("computing meta-feature 5")
  #g_f_5 <<- get_feature5()
  print("vector with meta-features v")
  g_meta_features_vector <<- cbind(g_f_1, g_f_2, g_f_3, g_f_4)

  print("building meta-training data set")
  g_cls_class <<- get_classifiers_class(data)
  g_meta_training_data_set <<- get_meta_training_data_set(g_meta_features_vector, data)
  g_data_joined <<- join_list(g_meta_training_data_set)

  #Start classifying
  #b <- predict(mp, meta_training_data_set$o[[2]], type = "class")
  xs <- match(g_data_joined['X1'], g_data_joined)
  predict(meta_classifier, g_data_joined[1:2,xs:length(g_data_joined)])

  print('Checking predictions')
  res_classifier_ens <- list(name="Final Classifiers", o = NULL)
  class_idx <- c()
  ns <- length(g_meta_training_data_set$o)
  for(i in 1:ns) {
    for(j in 1:length(d@pool_classifiers$mtrees)){
      #round(predict(meta_classifier, g_data_joined[j,xs:length(g_data_joined)]))
      a <- round(predict(meta_classifier, g_meta_training_data_set$o[[i]][j,xs:length(g_data_joined)]))
      b <- g_cls_class[j,]
      perc <- sum(a==b)/length(b)
      if (perc >= 0.6){
        class_idx <- c(class_idx, j)
      }
    }
  }

  #print(unique(class_idx))
  class_idx <<- unique(class_idx)
  print('Computing final Classifier')
  for(i in 1:length(unique(class_idx))){
    print(i)
    res_classifier_ens$o[[i]] <- d@pool_classifiers$mtrees[class_idx[i]]
  }
  res_ensemble <<- res_classifier_ens
}

join_list <- function(data){
  data_joined <- numeric()
  n <- length(data$o)
  for(i in 1:n){
    el <- data$o[[i]]
    data_joined <- rbind(data_joined, el)
  }
  return(data_joined)
}

get_meta_classifier <- function(mt_dataset, class){

  #Dividing data in 75% for training and 25% for testing
  train <- list(name="Training set", o = NULL)
  test <- list(name="Testing set", o = NULL)
  smp_size <- floor(0.75*length(meta_training_data_set$o))
  for(i in 1:smp_size){
    train$o[[i]] <- mt_dataset$o[[i]]
  }
  test_size <- length(meta_training_data_set$o) - smp_size
  for(i in smp_size+1:test_size){
    test$o[[i]] <- mt_dataset$o[[i]]
  }

  n <- length(train$o)

  class_joined <- numeric()
  train_joined <- numeric()
  for(i in 1:n){
    el <- train$o[[i]]
    train_joined <- rbind(train_joined, el)
    class_joined <- rbind(class_joined, class)
  }

  n <- length(test$o)
  test_cls <- numeric()
  test_joined <- numeric()
  for(i in 1:n){
    el <- test$o[[i]]
    test_joined<- rbind( test_joined, el)
    test_cls <- rbind(test_cls, class)
  }


  cja <<- class_joined
  tja <<- train_joined

  xs <- match(train_joined['X1'], train_joined)
  mlp_train <<-  mlp(train_joined[,xs:length(train_joined)], class_joined, learnFunc = "Std_Backpropagation")
  #prd <<- predict(mlp_train, test_joined, type="class")

  return(mlp_train)
}

get_meta_training_data_set <- function(meta_f_v, train_lambda){
  res <- list(name="Meta Training Dataset", o = NULL)
  res_final <- numeric()
  n <- dim(train_lambda)[1]
  n2 <- dim(meta_f_v)[1]
  for(i in 1:n){
    el <- train_lambda[i, 1:ncol(train_lambda) - 1]
    for(j in 1:n2){
      cls <- meta_f_v[j, 1:ncol(meta_f_v)]
      #conver cls to data frame
      m <- matrix(cls, nrow = 1)
      dfcls <- data.frame(m)

      el_cls <- cbind(el, dfcls)
      res_final <- rbind(res_final, el_cls)
    }
    res$o[[i]] <- res_final
    res_final <- numeric()
  }

  return(res)
}

get_classifiers_class <- function(train_lambda){
  n <- dim(train_lambda)[1]
  alpha <- matrix(0, length(d@pool_classifiers$mtrees), dim(train_lambda)[1])
  #For each element in t_lambda
  for(i in 1:n){
    #classify with each classifier
    el <- train_lambda[i, 1:ncol(train_lambda)-1]
    predictions <- get_predictions_class(el)
    n2 <- dim(predictions)[1]
    for (j in 1:n2){
      if(predictions[j] == train_lambda[i, ncol(train_lambda)]){
        alpha[j,i] <- 1
      }
    }
  }
  return(alpha)
}

get_feature5 <- function(){
  #this feature is to be done
}

get_feature4 <- function(fi, t_lambda_astr){
  n <- length(fi$o)
  f4 <- matrix(0, length(d@pool_classifiers$mtrees), n)
  for(i in 1:n){
    n2 <- length(d@pool_classifiers$mtrees)
    for(j in 1:n2){
      x_tilda_k <- fi$o[[i]][j]
      w_i_k <- t_lambda_astr[i,ncol(t_lambda_astr)]
      if (x_tilda_k == w_i_k){
        f4[j, i] <- 1
      }
      else {
        f4[j, i] <- 0
      }
    }
  }
  return(f4)
}

get_accuracy <- function(df, actual, predicted){
  y <- as.vector(table(df[,predicted], df[,actual]))
  if (length(y) < 4){
    if (length(y) == 0){
      y <- c(0,0,0,0)
    }
    if (length(y) == 1){
      y <- c(y, 0, 0, 0)
    }
    if (length(y) == 2){
      y <- c(y, 0, 0)
    }
    if (length(y) == 3){
      y <- c(y, 0)
    }
  }
  names(y) <- c("TN", "FP", "FN", "TP")
  acur <- (y["TP"]+y["TN"])/sum(y)
  return(as.numeric(acur))
}

get_feature3 <- function(teta){
  n <- length(teta$o)
  #join all elements from teta, cuz' we don't need them separately here.
  teta_elems <- numeric()
  for(i in 1:n){
    el <- teta$o[[i]]
    teta_elems <- rbind(teta_elems, el)
  }

  cust_elems <- teta_elems
  colnames(cust_elems)[ncol(cust_elems)] <- "class"
  cust_elems$scored <- 0

  n <- length(d@pool_classifiers$mtrees)
  #for each classifier
  f3 <- matrix(0, length(d@pool_classifiers$mtrees), 1)
  for(i in 1:n){
    classifier <- get_tree_n(i)
    n2 <- dim(cust_elems)[1]
    #classify each element
    for (j in 1:n2){
      el <- cust_elems[j,1:(ncol(cust_elems)-2)]
      prediction <- predict(classifier, el, type="class")
      cust_elems[j,ncol(cust_elems)] <- prediction
    }
    acr <- get_accuracy(cust_elems, "class", "scored")
    f3[i] <- acr
  }
  return(f3)
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
      #Check this!!
      if (cls == teta_elems[i, ncol(teta_elems)]){
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
  tl[is.na(tl)] <- 2.0 #think of this a bit better
  tla[is.na(tla)] <- 2.0 #think of this a bit better

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
  #pruning the tree gives better prediction results
  return(prune(d@pool_classifiers$mtrees[[n]]$btree, cp=0.3))
}


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mdpredict <- function(ensemble, data){
  n <- dim(data)[1]
  n2 <- length(ensemble$o)
  mtxpred <- numeric()
  res <- numeric()

  for(i in 1: n2){
    prd <- predict((ensemble$o[[i]])[[1]]$btree, predt, type="class")
    mtxpred <- rbind(mtxpred, prd)
  }

  for(i in 1:n){
    res[i] <- getmode(mtxpred[,i])
  }
  res <- t(t(res))
  return(res)
}

#(pool$o[[1]])[[1]]$btree

predt <- as.data.frame(my_data[100:120, ])
predt$B4 <- NULL
predt$B5 <- NULL
predt$B6 <- NULL

#TO-DO take it to another file
#Testing Bagging package
'data(BreastCancer)
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
indices <- attr(ks, "nn.index")'
#Testing MLP
#mp <- mlp(meta_training_data_set$o[[1]], classifiers_class, learnFunc = "SCG")
#b <- predict(mp, meta_training_data_set$o[[2]], type = "class")

# Testing the final classifier
#test1 <- as.data.frame(my_data[100:110, ])
#test1$B4 <- NULL
#test1$B5 <- NULL
#test1$B6 <- NULL

#predict(pool$o[[1]][1], test1)
