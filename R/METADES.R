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

METDES <- setClass("metades", slots=list(
  pool_classifiers = "ANY",
  meta_classifiers = "ANY",
  k = "numeric",
  Kp = "numeric",
  Hc = "numeric",
  selection_threshold="numeric",
  mode="character",
  DFP= "logical",
  with_IH= "logical",
  safe_k = "ANY",
  IH_rate = "numeric",
  random_state = "ANY",
  knn_classifier = "character"
))

init <- function(pool_classifiers=NaN,
                 meta_classifiers=NaN,
                 k=7,
                 Kp=5,
                 Hc=1.0,
                 selection_threshold=0.5,
                 mode='selection',
                 DFP=F,
                 with_IH=F,
                 safe_k=NaN,
                 IH_rate=0.30,
                 random_state=NaN,
                 knn_classifier='knn'){
  #initialization of the METDES class
  METDES()

  d <<- new("metades", pool_classifiers=pool_classifiers,
           meta_classifiers = meta_classifiers,
           k = k,
           Kp = Kp,
           Hc = Hc,
           selection_threshold = selection_threshold,
           mode = mode,
           DFP = DFP,
           with_IH = with_IH,
           safe_k = safe_k,
           IH_rate = IH_rate,
           random_state = random_state,
           knn_classifier = knn_classifier)
}

X <- as.data.frame(matrix(rnorm(1000), ncol=10))
y <- factor(ifelse(apply(X, 1, mean) > 0, 1, 0))
learn <- cbind(y, X)
mt <- bagging(y ~., data = learn, coob = T, ns = 9)

fit <- function(X) {
  overproduction()
  metatraining()
  generalization()
}

overproduction <- function() {
  d@pool_classifiers <- bagging(y ~., data = learn, coob = T)
}

metatraining <- function() {
  c <- d@pool_classifiers

}

generalization <- function() {

}


