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

init <- function(pool_classifiers=' ',
                 meta_classifiers=' ',
                 k=7,
                 Kp=5,
                 Hc=1.0,
                 selection_threshold=0.5,
                 mode='selection',
                 DFP=F,
                 with_IH=F,
                 safe_k=' ',
                 IH_rate=0.30,
                 random_state=' ',
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

fit <- function(X, y) {

}

predict <- function(X){

}

predict_proba <- function(X) {

}

select <- function(competences, selection_threshold = 0.5){
  #competences is a matrix,
  #where first column is the Number of the Classifier
  #second column is the competence lvl of the Classifier

  if(dim(competences)[1] < 2) competences <- 0 #todo
  res <- competences[(competences[,2] > selection_threshold)]
  selected_classifiers <- matrix(res, nrow = length(res)/2)

  #Returns the matrix of selected Classifiers as a matrix
  #first column is the Number of the Classifier
  #second column is the competence lvl of tthe Classifier
  return(selected_classifiers)
}

score <- function(X, y, sample_weight = NULL){

}

estimate_competence_from_proba <- function(query, neighbors, probabilities, distances = NULL){


}
#-----------------------------------------------
#Secondary function
get_similar_out_profiles <- function(probabilities, kp = 5) {

}


