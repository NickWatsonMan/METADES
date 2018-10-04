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

hello <- function() {
  print("Hello, world!")
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
