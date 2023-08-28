# Function called modifiedJaccard
# Last Updated: 3/16/23


# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'


modifiedJaccard <- function(x, X, band, scale = "n"){
  #########################################################################
  # Working on Jaccard distance implementation
  #########################################################################

  # instead of a strict inequality, choose a range of value for continuous ones (i.e. a band)
  # in that case, it would be good to normalize continuous variables so that the band applied equally.
  # categorical and numerical split
  factX <- X[,sapply(X, class) == "factor"]
  factx <- x[,sapply(x, class) == "factor"]
  numX <- X[,sapply(X, class) != "factor"]
  numx <- x[,sapply(x, class) != "factor"]

  # Issue with this method
  # Scaling could prove problematic even thought it helps with bands
  # and creating these intervals about observed x
  # Im pretty such the form of scaling we use here assumes normally distributed data which
  # is a very strong assumption. Can we scale distributions accordingly?
    #SPECIFICALLY: is there a function that can normalize values for us
  scale_numX <- scale(numX)

  mean_vector <- apply(numX, MARGIN = 2, FUN = mean)
  sd_vector <- apply(numX, MARGIN = 2, FUN = sd)

  scale_numx <- (numx - mean_vector)/sd_vector
  numX <- scale_numX
  numx <- scale_numx

  roughly_equals <- function(x, y, band_size){
    return((x <= y + band_size) & (x >= y - band_size))
  }

  equals <- function(x, y){
    return(x == y)
  }

  similar_numeric <- if(is.null(dim(numX))){
    ((numX <= numx + band) & (numX >= numx - band))
  }else{
    apply(numX, MARGIN = 1, roughly_equals, y = numx, band_size = band) %>% t()
  }

  similar_factor <- if(is.null(dim(factX))){
    factX == factx
  }else{
    apply(factX, MARGIN = 1, equals, y = factx) %>% t()}

  similar <- similar_numeric %>% cbind(similar_factor)
  jaccard_values <- rowSums(similar)/(rowSums(similar == FALSE) * 2 + rowSums(similar))
  jaccard_index <- which(max(jaccard_values) == jaccard_values) # NEED TO ADDRESS TIES AGAIN

  return(X[jaccard_index, ]) # most similar value
}
