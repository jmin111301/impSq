# Function called regImpute
# Last Updated: 3/16/23


# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

library(tidyverse)
library(nnet)

regImpute <- function(data, method = "deterministic"){
  # Test: drops full NA rows
  test = (dim(data)[2] == rowSums(data %>% is.na)) == FALSE
  if(!all(test)){
    n = sum(!all(test))
    data = data[!dim(data)[2] == rowSums(data %>% is.na),]
    warning("Data frame contained rows with full NA's. " , as.character(n), " rows dropped.")
  }

  numCols <- dim(data)[2]

  for (i in 1:numCols){
    col <- data[,i]
    colNA <- col %>% is.na()
    NAindeces <- which(colNA)

    # Our training set is all data without NA's
    train <- data %>% drop_na()

    # Our predictive set is all data corresponding to the NA values in the current column
    newdata = data[NAindeces, -i]
    # Case: rows with more than one NA.
    # Solution: set the value of NA to 0 so that the NA value does not affect our prediction

    # Handles numeric cases
    suppressWarnings(newdata[newdata %>% is.na()] <- 0)
    # Handles factor cases (with random draw)
    ######### MAY STILL NEED TO FIND A BETTER METHOD FOR THIS ################
    index_factors = which(sapply(newdata, class) == "factor")
    for(k in index_factors){
      numNA_factori <- sum(newdata[,k] %>% is.na())
      mini_impute <- newdata[,k] %>% levels() %>% sample(numNA_factori)
      newdata[,k][newdata[,k] %>% is.na()] = mini_impute
    }

    # Creates the formula for each regressions per column
    data_names <- data %>% names
    outcome <- data_names[i]
    variables <- data_names[-i]
    f <- as.formula(
      paste(outcome,
            paste(variables, collapse = " + "),
            sep = " ~ "))

    # Checks if the column that we are interested in predicting is of the right class
    if(col %>% class() == "factor"){
      # Multinomial regression + prediction
      model.fit <- multinom(f, data = train, family = "binomial", trace = FALSE)
      col_imputed_values <- predict(model.fit, newdata = newdata)
    }else if(col %>% class() %in% c("numeric", "integer", "double")){
      # Multiple linear regression + prediction
      model.fit <- lm(f, data = train)
      col_imputed_values <- predict(model.fit, newdata = newdata)
    }else{
      stop("The column is not a factor or numeric class. Cannot regress to impute missing value")
    }

    # Actually impute the missing values
    data[,i][colNA] = col_imputed_values
  }
  return(data)
}
