# Function called KNNimpute
# Last Updated: 3/16/23


# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'
library(bench)
library(tidyverse)
library(dplyr)
library(devtools)
library(fastDummies)

### NOTES ###
# Want to impute type based off of wheels and the other variables
# Go through missing type and then take smallest jaccard index is good
# Currently our function only utilizes one distance metric

# Maybe we can make it so that KNNimpute ignores all rows that aren't numeric/categorical
KNNimpute <- function(data, k = 10, method = "mean", split_categorical = FALSE, ties = "SOME METHOD", standardize = FALSE){
  # This is a KNN imputation function
  ### Distance Metrics ###
  # Euclidean distance
  norm <- function(x){
    return(sqrt(sum(x^2)))
  }
  # Consider other norms

  ### Test Cases ###
  ############### Initialized and later delete #################
  #k = 5
  if(sum(complete.cases(data)) <= k){
    stop("Not enough complete cases (cases without NAs).")
  }

  if(!all(sapply(data, class) %in% c("factor","numeric", "integer", "double", "logical"))){
    stop("Not all columns are of form factor or numeric!")
  }

  ### NEED TO REMOVE DATA WITH ONLY ONE FACTOR ### create if statements

  if(dim(data %>% is.na) %>% is.null){ # our data is one-dimensional: handle this case later
    # determine if the single vector is categorical/numeric
    # then appropriate calculation
    return()
  }


  if(sum(sapply(data, class) == "logical") > 0){
    data[sapply(data, class) == "logical"] = sapply(data[sapply(data, class) == "logical"], as.factor)
    warning("Logical rows mutated to factors.")
  }
  # Test: drops full NA rows
  test = (dim(data)[2] == rowSums(data %>% is.na)) == FALSE
  if(!all(test)){
    n = sum(!all(test))
    data = data[test, ]
    warning("Data frame contained rows with full NA's. " , as.character(n), " rows dropped.")
  }

  ############# Need to modify the data to change logical values to factors and then stopping if we dont get all values ##############


  ### Standardization & Dummies??? ###
  # Q: Should I just use dummies instead of evaluating categorical and numerical seperately?


  ### Method of Aggregation (for numeric variables) ###

  ############# Initialized and later delete ##############
  method = "mean"

  if(method == "mean"){
    agg = mean
  }else if(method == "median"){
    agg = median
  }else{
    # weighted average?
  }
  ### Imputation ###
#  data = missingCars[-3, ]
  if(split_categorical == TRUE){
    # Also consider other ways to impute data while combining factors and numeric columns.
    # 1) Quantitative Split

    data_numeric <- data[,sapply(data, class) %in% c("numeric", "integer", "double")]
    data_numeric_clean <- data_numeric %>% drop_na()


    if (dim(data_numeric %>% is.na) %>% is.null){
      row_has_NA <- which(data_numeric %>% is.na)
    }else{
      row_has_NA <- which((rowSums(data_numeric %>% is.na) %>% as.logical)) # we have a vector of indeces that contain an NA value
    }
    numNA <- length(row_has_NA)
    imputed_values <- rep(0, 0)

    screen <- (data_numeric %>% is.na) == FALSE
    # First need to handle cases with several rows

    for(i in 1:numNA){
      x <- (data_numeric[row_has_NA[i], ])[screen[row_has_NA[i], ]] # set we are trying to minimize distance over
      X <- data_numeric_clean[screen[row_has_NA[i], ]] # actual dataset we are comparing
      N <- dim(X)[1]
      (x %>% t())[rep(1, N), ]
      differenced_data <- X - (x %>% t())[rep(1, N),]
      distance_metric <- apply(differenced_data, 1, norm) # later on make it such that our function changes based off of command given
      minKNorm <- sort(distance_metric)[1:k] # gets k values with the smallest "distance"
      Kcluster_index <- which(distance_metric %in% minKNorm) # gets indeces of smallest distances
      # How to address ties
      # If the largest distance is tied (Better way to phrase this)
      if(length(Kcluster_index) > k){
        # the biggest value is tied
      }
      temp_imputed <- data_numeric_clean[!screen[row_has_NA[i],]][Kcluster_index, ]
      # Weighted Average Method
      if(method == "weightAvg"){
        if(is.null(dim(temp_imputed))){
          imputed_values = imputed_values %>% append(agg(temp_imputed))
        }else{
          imputed_values = imputed_values %>% append(apply(exp(-distance_metric[Kcluster_index]) * temp_imputed, MARGIN = 2, FUN = agg))
        }
      }else{
        if(is.null(dim(temp_imputed))){
          imputed_values = imputed_values %>% append(agg(temp_imputed))
        }else{
          imputed_values = imputed_values %>% append(apply(temp_imputed, MARGIN = 2, FUN = agg)) # the KNN imputed value
        }
      }
    }
    temp_df = t(data_numeric)
    temp_df[temp_df %>% is.na()] = imputed_values
    data_numeric = t(temp_df)

    # 2) Categorical Split
    # could also implement Jaccard index
    # impute our data numeric from before
    data[,sapply(data, class) %in% c("numeric", "integer", "double")] = data_numeric

    data_categorical <- data[,sapply(data, class) == "factor"]
    data_categorical_clean <- data %>% drop_na() # contains both categorical and numerical data
    if(dim(data_categorical %>% is.na) %>% is.null){
      row_has_NA <- which(data_categorical %>% is.na)
    }else{
      row_has_NA <- which((rowSums(data_categorical %>% is.na) %>% as.logical)) # we have a vector of indeces that contain an NA value
    }

    numNA <- length(row_has_NA)
    imputed_values = rep(9999, 0)
    screen <- (data %>% is.na) == FALSE

    # clean_dat numerical values
    X <- data_categorical_clean[,sapply(data_categorical_clean, class) %in% c("numeric", "integer", "double")]
    for(i in 1:numNA){
      # need to modify this a bit...
      x <- data_numeric[row_has_NA[i], ] # set we are trying to minimize distance over
      # actual thing we are comparing NEED TO MAKE SURE WE USE CLEAN CAT NUMERICAL DATASET
      N <- dim(X)[1]
      differenced_data <- X - (x %>% t())[rep(1, N),]
      distance_metric <- apply(differenced_data, 1, norm) # later on make is such that our function changes based off of command given
      minKNorm <- sort(distance_metric)[1:k] # gets k values with the smallest "distance"
      Kcluster_index <- which(distance_metric %in% minKNorm) # gets indeces of smallest distances

      # How to address ties???
      if(length(Kcluster_index) > k){
        # When the biggest value is tied, need to figure out solution
      }
      # Aggregation function: need to modify this for ties
      table_counts <- table(data_categorical_clean[Kcluster_index, screen[row_has_NA[i], ] == FALSE])
      if(sum(which.max(table_counts) == table_counts) > 1){
        max_indeces <- table_counts[which.max(table_counts) == table_counts] %>% sample(1) %>% names()
      }
      max_indeces <- which.max(table_counts) %>% names()
      data_categorical[row_has_NA[i], data_categorical[row_has_NA[i], ] %>% is.na] = max_indeces
    }
    data[,sapply(data, class) %in% c("factor")]  = data_categorical
    imputed_data = data
  }else{

      # Impute everything together
      df <- dummy_cols(data, ignore_na = TRUE)
      factor_screen <- (sapply(df, class) == "factor")
      factor_names <- df[, factor_screen] %>% names()
      factor_N <- factor_names %>% length()
      # drop columns that are factors
      data_numeric <- df[-3 , !factor_screen]
      data_numeric_clean <- data_numeric %>% drop_na()
      if (dim(data_numeric %>% is.na) %>% is.null){
        row_has_NA <- which(data_numeric %>% is.na)
      }else{
        row_has_NA <- which((rowSums(data_numeric %>% is.na) %>% as.logical)) # we have a vector of indeces that contain an NA value
      }
      imputed_values <- rep(0, 0)

      screen <- (data_numeric %>% is.na) == FALSE
      # First need to handle cases with several rows
      for(i in row_has_NA){
        x <- (data_numeric[i, ])[screen[i, ]] # set we are trying to minimize distance over
        X <- data_numeric_clean[screen[i, ]] # actual thing we are comparing
        N <- dim(X)[1]
        differenced_data <- X - x[rep(1, N),]
        distance_metric <- apply(differenced_data, 1, norm) # later on make is such that our function changes based off of command given
        minKNorm <- sort(distance_metric)[1:k] # gets k values with the smallest "distance"
        Kcluster_index <- which(distance_metric %in% minKNorm) # gets indeces of smallest distances
        # How to address ties
        # If the largest distance is tied (Better way to phrase this)
        if(length(Kcluster_index) > k){
          # the biggest value is tied
        }
        temp_imputed <- data_numeric_clean[!screen[i,]][Kcluster_index, ]
        # Weighted Average Method
        if(method == "weightAvg"){
          if(is.null(dim(temp_imputed))){
            imputed_values = imputed_values %>% append(agg(temp_imputed))
          }else{
            imputed_values = imputed_values %>% append(apply(exp(-distance_metric[Kcluster_index]) * temp_imputed, MARGIN = 2, FUN = agg))
          }
        }else{
          if(is.null(dim(temp_imputed))){
            imputed_values = imputed_values %>% append(agg(temp_imputed))
          }else{
            imputed_values = imputed_values %>% append(apply(temp_imputed, MARGIN = 2, FUN = agg)) # the KNN imputed value
          }
        }
      }
      temp_df = t(data_numeric)
      temp_df[temp_df %>% is.na()] = imputed_values
      data_numeric = t(temp_df)

      # need to choose the dummy with the largest value to set as 1 and the rest to 0 per row
      factor_names <- df[, factor_screen] %>% names()

      for(f_name in factor_names){
        f_name <- paste(f_name, "_", sep = "")
        screen <- str_detect((data_numeric %>% colnames()), f_name)

        max_vals = apply(data_numeric[,screen], 1, max)
        temp = ifelse(data_numeric[,screen] == max_vals, 1, 0)
        # Need to account for ties
        ties <- which(temp %>% rowSums() > 1)

        for(i in ties){
          set_as_1 <- which(temp[i, ] == 1) %>% names() %>% sample(size = 1)
          temp[i, ] <- (temp[i, ] %>% names() == set_as_1) %>% as.integer
        }
        data_numeric[,screen] <- temp
      }
      imputed_data = data_numeric
    }
  return(imputed_data)
}




