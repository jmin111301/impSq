# Function called Impute
# Last Updated: 3/19/23


# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#install.packages("Rcpp")
library(bench)
library(dplyr)
library(Rcpp)
library(tidyverse)


#x <- runif(1e6)
#bench::mark(
#  mean(x),
#  meanTest(x),
#  meanC(x)
#)


Impute<- function(data, method = "random"){
  sourceCpp("/Users/ymin/imputation/imputation/C/impute.cpp")
  ### Naive random imputation ###
  naCols <- names(data)[data %>% is.na() %>% colSums() > 0]
  for (name in naCols){
    col <- data[,name]
    colNA <- col %>% is.na()
    col_numNA <- sum(colNA)
    if (method == "random"){
      col_imputed_values <- sample(col[!colNA] %>% unique, size=col_numNA, replace=TRUE)
    }else if(method == "mean"){
      # need to account for categorical data (fills in the most frequent factor)
      if(col %>% class() == "factor"){
        col_imputed_values <- rep((col[!colNA] %>% table)[sample(which.max(col[!colNA] %>% table), 1)] %>% names %>% as.factor, col_numNA)
      }else if((col %>% class()) %in% c("integer", "numeric", "double")){
        col_imputed_values <- rep(meanC(col[!colNA]), col_numNA)
      }
    }else if(method == "median"){
      # need to account for categorical data (fills in the most frequent factor)
      if(col %>% class() == "factor"){
        col_imputed_values <- rep((col[!colNA] %>% table)[sample(which.max(col[!colNA] %>% table), 1)] %>% names %>% as.factor, col_numNA)
      }else if(((col %>% class()) %in% c("numeric", "integer", "double"))){
        col_imputed_values <- rep(median(col[!colNA]), col_numNA)
      }
    }else if(method == "-1"){
      # need to account for categorical data (fills in the most frequent factor)
      if(col %>% class() == "factor"){
        levels(data[,name]) <- c(levels(col), '-1')
        col_imputed_values <- '-1'
      }else if(((col %>% class()) %in% c("numeric", "integer", "double"))){
        col_imputed_values <- rep(meanC(col[!colNA]), col_numNA)
      }
    }else{
      stop("The method is not proper")
    }
    data[,name][colNA] <- col_imputed_values
  }
  return(data)
}
