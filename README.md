# NOTE

This package is outdated and I plan to rewrite/optimize this code in C

# imputation
An Imputation Package in R that provides tools to fill in missing values in data. This is currently a *WORK IN PROGRESS*. As such, any help/debugging would be greatly appreciated. 

## Functions:
### Imputation
1) `KNNimpute`: applies K-Nearest Neighbors to impute missing values. Works with both categorical and numeric data. Still refining the methodology and edge cases.
2) `RegImpute`: applied regression techniques to impute missing values. Works with both categorical and numeric data. Still refining the methodology and edge cases.
3) `Impute`: a naive imputation technique that fills in missing values with randomly sampled values from each column. Note: will probably make this into just `Impute` with options `random`, `mean`, `median` to fill in the column values. 
### General Functions
1) `modifiedJaccard`: a modified version of the Jaccard index that works with both continuous and discrete data by scaling continuous variables (currently normally) and then determining if the observed values within the `band`s of the value we want to compare against.

## Data:
1) `missingCars`: a modified version of the `cars` dataset from the library `car`. This includes the features `speed`, `dist`, `wheels`, and `type`. Note that this dataset contains 2 factor and 2 numerical variables. 
