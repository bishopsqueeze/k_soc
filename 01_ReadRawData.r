##------------------------------------------------------------------
## The purpose of this script is to:
##	1. Simply read the raw data files
##------------------------------------------------------------------

##------------------------------------------------------------------
## Load libraries
##------------------------------------------------------------------
##library(data.table)

##------------------------------------------------------------------
## Clear the workspace
##------------------------------------------------------------------
rm(list=ls())

##------------------------------------------------------------------
## Set the working directory
##------------------------------------------------------------------
setwd("/Users/alexstephens/Development/kaggle/social_circle/data/inputs")

##------------------------------------------------------------------
## A vector of feature combinations
##------------------------------------------------------------------
fileName            <- "featureList.txt"
featureDict         <- readLines(fileName, n = -1)


##------------------------------------------------------------------
## A vector of feature combinations
##------------------------------------------------------------------
## "0 last_name;0 first_name;0 birthday;0 name;0 gender;0 locale;0
##  hometown;name;0 hometown;id;0 education;school;name;0 education;school;id;0
##  education;type;0 education;year;name;0 education;year;id;0 education;school;name;1
##  education;school;id;1 education;type;1 education;concentration;name;0
##  education;concentration;id;0 education;year;name;1 education;year;id;1
##  id;0 location;name;0 location;id;0"

fileName            <- "features.txt"
features.rl         <- readLines(fileName, n = -1)

## use sapply & strsplit to break each sring into its feature/value pair
features.list       <- sapply(features.rl, strsplit, " ")

## define a matrix to hold the features
features.mat        <- matrix(, nrow=length(features.list), ncol=length(featureDict))

## loop over each element in the features list and process the line
for (i in 1:100) {
    tmp.vec     <- features.list[[i]]
    tmp.len     <- length(tmp.vec)
    
    ## loop over tmp.len
    feature.vec <- vector(, length=tmp.len)
    value.vec   <- vector(, length=tmp.len)
    for (j in 1:tmp.len) {
        if (j == 1) {
            tmp.id          <- tmp.vec[j]
        } else {
            tmp.pair        <- unlist(strsplit(tmp.vec[j], ";"))
            value.vec[j]    <- tmp.pair[length(tmp.pair)]
            feature.vec[j]  <- paste( tmp.pair[1:(length(tmp.pair)-1)], collapse=";")
        }
    }
    value.vec   <- value.vec[2:length(value.vec)]
    feature.vec <- feature.vec[2:length(feature.vec)]
 
    value.idx   <- which(featureDict %in% feature.vec)
    features.mat[i, value.idx]   <- value.vec
}


## Note:  a person can have multiple entries for (probably) a subset of the field (like education;year;name)
