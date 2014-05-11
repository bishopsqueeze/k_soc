##------------------------------------------------------------------
## From the kaggle website:
##------------------------------------------------------------------
## User:
##  == "target individual" whose network is the focus of the task
## Friend:
##  == friend of the user ("target individual") that needs to be placed into a circle
## Egonets:
##  == contains "ego-network" of a user
##  == filename in the format <userid>.egonet
## Features:
##  == contains "features" for all users (e.g., city, schooling, etc.)
## Training:
##  == contains "human-labeled" circles provided by a user
##  == filename in the format <user>.circles
## Testset_Users_Friends:
##  == List of users in the test set and friends of each user
##  == Same data as is in the <userid>.egonet files
##------------------------------------------------------------------

##------------------------------------------------------------------
## Clear the workspace
##------------------------------------------------------------------
rm(list=ls())

##------------------------------------------------------------------
## Set the working directory
##------------------------------------------------------------------
setwd("/Users/alexstephens/Development/kaggle/social_circle/data/inputs")

##------------------------------------------------------------------
<<<<<<< HEAD
## Read the list of all possible features
=======
## Read a dictionary of possible feautres
>>>>>>> FETCH_HEAD
##------------------------------------------------------------------

## read the file
fileName            <- "featureList.txt"
featureDict         <- readLines(fileName, n = -1)


## create a prototype list of all known features
proto.list          <- list()
for (i in 1:length(featureDict)) {
    tmp.feature               <- featureDict[i]
    proto.list[[tmp.feature]] <- NA
}


##------------------------------------------------------------------
## Read in the features associated with a given ID. File in the format:
##------------------------------------------------------------------
## <id>
## <feature_1><value_1>
## ...
## <feature_N><value_N>
##------------------------------------------------------------------
## "0 last_name;0 first_name;0 birthday;0 name;0 gender;0 locale;0
##  hometown;name;0 hometown;id;0 education;school;name;0 education;school;id;0
##  education;type;0 education;year;name;0 education;year;id;0 education;school;name;1
##  education;school;id;1 education;type;1 education;concentration;name;0
##  education;concentration;id;0 education;year;name;1 education;year;id;1
##  id;0 location;name;0 location;id;0"
##------------------------------------------------------------------
## Note that there can be repeats of a given feature
##------------------------------------------------------------------

## read the file
fileName            <- "features.txt"
features.rl         <- readLines(fileName, n = -1)

## use sapply & strsplit to break each string into a set of id + feature/value pairs
features.list       <- sapply(features.rl, strsplit, " ")

## create a list to hold the set of feature/values for each id
fv.list <- list()

## loop over each element in the features list and load into a prototype list
for (i in 1:length(features.list)) {
    
    ## grab the features for this id
    tmp.vec     <- features.list[[i]]
    tmp.len     <- length(tmp.vec)
    tmp.proto   <- proto.list
    
    ## create vectors
    feature.vec <- vector(, length=tmp.len)
    value.vec   <- vector(, length=tmp.len)
    
    ## loop over the set of values and load them into the prototype list
    for (j in 2:tmp.len) {
        
        tmp.pair    <- unlist(strsplit(tmp.vec[j], ";"))
        tmp.value   <- tmp.pair[length(tmp.pair)]
        tmp.feature <- paste( tmp.pair[1:(length(tmp.pair)-1)], collapse=";")
        
        if ( sum(is.na(tmp.proto[[tmp.feature]])) == 1 ) {
            tmp.proto[[tmp.feature]][is.na(tmp.proto[[tmp.feature]])] <- tmp.value
        } else {
            tmp.proto[[tmp.feature]] <- c(tmp.proto[[tmp.feature]], tmp.value)
        }
    }
    
    ## copy the prototype into the main fv.list
    fv.list[[i]] <- tmp.proto
 
}


##------------------------------------------------------------------
## Read in the egonets
##------------------------------------------------------------------
egonetFiles    <- dir("./egonets", pattern=".egonet")

#for (i in 1:length(egonetFiles)) {
for (i in 1:1) {
    
    tmp.file    <- egonetFiles[i]
    tmp.rl      <- readLines(paste("./egonets/",tmp.file,sep=""), n = -1)

}


##------------------------------------------------------------------
## Read in the training data
##------------------------------------------------------------------
trainingFiles    <- dir("./training", pattern=".circles")

#for (i in 1:length(egonetFiles)) {
for (i in 1:1) {
    
    tmp.file    <- trainingFiles[i]
    tmp.rl      <- readLines(paste("./training/",tmp.file,sep=""), n = -1)
    
}


##------------------------------------------------------------------
## Read in the testset_users data
##------------------------------------------------------------------

## read the file
fileName            <- "testSet_users_friends.csv"
testUsers.rl        <- readLines(fileName, n = -1)



## Note:  a person can have multiple entries for (probably) a subset of the field (like education;year;name)
