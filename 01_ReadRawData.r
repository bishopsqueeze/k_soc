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
## [x] Training:
##  - contains "human-labeled" circles provided by a user
##  - filename in the format <user>.circles
##  - a total of 60 training files
## Testset_Users_Friends:
##  == List of users in the test set and friends of each user
##  == Same data as is in the <userid>.egonet files
## [x] FeatureList
##  == List of all possible features
##------------------------------------------------------------------
## Goal of the project:
##  -- You are given the egonets of 110 individuals
##  -- You also have feature data for all individuals (and their friends)
##  -- Of the 110 individuals, you have labelled "social circles" for 60
##  -- The goal is to predict "social circles" for the remaining 50
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
## Load utility functions
##------------------------------------------------------------------
source("/Users/alexstephens/Development/kaggle/social_circle/k_soc/00_Utilities.r")


##******************************************************************
## Step 1:  Read the list of all possible features
##******************************************************************

## read the file
fileName            <- "featureList.txt"
featureDict         <- readLines(fileName, n = -1)

## create a prototype list of all known features
#proto.list          <- list()
#for (i in 1:length(featureDict)) {
#    tmp.feature               <- featureDict[i]
#    proto.list[[tmp.feature]] <- NA
#}


##******************************************************************
## Step 1:  Read in the "social circle" training data
##******************************************************************

## read the filenames
trainingFiles       <- dir("./training", pattern=".circles")

## define a list to hold the combined results
known_circles.list       <- list()

## loop over all of the files and load the known_circles
for (i in 1:length(trainingFiles)) {
    
    ## read the file (and extract some basic data)
    tmp.file        <- trainingFiles[i]                                         ## the file to read
    tmp.userid      <- strsplit(tmp.file, "[.]")[[1]][1]                        ## the user who's circles are identified
    tmp.id          <- paste("ID_",tmp.userid,sep="")                           ## a user-based ID = "ID_<userid>"
    tmp.rl          <- readLines(paste("./training/",tmp.file,sep=""), n = -1)  ## a character vector of the file contents
    tmp.num_circles <- length(tmp.rl)                                           ## the number of circles
    
    ## loop over each of the circles and load a list
    for (j in 1:tmp.num_circles) {
        
        ## extract the circle ID
        tmp.circle_id       <- strsplit( tmp.rl[j], "[:]")[[1]][1]
        tmp.circle_members  <- trim(strsplit(tmp.rl[j], "[:]")[[1]][2])
        
        ## define the list
        known_circles.list[[tmp.id]][[tmp.circle_id]]    <- list()
        
        ## extract the members associated with the circle
        if (tmp.circle_members  == "" ) {
            known_circles.list[[tmp.id]][[tmp.circle_id]]  <- -99999
        } else {
            known_circles.list[[tmp.id]][[tmp.circle_id]]  <- as.vector(as.integer(strsplit(trim(strsplit(tmp.rl[j], "[:]")[[1]][2]), " ")[[1]]))
        }
    }
}


##******************************************************************
## Step 2: Read in the egonets
##******************************************************************

## read the filenames
egonetFiles    <- dir("./egonets", pattern=".egonet")

## define a list to hold the combined results
egonets.list       <- list()

for (i in 1:length(egonetFiles)) {

    ## read the file (and extract some basic data)
    tmp.file        <- egonetFiles[i]                                           ## the file to read
    tmp.userid      <- strsplit(tmp.file, "[.]")[[1]][1]                        ## the user who's circles are identified
    tmp.id          <- paste("ID_",tmp.userid,sep="")                           ## a user-based ID = "ID_<userid>"
    tmp.rl          <- readLines(paste("./egonets/",tmp.file,sep=""), n = -1)  ## a character vector of the file contents
    tmp.num_friends <- length(tmp.rl)                                           ## the number of circles

    ## loop over each of the lines and load a list
    for (j in 1:tmp.num_friends) {

        ## extract the friend ID
        tmp.friend_id           <- paste("ID_",strsplit( tmp.rl[j], "[:]")[[1]][1],sep="")
        tmp.friends_of_friend   <- trim(strsplit(tmp.rl[j], "[:]")[[1]][2])
        
        ## define the list
        egonets.list[[tmp.id]][[tmp.friend_id]]    <- list()
         
        ## extract the friends associated with this friend
        if ( tmp.friends_of_friend == "" ) {
            egonets.list[[tmp.id]][[tmp.friend_id]]    <- -99999
        } else {
            egonets.list[[tmp.id]][[tmp.friend_id]]    <- as.vector(as.integer(strsplit(tmp.friends_of_friend, " ")[[1]]))
        }
    }
}


##******************************************************************
## Step 3:  Read in the feature list for *all* users/friends
##******************************************************************
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
## Inputs:
##
## Outputs:
##  The primary outputs here are two separate lists:
##    features.list:
##      Contains the non-null set of features for a user.
##      When there are multiple values for that feature, then we
##      append those values to what's already there.  So a user can
##      have several people tagged as being in the same class.
##    leaves.list:
##      Contains the unsplit feature/value pairs associated with a
##      users.
##------------------------------------------------------------------

## read the file
fileName            <- "features.txt"
features.rl         <- readLines(fileName, n = -1)

## use sapply & strsplit to break each string into a set of id + feature/value pairs
split.list          <- sapply(features.rl, strsplit, " ")

## create a list to hold the set of feature/values for each id
features.list       <- list()
leaves.list         <- list()

## hold a count of features per user
featPerUser.vec     <- vector(, length=length(split.list))

## loop over each element in the features list and load into a prototype list
for (i in 1:length(split.list)) {
    
    ## grab the features for this id
    tmp.vec     <- sort(split.list[[i]])   ## includes the id and the features
    tmp.len     <- length(tmp.vec)
    tmp.id      <- paste("ID_",tmp.vec[1],sep="")
    
    ## retain the number of features, but exclude the id from the  count
    featPerUser.vec[i]  <- tmp.len - 1
    
    ## initialize a list to hold output
    tmp.proto           <- NULL

    ## loop over the set of values and load them into the prototype list
    ## each feature/value pair has the form "node1;node2;...;nodeN;value"
    ## so we need to split that into the two separate components
    for (j in 2:tmp.len) {
        
        tmp.pair    <- unlist(strsplit(tmp.vec[j], ";"))
        tmp.value   <- tmp.pair[length(tmp.pair)]
        tmp.feature <- paste( tmp.pair[1:(length(tmp.pair)-1)], collapse=";")
        
        ## grow a tree/list and append values to each of the leafs
        if ( is.null(tmp.proto) ) {
            tmp.proto   <- list()
            tmp.proto[[tmp.feature]] <- as.integer(tmp.value)
        } else {
            tmp.proto[[tmp.feature]] <- c(tmp.proto[[tmp.feature]], as.integer(tmp.value))
        }
    }
    
    ## copy the prototype into the main features list
    features.list[[tmp.id]]  <- tmp.proto
    leaves.list[[tmp.id]]    <- tmp.vec[2:length(tmp.vec)]
 
}


##******************************************************************
## Step 4:  Read in a (redundant) file of test user friends. Similar
##          data is housed in the <userid>.egonets file, but this
##          is useful to cross-check those results
##******************************************************************

## read the file
fileName            <- "testSet_users_friends.csv"
testUsers.rl        <- readLines(fileName, n = -1)

## create a list to hold the results
testUsers.list      <- list()

## for each line in the file, extract the userid and its list of friends ids
for (i in 1:length(testUsers.rl)) {
    tmp.line            <- testUsers.rl[i]
    tmp.userid          <- strsplit(tmp.line, "[:]")[[1]][1]                                ## the user who's circles are identified
    tmp.id              <- paste("ID_",tmp.userid,sep="")                                   ## a user-based ID = "ID_<userid>"
    testUsers.list[[tmp.id]] <- as.vector(as.integer(strsplit(trim(strsplit(tmp.line, "[:]")[[1]][2]), " ")[[1]]))

}


##******************************************************************
## Step 5:  Save the results
##******************************************************************
save(   egonets.list,
        featureDict,
        features.list,
        leaves.list,
        known_circles.list,
        testUsers.list,
        featPerUser.vec,
        file="01_SocialCircle_RawData.Rdata")





