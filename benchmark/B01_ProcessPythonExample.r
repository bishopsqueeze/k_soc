##------------------------------------------------------------------
## The purpose of this script is to:
##  1. Process the raw python-based "connected components" example
##     submission
##  2. Note that the example contains social circle predictions for
##     all of the users (train, test).  So, this script strips out
##     the test users and does a sanity check to ensure the formatting
##     is correct for a submission.
##  3. As expected it generates a proper benchmark score
##  4. Save results as an Rdata file so I can compare the python-based
##     results against similar R-based results
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
## Read in the raw data files
##------------------------------------------------------------------
load("01_SocialCircle_RawData.Rdata")

##------------------------------------------------------------------
## Read in the python-generated connected_components example, which
## contains results for both the train & test users
##------------------------------------------------------------------
pythonExample   <- "/Users/alexstephens/Development/kaggle/social_circle/submissions/connected_components.txt"
py.ex           <- readLines(pythonExample, n = -1)

##******************************************************************
## Step 1:  Split each line of the python file into id(s) and cluster(s)
## and extract the id(s) that correspond to the test users. Note
## that there is a header row in the example file.
##******************************************************************
num.ex          <- length(py.ex)                ## length of the file
id.vector       <- vector(, length(num.ex))     ## number of ids to capture

## loop over the file and grab id(s)
for (i in 1:num.ex) {
    if (i == 1) {
        id.vector[i] <- -99999  ## dummy for the header row
    } else {
        tmp.line        <- py.ex[i]
        tmp.id          <- strsplit(tmp.line, ",")[[1]][1]
        id.vector[i]    <- as.integer(tmp.id)
    }
}

##------------------------------------------------------------------
## Get the list of known test users from testUsers.list file
##------------------------------------------------------------------
test.id     <- as.integer(gsub("ID_", "", names(testUsers.list)))   ## all this text stripping is dumb

##------------------------------------------------------------------
## Identify the location of the test users in the python example
## and the extract that subset from the file
##------------------------------------------------------------------
py.idx  <- which(id.vector %in% test.id)    ## index of test users
py.test <- c(py.ex[1] , py.ex[py.idx])      ## add the header to the index


##******************************************************************
## Step 2:  Confirm you've got the right id(s) by looping back over
## the subset of the file and extracting the id(s) ... then match
## against the known set of test user id(s) ... just as a double-check
##******************************************************************
py.test.id  <- vector(, length=length(py.test))
for (i in 1:length(py.test)) {
    if (i == 1) {
        py.test.id[i]  <- -99999
    } else {
        tmp.line <- py.test[i]
        py.test.id[i]  <- as.integer(strsplit(tmp.line, ",")[[1]][1])
    }
}

##------------------------------------------------------------------
## Compare the two sets of ids
##------------------------------------------------------------------
py.compare  <- cbind( sort(py.test.id[2:length(py.test.id)]), sort(test.id) )

##------------------------------------------------------------------
## Create submission file for uploading
##------------------------------------------------------------------
##write(py.test, file="/Users/alexstephens/Development/kaggle/social_circle/submissions/connected_submission.csv")

##------------------------------------------------------------------
## Write the connected components example to an .Rdata file
##------------------------------------------------------------------
save(py.ex, file="/Users/alexstephens/Development/kaggle/social_circle/data/inputs/py_connected_components.Rdata")




