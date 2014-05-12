##------------------------------------------------------------------
##
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
## Test 1:  Confirm that the egonets files produce the same
##          number of friends as is stored in the testUsers file
##          (Note that the testUsers file only contains 50 of the
##          110 userid population).
##------------------------------------------------------------------
## Result:
##  - No mismatches in the two sets of data
##------------------------------------------------------------------

## consolidated file list of test names
test.ids    <- names(testUsers.list)

## loop over the test IDs and confirm consistency
for (i in 1:length(test.ids)) {
    
    ## grab a testuser id
    tmp.id  <- names(testUsers.list)[i]
    cat("\n Testing ID = ", tmp.id, "\n\n")
    
    ## extract the friends list from the testUsers and egonets data
    test.friends    <- sort( testUsers.list[[tmp.id]] )
    egonets.friends <- sort( as.integer(gsub("FR_","",names(egonets.list[[tmp.id]]))) )    ## clunky
    
    ## check for consistency in length and content
    if (length(test.friends) != length(egonets.friends)) {
        
        cat("Length mismatch for ID = ", tmp.id, "\n")
        cat("testUsers == ", length(test.friends), " ... and ... egonets == ", length(egonets.friends), "\n\n")
        
    } else {
        
        cat("No length mismatch \n")
        cat("Number of matching friend IDs == ", sum(test.friends == egonets.friends), "\n")
        cat("Number of mis-matched friend IDs == ", sum(test.friends != egonets.friends), "\n")
        
    }
    
}


##------------------------------------------------------------------
## Test 2:  Count the number of total and total unique friends in
##          the egonets file and the features file
##------------------------------------------------------------------
## Result:
##  - The combined set of unique <userids> and <egonets> friends equals
##    the number of entries in the <features> file
##  - So each <userid> and <egonets> friend has some identifying data
##------------------------------------------------------------------
egonets.names   <- names(egonets.list)
egonets.num     <- length(egonets.names)
egonets.ids     <- c()

## loop over all egonets and extract all friend ids associated with all users
for (i in 1:egonets.num) {
    
    tmp.id      <- egonets.names[i]
    tmp.friends <- sort( as.integer(gsub("FR_","",names(egonets.list[[tmp.id]]))) )
    egonets.ids <- c(egonets.ids, tmp.friends)
    
}
egonets.ids <- sort(egonets.ids)

cat("Total number of egonet friends == ", length(egonets.ids))
cat("Total number of unique friends == ", length(unique(egonets.ids)))

## extract id information from the features.list file
features.ids    <- sort(as.integer(unlist(lapply(features.list, function(x){x$id}))))
cat("Total number of features == ", length(features.ids))

## extract the id information from the egonets file
users.ids       <- sort( as.integer(gsub("ID_","",names(egonets.list))) )

## now, combine the data from <users.ids> and <egonets.ids>
egonets.comb    <- sort(c(users.ids, egonets.ids))
egonets.uniq    <- unique(egonets.comb)

cat("Total number of combined egonet friends == ", length(egonets.comb))
cat("Total number of combined unique friends == ", length(egonets.uniq))
