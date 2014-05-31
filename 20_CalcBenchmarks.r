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
load("B02_ConnectedClustersBenchmark.Rdata")
load("py_connected_components.Rdata")

##------------------------------------------------------------------
## Source the utility functions
##------------------------------------------------------------------
source("/Users/alexstephens/Development/kaggle/social_circle/k_soc/00_Utilities.r")


##------------------------------------------------------------------
##Load the known circles
##------------------------------------------------------------------
known.ids   <- sort(names(known_circles.list))
known.num   <- length(known.ids)

##------------------------------------------------------------------
## Case 1:  Load the python-generated benchmark [py]
##------------------------------------------------------------------
## The python-generated benchmark contains a set of circles for each
## of the users in the sample (both train & test) ... 110 in total.
##------------------------------------------------------------------

## remove the header, find length, set-up placeholder list
py.ch   <- py.ex[-1]
py.len  <- length(py.ch)
py.list <- list()
py.res  <- data.frame()

## loop over each user and load the
for (i in 1:py.len) {
    tmp.ch  <- py.ch[i]
    tmp.res <- textToCircleList(tmp.ch)
    py.list[[tmp.res$ego]] <- lapply(tmp.res$res, '[')
    
    ## compute edit length for the train subset
    if (tmp.res$ego %in% known.ids) {
        tmp.true    <- known_circles.list[[tmp.res$ego]]
        tmp.pred    <- py.list[[tmp.res$ego]]
        tmp.edits   <- circleEdits(tmp.true, tmp.pred)
        py.res      <- rbind(py.res, data.frame(id=tmp.res$ego, n.true=length(tmp.true),
                                                n.pred=length(tmp.pred), n.edits=tmp.edits))
    }
}
py.res  <- py.res[order(py.res$id),]

##------------------------------------------------------------------
## Case 2:  Connected Clusters benchmark [cc]
##------------------------------------------------------------------
cc.res   <- data.frame()
for (i in 1:known.num) {
    
    tmp.id      <- known.ids[i]
    tmp.true    <- known_circles.list[[tmp.id]]
    tmp.pred    <- connectedClusters.list[[tmp.id]]

    tmp.edits   <- circleEdits(tmp.true, tmp.pred)
    cc.res      <- rbind(cc.res, data.frame(id=tmp.id, n.true=length(tmp.true),
                                         n.pred=length(tmp.pred), n.edits=tmp.edits))
}
cc.res  <- cc.res[order(cc.res$id),]



##------------------------------------------------------------------
## Case 1:  All vertices in a single circle
##------------------------------------------------------------------

for (i in 1:known.num) {
    
    tmp.id  <- known.ids[i]
    tmp.known   <- known_circles.list[[tmp.id]]
    
    ##
}


