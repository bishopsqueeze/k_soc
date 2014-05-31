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
## Load the known circles
##------------------------------------------------------------------
known.ids   <- sort(names(known_circles.list))
known.num   <- length(known.ids)


##------------------------------------------------------------------
## Case 1:  Load the python-generated benchmark [py]
##------------------------------------------------------------------
## The python-generated benchmark contains a set of circles for each
## of the users in the sample (both train & test) ... 110 in total.
##------------------------------------------------------------------
## > sum(py.res$n.edits) = 19571
##------------------------------------------------------------------

## remove the header, find length, set-up placeholder list
py.ch   <- py.ex[-1]
py.len  <- length(py.ch)
py.list <- list()
py.res  <- data.frame()

## loop over each user and load the circles
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
## This uses my R-based version of the connected clusters benchmark
##------------------------------------------------------------------
## > sum(cc.res$n.edits) = 19571
##------------------------------------------------------------------

## set-up placeholder list
cc.res   <- data.frame()

## loop over each known user and compare circles
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
## Case 3:  All vertices in a single circle
##------------------------------------------------------------------
## > sum(singleCircle.res$n.edits) = 18212
##------------------------------------------------------------------

## set-up placeholder list
singleCircle.res   <- data.frame()

## loop over each known user and compare circles
for (i in 1:known.num) {
    
    tmp.id      <- known.ids[i]
    tmp.known   <- known_circles.list[[tmp.id]]
    tmp.true    <- known_circles.list[[tmp.id]]
    tmp.pred    <- list(sort(unique(as.vector(unlist(lapply(tmp.known,'['))))))
    
    tmp.edits           <- circleEdits(tmp.true, tmp.pred)
    singleCircle.res    <- rbind(singleCircle.res,
                            data.frame(id=tmp.id, n.true=length(tmp.true),
                            n.pred=length(tmp.pred), n.edits=tmp.edits))
}
singleCircle.res  <- singleCircle.res[order(singleCircle.res$id),]


##------------------------------------------------------------------
## Case 4:  Each vertex in its owne circle
##------------------------------------------------------------------
## > sum(eachToOneCircle.res$n.edits) = 27488
##------------------------------------------------------------------

## set-up placeholder list
eachToOneCircle.res   <- data.frame()

## loop over each known user and compare circles
for (i in 1:known.num) {
    
    tmp.id      <- known.ids[i]
    tmp.known   <- known_circles.list[[tmp.id]]
    tmp.true    <- known_circles.list[[tmp.id]]
    tmp.pred    <- lapply(sort(unique(as.vector(unlist(lapply(tmp.known,'['))))), function(x){x})

    tmp.edits             <- circleEdits(tmp.true, tmp.pred)
    eachToOneCircle.res   <- rbind(eachToOneCircle.res,
                                    data.frame(id=tmp.id, n.true=length(tmp.true),
                                    n.pred=length(tmp.pred), n.edits=tmp.edits))
}











