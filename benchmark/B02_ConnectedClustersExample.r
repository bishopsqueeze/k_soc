##------------------------------------------------------------------
##
##------------------------------------------------------------------

##------------------------------------------------------------------
## Load libraries
##------------------------------------------------------------------
library(igraph)
library(clue)

##------------------------------------------------------------------
## Clear the workspace
##------------------------------------------------------------------
rm(list=ls())

##------------------------------------------------------------------
## Set the working directory
##------------------------------------------------------------------
setwd("/Users/alexstephens/Development/kaggle/social_circle/data/inputs")

##------------------------------------------------------------------
## Source the utility functions
##------------------------------------------------------------------
source("/Users/alexstephens/Development/kaggle/social_circle/k_soc/00_Utilities.r")

##------------------------------------------------------------------
## Read in the raw data files
##------------------------------------------------------------------
load("01_SocialCircle_RawData.Rdata")

##------------------------------------------------------------------
## Read in the python-based connected components example
##------------------------------------------------------------------
load("py_connected_components.Rdata")

##------------------------------------------------------------------
## Step 1:  Use the <igraph> package to reproduce the python example
##------------------------------------------------------------------

## define egonets data
egonets.names   <- names(egonets.list)
egonets.num     <- length(egonets.list)

##------------------------------------------------------------------
## Loop over each of the egonets and load a dataframe to be used in
## the connected cluster computation.
##------------------------------------------------------------------
out.file                <- c()      ## to hold the submission text data
connectedClusters.list  <- list()   ## to hold a list of circles

for (i in 1:egonets.num) {
    
    ## grab the egonet data
    tmp.id  <- egonets.names[i]
    tmp.net <- egonets.list[[tmp.id]]
    
    ## loop over each of the friends in the egonet
    tmp.df  <- data.frame()
    for (j in 1:length(tmp.net)) {

        ## for each userid friend, create an "edge" between it and each of its friends
        tmp.from    <- as.integer(gsub("ID_", "", names(tmp.net)[j]))
        tmp.to      <- tmp.net[[j]]
        
        ## for now, just append to a growing datafame (ugly)
        if (j == 1) {
            tmp.df  <- data.frame( from=rep(tmp.from, length(tmp.to)), to=sort(tmp.to) )
        } else {
            tmp.df  <- rbind(tmp.df, data.frame( from=rep(tmp.from, length(tmp.to)), to=sort(tmp.to) ))
        }
    }
    
    ## purge friends with no connection
    tmp.df  <- tmp.df[(tmp.df$to != -99999),]
    
    ## create a graph from the dataframe & generate the clusters
    g       <- dedupEdgeList(graph.data.frame(tmp.df, directed=F))
    g.num   <- vcount(g)
    gc      <- clusters(g)
    
    ## create an equivalent cluster file
    out.line    <- paste(tmp.id, ",", sep="")
    tmp.members <- as.integer(names(g[[1:g.num,]]))
    
    for (k in 1:(gc$no)) {
        tmp.cluster <- tmp.members[ which(gc$membership == k) ]
        out.line    <- paste(out.line, paste(tmp.cluster, collapse=" "), sep="", ";")
        connectedClusters.list[[tmp.id]][[paste("circle",k,sep="")]] <- sort(tmp.cluster)
    }
    
    out.file <- c(out.file, out.line)
}

##------------------------------------------------------------------
## save the results to disk for later benchmarking
##------------------------------------------------------------------
save(connectedClusters.list, file="B02_ConnectedClustersBenchmark.Rdata")
write.table(out.file, file="B02_ConnectedClustersBenchmark.txt", row.names=FALSE, col.names=FALSE)

