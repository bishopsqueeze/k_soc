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
out.file    <- c()

for (i in 1:egonets.num) {
    
    ## grab the egonet data
    tmp.id  <- egonets.names[i]
    tmp.net <- egonets.list[[tmp.id]]
    
    ## loop over each of the friends in the egonet
    tmp.df  <- data.frame()
    for (j in 1:length(tmp.net)) {

        ## for each userid friend, create an "edge" between it and each of its friends
        tmp.from    <- as.integer(gsub("FR_", "", names(tmp.net)[j]))
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
    g   <- graph.data.frame(tmp.df, directed=F)
    gc  <- clusters(g)
    
    ## create an equivalent cluster file
    out.line    <- paste(tmp.id, ",", sep="")
    tmp.members <- sort(unique(tmp.df$from))
    
    for (k in 1:(gc$no)) {
        tmp.cluster <- tmp.members[ which(gc$membership == k) ]
        out.line    <- paste(out.line, paste(tmp.cluster, collapse=" "), sep="", ";")
    }
    
    out.file <- c(out.file, out.line)
}



##------------------------------------------------------------------
## *** To do:
##  - Compare with the python example ... should be close
##  - Compute different estimates using the different options in clusters()
##  - Learn more about this process
