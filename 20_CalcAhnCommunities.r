##------------------------------------------------------------------
## In this script we'll compute Ahn-like communities using both the
## similarity matrices that I constructed as well as the linkcomm()
## package. I'd like to see agreement between the two.  If that
## happens, then we can use a variety of hierarchical clustering
## methods to create communities.
##------------------------------------------------------------------

##------------------------------------------------------------------
## Load libraries
##------------------------------------------------------------------
library(igraph)                 ## contains graph functions
library(caTools)
library(linkcomm)
library(data.table)

##------------------------------------------------------------------
## Clear the workspace
##------------------------------------------------------------------
rm(list=ls())

##------------------------------------------------------------------
## Clear the workspaceBinary flag to enable/disable cross checks
##------------------------------------------------------------------
DO_CHECKS   <- TRUE

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
load("01_SocialCircle_RawData.Rdata")       ## raw data
load("02_SocialCircle_Edges.Rdata")         ## edge lists

##------------------------------------------------------------------
## Define the directory holding the similarity matrices
##------------------------------------------------------------------
simDirectory    <- "/Users/alexstephens/Development/kaggle/social_circle/data/inputs/sim"


##------------------------------------------------------------------
## Loop over the egonets and ...
##  - Load my similarity matrix
##  - Compute distances, clusters
##  - Compute partition density
##  - Use linkcomm() to compute similar data
##  - Compare the two
##------------------------------------------------------------------

if (DO_CHECKS) {
    
    ## extract egonet data
    ego.names       <- sort(names(egoedges.list))
    ego.num         <- length(ego.names)
    
    ## get the edge count for each, so we can work from smallest to largest
    egoedges.count  <- unlist(lapply(egoedges.list, ecount))
    egoedges.order  <- order(egoedges.count)
    
    ## define the ouput directory for individual edges files
    output.dir  <- paste(getwd(),"sim_xcheck",sep="/")
    
    ## loop over each egonet and compute the similarity matrices
    for (i in 3:3) {
        
        ## set-up
        tmp.id          <- ego.names[egoedges.order[i]]
        tmp.edges       <- egoedges.list[[tmp.id]]
        
        ## echo progress
        cat("Iteration", i, "of", ego.num, " :: Similarity Matrix for", tmp.id, " :: # Edges =", ecount(tmp.edges), "\n")
        
        ##------------------------------------------------------------------
        ## load similarity matrix (called tmp.sim)
        ##------------------------------------------------------------------
        tmp.simfile     <- paste(simDirectory, "/", paste0(tmp.id, ".SimilarityMatrix.Rdata"), sep="")
        load(tmp.simfile)
        
        ## compute distances (or the "dissimilarity" measure)
        tmp.dist        <- as.dist(1 - tmp.sim)

        ## generate a cluster
        tmp.hclust      <- hclust(tmp.dist, method="single")
        
        ## compute the partition density
        tmp.pdens   <- calcPartitionDensity(tmp.hclust, tmp.edges)
        
        ##------------------------------------------------------------------
        ## linkcomm() version
        ##------------------------------------------------------------------
        tmp.lc <- getLinkCommunities(get.data.frame(tmp.edges), hcmethod="single", plot=FALSE, verbose=FALSE)
        
        ## Compare computed heights (note that linkcomm() rounds to 5 digits
        my.h    <- tmp.hclust$height
        lc.h    <- tmp.lc$hclust$height
        cat("Sum of height differences = ", sum(my.h-lc.h), "\n")
        
        ## Compare computed partition densities
        my.d    <- c(0, tmp.pdens$pdens[,c("den")])
        lc.d    <- tmp.lc$pdens[,c("pdens")]
        cat("Sum of partition density differences = ", sum(my.h-lc.h), "\n")
        
        ## Compare cluster membership
        tmp.lc$clusters
        
        cat("\n")
    }

}


a <- extractHclustClusters(tmp.hclust, tmp.pdens$hmax, tmp.edges)

myIgraph <- tmp.edges
myHclust <- tmp.hclust
myHmax <- tmp.pdens$hmax


which(ego.names[egoedges.order] %in% names(known_circles.list))


##------------------------------------------------------------------
## double check the results for the test case (i==40)
##------------------------------------------------------------------
#tmp.lc <- getLinkCommunities(get.data.frame(egoedges.list[[tmp.id]]), hcmethod="single", plot=FALSE, verbose=FALSE)
#tmp.d  <- as.dist(1-tmp.sim)
#tmp.h  <- hclust(tmp.d, method="single")
#cbind(tmp.h$height, tmp.lc$hclust$height)








