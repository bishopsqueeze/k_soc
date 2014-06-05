##------------------------------------------------------------------
##
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
## Define the parallel flag
##------------------------------------------------------------------
DOPARALLEL  <- FALSE

##------------------------------------------------------------------
## Register the clusters
##------------------------------------------------------------------
if (DOPARALLEL) {
    library(foreach)
    library(doMC)
    registerDoMC(4)
}

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
## Loop over the egonets, define graph objects, save to file
##------------------------------------------------------------------

## extract egonet data
ego.names       <- sort(names(egoedges.list))
ego.num         <- length(ego.names)

## get the edge count for each, so we can work from smallest to largest
egoedges.count  <- unlist(lapply(egoedges.list, ecount))
egoedges.order  <- order(egoedges.count)

## define the ouput directory for individual edges files
output.dir  <- paste(getwd(),"sim",sep="/")

## loop over each egonet and compute the similarity matrices
for (i in 1:10) {
    
    ## set-up
    tmp.id          <- ego.names[egoedges.order[i]]
    tmp.edges       <- egoedges.list[[tmp.id]]
    
    ## echo progress
    cat("Iteration", i, "of", ego.num, " :: Profile Similarity Matrix for", tmp.id, " :: # Edges =", ecount(tmp.edges), "\n")
    
    ## load the profile matrix (called the leafMatrix
    tmp.profFile    <- paste0(getwd(),"/prof/",paste0(tmp.id,".ProfileMatrix.Rdata"))
    load(tmp.profFile)
    
    ## output files
    tmp.rdataName   <- paste(output.dir, paste0(tmp.id,".UserProfileSimilarityMatrix.Rdata"), sep="/")
    
    ## compute the matrices
    #tmp.userProfSim  <- calcProfileSimilarityMatrix(tmp.edges, tmp.leafMatrix)
    tmp.userProfSim  <- calcEgoSimilarityMatrix(tmp.id, tmp.edges, tmp.leafMatrix)

    cat("max value =", max(max(tmp.userProfSim)), "\n")
    ## write intermediate results to a file
    #save(tmp.userProfSim, file=tmp.rdataName)

}






## --- debug ---
#myIgraph <- tmp.edges
#myLeafMatrix <- tmp.leafMatrix

## load the profile matrix
#tmp.simFile    <- paste0(getwd(),"/sim/",paste0(tmp.id,".SimilarityMatrix.Rdata"))
#load(tmp.simFile)


##------------------------------------------------------------------
## double check the results for the test case (i==40)
##------------------------------------------------------------------
#tmp.lc <- getLinkCommunities(get.data.frame(egoedges.list[[tmp.id]]), hcmethod="single", plot=FALSE, verbose=FALSE)
#tmp.d  <- as.dist(1-tmp.sim)
#tmp.h  <- hclust(tmp.d, method="single")
#cbind(tmp.h$height, tmp.lc$hclust$height)








