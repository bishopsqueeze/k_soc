##------------------------------------------------------------------
## This script is used to compute similarity matrices for the
## et of "reconstructed" egonets that were based on the original
## data provided by kaggle.  See "06_CalcReconstructedEdgeNetwork"
## for details of the reconstruction process.
##
## This script takes the "reconstructed" egonets and computes
## similarity matrices for those networks.
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
DOPARALLEL  <- TRUE

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
##load("02_SocialCircle_Edges.Rdata")       ## edge lists
load("06_SocialCircle_ReconEdges.Rdata")    ## reconstructed network edge lists

##------------------------------------------------------------------
## Loop over the egonets, define graph objects, save to file
##------------------------------------------------------------------

## extract egonet data
ego.names       <- names(recon_egoedges.list)
ego.num         <- length(ego.names)

## get the edge count for each, so we can work from smallest to largest
recon_egoedges.count  <- unlist(lapply(recon_egoedges.list, ecount))
recon_egoedges.order  <- order(recon_egoedges.count)

## define the ouput directory for individual edges files
output.dir  <- paste(getwd(),"recon.sim.topo",sep="/")

## loop over each egonet and compute the similarity matrices
for (i in 110:1) {
    
    ## set-up
    tmp.id          <- ego.names[recon_egoedges.order[i]]
    tmp.edges       <- recon_egoedges.list[[tmp.id]]
    
    ## echo progress
    cat("Iteration", i, "of", ego.num, " :: Similarity Matrix for", tmp.id, " :: <Reconstructed> # Edges =", ecount(tmp.edges), "\n")
    
    ## output files
    tmp.rdataName   <- paste(output.dir, paste0(tmp.id,".ReconSimilarityMatrix.Rdata"), sep="/")
    
    ## compute the matrices
    tmp.reconSim  <- calcSimilarityMatrix(tmp.edges)

    ## write intermediate results to a file
    save(tmp.reconSim, file=tmp.rdataName)
}


##------------------------------------------------------------------
## double check the results for the test case (i==40)
##------------------------------------------------------------------
#tmp.lc <- getLinkCommunities(get.data.frame(recon_egoedges.list[[tmp.id]]), hcmethod="single", plot=FALSE, verbose=FALSE)
#tmp.d  <- as.dist(1-tmp.sim)
#tmp.h  <- hclust(tmp.d, method="single")
#cbind(tmp.h$height, tmp.lc$hclust$height)








