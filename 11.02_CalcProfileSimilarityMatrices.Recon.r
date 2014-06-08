##------------------------------------------------------------------
## The purpose of this script is to compute a "profile similarity"
## matrix for each egonet, where the profile similarity represents
## the level of similarity between two sets of feature vectors.
##
## The feature vectors encode information about each individual
## (e.g., name, birthday, education, work, etc.) ... so it serves
## as another means of identifying common information within an
## ego network.
##
## The script will compute similarity matrices for two types of
## feature vector:  the full dataset, and subset of data that is
## likely to elicit an acutal match.
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
##load("02_SocialCircle_Edges.Rdata")         ## edge lists
load("06_SocialCircle_ReconEdges.Rdata")    ## reconstructed network edge lists

##------------------------------------------------------------------
## set-up
##------------------------------------------------------------------

## extract egonet data
ego.names       <- names(recon_egoedges.list)
ego.num         <- length(ego.names)

## get the edge count for each, so we can work from smallest to largest
recon_egoedges.count  <- unlist(lapply(recon_egoedges.list, ecount))
recon_egoedges.order  <- order(recon_egoedges.count)

##------------------------------------------------------------------
## loop over each egonet and compute the similarity matrices for
## both the full feature vector and the reduced size feature vector
##------------------------------------------------------------------
for (i in 110:1) {
    
    ##------------------------------------------------------------------
    ## set-up
    ##------------------------------------------------------------------
    tmp.id          <- ego.names[recon_egoedges.order[i]]
    tmp.edges       <- recon_egoedges.list[[tmp.id]]
    
    ##------------------------------------------------------------------
    ## echo progress
    ##------------------------------------------------------------------
    cat("Iteration", i, "of", ego.num, " :: Profile Similarity Matrix for", tmp.id, " :: # Edges =", ecount(tmp.edges), "\n")
    
    ##------------------------------------------------------------------
    ## load the profile matrix
    ##------------------------------------------------------------------
    load(paste0(getwd(),"/profiles/",paste0(tmp.id,".ProfileMatrix.Rdata")))
    
    ##------------------------------------------------------------------
    ## deifne output filenames
    ##------------------------------------------------------------------
    tmp.fullFeatureset      <- paste( paste(getwd(),"recon.sim.prof.full",sep="/"), paste0(tmp.id, ".FullProfileSimilarityMatrix.Rdata"), sep="/")
    tmp.reducedFeatureset   <- paste( paste(getwd(),"recon.sim.prof.reduced",sep="/"), paste0(tmp.id, ".ReducedProfileSimilarityMatrix.Rdata"), sep="/")
    
    ##------------------------------------------------------------------
    ## compute the matrices & immediately write to file
    ##------------------------------------------------------------------
    
    ## full profile
    tmp.fullProfileSim      <- calcProfileSimilarityMatrix(tmp.edges, tmp.leafMatrix)
    save(tmp.fullProfileSim,    file=tmp.fullFeatureset)
    
    ## reduced profile
    tmp.reducedProfileSim   <- calcProfileSimilarityMatrix(tmp.edges, tmp.leafMatrixExDropsRows)
    save(tmp.reducedProfileSim, file=tmp.reducedFeatureset)
}








