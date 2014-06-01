##------------------------------------------------------------------
## Load the profile vectors for each of the IDs in the dataset
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
## Source the utility functions
##------------------------------------------------------------------
source("/Users/alexstephens/Development/kaggle/social_circle/k_soc/00_Utilities.r")

##------------------------------------------------------------------
## Read in the raw data files
##------------------------------------------------------------------
load("01_SocialCircle_RawData.Rdata")
load("02_SocialCircle_Edges.Rdata")


##------------------------------------------------------------------
## Loop over the egonets, define sparse profile matrices, save to file
##------------------------------------------------------------------

## extract egonet data
ego.names       <- sort(names(egoedges.list))
ego.num         <- length(ego.names)

## get the edge count for each, so we can work from smallest to largest
egoedges.count  <- unlist(lapply(egoedges.list, ecount))
egoedges.order  <- order(egoedges.count)

## define the ouput directory for individual edges files
output.dir  <- paste(getwd(),"prof",sep="/")

## loop over all the egonets and load the profile matrices
for (i in 1:ego.num) {
    
    ## set-up
    tmp.id          <- ego.names[egoedges.order[i]]
    tmp.edges       <- egoedges.list[[tmp.id]]
    
    ## echo progress
    cat("Iteration", i, "of", ego.num, " :: Connected Edge Matrix for", tmp.id, " :: # Edges =", ecount(tmp.edges), "\n")
    
    ## output files
    tmp.profName   <- paste(output.dir, paste0(tmp.id,".ProfileMatrix.Rdata"), sep="/")
    
    ## define the superset of granualr features within this
    tmp.leaves <- unionEgonetLeaves(tmp.id, tmp.edges, leaves.list)
    
    ## load the (sparse) leaf matrix
    tmp.leafMatrix <- loadLeafMatrix(tmp.id, tmp.edges, leaves.list, tmp.leaves)
    
    ## save the results
    save(tmp.leafMatrix, tmp.leaves, file=tmp.profName)
    
    ## echo results
    cat("Matrix Dimensions = ", dim(tmp.leafMatrix),"\n\n")
}
