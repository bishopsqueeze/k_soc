##------------------------------------------------------------------
## The purpose of this script is to populate the full union of
## "profile feature vectors" for an egonet into a single matrix.
## The matrix will contain the unique feature vector for each
## member contained in the egonet as well as the ego itself.
##
## Note:
##
## On June 7, 2014, I noticed that the full feature vector has some
## potentially redundance information.  For example, there is both
## "education;school;name" and "education;school;id"
##
## There may not be a true one-to-one correpsondence between the set
## of "ids" and the set of "names", but we know for a fact that each
## user "id" will be unique ... which means that the profile
## similarity matrix could never be identical across users.  So create
## a couple of subsets of the original data.
##
## Subset #1:  Expunge all "id" rows from the matrix
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
## extract egonet data
##------------------------------------------------------------------
ego.names       <- sort(names(egoedges.list))
ego.num         <- length(ego.names)

##------------------------------------------------------------------
## get the edge count for each, so we can work from smallest to largest
##------------------------------------------------------------------
egoedges.count  <- unlist(lapply(egoedges.list, ecount))
egoedges.order  <- order(egoedges.count)

##------------------------------------------------------------------
## define the ouput directory for individual edges files
##------------------------------------------------------------------
output.dir  <- paste(getwd(),"profiles",sep="/")

##------------------------------------------------------------------
## loop over all the egonets and load the profile matrices
##------------------------------------------------------------------
for (i in 1:ego.num) {
    
    ##------------------------------------------------------------------
    ## set-up
    ##------------------------------------------------------------------
    tmp.id          <- ego.names[egoedges.order[i]]
    tmp.edges       <- egoedges.list[[tmp.id]]
    
    ##------------------------------------------------------------------
    ## echo progress
    ##------------------------------------------------------------------
    cat("Iteration", i, "of", ego.num, " :: Create Feature Profile Matrix for", tmp.id, " :: # Edges =", ecount(tmp.edges), "\n")
    
    ##------------------------------------------------------------------
    ## output files
    ##------------------------------------------------------------------
    tmp.profName   <- paste(output.dir, paste0(tmp.id,".ProfileMatrix.Rdata"), sep="/")
    
    ##------------------------------------------------------------------
    ## define the superset of granualr features within this
    ##------------------------------------------------------------------
    tmp.leaves <- unionEgonetLeaves(tmp.id, tmp.edges, leaves.list)
    
    ##------------------------------------------------------------------
    ## load the full (... but sparse) leaf matrix
    ##------------------------------------------------------------------
    tmp.leafMatrix <- loadLeafMatrix(tmp.id, tmp.edges, leaves.list, tmp.leaves)

    ##------------------------------------------------------------------
    ## load the leaf matrix with improbable match data expunged
    ##------------------------------------------------------------------
    drop.rows   <- c(   ## remove id codes b/c these are likely redundanct
                        grep("id", rownames(tmp.leafMatrix)),
                        ## remove first names
                        grep("^first_name", rownames(tmp.leafMatrix)),
                        ## remove "name" (equivalent to a unique id)
                        grep("^name", rownames(tmp.leafMatrix)),
                        ## remove "birthday" for now
                        grep("^birthday", rownames(tmp.leafMatrix))
                    )
    tmp.leafMatrixExDropsRows <- tmp.leafMatrix[ -drop.rows, ]
    
    ##------------------------------------------------------------------
    ## save the results
    ##------------------------------------------------------------------
    save(tmp.leafMatrix, tmp.leaves, tmp.leafMatrixExDropsRows, file=tmp.profName)
    
    ##------------------------------------------------------------------
    ## echo results
    ##------------------------------------------------------------------
    cat("Full Matrix Dimensions = ", dim(tmp.leafMatrix),"\n")
    cat("Reduced Matrix Dimensions = ", dim(tmp.leafMatrixExDropsRows),"\n\n")
    
}
