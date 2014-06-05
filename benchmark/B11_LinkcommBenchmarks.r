##------------------------------------------------------------------
## Use the similarity matrices to compare my results against
## similar results computed using the linkcomm() package.
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
my.clust <- list()
lc.clust <- list()

if (DO_CHECKS) {
    
    ## extract egonet data
    ego.names       <- sort(names(egoedges.list))
    ego.num         <- length(ego.names)
    
    ## get the edge count for each, so we can work from smallest to largest
    egoedges.count  <- unlist(lapply(egoedges.list, ecount))
    egoedges.order  <- order(egoedges.count)
    
    ## define the ouput directory for individual edges files
    output.dir  <- paste(getwd(),"sim_xcheck",sep="/")
    
    ## define a data.frame to hold benchmark statistics
    benchmark.df    <- data.frame()
    
    ## loop over each egonet and compute the similarity matrices
    for (i in 1:ego.num) {
    #for (i in 1:20) {
        
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
        tmp.pdens       <- calcPartitionDensity(tmp.hclust, tmp.edges)
        
        ## extract clusters
        tmp.clust       <- extractHclustClusters(tmp.hclust, tmp.pdens$hmax, tmp.edges)
        
        my.clust[[tmp.id]]$memb <- tmp.clust$cl.nodes
        my.clust[[tmp.id]]$pden <- tmp.pdens
        
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
        cat("Sum of partition density differences = ", sum(my.d-lc.d), "\n")
        
        ## Compare cluster membership (assume lincomm() == TRUE)
        lc.clust_memb <- lapply(tmp.lc$clusters, function(x) {
                as.integer(union(tmp.lc$edgelist[x, 1], tmp.lc$edgelist[x, 2]))
            })
        num.edits   <- circleEdits(lc.clust_memb, tmp.clust$cl.nodes)
        cat("Number of Circle Edits = ", num.edits, "\n\n")
        
        lc.clust[[tmp.id]]$memb <- lc.clust_memb
        lc.clust[[tmp.id]]$pden <- tmp.lc$pdens
        
        ## load the benchmark dataframe
        benchmark.df    <- rbind(benchmark.df,
                            data.frame(id=tmp.id, ne=ecount(tmp.edges), nv=vcount(tmp.edges),
                                       dh=sum(my.h-lc.h), dd=sum(my.d-lc.d), num.edits=num.edits))
    }

}

##------------------------------------------------------------------
## Only run through ~ 104 of 110 b/c I don't know if I'm the bottleneck
## (probably) or if the linkcomm program that is writing to disk is slow.
##------------------------------------------------------------------


##------------------------------------------------------------------
## Save results to disk
##------------------------------------------------------------------
##save(benchmark.df, file="B11_LinkcommBenchmarks.Rdata")


##------------------------------------------------------------------
## double check the results for the test case (i==40)
##------------------------------------------------------------------

#lc.clust[[tmp.id]]$memb
#my.clust[[tmp.id]]$memb
#> myIgraph <- tmp.edges
#> myHclust <- tmp.hclust

#total of 17; two with 3 members
#272 299 308  # last 3 in my list
#244 257 328 # 5th in my list







