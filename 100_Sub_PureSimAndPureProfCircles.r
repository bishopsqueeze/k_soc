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
## Directories holding the topological & profile similarity data
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

##------------------------------------------------------------------
## Set-up
##------------------------------------------------------------------

## extract egonet data
ego.names       <- names(egoedges.list)
ego.known       <- names(known_circles.list)
ego.num         <- length(ego.names)

## get the edge count for each, so we can work from smallest to largest
egoedges.count  <- unlist(lapply(egoedges.list, ecount))
ego.order       <- order(egoedges.count)

## define the ouput directory for individual edges files
output.dir  <- paste(getwd(),"sweep",sep="/")

## placeholder for output
benchmark.df    <- data.frame()
sweep.res       <- list()

##------------------------------------------------------------------
## for the sweep tests, loop over known circles only
##------------------------------------------------------------------
for (i in 1:20) { #train.num
    
    ## set-up
    tmp.id          <- ego.names[ego.order[i]]
    tmp.edges       <- egoedges.list[[tmp.id]]
    tmp.known       <- known_circles.list[[tmp.id]]
    
    ## the nodes in the original egonet -- will need to make sure you only have these in circles
    tmp.nodes       <- as.integer(gsub("ID_","",names(egonets.list[[tmp.id]])))
    
    ## echo progress
    cat("Iteration", i, "of", ego.num, " :: Cluster Construction for", tmp.id, " :: # Edges =", ecount(tmp.edges), "\n")
   
    ## load similarity matrix (called tmp.sim)
    tmp.topofile     <- paste(simDirectory, "/", paste0(tmp.id, ".SimilarityMatrix.Rdata"), sep="")
    load(tmp.topofile)
    
    ## load profile matrix (called tmp.profSim)
    tmp.proffile     <- paste(simDirectory, "/", paste0(tmp.id, ".ProfileSimilarityMatrix.Rdata"), sep="")
    load(tmp.proffile)

    ##------------------------------------------------------------------
    ## can loop over a weighting of the
    ## can loop over the heights/cutoffs in hclust
    ##------------------------------------------------------------------
    
    tmp.summary <- list()
    w.seq   <- seq(0,1)

    ## for now, lay out all of the steps so you know what is happening
    for (j in 1:length(w.seq)) {
        
        ## weight the matrices
        p               <- w.seq[j]
        sim.combined    <- (p)*(tmp.sim) + (1-p)*(tmp.profSim)
        
        ## calc dissimilarity, generate a cluster, calc parition density
        tmp.dist        <- as.dist(1 - sim.combined)
        tmp.hclust      <- hclust(tmp.dist, method="single")
        tmp.pdens       <- calcPartitionDensity(tmp.hclust, tmp.edges)
        
        ## extract clusters; purge clusters of non-vertices from the egonet
        tmp.clust       <- extractHclustClusters(tmp.hclust, tmp.pdens$hmax, tmp.edges)
        beg.clustnodes  <- tmp.clust$cl.nodes
        end.clustnodes  <- lapply(beg.clustnodes, intersect, tmp.nodes)
        end.tot         <- sum(unlist(lapply(end.clustnodes, length)))
        end.uniq        <- sum(unique(unlist(lapply(end.clustnodes, length))))
        
        ## gather statistics
        tmp.summary[[j]]  <- data.frame(id=tmp.id, p=p,
                                        pred.cl=tmp.clust$cl.count,
                                        pred.uniq=end.uniq,
                                        pred.tot=end.tot)
        
        ## save clusters
        p.name  <- paste0("p",p)
        sweep.res[[tmp.id]][[p.name]]  <- end.clustnodes
    }
    ## accumulate results
    benchmark.df    <- do.call("rbind", tmp.summary)
    
    sweep.res[[tmp.id]]$benchmark   <- benchmark.df
}

