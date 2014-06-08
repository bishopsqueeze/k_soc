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
## Direcotry holding the topological & profile similarity data
##------------------------------------------------------------------
profDirectory   <- "/Users/alexstephens/Development/kaggle/social_circle/data/inputs/profiles"


##------------------------------------------------------------------
## Loop over the egonets, define graph objects, save to file
##------------------------------------------------------------------

## extract egonet data
ego.names       <- sort(names(egoedges.list))
ego.num         <- length(ego.names)

## get the edge count for each, so we can work from smallest to largest
egoedges.count  <- unlist(lapply(egoedges.list, ecount))
egoedges.order  <- order(egoedges.count)

## placeholder matrix for the number of reconstructed links
reconLinkCount.mat              <- matrix(0,nrow=ego.num, ncol=2)
colnames(reconLinkCount.mat)    <- c("n.edges", "n.recon")
rownames(reconLinkCount.mat)    <- ego.names[egoedges.order]

## placeholder for the reconstructed edge list
recon_egoedges.list <- list()

##------------------------------------------------------------------
## loop over each egonet and "reconstruct" the network using the
## method outlined in a paper by Wang & Gao (2013)
##------------------------------------------------------------------
for (k in 1:ego.num) {
    
    ## set-up
    tmp.id          <- ego.names[egoedges.order[k]]
    tmp.edgelist    <- egoedges.list[[tmp.id]]
    
    ## extract original adjacency matrix, vertices & edges
    orig.adj        <- get.adjacency(tmp.edgelist, sparse=FALSE)
    orig.verts      <- V(tmp.edgelist)
    orig.edges      <- get.data.frame(tmp.edgelist, what="edges")
    
    ## create a list of neighbors for each vertex in the graph
    neighbors.list  <- list()
    for (i in 1:length(orig.verts)) {
        vert.id                   <- V(tmp.edgelist)[i]$name
        neighbors.list[[vert.id]] <- c(V(tmp.edgelist)[i]$name, V(tmp.edgelist)[nei(i)]$name)
    }
    
    ##------------------------------------------------------------------
    ## load the profile data matrix (called tmp.leafMatrix)
    ##------------------------------------------------------------------
    load(paste(profDirectory, "/", paste0(tmp.id, ".ProfileMatrix.Rdata"), sep=""))

    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Use the Wang & Gao procedure to "reconstruct" the ego network
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    ##------------------------------------------------------------------
    ## Step 1: Compute the topological- and profile-similarity of each edge
    ##------------------------------------------------------------------
    edgeSimilarity.list <- foreach (i=1:nrow(orig.edges)) %dopar% {
        
        ##------------------------------------------------------------------
        ## compute topological similarity
        ##------------------------------------------------------------------
        ei          <- orig.edges[i,1]
        ej          <- orig.edges[i,2]
        np_i        <- neighbors.list[[ei]]
        np_j        <- neighbors.list[[ej]]
        topo.sim    <- length(intersect(np_i, np_j)) / length(union(np_i, np_j))
        
        ##------------------------------------------------------------------
        ## compute profile similarity
        ##  - use the "reduced" profile (really "more informative") matrix
        ##------------------------------------------------------------------
        prof.sim  <- calcCosineSimilarity( tmp.leafMatrixExDropsRows[,paste0("ID_",ei)], tmp.leafMatrixExDropsRows[,paste0("ID_",ej)])   ## reduced matrix
        
        ##------------------------------------------------------------------
        ## load the edge similarities into a matrix
        ##------------------------------------------------------------------
        return(c(topo.sim, prof.sim))
    }
    
    ## transform foreach results to a matrix
    edgeSimilarity.mat <- do.call("rbind", edgeSimilarity.list)
    colnames(edgeSimilarity.mat)    <- c("topo", "prof")
    
    ## calculate median values
    topo.median <- median(edgeSimilarity.mat[,c("topo")])
    prof.median <- median(edgeSimilarity.mat[,c("prof")])
    
    
    ##------------------------------------------------------------------
    ## Step 2: Loop over the vertices and identify new connections
    ##------------------------------------------------------------------
    
    ## copy the original adjacency matrix
    recon.adj   <- orig.adj
    
    ## loop over all the vertices
    for (i in 1:length(orig.verts)) {
        for (j in 1:length(orig.verts)) {
         
            vi          <- orig.verts$name[i]
            vj          <- orig.verts$name[j]
            np_i        <- neighbors.list[[vi]]
            np_j        <- neighbors.list[[vj]]
            
            topo.sim    <- length(intersect(np_i, np_j)) / length(union(np_i, np_j))
            prof.sim    <- calcCosineSimilarity( tmp.leafMatrixExDropsRows[,paste0("ID_",vi)], tmp.leafMatrixExDropsRows[,paste0("ID_",vj)])
            
            if ((i != j) & (topo.sim > topo.median) & (prof.sim > prof.median)) {
                recon.adj[i,j] <- 1
            }
        }
    }
    
    
    ##------------------------------------------------------------------
    ## Step 3: diagnositcs
    ##------------------------------------------------------------------
    recon.diff  <- 0.5*sum(recon.adj - orig.adj)    ## divide by 2 b/c of symmetry
    cat("Id=", tmp.id, "Recon Difference=", recon.diff, "\n")
    
    reconLinkCount.mat[k, ] <- c(nrow(orig.edges), recon.diff)
   
   ##------------------------------------------------------------------
   ## Step 4: Transform adjacency matrix to an iGraph object and store
   ##------------------------------------------------------------------
   tmp.iGraph                     <-  graph.adjacency(recon.adj, mode=c("undirected"))
   recon_egoedges.list[[tmp.id]]  <- tmp.iGraph
   
}

##------------------------------------------------------------------
## Save the results
##------------------------------------------------------------------
save(edgeSimilarity.mat, reconLinkCount.mat, recon_egoedges.list, file="06_SocialCircle_ReconEdges.Rdata")





