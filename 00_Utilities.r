##------------------------------------------------------------------
## Utility functions for the kaggle "social circle" competition:
##------------------------------------------------------------------
## Reference:
## http://www.kaggle.com/c/learning-social-circles
##------------------------------------------------------------------

##------------------------------------------------------------------
## <function> :: trim
##------------------------------------------------------------------
## Remove leading/trailing whitespace from a character string
##------------------------------------------------------------------
trim <- function (x)
{
    return(gsub("^\\s+|\\s+$", "", x))
}

##------------------------------------------------------------------
## <function> :: calcCosineSimilarity
##------------------------------------------------------------------
## Cosine similarity between two vectors
##------------------------------------------------------------------
calcCosineSimilarity  <- function(aVec, bVec)
{
    return((aVec %*% bVec) / (sqrt(aVec %*% aVec) * sqrt(bVec %*% bVec)))
}


##------------------------------------------------------------------
## <function> :: convert.magic
##------------------------------------------------------------------
## A function to perform a type switch on a data.frame
##------------------------------------------------------------------
convert.magic   <- function(obj, col, type)
{
    ## isolate the columns to convert
    idx <- which(colnames(obj) %in% col)
    
    ## loop over the columns and convert via a swtich()
    for (i in 1:length(idx)) {
        FUN <- switch(type[i], character = as.character, numeric = as.numeric, factor = as.factor, integer = as.integer)
        obj[, idx[i]]   <- FUN(obj[, idx[i]])
    }
    return(obj)
}


##------------------------------------------------------------------
## <function> :: dedupEdgeList
##------------------------------------------------------------------
## A function to remove duplicates from an iGraph object
##------------------------------------------------------------------
dedupEdgeList <- function(myIgraph)
{
    ## translate igraph object into a data frame
    edge.df     <- get.data.frame(myIgraph)
    
    ## collapse edge members and return the unique set
    edges.all   <- paste(edge.df$from, edge.df$to, sep="_")
    edges.uniq  <- unique(edges.all)
    
    ## translate back into an igraph object
    return(graph.data.frame(as.data.frame(do.call("rbind",(strsplit(edges.uniq,"_")))), directed=FALSE))
}


##------------------------------------------------------------------
## <function> :: textToCircleList
##------------------------------------------------------------------
## Take a line in the format of a set of social circles tied to a user
##
##  <userid>, <circle1>; <circle2>; ... ; <circleN>
##
## Where a circle (e.g., <circleN>) will contain 1 ... m integers
## representing the members of that circle.
##------------------------------------------------------------------
## Test Cases:
## example1 <- c("0, 3 1 2; 2 3; 5 4 6")
## example2 <- c("0, 4 5; 1 2 3 4")
##------------------------------------------------------------------
textToCircleList <- function(ch)
{
    if (!is.character(ch)) {
        stop("expecting a character string.")
    }
    
    ## define a placeholder list for the output
    tmp.list    <- list()
    
    ## split the userid from the circles
    tmp.split   <- strsplit(ch, ",")[[1]]
    tmp.id      <- paste("ID_", trim(tmp.split)[1], sep="")
    tmp.circles <- trim(strsplit(tmp.split[2],";")[[1]])
    
    ## load and name the circles into a list
    tmp.list    <- lapply(tmp.circles,
                            function(x)
                            {
                                as.integer( unlist(strsplit(x," ")[[1]]) )
                            })
    names(tmp.list) <- paste("circle", 1:length(tmp.list),sep="")
    
    ## return a list
    return(list(ego=tmp.id, res=tmp.list))
}


##------------------------------------------------------------------
## <function> :: circleEdits
##------------------------------------------------------------------
## Implement the kaggle "social circles" scoring metric
##------------------------------------------------------------------
## Inputs:
##
##  trueCircles: a list of known circles associated with an ego
##  predCircles: a list of predicted circles associated with an ego
##------------------------------------------------------------------
## Outputs:
##
##  The minimum number of edits to force the predicted circles
##  to match the known circles
##------------------------------------------------------------------
## Test Cases:
## s <- list(s1=c(3,1,2), s2=c(2,3), s3=c(5,4,6))
## t <- list(t1=c(4,5), t2=c(1,2,3,4))
##------------------------------------------------------------------
## test cases for a given set of circles
## t    <- list(t1=c(4,5), t2=c(1,2,3,4), t3=c(2,3))            ## truth
## p1   <- list(p1=c(1,2,3,4,5))                                ## all in one circle (5 edits)
## p2   <- list(p1=c(1), p2=c(2), p3=c(3), p4=c(4), p5=c(5))    ## each in a circle (7 edits)
##------------------------------------------------------------------
circleEdits  <- function(trueCircles, predCircles)
{
    ## error check
    if (!is.list(trueCircles) || any(unlist(trueCircles) < 0)) {
        stop("trueCircles must be a list with nonnegative entries.")
    }
    if (!is.list(predCircles) || any(unlist(predCircles )< 0)) {
        stop("predCircles must be a list with nonnegative entries.")
    }
    
    ## check that the package for solve_LSAP is loaded
    if (length(grep("package:clue", search())) == 0) {
        require("clue")
    }
    
    ## get largest number of circles
    psize   <- max(length(trueCircles), length(predCircles))

    ## define a matching matrix
    mm      <- matrix(0, nrow=psize, ncol=psize)

    ## loop over the matrix indices an load the number of edits
    for (i in 1:psize) {
        for (j in 1:psize) {
            
            tmp.true  <- NULL
            tmp.pred  <- NULL
            
            ## grab a predicted and true circle
            if (i <= length(predCircles)) {
                tmp.pred  <- unlist(predCircles[i])
            }
            if (j <= length(trueCircles)) {
                tmp.true  <- unlist(trueCircles[j])
            }
            
            ## compute number of edits needed bewteen circles and load into matching matrix
            mm[i,j]  <- length(union(tmp.pred,tmp.true)) - length(intersect(tmp.pred,tmp.true))
        }
    }

    ## use Hungarian method to identify minimum edit indices
    mm.idx  <- cbind(1:psize, solve_LSAP(mm))

    ## compute the total cost and return
    return(sum(mm[mm.idx]))
}


##------------------------------------------------------------------
## <function> :: convEgonetListToIgraphObject
##------------------------------------------------------------------
## Take an egonet stored in list format, generally:
##
##  egonet.list[[ i ]]
##      $ID_1 = {"ID_2", "ID_3", "ID_7"}
##      $ID_10 = {"ID_12", "ID_4", ... , "ID_999"}
##      ...
##
## And translate it into an iGraph object
##------------------------------------------------------------------
convEgonetListToIgraphObject   <- function(myEgonet)
{
    nodes.num   <- length(myEgonet)
    edges.df    <- data.frame()
    
    ## for each node in the list, strip the ID tag and load edges
    for (i in 1:nodes.num) {
        node        <- as.integer(gsub("ID_","",names(myEgonet)[i]))
        edges       <- as.integer(gsub("ID_","",myEgonet[[i]]))
        
        ## check to see if there is more than one edge in the list,
        ## because if there is only one edge, we need to check
        ## wether or not that edge is a "NULL"
        if (length(edges) > 1) {
            edges.df <- rbind(edges.df, data.frame(E1=rep(node, length(edges)), E2=edges))
        } else {
            ## here we (indirectly) ignore vertices with NULL edges
            if ( !(edges == -99999) ) {
                edges.df <- rbind(edges.df, data.frame(E1=rep(node, length(edges)), E2=edges))
            }
        }
    }
    return(graph.data.frame(edges.df, directed=FALSE))
}


##------------------------------------------------------------------
## <function> :: calcSimilarityMatrix
##------------------------------------------------------------------
## For an undirected iGprah object, compute the Jaccard similarity
## measure between all cominations of edges in the object.
##------------------------------------------------------------------
## This function is slow.  Even after some attempts to remove wasted
## computations.  Can't match the speed of an external C call.
##------------------------------------------------------------------
calcSimilarityMatrix <- function(myIgraph)
{
    
    ## load the edges and define an output matrix
    edges       <- get.data.frame(myIgraph, what="edges")
    edges.num   <- ecount(myIgraph)
    
    ## create a list of neighbors for each vertex in the graph
    verts       <- V(myIgraph)$name
    np.list     <- list()
    for (i in 1:length(verts)) {
        np.list[[verts[i]]] <- c(V(myIgraph)[i]$name, V(myIgraph)[nei(i)]$name)
    }
    
    ## create a list of edges
    edges.list  <- sapply(1:edges.num, function(x){list(as.character(edges[x,]))})
    
    ## define the output matrix
    sim.mat             <- matrix(0,nrow=edges.num, ncol=edges.num)
    rownames(sim.mat)   <- paste0("edge",1:edges.num)
    colnames(sim.mat)   <- paste0("edge",1:edges.num)
  
    ##-------------------------------------------------------------
    ## populate the lower triangular matrix with similarity coefficients
    ##-------------------------------------------------------------
    cat("calcSimilarityMatrix::non-parallel\n")
    for (i in 1:(edges.num-1)) {
        
        ## try to vectorize as much as possible
        jvec    <- (i+1):edges.num
        ej      <- edges[jvec,]
        ei      <- unlist(edges.list[i])
        
        ## boolean to locate any pairwise match amongst edges
        jidx <- (ei[1] == ej[,1]) | (ei[2] == ej[,1]) | (ei[1] == ej[,2]) | (ei[2] == ej[,2])
        
        ## if there's a match (i.e., jidx > 0), then loop over the
        ## subset of rows that match & calc the Jaccard coefficient
        if (sum(jidx) > 0) {
            tidx <- jvec[jidx]
            sim.mat[jvec[jidx],i] <- sapply(tidx,
                function(x) {
                    np_i  <- np.list[[ setdiff(ei, intersect(ei, as.character(edges[x,])   )) ]]
                    np_j  <- np.list[[ setdiff(as.character(edges[x,]), intersect(ei, as.character(edges[x,]))) ]]
                    return( length(intersect(np_i, np_j)) / length(union(np_i, np_j)) )
                })
        }
        
        ## report progress
        if ( (i %% 1000) == 0 ) { cat("Iteration",i,"of",edges.num,"\n") }
    }
    
    ## return the *similarity* matrix
    return(sim.mat)
}


##------------------------------------------------------------------
## <function> :: calcProfileSimilarityMatrix
##------------------------------------------------------------------
## should be similar to calcSimilarity matrrix, but the initial
## difference would be that if jidx > 0, then you'd need to
## compute the cosine of the two feature vectors istead of the
## Jaccard similarity matrix
##
## Cosine similarity
## http://www.gettingcirrius.com/2010/12/calculating-similarity-part-1-cosine.html
## cosine(A,B) = A %*% B / (SQRT(A %*% A) * SQRT( B %*% B))
##------------------------------------------------------------------
calcProfileSimilarityMatrix <- function(myIgraph, myLeafMatrix)
{
    
    ## load the edges and define an output matrix
    edges       <- get.data.frame(myIgraph, what="edges")
    edges.num   <- ecount(myIgraph)
    
    ## create a list of neighbors for each vertex in the graph
    verts       <- V(myIgraph)$name
    np.list     <- list()
    for (i in 1:length(verts)) {
        np.list[[verts[i]]] <- c(V(myIgraph)[i]$name, V(myIgraph)[nei(i)]$name)
    }
    
    ## create a list of edges
    edges.list  <- sapply(1:edges.num, function(x){list(as.character(edges[x,]))})
    
    ## define the output matrix
    prof.mat             <- matrix(0,nrow=edges.num, ncol=edges.num)
    rownames(prof.mat)   <- paste0("edge",1:edges.num)
    colnames(prof.mat)   <- paste0("edge",1:edges.num)
    
    ##-------------------------------------------------------------
    ## populate the lower triangular matrix with similarity coefficients
    ##-------------------------------------------------------------
    cat("calcSimilarityMatrix::non-parallel\n")
    for (i in 1:(edges.num-1)) {
        
        ## try to vectorize as much as possible
        jvec    <- (i+1):edges.num
        ej      <- edges[jvec,]
        ei      <- unlist(edges.list[i])
        
        ## boolean to locate any pairwise match amongst edges
        jidx <- (ei[1] == ej[,1]) | (ei[2] == ej[,1]) | (ei[1] == ej[,2]) | (ei[2] == ej[,2])
        
        ## if there's a match (i.e., jidx > 0), then loop over the
        ## subset of rows that match & calc the Jaccard coefficient
        if (sum(jidx) > 0) {
            tidx <- jvec[jidx]
            
            prof.mat[jvec[jidx],i] <- sapply(tidx,
            function(x) {
                ij_id  <- paste0("ID_",setdiff(union(ei, as.character(edges[x,])),intersect(ei, as.character(edges[x,]))))
                 return( calcCosineSimilarity( myLeafMatrix[, ij_id[1]], myLeafMatrix[, ij_id[2]] ) )
            })
        }
        
        ## report progress
        if ( (i %% 1000) == 0 ) { cat("Iteration",i,"of",edges.num,"\n") }
    }
    
    ## return the *similarity* matrix
    return(prof.mat)
}





##------------------------------------------------------------------
## <function> :: calcConnectedEdgeMatrix
##------------------------------------------------------------------
## For an undirected iGprah object, compute a matrix indicating
## whether or not a pair of edges has a connecting node.  The idea
## was to use this matrix as a lookup table to speed calculations
## that depend on finding a match between two edges.
##------------------------------------------------------------------
calcConnectedEdgeMatrix <- function(myIgraph)
{
    ## load the edges
    edges       <- get.data.frame(myIgraph, what="edges")
    edges.num   <- ecount(myIgraph)
    
    ## create a list of edges
    edges.list  <- sapply(1:edges.num, function(x){ list(as.character(edges[x,])) })
    
    ## define the output matrix
    connectedEdge.mat             <- matrix(0,nrow=edges.num, ncol=edges.num)
    rownames(connectedEdge.mat)   <- paste0("edge",1:edges.num)
    colnames(connectedEdge.mat)   <- paste0("edge",1:edges.num)
    
    cat("calcConnectedEdgeMatrix::non-parallel\n")
    for (i in 1:(edges.num-1)) {
        
        ## try to vectorize as much as possible
        jvec    <- (i+1):edges.num
        ej      <- edges[jvec,]
        ei      <- unlist(edges.list[i])
        
        ## boolean to locate any pairwise match amongst edges
        jidx <- 1*((ei[1] == ej[ ,1]) | (ei[2] == ej[ ,1]) | (ei[1] == ej[ ,2]) | (ei[2] == ej[ ,2]))
        
        ## matrix indicating where edges connect
        connectedEdge.mat[jvec,i] <- jidx
        
        ## report progress
        if ( (i %% 1000) == 0 ) { cat("Iteration",i,"of",edges.num,"\n") }
    }
    ## return the matrix
    return(connectedEdge.mat)
}


##------------------------------------------------------------------
## <function> :: calcPartitionDensity
##------------------------------------------------------------------
## For a given hclust object and an iGraphs set of edges, compute
## the partition density for that "community of links"
## based on the cluster
##------------------------------------------------------------------
calcPartitionDensity    <- function(myHclust, myIgraph)
{
    h.vec       <- unique(myHclust$height)
    h.num       <- length(h.vec)
    e.num       <- ecount(myIgraph)
    e.idx       <- 1:e.num
    
    ## vector to hold the identify of group members at each height
    clust.vec   <- vector(, length=h.num)
    dens.vec    <- vector(, length=h.num)
    
    ## loop each height and compute the partition density
    for (i in 1:h.num) {
        
        clust.vec     <- as.vector(cutree(myHclust, h=h.vec[i]))
        groups.uniq   <- unique(clust.vec)
        groups.num    <- length(groups.uniq)
        tmp.dens      <- 0
        
        ## loop over all groups at a height and compute the density
        for (j in 1:groups.num) {
            
            ## identify location of the group memebers
            tmp.idx     <- which(clust.vec == groups.uniq[j])
            tmp.memb    <- union(get.edgelist(myIgraph)[tmp.idx,1], get.edgelist(myIgraph)[tmp.idx,2])
            
            ## compute the number of nodes and edges in the group
            tmp.mc      <- length(tmp.idx)
            tmp.nc      <- length(tmp.memb)
            
            ## by convention, clusers with two nodes has a density of 0
            if (tmp.nc > 2) {
                tmp.dens    <- tmp.dens + 2*tmp.mc*(tmp.mc - (tmp.nc-1))/(e.num*(tmp.nc-2)*(tmp.nc-1))
            }
        }
        dens.vec[i] <- tmp.dens
        
    }
    
    ## idenitfy the maximum density & corresponding height
    pdens   <- data.frame(h=h.vec, den=dens.vec)
    pdmax   <- max(dens.vec)
    hmax    <- min(h.vec[dens.vec == pdmax])    ## extra max wrapper when there are pdens ties
    
    return(list(pdens=pdens, pdmax=pdmax, hmax=hmax))
}


##------------------------------------------------------------------
## <function> :: extractHclustClusters
##------------------------------------------------------------------
## Isolate cluster members from a tree clipped at a given height
##------------------------------------------------------------------
extractHclustClusters <- function(myHclust, myHmax, myIgraph)
{
    
    clust.ids   <- cutree(myHclust, h=myHmax)
    clust.uniq  <- unique(clust.ids)
    clust.num   <- length(clust.uniq)

    e.num       <- ecount(myIgraph)
    e.idx       <- 1:e.num

    clust.edges <- list()
    clust.nodes <- list()
    
    for (i in 1:clust.num) {
       
       tmp.clust   <- clust.uniq[i]
       tmp.idx     <- e.idx[(clust.ids == tmp.clust)]
       tmp.memb    <- union(get.edgelist(myIgraph)[tmp.idx,1], get.edgelist(myIgraph)[tmp.idx,2])

       clust.edges[[i]] <- tmp.idx
       clust.nodes[[i]] <- as.integer(tmp.memb)
    }

    nodes.num <- unlist(lapply(clust.nodes, length))
    final.clust.edges    <- clust.edges[which(nodes.num > 2)]
    final.clust.nodes    <- clust.nodes[which(nodes.num > 2)]
   
   return(list(cl.edges=final.clust.edges, cl.nodes=final.clust.nodes, cl.count=length(final.clust.nodes)))
}



##------------------------------------------------------------------
## <function> :: unionEgonetLeaves
##------------------------------------------------------------------
## Given an egonet tied to a user, create the union of all possible
## profile leaves associated with all vertices in that egonet
##------------------------------------------------------------------
unionEgonetLeaves   <- function(myId, myIgraph, myLeaves)
{
    ## get ids for the user and all of the vertices in the egonet
    vertices.ids    <- c(myId, paste0("ID_", V(myIgraph)$name))
    
    ## return the sorted list of all possible leaves
    return( sort(unique(unlist(sapply(vertices.ids, function(x){myLeaves[[x]]} )))) )
}


##------------------------------------------------------------------
## <function> :: loadLeafMatrix
##------------------------------------------------------------------
## Create a matrix that holds a binary vector for each set of
## profile leaves in the union of all leaf profiles tied to an
## egonet.  The columns include all vertices in the egonet as
## well as the profile for the ego.
##------------------------------------------------------------------
loadLeafMatrix   <- function(myId, myIgraph, myLeaves, myUnion)
{
    vertices.ids    <- c(myId, paste0("ID_", V(myIgraph)$name))
    vertices.num    <- length(vertices.ids)
    union.num       <- length(myUnion)
    
    ## define a placeholder matrix;
    ##  nrow    = # leaves in the union
    ##  ncol    = # vertices
    leaf.mat        <- matrix(0,nrow=union.num, ncol=vertices.num)
    
    ## loop over all vertices and find matches
    for (i in 1:vertices.num) {
        tmp.id                  <- vertices.ids[i]
        tmp.leaf                <- myLeaves[[tmp.id]]
        tmp.index               <- which(myUnion %in% tmp.leaf)
        leaf.mat[tmp.index,i]   <- 1
    }
    leaf.df             <- as.data.frame(leaf.mat)
    colnames(leaf.df)   <- vertices.ids
    rownames(leaf.df)   <- myUnion

    return(leaf.df)
}



##------------------------------------------------------------------
## <function> :: circleBalancedErrorRate
##------------------------------------------------------------------
##
## under construction
##
## a commonly used metric of community prediction in this area
##------------------------------------------------------------------
circleBalancedErrorRate <- function(trueCircle, predCircle)
{
    ## error check
    if (!is.list(trueCircles) || any(unlist(trueCircles) < 0)) {
        stop("trueCircles must be a list with nonnegative entries.")
    }
    if (!is.list(predCircles) || any(unlist(predCircles )< 0)) {
        stop("predCircles must be a list with nonnegative entries.")
    }

    ## 0.5 * ( (( pred \ true ) / pred) + ((true \ pred) / true) )
    #
    # need to confirm the denominator
    #
    #0.5*( length(setdiff(true, pred))/length(true) + length(setdiff(pred, true))/length(pred) )
    
}



## under construction
#convClustersToTextCircle <- function(myId, myCluster)
#{
#    clust.num   <- length(myCluster)
#    circ.str    <- paste(myId,",",sep="")
#
#    for (i in 1:clust.num) {
#        tmp.nodes   <- myCluster[[i]]
#
#        if (i < clust.num) {
#            circ.str    <- paste(circ.str," ",paste(tmp.nodes, collapse=" "),"; ",sep="")
#        } else {
#            circ.str    <- paste(circ.str," ",paste(tmp.nodes, collapse=" "),sep="")
#        }
#    }
#    return(circ.str)
#}



