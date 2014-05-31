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
## <function> :: convert.magic
##------------------------------------------------------------------
## A function to perform a type switch on a data.frame
##------------------------------------------------------------------
convert.magic   <- function(obj, col, type) {
    
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
dedupEdgeList <- function(myIgraph) {
    
    ## translate igraph object into a data frame
    edge.df     <- get.data.frame(myIgraph)
    
    ## contact edge members and then return the unique set
    edges.all   <- paste(edge.df$from, edge.df$to, sep="_")
    edges.uniq  <- unique(edges.all)
    
    ## translate back into a data.frame
    output.df   <- as.data.frame(do.call("rbind",(strsplit(edges.uniq,"_"))))
    
    return(graph.data.frame(output.df, directed=FALSE))
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
    ## NEED TO ADD CHECK FOR IGRAPH
    
    
    nodes.num   <- length(myEgonet)
    edges.df    <- data.frame()
    
    ## for each node in the list, strip the ID tag and load edges
    for (i in 1:nodes.num) {
        node        <- as.integer(gsub("ID_","",names(myEgonet)[i]))
        edges       <- as.integer(gsub("ID_","",myEgonet[[i]]))
        if (length(edges) > 1) {
            edges.df <- rbind(edges.df, data.frame(E1=rep(node, length(edges)), E2=edges))
        } else {
            ## here we ignore friends with no friends
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
## measure between all cominations of edges in the object
##------------------------------------------------------------------
calcSimilarityMatrix <- function(myIgraph, DOPARALLEL=FALSE)
{
    
    ## load the edges and define an output matrix
    edges       <- get.data.frame(myIgraph, what="edges")
    edges.ch    <- convert.magic(edges, c("from","to"), c("character","character"))
    edges.num   <- ecount(myIgraph)
    
    ## do a neighbor lookup once on all vertices
    verts       <- V(myIgraph)$name
    np.list    <- list()
    for (i in 1:length(verts)) {
        np.list[[verts[i]]]    <- c(V(myIgraph)[i]$name, V(myIgraph)[nei(i)]$name)
    }
    
    ## define the output matrix (this is the memory concern)
    sim.mat             <- matrix(0,nrow=edges.num, ncol=edges.num)
    rownames(sim.mat)   <- edges$from
    colnames(sim.mat)   <- edges$to
    
    ## define the set of iterators for the parallel loop
    ivec    <- 1:edges.num
    jvec    <- 1:edges.num
    
    ## compute the matrix elements for each iteration
    if (DOPARALLEL) {
        cat("calcSimilarityMatrix::parallel\n")
        sim.mat <- foreach(i=ivec, .combine='cbind') %:%
            foreach(j=jvec, .combine='c') %dopar% {
                calcSimEikEjk(edges, np.list, i, j)
            }
    } else {
        cat("calcSimilarityMatrix::non-parallel\n")
        for (i in 1:(edges.num-1)) {
            for (j in (i+1):edges.num) {
                sim.mat[j,i] <- calcSimEikEjk(edges, np.list, i, j)
            }
        }
    }
    
    ## return the *similarity* matrix (and not the distance)
    return(sim.mat)
}


##------------------------------------------------------------------
## <function> :: calcSimilarityMatrix
##------------------------------------------------------------------
## Compute the (i,j)th element of a similarity matrix, given a pair
## of edges and the neighbors for each node in the edges
##------------------------------------------------------------------
calcSimEikEjk <- function(myEdges, myNeighbors, i, j)
{
    ## get the edges to compare
    ei  <- myEdges[i,]
    ej  <- myEdges[j,]
    
    ## define a dummy keystone
    keystone <- -1
    
    ## cases where there is a keystone
    if (ei[1,1] == ej[1,1]) {
        keystone <- ei[1,1]
        tmp.i    <- ei[1,2]
        tmp.j    <- ej[1,2]
    } else if (ei[1,1]==ej[1,2]) {
        keystone <- ei[1,1]
        tmp.i    <- ei[1,2]
        tmp.j    <- ej[1,1]
    } else if (ei[1,2]==ej[1,1]) {
        keystone <- ei[1,2]
        tmp.i    <- ei[1,1]
        tmp.j    <- ej[1,2]
    } else if (ei[1,2]==ej[1,2]) {
        keystone <- ei[1,2]
        tmp.i    <- ei[1,1]
        tmp.j    <- ej[1,1]
    }
    
    ## if there is a keystone, then compute the Jaccard coefficient
    if (!(keystone == -1)) {
        np_i      <- myNeighbors[[tmp.i]]
        np_j      <- myNeighbors[[tmp.j]]
        SimEikEjk <- (length(intersect(np_i, np_j))) / (length(union(np_i, np_j))) #+ 0.000001*runif(1)
    } else {
        SimEikEjk <- 0
    }
    
    ## return the (i,j)th element of the matrix
    return(SimEikEjk)
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
    e.num       <- nrow(get.edgelist(myIgraph))
    e.idx       <- 1:e.num
    
    ## matrix to hold the identify of group members at each height
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
    hmax    <- h.vec[dens.vec == pdmax]
    
    ## isolate cluster members at the maximum density
    clust.ids   <- cutree(myHclust, h=hmax)
    clust.uniq  <- unique(clust.ids)
    clust.num   <- length(clust.uniq)
    clust.edges <- list()
    clust.nodes <- list()
    
    ## identify the index of the edges that are clusters
    clust.cnt   <- 1
    for (i in 1:clust.num) {
        tmp.clust   <- clust.uniq[i]
        tmp.idx     <- e.idx[which(clust.ids == tmp.clust)]
        tmp.memb    <- union(get.edgelist(myIgraph)[tmp.idx,1], get.edgelist(myIgraph)[tmp.idx,2])
        
        if ( length(tmp.idx) >= 3 ) {
            clust.edges[[clust.cnt]] <- tmp.idx
            clust.nodes[[clust.cnt]] <- as.integer(tmp.memb)
            clust.cnt   <- clust.cnt + 1
        }
        
    }
    return(list(pdens=pdens, pdmax=pdmax, hmax=hmax, clust.nodes=clust.nodes, cluster.edges=clust.edges))
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

## test cases for a given set of circles
## t    <- list(t1=c(4,5), t2=c(1,2,3,4), t3=c(2,3))            ## truth
## p1   <- list(p1=c(1,2,3,4,5))                                ## all in one circle (5 edits)
## p2   <- list(p1=c(1), p2=c(2), p3=c(3), p4=c(4), p5=c(5))    ## each in a circle (7 edits)




## under construction
convClustersToTextCircle <- function(myId, myCluster)
{
    clust.num   <- length(myCluster)
    circ.str    <- paste(myId,",",sep="")
    
    for (i in 1:clust.num) {
        tmp.nodes   <- myCluster[[i]]
        
        if (i < clust.num) {
            circ.str    <- paste(circ.str," ",paste(tmp.nodes, collapse=" "),"; ",sep="")
        } else {
            circ.str    <- paste(circ.str," ",paste(tmp.nodes, collapse=" "),sep="")
        }
    }
    return(circ.str)
    
}



