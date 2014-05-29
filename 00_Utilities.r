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
## solution <- c("0, 3 1 2; 2 3; 5 4 6")
## truth    <- c("0, 4 5; 1 2 3 4")
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
    tmp.id      <- paste("ID_", trim(tmp.split)[1])
    tmp.circles <- trim(strsplit(tmp.split[2],";")[[1]])
    
    ## load and name the circles into a list
    tmp.list    <- lapply(tmp.circles,
                            function(x)
                            {
                                            as.integer( unlist(strsplit(x," ")[[1]]) )
                            })
    names(tmp.list) <- paste("circle", 1:legnth(tmp.list),sep="")
    
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




convEgonetListToIgraphObject   <- function(myEgonet)
{
    nodes.num   <- length(myEgonet)
    edges.df    <- data.frame()
    
    ## for each node in the list, strip the ID tag and load edges
    for (i in 1:nodes.num) {
        node        <- gsub("ID_","",names(myEgonet)[i])
        edges       <- gsub("ID_","",myEgonet[[i]])
        edges.df    <- rbind(edges.df, data.frame(E1=rep(node, length(edges)), E2=edges))
    }
    
    return(graph.data.frame(edges.df, directed=FALSE))
}




calcSimilarityMatrix <- function(myIgraph)
{
    ## load the edges and define an output matrix
    edges       <- get.data.frame(myIgraph, what="edges")
    edges.num   <- nrow(edges)
    edges.perm  <- combs(1:edges.num, 2)
    sim.vec     <- vector(,length=nrow(edges.perm))
    sim.mat     <- matrix(1,nrow=edges.num, ncol=edges.num)
    
    ## compute the similarity matrix
    #for (i in 1:edges.num) {
        #for (j in 1:edges.num) {
    for (i in 1:nrow(edges.perm)) {
        
            ## get the edges to compare
            ei  <- get.edge(myIgraph,id=edges.perm[i,1])
            ej  <- get.edge(myIgraph,id=edges.perm[i,2])
            
            ## define a dummy keystone
            keystone <- -1
            
            ## cases where there is a keystone
            if (ei[1] == ej[1]) {
                keystone <- ei[1]
                tmp.i    <- ei[2]
                tmp.j    <- ej[2]
            } else if (ei[1]==ej[2]) {
                keystone <- ei[1]
                tmp.i    <- ei[2]
                tmp.j    <- ej[1]
            } else if (ei[2]==ej[1]) {
                keystone <- ei[2]
                tmp.i    <- ei[1]
                tmp.j    <- ej[2]
            } else if (ei[2]==ej[2]) {
                keystone <- ei[2]
                tmp.i    <- ei[1]
                tmp.j    <- ej[1]
            }
            
            ## if there is a keystone, then compute the Jaccard coefficient
            if (!(keystone == -1)) {
                np_i     <- c(keystone, tmp.i, neighbors(myIgraph, tmp.i))
                np_j     <- c(keystone, tmp.j, neighbors(myIgraph, tmp.j))
                #sim[i,j] <- (length(intersect(np_i, np_j))) / (length(union(np_i, np_j))) #+ 0.000001*runif(1)
                sim.vec[i] <- (length(intersect(np_i, np_j))) / (length(union(np_i, np_j))) #+ 0.000001*runif(1)
            }
            #}
    }
    sim.mat[lower.tri(sim.mat)] <- sim.vec
    colnames(sim.mat) <- paste("idx",1:edges.num,sep="")
    rownames(sim.mat) <- paste("idx",1:edges.num,sep="")
    return(sim.mat)
}




##
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





