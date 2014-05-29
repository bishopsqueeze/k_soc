

calcSimilarityMatrix <- function(myIgraph)
{
    ## load the edges and define an output matrix
    edges       <- get.data.frame(myIgraph, what="edges")
    edges.num   <- nrow(edges)
    sim         <- matrix(0,nrow=edges.num, ncol=edges.num)
    
    ## compute the similarity matrix
    for (i in 1:edges.num) {
        for (j in 1:edges.num) {
            
            ## get the sample edges
            ei  <- get.edge(myIgraph,id=i)
            ej  <- get.edge(myIgraph,id=j)
            
            ## define a dummy keystone
            keystone <- -1
            
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
                sim[i,j] <- (length(intersect(np_i, np_j))) / (length(union(np_i, np_j))) #+ 0.000001*runif(1)
            }
        }
    }
    return(sim)
}


tmp <- graph.data.frame(rbind(  c(1,3),c(1,2),c(2,4),
c(2,5),c(2,6),c(2,7),
c(2,8),c(2,9),c(2,10),
c(3,8),c(3,9),c(3,10),
c(3,11),c(3,12)), directed=FALSE)


tmp <- graph.data.frame(rbind(c(1,3),c(1,2),c(2,3)), directed=FALSE)


b <- calcSimilarityMatrix(a)
b <- calcSimilarityMatrix(tmp)