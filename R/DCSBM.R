#' DCSBM
#' 
#' Simulate an undirected graph realization from the degree corrected stochastic block random graph model. Edge weights are discrete valued and are generated independently where e_ij ~ Poisson(theta_i*theta_j*P_{c_i, c_j})
#' @param n: number of nodes in the graph
#' @param k: number of communities in the graph
#' @param P: the k x k matrix of probabilities whose i,jth entry specifies the probability of connection between nodes in community i and community j
#' @param sizes: a numeric vector of length k whose ith entry specifies the size of the ith community. The entries must add to n.
#' @param random.community.assignment: a logical that specifies whether or not community labels are determined at random. Default is FALSE.
#' @param community.labels: an integer vector of length n whose ith entry is the community label of the ith vertex. Default is NULL. If provided, community labels are no longer assigned.
#' @param delta: a numeric vector of length k whose values must be between 0 and 1. Theta parameters for community r are generated as an iid sample from a U(0 + delta, 1 - delta) distribution
#' @param edge.list: a logical that specifies whether or not the adjacency matrix should be returned as an edge list.
#' 
#' @keywords community detection, random graph model, network monitoring
#' @return a list containing the objects 
#' \itemize{
#'    \item Adjacency: the adjacency matrix (or edge list if edge.list == TRUE) of the generated network
#'    \item Thetas: an n x 1 vector with values of each theta
#'    \item Membership: an n x 1 vector specifying community membership of each node
#' }
#'@references
#'\itemize{
#'     \item Wilson, James D., Stevens, Nathaniel T., and Woodall, William H. (2016). “Modeling and estimating change in temporal networks via a dynamic degree corrected stochastic block model.” 
#'     arXiv Preprint: http://arxiv.org/abs/1605.04049
#' } 
#' @author James D. Wilson and Nathaniel T. Stevens
#' @examples
#' net <- DCSBM(n = 500, k = 2, P = cbind(c(0.10, 0.01), c(0.02, 0.075)),
#'              sizes = c(200, 300), random.community.assignment = FALSE,
#'              delta = c(0.2, 0.7), edge.list = FALSE)
#' image(Matrix(net$Adjacency))
#' @export 

DCSBM = function(n, k = 2, P, sizes = c(round(n / 2), n - round(n / 2)), random.community.assignment = c(FALSE, TRUE),
                 community.labels = NULL, delta = rep(0, k), 
                 edge.list = c(FALSE, TRUE)){
  #Check that entries are appropriate
  if(sum(sizes) != n){
    stop("argument sizes must sum to n")
  }
  if(length(sizes) != k){
    stop("argument sizes must be of length k")
  }
  
  #If community labels are provided, keep these
  if(!is.null(community.labels)){
    Membership <- as.numeric(as.factor(community.labels))
    k <- length(unique(Membership))
    Y <- matrix(rep(0, n*k), ncol = k)
    for(i in 1:k){
      Y[which(Membership == i), i] = 1
    }
  }
  
  
  if(is.null(community.labels)){
    Y <- matrix(rep(0, n*k), ncol = k)
    index <- list()
    possible <- 1:n
    Membership <- rep(1, n)
    random.community.assignment <- random.community.assignment[1]
    #assign vertex labels randomly if random.assignment == TRUE
    if(random.community.assignment == TRUE){
      for(i in 1:k){
        index[[i]] <- sample(possible, sizes[i])
        Membership[index[[i]]] = i
        possible <- setdiff(possible, index[[i]])
      }
    }
    
    #assign vertex labels in order if random.assignment == FALSE
    if(random.community.assignment == FALSE){
      for(i in 1:k){
        index[[i]] <- possible[1:sizes[i]]
        Membership[index[[i]]] = i
        possible <- setdiff(possible, index[[i]])
      }
    }
    for(i in 1:k){
      Y[index[[i]], i] = 1
    }
  }
  
  edge.list <- edge.list[1]
  #Sample the theta parameters
  
  if(length(which(delta > 1)) > 0){stop("argument delta parameters must be less than or equal to 1")}
    thetas <- rep(1, n)
    for(i in 1:k){
      thetas[Membership == i] <- runif(sizes[i], min = 1 - delta[i], max = 1 + delta[i])
    }
  
  #generate theta matrix
  theta.matrix <- matrix(0, n, n)
  for(i in 1:n){
    for(j in 1:n){
      theta.matrix[i,j] <- thetas[i]*thetas[j] 
    }
  }
  

  #Generate expected value of the adjacency matrix
  expected.A <- Y%*%P%*%t(Y)
  
  expected.A <- theta.matrix*expected.A
  
  #Generate a realization of the adjacency matrix via Poisson draws
  temp <- matrix(rpois(n^2, matrix(expected.A, ncol = 1)), ncol = n)
  Adj <- matrix(0, ncol = n, nrow = n)
  Adj[upper.tri(Adj)] <- temp[upper.tri(temp)]
  Adj <- Adj + t(Adj)
  diag(Adj) <- 0
  Adj <- Matrix(Adj, sparse = TRUE)
  
  
  
  if(edge.list == TRUE){
    return(list(Adjacency = as.vector(Adj), Thetas = thetas, Membership = Membership))
  }
  if(edge.list == FALSE){
    return(list(Adjacency = Adj, Thetas = thetas, Membership = Membership))
  }
}