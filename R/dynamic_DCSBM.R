#' dynamic.DCSBM
#' 
#' Simulate an ordered sequence of undirected graphs from the degree corrected stochastic block random graph model. Edge weights are discrete valued and are generated independently where e_ij ~ Poisson(theta_i*theta_j*P_{c_i, c_j})
#' @param n: number of nodes in the graph
#' @param T: number of graphs in the temporal sequence
#' @param P.array: an array of length T whose tth entry is the matrix of probabilities for network t
#' @param community.array: an array of length T whose tth entry is a numeric vector of length n specifying community labels at time t
#' @param delta.array: an array of length T whose tth entry is a numeric vector of length k whose values must be between 0 and 1. 
#' @param edge.list: a logical that specifies whether or not the adjacency matrix should be returned as an edge list.
#' 
#' @keywords community detection, random graph model, network monitoring
#' @return a list containing the objects 
#' \itemize{
#'    \item Adjacency.list: a list of length T whose tth entry is the adjacency matrix (or edge list if edge.list == TRUE) of the tth generated network
#'    \item Theta.list: a list of length T whose tth entry is an n x 1 vector with values of each theta for the tth network
#' }
#'@references
#'\itemize{
#'     \item Wilson, James D., Stevens, Nathaniel T., and Woodall, William H. (2016). “Modeling and estimating change in temporal networks via a dynamic degree corrected stochastic block model.” 
#'     arXiv Preprint: http://arxiv.org/abs/1605.04049
#' } 
#' @author James D. Wilson and Nathaniel T. Stevens
#' @examples
#' #Generate a collection of 50 networks with a change at time 25. The change is a local 
#' #change in connection propensity in community 1
#' n <- 100
#' P.old <- cbind(c(0.10, 0.01), c(0.02, 0.075))
#' P.new <- cbind(c(0.20, 0.025), c(0.02, 0.075))
#' P.array <- array(c(replicate(25, P.old), replicate(25, P.new)), dim = c(2, 2, 50))
#' community.array <- array(rep(c(rep(1, 50), rep(2, 50)), 50), dim = c(1, 100, 50))
#' delta.array <- array(rep(rep(0.2, 2), 50), dim = c(1, 2, 50))
#' 
#' dynamic.net <- dynamic.DCSBM(n = 100, T = 50, P.array = P.array,
#'                              community.array = community.array,
#'                              delta.array = delta.array, edge.list = FALSE)
#' image(Matrix(dynamic.net$Adjacency.list[[1]]))
#' image(Matrix(dynamic.net$Adjacency.list[[30]]))
#' @export 

dynamic.DCSBM = function(n, T, P.array, community.array, delta.array,
                         edge.list = c(FALSE, TRUE)){
  
  edge.list <- edge.list[1]
  Adjacency.list <- list()
  Theta.list <- list()
  for(t in 1:T){
    temp <- DCSBM(n = n, P = P.array[, , t], community.labels = community.array[, , t], delta = delta.array[, , t],
                  edge.list = edge.list)
    Adjacency.list[[t]] <- temp$Adjacency
    Theta.list[[t]] <- temp$Thetas
  }
  return(list(Adjacency.list = Adjacency.list, Theta.list = Theta.list))
}