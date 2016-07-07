#' MLE.DCSBM
#' 
#' Estimate the maximum likelihood estimators for P and delta at each time point in the list of adjacency matrices
#' @param Adjacency.list: list of adjacency matrices in the observed dynamic network
#' @param community.array: an array of length T whose tth entry is a numeric vector of length n specifying community labels at time t
#' @param T: number of graphs in the temporal sequence
#' @param k: number of communities (fixed accross time) Note, this is the pre-conceived idea of how many communities there are.
#' 
#' @keywords community detection, random graph model, network monitoring, statistical process control
#' @return a list containing the objects 
#' \itemize{
#'    \item P.hat.array: an array of length T whose tth entry is the estimated MLE of P for the tth network
#'    \item delta.hat.array: an array of length T whose tth entry are the estimated MLEs of the delta parameters for the tth network
#'    \item delta.hat.global: a numeric of length T whose tth entry is the estimated MLE of the overall standard deviation of the theta parameters for the tth network
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
#' 
#' #Estimate the MLEs
#' MLEs.example <- MLE.DCSBM(dynamic.net$Adjacency.list, community.array = community.array,
#'                            T = 50, k = 2)
#' @export 

MLE.DCSBM <- function(Adjacency.list, community.array, T, k){

  if(T > 1){
    P.hat.array <- array(0, dim = c(k, k, T))
    delta.hat.array <- array(0, dim = c(1, k, T))
    delta.hat.global <- numeric(length = T)
    for(t in 1:T){
      Network <- Adjacency.list[[t]]
      community.labels = as.numeric(as.factor(community.array[, , t]))
      community.labels[is.na(community.labels)] = 1
      number.comms <- max(community.labels)
      n.comm <- rep(0, number.comms)
      P.hat <- matrix(0, ncol = number.comms, nrow = number.comms)
      theta.hat <- rep(0, n) #overall propensity of connection
      delta.hat <- rep(0, number.comms)
      for(i in 1:number.comms){
        indx.i <- which(community.labels == i)
        n.comm[i] <- length(indx.i)
        if(length(indx.i) == 1){
          theta.hat[indx.i] <- 1
        }
        if(length(indx.i) > 1){
          theta.hat[indx.i] <- rowSums(Network[indx.i, ]) / ((1 / n.comm[i])* sum(rowSums(Network[indx.i, ])))
        }
        delta.hat[i] <- sd(theta.hat[indx.i])
      }
      delta.hat.global[t] <- sd(theta.hat)
      
      for(i in 1:number.comms){
        indx.i <- which(community.labels == i)
        for(j in 1:number.comms){
          indx.j <- which(community.labels == j)
          P.hat[i,j] <- sum(Network[indx.i, indx.j]) / (n.comm[i]*n.comm[j])
        }
      }
    P.hat.array[, , t] <- P.hat
    delta.hat.array[, , t] <- delta.hat
    }
  }
  return(list(P.hat.array = P.hat.array, delta.hat.array = delta.hat.array, 
              delta.hat.global = delta.hat.global))
}
  
  
 