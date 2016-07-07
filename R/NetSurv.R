#' NetSurv
#' 
#' Moving range surveillance control chart and plots for a desired collection of statistics
#' @param Statistics: a data frame whose rows represent time and columns represent a desired statistic to be monitored
#' @param phase1.length: number of networks to use in phase 1 of monitoring
#' @param plot: a logical specifying whether or not to plot (and save) the control chart for each statistic
#' @param directory: the directory where a .pdf version of the plot is stored (if plot == TRUE). Default is the current directory
#' @param height: height (in inches) of the printed plot
#' @param width: width (in inches) of the printed plot
#' @param xlab: the label on the x axis. Default is "Time"
#' @param ylab: the label on the y axis. Default is "Value"
#' @param xaxis.old: the old labels for the time variable on the x axis. Default is 1:T
#' @param xaxis.new: the new labels that you wish to have on the x axis. Default is 1:T. Note that this must have the same length as xaxis.old
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
#' #Store the statistics in a data frame
#' statistics.df <- data.frame(Phat_11 = MLEs.example$P.hat.array[1, 1, ], 
#'                             Phat_12 = MLEs.example$P.hat.array[1, 2, ],
#'                             delta_hat = MLEs.example$delta.hat.global)
#' control.chart <- NetSurv(statistics.df, phase1.length = 20, plot = TRUE)
#' @export 

NetSurv <- function(Statistics, phase1.length, plot = c(FALSE, TRUE), 
                    directory = getwd(), height = 7, width = 7, 
                    xlab = "Time", ylab = "Value", xaxis.old = c(1:dim(Statistics)[1]),
                    xaxis.new = c(1:dim(Statistics)[1])){
  
  #number of time steps
  T <- dim(Statistics)[1]
  #number of statistics analyzed
  p <- dim(Statistics)[2]
  names.stats <- names(Statistics)
  x <- 1:T
  plot <- plot[1]
  if(phase1.length > T){stop("Phase I must be shorter than the entire time series")}
  
  control.df <- Statistics
  
  #build Shewhart control chart for each statistic
  #note that we can use any control chart here. More to come on this later
  for(j in 1:p){
    control.df[, j] <- 0
    phase.I.stat <- Statistics[1:phase1.length, j]
    sd.est <- mean(abs(diff(phase.I.stat)))/1.128
    avg.est <- mean(phase.I.stat)
    
    upper <- avg.est + 3*sd.est
    lower <- avg.est - 3*sd.est
    result <- c(lower, avg.est, upper)
    indx <- which(Statistics[, j] > upper | Statistics[, j] < lower)
    control.df[indx, j] <- 1
    
    #Plot control chart if desired
    if(plot == TRUE){
      statistic <- Statistics[, j]
      file <- paste(directory, "/", names.stats[j], ".pdf", sep = "")
      ylim.lower <- min(lower, min(statistic)) - 0.015
      ylim.upper <- max(upper, max(statistic)) + 0.015
      pdf(file = file, height = height, width = width)
      print(plot(y = statistic, x = x, xlab = xlab, ylab = "Value", main = names.stats[j], type = "l", col = "black", ylim = c(ylim.lower, ylim.upper), lwd = 2, 
           pch = 15, cex = 1.25, cex.lab = 1.25, cex.axis = 1.22, xaxt = "n"))
      points(y = statistic[indx], x = indx, col = "red", pch = 12, cex = 1.1)
      abline(h = upper, lty = 2, lwd = 2, col = "blue")
      abline(h = lower, lty = 2, lwd = 2,col = "blue")
      axis(1, at = xaxis.old, labels = xaxis.new)
      dev.off()
    }
  }
  return(Control.Chart = control.df)
}

