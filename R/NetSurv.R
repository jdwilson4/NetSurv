#' NetSurv
#' 
#' Moving range surveillance control chart and plots for a desired collection of statistics
#' @param Statistics: a data frame whose rows represent time and columns represent a desired statistic to be monitored
#' @param phase1.length: number of networks to use in phase 1 of monitoring
#' @param save.plot: a logical specifying whether or not to plot (and save) the control chart for each statistic
#' @param control.chart: which control chart to apply for surveillance. Choices include "Shewhart" or "EWMA". Default is "Shewhart"
#' @param lambda: smoothing constant for the EWMA control chart. Default is 0.2
#' @param directory: the directory where a .pdf version of the plot is stored (if plot == TRUE). Default is the current directory
#' @param height: height (in inches) of the printed plot
#' @param width: width (in inches) of the printed plot
#' @param xlab: the label on the x axis. Default is "Time"
#' @param ylab: the label on the y axis. Default is "Value"
#' @param xaxis.old: the old labels for the time variable on the x axis. Default is 1:T
#' @param xaxis.new: the new labels that you wish to have on the x axis. Default is 1:T. Note that this must have the same length as xaxis.old
#' @param par.plot: an integer of length 2 that specifies the number of c(rows, columns) to print the control charts
#' @param points.of.interest: a vector of time points at which it is believed a change point may occur. There will be lines drawn here in the plot.
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
#' control.chart <- NetSurv(statistics.df, phase1.length = 20, save.plot = TRUE)
#' @export 

NetSurv <- function(Statistics, phase1.length, save.plot = c(FALSE, TRUE),
                    control.chart = c("Shewhart", "EWMA"),
                    lambda = 0.2,
                    directory = getwd(), height = 7, width = 7, 
                    xlab = "Time", ylab = "Value", xaxis.old = c(1:dim(Statistics)[1]),
                    xaxis.new = c(1:dim(Statistics)[1]), points.of.interest = NULL){
  
  #Control Chart
  control.chart <- control.chart[1] #default is Shewhart
  #number of time steps
  T <- dim(Statistics)[1]
  #number of statistics analyzed
  p <- dim(Statistics)[2]
  names.stats <- names(Statistics)
  x <- 1:T
  save.plot <- save.plot[1]
  if(phase1.length > T){stop("Phase I must be shorter than the entire time series")}
  
  
  control.df <- Statistics
  
  #build Shewhart control chart for each statistic
  #note that we can use any control chart here. More to come on this later
  if(save.plot == TRUE){
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
      statistic <- Statistics[, j]
      file <- paste(directory, "/", names.stats[j], ".pdf", sep = "")
      ylim.lower <- min(lower, min(statistic)) - 0.015
      ylim.upper <- max(upper, max(statistic)) + 0.015
      pdf(file = file, height = height, width = width)
      print(plot(y = statistic, x = x, xlab = xlab, ylab = "Value", main = paste(names.stats[j]), type = "l", col = "black", ylim = c(ylim.lower, ylim.upper), lwd = 2, 
           pch = 15, cex = 1.25, cex.lab = 1.25, cex.axis = 1.22, xaxt = "n"))
      points(y = statistic[indx], x = indx, col = "red", pch = 12, cex = 1.1)
      abline(h = upper, lty = 2, lwd = 2, col = "blue")
      abline(h = lower, lty = 2, lwd = 2,col = "blue")
      axis(1, at = xaxis.old, labels = xaxis.new)
      if(!is.null(points.of.interest)){
        for(k in 1:length(points.of.interest)){
          abline(v = points.of.interest[k], lwd = 3)
        }
      }
      dev.off()
    }
  }
  #Draw the control chart here
  if(control.chart == "Shewhart"){
    par(mfrow = c(2, round(length(Statistics) / 2)))
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
      statistic <- Statistics[, j]
      file <- paste(directory, "/", names.stats[j], ".pdf", sep = "")
      ylim.lower <- min(lower, min(statistic)) - 0.015
      ylim.upper <- max(upper, max(statistic)) + 0.015
      plot(y = statistic, x = x, xlab = xlab, ylab = "Value", main = names.stats[j], type = "l", col = "black", ylim = c(ylim.lower, ylim.upper), lwd = 2, 
           pch = 15, cex = 1.25, cex.lab = 1.25, cex.axis = 1.22, xaxt = "n")
      points(y = statistic[indx], x = indx, col = "red", pch = 12, cex = 1.1)
      abline(h = upper, lty = 2, lwd = 2, col = "red")
      abline(h = lower, lty = 2, lwd = 2,col = "red")
      axis(1, at = xaxis.old, labels = xaxis.new)
      if(!is.null(points.of.interest)){
        for(k in 1:length(points.of.interest)){
          abline(v = points.of.interest[k], lwd = 2, lty = 2, col = "green")
        }
      }
    }
  }
  
  ##EWMA chart##
  if(control.chart == "EWMA"){
    par(mfrow = c(2, round(length(Statistics) / 2)))
    for(j in 1:p){
      control.df[, j] <- 0
      phase.I.stat <- Statistics[1:phase1.length, j]
      sd.est <- mean(abs(diff(phase.I.stat)))/1.128
      avg.est <- mean(phase.I.stat)
      
      upper <- avg.est + 3*sd.est*sqrt(lambda / (2 - lambda)) #asymptotic control limits
      lower <- avg.est - 3*sd.est*sqrt(lambda / (2 - lambda)) 
      result <- c(lower, avg.est, upper)
      
      #update to the statistics
      z <- rep(0, T) #statistic to monitor
      z[1] <- avg.est
      for(t in 2:T){
        z[t] <- lambda*Statistics[t, j] + (1 - lambda)*z[t-1]
      }
      
      
      indx <- which(z > upper | z < lower)
      control.df[indx, j] <- 1
      
      #Plot control chart if desired
      statistic <- z
      file <- paste(directory, "/", names.stats[j], ".pdf", sep = "")
      ylim.lower <- min(lower, min(statistic)) - 0.015
      ylim.upper <- max(upper, max(statistic)) + 0.015
      plot(y = statistic, x = x, xlab = xlab, ylab = "Value", main = names.stats[j], type = "l", col = "black", ylim = c(ylim.lower, ylim.upper), lwd = 2, 
           pch = 15, cex = 1.25, cex.lab = 1.25, cex.axis = 1.22, xaxt = "n")
      points(y = statistic[indx], x = indx, col = "red", pch = 12, cex = 1.1)
      abline(h = upper, lty = 2, lwd = 2, col = "red")
      abline(h = lower, lty = 2, lwd = 2,col = "red")
      axis(1, at = xaxis.old, labels = xaxis.new)
      if(!is.null(points.of.interest)){
        for(k in 1:length(points.of.interest)){
          abline(v = points.of.interest[k], lwd = 2, lty = 2, col = "green")
        }
      }
    }
  }
  
  return(Control.Chart = control.df)
}

