## NetSurv
An R package that implements the model-based network surveillance procedure based on the dynamic degree corrected stochastic block model. Functions in the package can simulate, estimate and generate Shewhart control charts for dynamic sequences of graphs with a structural change. 

The key reference for this monitoring method is

- Wilson, J.D., Stevens, N.T., and Woodall, W.H. (2016) **Modeling and estimating change in temporal networks via a dynamic degree corrected stochastic block model.** *arXiv Preprint: http://arxiv.org/abs/1605.04049*

## Installation

To install NetSurv, use the following commands. Be sure to include the required packages **Matrix**, **Rlab**, and **devtools** from R version 3.1.2 or higher.

``` 
#install the latest version of devtools
install.packages("devtools")
library(devtools, quietly = TRUE)

#install and load NetSurv
devtools::install_github("jdwilson4/NetSurv")
library(NetSurv, quietly = TRUE)

#load other required packages
library(Matrix, quietly = TRUE)
library(Rlab, quietly = TRUE)
```

## Description
This package contains four primary functions, which are briefly described below. For a function named ```function``` below, type ```?function``` in R to get full documentation.

- ```DCSBM()```: simulate an undirected graph realization from the degree corrected stochastic block random graph model. Edge weights are discrete valued and are generated independently where 
\[{e_{ij} ~ Poisson(theta_{i}*theta_{j}*P_{c_{i}, c_{j}})\]
- ```dynamic.DCSBM()```: simulate an ordered sequence of undirected graphs from the degree corrected stochastic block random graph model.
- ```MLE.DCSBM()```: estimate the maximum likelihood estimators for P and \(\delta\) at each time point in a time-varying collection of networks.
- ```NetSurv()```: Shewhart surveillance control chart and plots for a desired collection of statistics

## Examples

- **Example 1: simulate a single realization of a DCSBM at one time point**
```
?DCSBM
net <- DCSBM(n = 500, k = 2, P = cbind(c(0.10, 0.01), c(0.02, 0.075)),
             sizes = c(200, 300), random.community.assignment = FALSE,
             delta = c(0.2, 0.7), edge.list = FALSE)

image(Matrix(net$Adjacency))

```
- **Example 2: simulate a dynamic DCSBM with 50 time steps and a change at time 25, where the change is a local change in connection propensity in community 1**
```
?dynamic.DCSBM

n <- 100
P.old <- cbind(c(0.10, 0.01), c(0.02, 0.075))
P.new <- cbind(c(0.20, 0.025), c(0.02, 0.075))
P.array <- array(c(replicate(25, P.old), replicate(25, P.new)), dim = c(2, 2, 50))
community.array <- array(rep(c(rep(1, 50), rep(2, 50)), 50), dim = c(1, 100, 50))
delta.array <- array(rep(rep(0.2, 2), 50), dim = c(1, 2, 50))
 
dynamic.net <- dynamic.DCSBM(n = 100, T = 50, P.array = P.array,
                             community.array = community.array,
                             delta.array = delta.array, edge.list = FALSE)
                             
#View instances of the network before and after the change
image(Matrix(dynamic.net$Adjacency.list[[1]]))
image(Matrix(dynamic.net$Adjacency.list[[30]]))
```

- **Example 3: Estimating MLEs of a dynamic DCSBM**
```
MLEs.example <- MLE.DCSBM(dynamic.net$Adjacency.list, community.array = community.array,
                          T = 50, k = 2)
```                          
- **Example 4: Generate control charts for maximum likelihood estimators**
```
#Store the statistics in a data frame
statistics.df <- data.frame(Phat_11 = MLEs.example$P.hat.array[1, 1, ], 
                           Phat_12 = MLEs.example$P.hat.array[1, 2, ],
                           delta_hat = MLEs.example$delta.hat.global)
control.chart <- NetSurv(statistics.df, phase1.length = 20, plot = TRUE)
print(control.chart)
```

- **Political Network Application**
Now, we apply the NetSurv methodology on the dynamic networks that describe the co-voting habits of the U.S. Senators over time. See the above reference for more information on the results and description of the data set. 

In this application, we assume that community labels correspond to political affiliation of the Senators (Republican vs. Democrat)

```
#Load data
data(Senate_Rollcall.RData)
##More to come soon...
```

## Contributors
- **James D. Wilson**, Assistant Professor of Statistics, University of San Francisco. Developor, contributor, and maintainer. 

- **Nathaniel T. Stevens**, Assistant Professor of Statistics, University of San Francisco. Contributor. 

Please send any comments or questions to the developer James D. Wilson at jdwilson4@usfca.edu. 