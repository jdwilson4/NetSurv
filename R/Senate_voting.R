#' data set: voting
#'
#' Data set that contains the co-voting network of U.S. Senators from Congress 40 (1867) to 
#' Congress 113 (2013). Nodes are Senators from the specified Congress, and edges are placed 
#' between Senators who voted concurrently on 75% of the bills for which they voted.
#'
#' @docType data
#'
#' @usage data(voting)
#'
#' @format This data set contains three components
#' 
#' \describe{
#'    \item{US.data} a dataframe describing the Senate majority, and the majority percentage.
#'    \item{political.affiliation}: a list of length 74 describing the political affiliation of each Senator for Congresses 40 - 113.
#'    \item{voting.network}: a list of length 74 where the tth entry is the adjacency matrix representing the co-voting network for Congress 39 + t.
#' }
#'
#' @keywords datasets
#'
#'@source \url{http://voteview.com}
#'
#'@references
#'\itemize{
#'     \item Wilson, James D., Stevens, Nathaniel T., and Woodall, William H. (2016). “Modeling and estimating change in temporal networks via a dynamic degree corrected stochastic block model.” 
#'     arXiv Preprint: http://arxiv.org/abs/1605.04049
#' } 
#' @author James D. Wilson
#'
#' @examples
#' data(voting)
#' image(Matrix(voting.network[[1]])) #look at co-voting network for Congress 40
#' @export
"voting"
