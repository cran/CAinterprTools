#' Average Rule chart
#'
#' This function helps locating the number of dimensions that are
#' important for CA interpretation, according to the so-called 'average rule'. The
#' reference line showing up in the returned histogram indicates the threshold
#' for an optimal dimensionality of the solution according to the average rule.
#' 
#' @param data Name of the dataset (must be in dataframe format).
#' 
#' @keywords aver.rule
#' 
#' @export
#' 
#' @importFrom graphics abline axis barplot hist legend par plot points rug symbols text title
#' @importFrom stats aggregate cutree dist hclust median pchisq quantile r2dtable rect.hclust
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom ca ca
#' @importFrom FactoMineR CA HCPC plot.CA
#' @importFrom RcmdrMisc assignCluster
#' @importFrom Hmisc dotchart2
#' @importFrom classInt jenks.tests
#' @importFrom reshape2 melt
#' @import ggplot2
#' @import ggrepel
#' @import cluster
#' 
#' @examples
#' data(greenacre_data)
#' aver.rule(greenacre_data)
#' 
aver.rule <- function (data){
  mydataasmatrix<-as.matrix(data)
  dataframe.after.ca<- summary(ca(data))
  nrows <- nrow(data)
  ncols <- ncol(data)
  c.dim<-round(100/(ncols-1), digits=1)
  r.dim<-round(100/(nrows-1), digits=1)
  thresh.sig.dim<-(max(c.dim, r.dim))
  n.dim.average.rule <- length(which(dataframe.after.ca$scree[,3]>=thresh.sig.dim))
  mydataasmatrix<-as.matrix(data)
  barplot(dataframe.after.ca$scree[,3], xlab="Dimensions", ylab="% of Inertia", names.arg=dataframe.after.ca$scree[,1])
  abline(h=thresh.sig.dim)
  title (main="Percentage of inertia explained by the dimensions", 
         sub="reference line: threshold of an optimal dimensionality of the solution, according to the average rule", 
         cex.main=0.80, cex.sub=0.80)
}