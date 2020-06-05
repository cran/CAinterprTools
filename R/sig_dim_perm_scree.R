#' Scree plot to test the significance of CA dimensions by means of a randomized
#' procedure
#'
#' This function tests the significance of the CA dimensions by means
#' of permutation of the input contingency table. Number of permutation set at
#' 999 by default, but can be increased by the user. The function return a
#' scree-plot displaying for each dimension the observed eigenvalue and the 95th
#' percentile of the permuted distribution of the corresponding eigenvalue.
#' Observed eigenvalues that are larger than the corresponding 95th percentile
#' are significant at least at alpha 0.05. Permuted p-values are displayed into the
#' chart and also returned as dataframe.
#' 
#' @param data Name of the contingency table (must be in dataframe format).
#' @param B Number of permutations to be used (999 by default).
#' @param cex Controls the size of the labels reporting the p values; see the
#'   help documentation of the text() function by typing ?text.
#' @param pos Controls the position of the labels reporting the p values; see
#'   the help documentation of the text() function by typing ?text.
#' @param offset Controls the offset of the labels reporting the p values; see
#'   the help documentation of the text() function by typing ?text.
#'   
#' @return The function returns a dataframe storing the permuted p-values of each CA dimension.
#'   
#' @keywords sig.dim.perm.scree
#' 
#' @export
#' 
#' @examples
#' data(greenacre_data)
#'
#' pvalues <- sig.dim.perm.scree(greenacre_data, 99)
#' 
#' @seealso \code{\link{sig.dim.perm}}
#' 
sig.dim.perm.scree <- function(data, B=999, cex=0.7, pos=4, offset=0.5){
  options(scipen = 999)
  
  nIter <- B
  
  numb.dim.cols <- ncol(data) - 1
  
  numb.dim.rows <- nrow(data) - 1
  
  table.dim <- min(numb.dim.cols, numb.dim.rows)
  
  d <- as.data.frame(matrix(nrow=nIter+1, ncol=table.dim))
  
  res <- CA(data, graph=FALSE)
  
  d[1,]<- rbind(res$eig[,1])
  
  pb <- txtProgressBar(min = 0, max = nIter, style = 3)#set the progress bar to be used inside the loop
  
  for (i in 2:nrow(d)){
    rand.table <- as.data.frame(r2dtable(1, apply(data, 1,sum), apply(data, 2, sum)))  
    res <- CA(rand.table, graph=FALSE)
    d[i,] <- rbind(res$eig[,1])
    setTxtProgressBar(pb, i)
  }
  
  target.percent <- apply(d[-c(1),],2, quantile, probs = 0.95) #calculate the 95th percentile of the randomized eigenvalues, excluding the first row (which store the observed eigenvalues)
  
  max.y.lim <- max(d[1,], target.percent)
  
  obs.eig <- as.matrix(d[1,])
  
  obs.eig.to.plot <- melt(obs.eig) #requires reshape2
  
  perm.p.values <- round((1 + colSums(d[-1,] > d[1,][col(d[-1,])])) / (1 + B), 4)
  
  plot(obs.eig.to.plot$value, type = "o", ylim = c(0, max.y.lim), xaxt = "n", xlab = "Dimensions", ylab = "Eigenvalue", pch=20)
  
  text(obs.eig.to.plot$value, labels = perm.p.values, cex = cex, pos = pos, offset = offset)
  
  axis(1, at = 1:table.dim)
  
  title(main = "Correspondence Analysis: \nscree-plot of observed and permuted eigenvalues", sub = paste0("Black dots=observed eigenvalues; blue dots=95th percentile of the permutated eigenvalues' distribution. Number of permutations: ", B), cex.sub = 0.8)
  
  par(new = TRUE)
  
  percentile.to.plot <- melt(target.percent)
  
  plot(percentile.to.plot$value, type = "o", lty = 2, col = "blue", ylim = c(0, max.y.lim), xaxt = "n", xlab = "", ylab = "", sub = "")
  
  pvalues.df <- as.data.frame(perm.p.values)
  
  row.names(pvalues.df) <- paste0("dim.", seq(1,table.dim))
  
  return(pvalues.df)
}