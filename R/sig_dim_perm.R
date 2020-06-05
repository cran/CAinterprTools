#' Permuted significance of CA dimensions
#'
#' This function calculates the permuted significance of a pair of selected
#' CA dimensions. Number of permutation set at 999 by default, but can be
#' increased by the user. 
#' A scatterplot of the permuted inertia of a pair of selected dimensions is produced.
#' Permuted p.values are reported in the axes' labels and are also returned in a dataframe.
#' 
#' @param data Name of the dataset (must be in dataframe format).
#' @param x First dimension whose significance is calculated (x=1 by default).
#' @param y Second dimension whose significance is calculated (y=2 by default).
#' @param B Number of permutations (999 by default).
#' 
#' @return The function returns a dataframe storing the permuted p-values of each CA dimension.
#' 
#' @keywords sig.dim.perm
#' 
#' @export
#' 
#' @examples
#' data(greenacre_data)
#'
#' #Produces a scatterplot of the permuted inertia of the 1 CA dimension
#' #against the permuted inertia of the 2 CA dimension.
#' #The observed inertia of the selected dimensions is displayed as a large red dot; 
#' #pvalues are reported in the axes labels (and are stored in a 'pvalues' object).
#' 
#' pvalues <- sig.dim.perm(greenacre_data, 1,2, B=99)
#' 
#' @seealso \code{\link{sig.dim.perm.scree}}
#' 
sig.dim.perm <- function(data, x=1, y=2, B=999) {
  nIter <- B
  
  numb.dim.cols <- ncol(data) - 1
  
  numb.dim.rows <- nrow(data) - 1
  
  table.dim <- min(numb.dim.cols, numb.dim.rows)
  
  d <- as.data.frame(matrix(nrow=nIter+1, ncol=table.dim)) 
  
  res <- CA(data, graph=FALSE)
  
  d[1,]<- rbind(res$eig[,1])
  
  pb <- txtProgressBar(min = 0, max = nIter, style = 3) #set the progress bar to be used inside the loop
  
  for (i in 2:nrow(d)){
    rand.table <- as.data.frame(r2dtable(1, apply(data, 1,sum), apply(data, 2, sum)))  
    res <- CA(rand.table, graph=FALSE)
    d[i,] <- rbind(res$eig[,1])
    setTxtProgressBar(pb, i)
  }
  
  perm.pvalues <- round((1 + colSums(d[-1,] > d[1,][col(d[-1,])])) / (1 + B), 4)
  
  pvalues.toreport <- ifelse(perm.pvalues < 0.001, "< 0.001", ifelse(perm.pvalues < 0.01, "< 0.01", ifelse(perm.pvalues < 0.05, "< 0.05","> 0.05")))
  
  plot(d[,x], d[,y], 
       main=" Scatterplot of permuted dimensions' inertia", 
       sub="large red dot: observed inertia", 
       xlab=paste0("inertia of permuted ", x," Dim. (p-value: ", pvalues.toreport[x], " [", perm.pvalues[x], "])"), 
       ylab=paste0("inertia of permuted ", y," Dim. (p-value: ", pvalues.toreport[y]," [", perm.pvalues[y], "])"), 
       cex.sub=0.75, 
       pch=20,
       col="#00000088") # hex code for 'black'; last two digits set the transparency
  
  par(new=TRUE)
  
  plot(d[1,x], d[1,y], xlim=c(min(d[,x]), max(d[,x])), ylim=c(min(d[,y]), max(d[,y])), pch=20, cex=1.5, col="red", xaxt = "n", xlab = "", ylab = "", sub = "") #add the observed inertia as a large red dot
  
  pvalues.df <- as.data.frame(perm.pvalues)
  
  row.names(pvalues.df) <- paste0("dim.", seq(1,table.dim))
  
  return(pvalues.df)
  }