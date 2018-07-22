#' Chart of columns quality of the display
#'
#' This function allows you to calculate the quality of the display of the
#' column categories on pairs of selected dimensions.
#' @param data Name of the dataset (must be in dataframe format).
#' @param x First dimension for which the quality is calculated (x=1 by
#'   default).
#' @param y Second dimension for which the quality is calculated (y=2 by
#'   default).
#' @param categ.sort Logical value (TRUE/FALSE) which allows to sort the categories in
#'   descending order of quality of the representation on the subspace defined
#'   by the selected dimensions. TRUE is set by default.
#' @param cex.labls Adjust the size of the dot plot's labels.
#' @keywords cols.qlt
#' @export
#' @examples
#' data(greenacre_data)
#'
#' #Plots the quality of the display of the column categories on the 1&2 dimensions.
#' cols.qlt(greenacre_data, 1,2, categ.sort=TRUE)
#' 
#' @seealso \code{\link{rows.qlt}}
#' 
cols.qlt <- function (data, x=1, y=2, categ.sort=TRUE, cex.labls=0.75){
  cadataframe <- CA(data, graph=FALSE)
  df <- data.frame(qlt=cadataframe$col$cos2[,x]*100+cadataframe$col$cos2[,y]*100, labels=colnames(data))
  ifelse(categ.sort==TRUE, df.to.use <- df[order(-df$qlt),], df.to.use <- df)
  dotchart2(df.to.use$qlt, labels=df.to.use$labels, sort.=FALSE,lty=2, xlim=c(0, 100), cex.labels=cex.labls, xlab=paste("Column categories' quality of the display (% of inertia) on Dim.", x, "+",y))
}