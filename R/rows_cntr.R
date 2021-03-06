#' Rows contribution chart
#'
#' This function allows to calculate the contribution of the row categories to
#' the selected dimension.
#'
#' The function displays the contribution of the categories as a dot plot. A
#' reference line indicates the threshold above which a contribution can be
#' considered important for the determination of the selected dimension. The
#' parameter categ.sort=TRUE sorts the categories in descending order of contribution
#' to the inertia of the selected dimension. At the left-hand side of the plot,
#' the categories' labels are given a symbol (+ or -) according to whether each
#' category is actually contributing to the definition of the positive or
#' negative side of the dimension, respectively. The categories are grouped into
#' two groups: 'major' and 'minor' contributors to the inertia of the selected
#' dimension. At the right-hand side, a legend (which is enabled/disabled using
#' the 'leg' parameter) reports the correlation (sqrt(COS2)) of the column
#' categories with the selected dimension. A symbol (+ or -) indicates with
#' which side of the selected dimension each column category is correlated.
#' @param data Name of the dataset (must be in dataframe format).
#' @param x Dimension for which the row categories contribution is returned (1st
#'   dimension by default).
#' @param categ.sort Logical value (TRUE/FALSE) which allows to sort the categories in
#'   descending order of contribution to the inertia of the selected dimension.
#'   TRUE is set by default.
#' @param corr.thrs Threshold above which the column categories correlation will
#'   be displayed in the plot's legend.
#' @param leg Enable (TRUE; default) or disable (FALSE) the legend at the
#'   right-hand side of the dot plot.
#' @param cex.labls Adjust the size of the dot plot's labels.
#' @param dotprightm Increases the empty space between the right margin of the
#'   dot plot and the left margin of the legend box.
#' @param cex.leg Adjust the size of the legend's characters.
#' @param leg.x.spc Adjust the horizontal space of the chart's legend. See more
#'   info from the 'legend' function's help (?legend).
#' @param leg.y.spc Adjust the y interspace of the chart's legend. See more info
#'   from the 'legend' function's help (?legend).
#' @keywords rows.cntr
#' @export
#' @examples
#' data(greenacre_data)
#'
#' #Plots the contribution of the row categories to the 2nd CA dimension,
#' #and also displays the contribnution to the total inertia.
#' #The categories are sorted in descending order of contribution to the inertia
#' #of the selected dimension.
#' rows.cntr(greenacre_data, 2, categ.sort=TRUE)
#' 
#' @seealso \code{\link{rows.cntr.scatter}} , \code{\link{cols.cntr}} ,
#'  \code{\link{cols.cntr.scatter}}
#' 
rows.cntr <- function (data, x = 1, categ.sort = TRUE, corr.thrs=0.0, leg=TRUE, cex.labls=0.75, dotprightm=5, cex.leg=0.6, leg.x.spc=1, leg.y.spc=1){
  
  corr=NULL
  
  nrows <- nrow(data)
  cadataframe <- CA(data, graph = FALSE)
  res.ca <- summary(ca(data))
  df <- data.frame(cntr = cadataframe$row$contrib[, x] * 10, cntr.tot = res.ca$rows[, 4], coord=cadataframe$row$coord[,x])
  df$labels <- ifelse(df$coord<0,paste(rownames(df), " -", sep = ""), paste(rownames(df), " +", sep = ""))
  df.col.corr <- data.frame(coord=cadataframe$col$coord[,x], corr=round(sqrt(cadataframe$col$cos2[,x]), 3))
  df.col.corr$labels <- ifelse(df.col.corr$coord<0,paste(rownames(df.col.corr), " - ", sep = ""), paste(rownames(df.col.corr), " + ", sep = ""))
  df.col.corr$specif <- paste0(df.col.corr$labels, "(", df.col.corr$corr, ")")
  ifelse(corr.thrs==0.0, df.col.corr <- df.col.corr, df.col.corr <- subset(df.col.corr, corr>=corr.thrs))
  ifelse(categ.sort == TRUE, df.to.use <- df[order(-df$cntr), ], df.to.use <- df)
  df.to.use$majcontr <- ifelse(df.to.use$cntr>round(((100/nrows) * 10)), "maj. contr.", "min. contr.")
  if(leg==TRUE){ 
    par(oma=c(0,0,0,dotprightm))
  } else {}
  dotchart2(df.to.use$cntr, 
            labels = df.to.use$labels, 
            groups=df.to.use$majcontr, 
            sort. = FALSE, 
            lty = 2, 
            xlim = c(0, 1000), 
            cex.labels=cex.labls, 
            xlab = paste("Row categories' contribution to Dim. ", x, " (in permills)"))
  if(leg==TRUE){ 
  par(oma=c(0,0,0,0))
  legend(x="topright", 
         legend=df.col.corr[order(-df.col.corr$corr),]$specif, 
         xpd=TRUE, 
         cex=cex.leg, 
         x.intersp = leg.x.spc, 
         y.intersp = leg.y.spc)
  par(oma=c(0,0,0,dotprightm))
  } else {}
  abline(v = round(((100/nrows) * 10), digits = 0), lty = 2, col = "RED")
  par(oma=c(0,0,0,0))
}
