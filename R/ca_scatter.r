#' Scatterplot visualization facility
#'
#' This function allows to get different types of CA scatterplots. It is just a wrapper for
#' functions from the 'ca' and 'FactoMineR' packages.
#' @param data Name of the contingency table (must be in dataframe format).
#' @param x First dimension to be plotted (x=1 by default).
#' @param y Second dimensions to be plotted (y=2 by default).
#' @param type Type of scatterplot to be returned (see examples).
#' @keywords caScatter
#' @export
#' @examples
#' data(greenacre_data)
#' 
#' # symmetric scatterplot for rows and columns
#' caScatter(greenacre_data, 1, 2, type=1) 
#'
#' # Standard Biplot; 2 plots are returned:
#' #one with row-categories vectors displayed, one for columns categories vectors.
#' caScatter(greenacre_data, 1, 2, type=2) 
#'
#' # scaterplot of row categories with groupings
#' #shown by different colors; scatterplot for column categories is also returned
#' caScatter(greenacre_data, 1, 2, type=3) 
#'
#' # 3D scatterplot with cluster tree for row categories;
#' #scatterplot for column categories is also returned.
#' caScatter(greenacre_data, 1, 2, type=4) 
#'
#' @seealso \code{\link{caPlot}} , \code{\link{caPercept}} , \code{\link{caPlus}} ,
#'   \code{\link[ca]{ca}} , \code{\link[FactoMineR]{plot.CA}} , \code{\link[FactoMineR]{HCPC}}
#'   
caScatter <- function(data,x=1,y=2,type){
  numb.dim.cols<-ncol(data)-1
  numb.dim.rows<-nrow(data)-1
  dimensionality <- min(numb.dim.cols, numb.dim.rows)
  res.ca <- ca(data)
  ca.factom <- CA(data, ncp=dimensionality, graph=FALSE)
  resclust.rows<-HCPC(ca.factom, nb.clust=-1, metric="euclidean", method="ward", order=TRUE, graph.scale="inertia", graph=FALSE, cluster.CA="rows")
  resclust.cols<-HCPC(ca.factom, nb.clust=-1, metric="euclidean", method="ward", order=TRUE, graph.scale="inertia", graph=FALSE, cluster.CA="columns")
  if (type==1) {
    plot.CA(ca.factom, axes=c(x,y), autoLab = "auto", cex=0.75)
  } else {
    if (type==2) {
      plot(res.ca, mass = FALSE, dim=c(x,y), contrib = "none", col=c("black", "red"), map ="rowgreen", arrows = c(FALSE, TRUE)) #for rows
      plot(res.ca, mass = FALSE, dim=c(x,y), contrib = "none", col=c("black", "red"), map ="colgreen", arrows = c(TRUE, FALSE)) #for columns
    } else {
      if (type==3) {
        plot(resclust.rows, axes=c(x,y), choice="map", draw.tree=FALSE, ind.names=TRUE, new.plot=TRUE)
        plot(resclust.cols, axes=c(x,y), choice="map", draw.tree=FALSE, ind.names=TRUE, new.plot=TRUE)
      } else {
        if (type==4) {
          plot(resclust.rows, axes=c(x,y), choice="3D.map", draw.tree=TRUE, ind.names=TRUE, new.plot=TRUE)
          plot(resclust.cols, axes=c(x,y), choice="3D.map", draw.tree=TRUE, ind.names=TRUE, new.plot=TRUE)
        } 
      }
    }
  }
}