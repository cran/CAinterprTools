% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rows_corr_scatter.R
\name{rows.corr.scatter}
\alias{rows.corr.scatter}
\title{Scatterplot for row categories correlation with dimensions}
\usage{
rows.corr.scatter(data, x = 1, y = 2, cex.labls = 3)
}
\arguments{
\item{data}{Name of the dataset (must be in dataframe format).}

\item{x}{First dimension for which the correlations are reported (x=1 by
default).}

\item{y}{Second dimension for which the correlations are reported (y=2 by
default).}

\item{cex.labls}{Adjust the size of the categories' labels}
}
\description{
This function allows to plot a scatterplot of the correlation (sqrt(COS2)) of
row categories with two selected dimensions. A diagonal line is a visual aid
to eyeball whether a category is actually more correlated (in relative terms)
to either of the two dimensions. The row categories' labels are coupled with
two + or - symbols within round brackets indicating to which side of the two
selected dimensions the correlation values that can be read off from the
chart are actually referring. The first symbol (i.e., the one to the left),
either + or -, refers to the first of the selected dimensions (i.e., the one
reported on the x-axis). The second symbol (i.e., the one to the right)
refers to the second of the selected dimensions (i.e., the one reported on
the y-axis).
}
\examples{
data(greenacre_data)

#Plots the scatterplot of the row categories correlation with dimensions 1&2.
rows.corr.scatter(greenacre_data,1,2)

}
\seealso{
\code{\link{rows.corr}} , \code{\link{cols.corr}} ,
  \code{\link{cols.corr.scatter}}
}
\keyword{rows.corr.scatter}
