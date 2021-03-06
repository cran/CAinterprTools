% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rows_qlt.R
\name{rows.qlt}
\alias{rows.qlt}
\title{Chart of rows quality of the display}
\usage{
rows.qlt(data, x = 1, y = 2, categ.sort = TRUE, cex.labls = 0.75)
}
\arguments{
\item{data}{Name of the dataset (must be in dataframe format).}

\item{x}{First dimension for which the quality is calculated (x=1 by
default).}

\item{y}{Second dimension for which the quality is calculated (y=2 by
default).}

\item{categ.sort}{Logical value (TRUE/FALSE) which allows to sort the categories in
descending order of quality of the representation on the subspace defined
by the selected dimensions. TRUE is set by default.}

\item{cex.labls}{Adjust the size of the dot plot's labels.}
}
\description{
This function allows you to calculate the quality of the display of the row
categories on pairs of selected dimensions.
}
\examples{
data(greenacre_data)

#Plots the quality of the display of the row categories on the 1&2 dimensions.
rows.qlt(greenacre_data,1,2,categ.sort=TRUE)

}
\seealso{
\code{\link{cols.qlt}}
}
\keyword{rows.qlt}
