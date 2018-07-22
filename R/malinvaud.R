#' Malinvaud's test for significance of the CA dimensions
#'
#' This function allows you to perform the Malinvaud's test, which assesses the
#' significance of the CA dimensions.
#'
#' The function returns both a table in the R console and a plot. The former
#' lists relevant information, among which the significance of each CA
#' dimension. The dot chart graphically represents the p-value of each dimension;
#' dimensions are grouped by level of significance; a red reference lines
#' indicates the 0.05 threshold.
#' @param data Name of the dataset (must be in dataframe format).
#' @keywords malinvaud
#' @export
#' @examples
#' data(greenacre_data)
#'
#' #perform the Malinvaud test using the 'greenacre_data' dataset
#' #and store the output table in a object named 'res'
#' res <- malinvaud(greenacre_data)
#' 
#' @seealso \code{\link{sig.dim.perm.scree}} 
#' 
malinvaud <- function (data) {
  grandtotal <- sum(data)
  nrows <- nrow(data)
  ncols <- ncol(data)
  numb.dim.cols <- ncol(data) - 1
  numb.dim.rows <- nrow(data) - 1
  a <- min(numb.dim.cols, numb.dim.rows)
  labs <- c(1:a)
  res.ca <- CA(data, ncp = a, graph = FALSE)
  malinv.test.rows <- a
  malinv.test.cols <- 7
  malinvt.output <- as.data.frame(matrix(ncol = malinv.test.cols, nrow = malinv.test.rows))
  colnames(malinvt.output) <- c("K", "Dimension", "Eigenvalue", "Chi-square", "df", "p-value", "p-class")
  malinvt.output[,1] <- c(0:(a - 1))
  malinvt.output[,2] <- paste0("dim. ",c(1:a))
  for (i in 1:malinv.test.rows) {
    k <- -1 + i
    malinvt.output[i,3] <- res.ca$eig[i, 1]
    malinvt.output[i,5] <- (nrows - k - 1) * (ncols - k - 1)
  }
  malinvt.output[,4] <- rev(cumsum(rev(malinvt.output[, 3]))) * grandtotal
  pvalue <- pchisq(malinvt.output[,4], malinvt.output[,5], lower.tail = FALSE)
  malinvt.output[,6] <- pvalue
  malinvt.output[,7] <- ifelse(pvalue < 0.001, "p < 0.001", 
                               ifelse(pvalue < 0.01, "p < 0.01", 
                                      ifelse(pvalue < 0.05, "p < 0.05", 
                                             "p > 0.05")))
  dotchart2(pvalue, 
            labels = malinvt.output[,2], 
            groups=malinvt.output[,7],
            sort. = FALSE, 
            lty = 2, 
            xlim = c(0, 1), 
            main="Malinvaud's test for the significance of CA dimensions",
            xlab = paste("p-value"), 
            ylab = "Dimensions",
            cex.main=0.9,
            cex.labels=0.75)
  abline(v = 0.05, lty = 2, col = "RED")
  return(malinvt.output)
}