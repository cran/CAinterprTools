#'Permuted significance of the CA total inertia
#'
#'This function calculates the permuted significance of CA total
#'inertia. Number of permutation is customizable (set at 999 by default). A frequency distribution 
#'histogram of permuted CA total inertia is produces and p.value of the observed total inertia is reported.
#'
#'@param data Name of the dataset (must be in dataframe format).
#'@param B Number of permutations (999 by default).
#'
#'@keywords sig.tot.inertia.perm
#'
#'@export
#'
#' @examples
#' data(greenacre_data)
#'
#' #Returns the frequency distribution histogram of the permuted total inertia
#' #(using 99 permutations). The observed total inertia and the 95th percentile
#' #of the permuted inertia are also displayed for testing the significance
#' #of the observed total inertia.
#' 
#' sig.tot.inertia.perm(greenacre_data, 99)
#'
#' @seealso \code{\link{sig.dim.perm.scree}} , \code{\link{sig.dim.perm}}
#'
sig.tot.inertia.perm <- function (data, B = 999) {
  rowTotals <- rowSums(data)
  
  colTotals <- colSums(data)
  
  obs.totinrt <- round(sum(ca(data)$rowinertia), 3)
  
  tot.inrt <- function(x) sum(ca(x)$rowinertia)
  
  perm.totinrt <- sapply(r2dtable(B, rowTotals, colTotals), tot.inrt)
  
  thresh <- round(quantile(perm.totinrt, c(0.95)), 5)
  
  perm.p.value <- (1 + length(which(perm.totinrt > obs.totinrt))) / (1 + B)
  
  p.to.report <- ifelse(perm.p.value < 0.001, "< 0.001", ifelse(perm.p.value < 0.01, "< 0.01", ifelse(perm.p.value < 0.05, "< 0.05", round(perm.p.value, 3))))
  
  hist(perm.totinrt, xlab = "",
       main = "Frequency distribution of Correspondence Analysis permuted total inertia", 
       sub = paste0("solid line: obs. inertia (", obs.totinrt, "); dashed line: 95th percentile of the permut. distrib. (",thresh, ")", "\np-value: ", p.to.report, " (",perm.p.value,")", " (number of permutations: ", B, ")"), 
       cex.sub = 0.8)
  
  abline(v = obs.totinrt)
  
  abline(v = thresh, lty = 2, col = "blue")
  
  rug(perm.totinrt, col="#0000FF") #hex code for 'blue'; last two digits set the transparency
}