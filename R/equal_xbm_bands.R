#' equal_xbm_bands
#'
#' `equal_xbm_bands` Divide frequency interval into N bands of equal width along
#' the human basilar membrane. Based on M.C. Liberman's cochlear frequency map
#' for the cat scaled to match human frequency range of hearing.
#'
#' @param fmin minimum frequency in Hz
#' @param fmax maximum frequency in Hz
#' @param N number of frequency bands
#' @return Returns a vector of band cutoff frequencies in Hz
#'
#' Copyright Bertrand Delgutte, 1999-2000
#' @source Translated from MATLAB code in Smith, Delgutte & Oxenham (2002).
#' @references Smith, Z. M., Delgutte, B., & Oxenham, A. J. (2002). Chimaeric sounds reveal dichotomies in auditory perception. Nature, 416(6876), 87–90. doi:10.1038/416087a
#'
#' @seealso inv_cochlear_map
#' @export

equal_xbm_bands <- function(fmin, fmax, N){

  xmin <- cochlear_map(fmin, 20000)
  xmax <- cochlear_map(fmax, 20000)

  dx <- (xmax-xmin)/N
  x <- seq(xmin, xmax, dx)

  fco <- inv_cochlear_map(x, 20000)

  return(fco)
}
