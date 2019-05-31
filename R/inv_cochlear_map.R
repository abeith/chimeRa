#' inv_cochlear_map
#'
#' `inv_cochlear_map` Convert distance along the basilar membrane to frequency using M.C. Liberman's
#' cochlear frequency map for the cat.
#'
#' @param x Percent distance from apex of basilar membrane
#' @param Fmax Maximum frequency represented on the basilar membrane in Hz. By default, this is
#' 57 kHz, the value for the cat. Setting Fmax to 20 kHz gives a map appropriate for the human
#' cochlea.
#' @return Returns frequency in Hz
#'
#' Copyright Bertrand Delgutte, 1999-2000
#'
#' @source Translated from MATLAB code in Smith, Delgutte & Oxenham (2002).
#' @references Smith, Z. M., Delgutte, B., & Oxenham, A. J. (2002). Chimaeric sounds reveal dichotomies in auditory perception. Nature, 416(6876), 87–90. doi:10.1038/416087a
#' @seealso cochlear_map
#' @export

inv_cochlear_map <- function(x, Fmax = 2e4){
  f <- 456 * 10.^(.021 *x) - 364.8

  if(Fmax != 57e3){
    f <- f * Fmax/inv_cochlear_map(100, 57e3)
  }

  return(f)
}
