#' cochlear_map
#'
#' `cochlear_map` Convert frequency to distance along the basilar membrane using M.C. Liberman's
#' cochlear frequency map for the cat.
#'
#' @param f Frequency in Hz
#' @param Fmax Maximum frequency represented on the basilar membrane in Hz. By default, this is
#' 20 kHz, the value for the human cochlea. Setting Fmax to 57 kHz gives a map appropriate for the cat
#' cochlea.
#' @return Returns percent distance from apex of basilar membrane
#'
#' Copyright Bertrand Delgutte, 1999-2000
#'
#' @source Translated from MATLAB code in Smith, Delgutte & Oxenham (2002).
#' @references Smith, Z. M., Delgutte, B., & Oxenham, A. J. (2002). Chimaeric sounds reveal dichotomies in auditory perception. Nature, 416(6876), 87–90. doi:10.1038/416087a
#' @seealso inv_cochlear_map
#' @export

cochlear_map <- function(f, Fmax = 2e4){

  if (Fmax != 57e3){
    f <- f * inv_cochlear_map(100, 57e3)/Fmax
  }

  x <- log10(f/456 + .8)/.021

  return(x)
}
