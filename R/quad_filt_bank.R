#' quad_filt_bank
#'
#' `quad_filt_bank` Create bank of FIR complex filters whose real and imaginary parts are in quadrature.
#'
#' @param cutoffs band cutoff frequencies
#' @param f sampling rate. 1 will be used if sample rate is not provided.
#' @param N filter order. Default is 8*f/(min(bandwidth))
#' @return Returns a list of filter banks
#'
#' Copyright Bertrand Delgutte, 1999-2000
#'
#' @source Translated from MATLAB code in Smith, Delgutte & Oxenham (2002).
#' @references Smith, Z. M., Delgutte, B., & Oxenham, A. J. (2002). Chimaeric sounds reveal dichotomies in auditory perception. Nature, 416(6876), 87–90. doi:10.1038/416087a
#' @seealso equal_xbm_bands
#' @export

quad_filt_bank <- function(cutoffs, f = 1, N = NULL){

  if(!is.numeric(cutoffs) || !is.vector(cutoffs)){
    stop("A numeric vector must be provided for frequency band cutoffs")
  }

  if(as.integer(f) != f || length(f) != 1){
    stop("Sample frequency must be a single whole number")
  }

  w <- 2 * cutoffs / f # Normalized frequency bands: 0 < w < 1
  if (is.null(N)){
    N <- round(8/min(diff(w)))

    # N%%2 == 1 if odd and 0 if even
    # Matlab code says this is to make sure N is even, but code was N%%2 != 1
    # was code wrong or comment wrong?
    if (N%%2 != 1){
      N <- N + 1
    }
  }

  nbands <- length(cutoffs) - 1

  # lowpass filter bandwidths
  bw <- diff(w)/2

  # frequency offsets
  fo <- w[1:nbands] + bw

  # time vector
  t <- (-N/2):(N/2)

  #b = matrix(rep(NaN, N+1 * nbands), N+2, nbands)

  make_filter <- function(i, N, t){
    bw_i <- bw[i]
    fo_i <- fo[i]

    signal::fir1(N, bw_i) * exp(1i*pi*fo_i*t)
  }

  b <- purrr::map(1:length(bw), make_filter, N = N, t = t)

#  b <- signal::fir1(N, bw[1]) * exp(1i*pi*fo[1]*t)

#  for(k in 2:length(bw)){
#    b_i <- signal::fir1(N, bw[k]) * exp(1i*pi*fo[k]*t)
#    b <- cbind(b, b_i)
#  }
  return(b)
}
