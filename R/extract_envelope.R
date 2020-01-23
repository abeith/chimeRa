#' extract_envelope
#'
#' `extract_envelope` Extract amplitude envelope of a signal.
#'
#' @param x signal
#' @param f sampling rate in Hz
#' @param cutoffs band cutoff frequencies
#' @param complex if TRUE returns envelope and fine stucture as a complex valued vector
#' @return a list of amplitude envelopes for the specified cutoffs, or a vector if only two cutoffs are provided
#'
#'
#' @source Method based on MATLAB code in Smith, Delgutte & Oxenham (2002).
#' @references Smith, Z. M., Delgutte, B., & Oxenham, A. J. (2002). Chimaeric sounds reveal dichotomies in auditory perception. Nature, 416(6876), 87–90. doi:10.1038/416087a
#' @seealso make_band_chimeras
#' @export

extract_envelope <- function(x, f = 1, cutoffs, complex = FALSE){
  if(!is.vector(x) || !is.numeric(x)){
    stop("Signals must be specified as numerica vectors")
  }
  if(!is.vector(cutoffs) || (length(cutoffs) <= 1)){
    stop("Specify valid cutoff frequencies or filter banks")
  }

  # make filters
  b <- quad_filt_bank(cutoffs, f)

  nbanks <- length(b)

  # filter signals
  zfilt <- purrr::map(b, signal::fftfilt, x = x)

  # if(refilter){
  #
  #   x <- purrr::map2(zfilt, zfilt, cross_filters)
  #
  #   filt_back <- function(filt_bank, signal){
  #     signal_rev <- rev(signal)
  #     signal_filt <- signal::fftfilt(Re(filt_bank), signal_rev)
  #     rev(signal_rev)
  #   }
  #
  #   zfilt <- purrr::map2(b, x, filt_back)
  #
  # }

  if(complex){
    return(zfilt)
  }else{
    # get envelope
    env <- purrr::map(zfilt, abs)
    return(env)
  }
}
