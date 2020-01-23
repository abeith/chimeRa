#' multi_band_chimera
#'
#' `multi_band_chimera` Synthesize pair of multi-band "auditory chimeras" by
#' dividing each signal into frequency bands, then interchanging envelope and
#' fine structure in each band using Hilbert transforms.
#'
#' @param x1,x2 original signals
#' @param cutoffs band cutoff frequencies
#' @param fb filter banks. Not required if cutoffs are specified
#' @param f sampling rate in Hz
#' @param refilter set to 1 to filter again after exchange operation (default 0)
#' @return a list of two auditory chimeras. The first has envelope of x1 and the
#' fine structure of x2 and the second has the envelope of x2 and the fine structure
#' of x1
#'
#' Copyright Bertrand Delgutte, 1999-2000
#'
#' @source Translated from MATLAB code in Smith, Delgutte & Oxenham (2002).
#' @references Smith, Z. M., Delgutte, B., & Oxenham, A. J. (2002). Chimaeric sounds reveal dichotomies in auditory perception. Nature, 416(6876), 87–90. doi:10.1038/416087a
#' @seealso quad_filt_bank
#' @export

multi_band_chimera <- function(x1, x2, f = 1, cutoffs, fb = NULL, refilter = 0){
  if(!is.vector(x1) || !is.vector(x2) || !is.numeric(x1) || !is.numeric(x2)){
    stop("Signals must be specified as numerica vectors")
  }
  if(!is.vector(cutoffs) || (length(cutoffs) <= 1) && is.null(fb)){
    stop("Specify valid cutoff frequencies or filter banks")
  }

  # pad with zeroes to match lengths
  if(length(x1) < length(x2)){
  	x1 <- c(x1, rep(0, length(x2) - length(x1)))
  } else if(length(x2) < length(x1)){
  	x2 <- c(x2, rep(0, length(x1) - length(x2)))
  }

  # Because the Hilbert transform and the filter bank are both linear operations,
  # they commute and associate.  We create a bank of complex FIR filters whose
  # real and imaginary parts are in quadrature (cos and sin).  This complex filter
  # is directly applied the original signals. The Hilbert envelope in each band
  # is the absolute value of the complex filter output.
  # This approach avoids computation of large FFTs as in Matlab's 'hilbert'.

  # make filters if not provided
  if(!is.null(fb)){
    b <- fb
  }else{
    b <- quad_filt_bank(cutoffs, f)
  }

  nbanks <- length(b)

  # filter signals
  zfilt1 <- purrr::map(b, signal::fftfilt, x = x1)
  zfilt2 <- purrr::map(b, signal::fftfilt, x = x2)

  # cross filters
  e1_fs2 <- purrr::map2(zfilt1, zfilt2, cross_filters)
  e2_fs1 <- purrr::map2(zfilt2, zfilt1, cross_filters)

  # refilter backward to avoid delay accumulation

  if (refilter){

    filt_back <- function(filt_bank, signal){
      signal_rev <- rev(signal)
      signal_filt <- signal::fftfilt(Re(filt_bank), signal_rev)
      rev(signal_rev)
    }

    e1_fs2 <- purrr::map2(b, e1_fs2, filt_back)
    e2_fs1 <- purrr::map2(b, e2_fs1, filt_back)
  }

  # add bands together
  e1_fs2 <- e1_fs2 %>%
    dplyr::bind_cols() %>%
    rowSums()

  e2_fs1 <- e2_fs1 %>%
    dplyr::bind_cols() %>%
    rowSums()

  chimeras <- list(e1_fs2,
                   e2_fs1)

  return(chimeras)
}

cross_filters <- function(filt1, filt2){

  abs(filt1) * cos(angle(filt2))
}

angle <- function(x){
  atan2(Im(x), Re(x))
}
