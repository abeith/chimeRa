#' psd_matched_noise
#'
#' `psd_matched_noise` Synthesize noise N having the same power spectrum and duration as original signal
#' X. Done by randomizing phase of Fourier spectrum.
#'
#' @param x A vector of a sound wave or an object of class "Wave"
#' @return Returns a vector of equal length or an object of class "Wave".
#'
#' Copyright Bertrand Delgutte, 1999-2000
#'
#' @source Translated from MATLAB code in Smith, Delgutte & Oxenham (2002).
#' @references Smith, Z. M., Delgutte, B., & Oxenham, A. J. (2002). Chimaeric sounds reveal dichotomies in auditory perception. Nature, 416(6876), 87–90. doi:10.1038/416087a
#' @importFrom stats fft runif
#' @export

psd_matched_noise <- function(x){

  make_noise <- function(signal){
    fft_x <- abs(fft(signal))
    noise <- exp(1i*2*pi*runif(length(signal)))

    n <- Re(pracma::ifft(fft_x * noise))

    return(n)
  }

  if(class(x) == "Wave"){
    x <- tuneR::mono(x)
    signal <- x@left
    n <- make_noise(signal)
    x@left <- n
    return(x)
  }else if(is.vector(x) && is.numeric(x) && length(x) > 0){
    signal <- x
    n <- make_noise(signal)
    return(n)
  }else{
    stop("x must be a numeric vector or wave")
  }
}
