#' make_band_chimeras
#'
#' `make_band_chimeras` Synthesize and store single or multi-band auditory chimeras from pair of original signals.
#'
#' @param x1,x2 original signals. Can be a numeric vector or object of class "Wave"
#' @param f sampling rate in Hz. Not required if using a "Wave" object. Must match for both signals.
#' @param n_bands Number of frequency bands used for synthesis. Can be scalar or vector, in which case one pair of
#' chimeras is synthesized for each element of n_bands.
#' @param savepath path to folder where chimeras will be saved
#' @param bitrate bitrate of signal. 16 used as default, if "Wave" object is used bitrate will be extracted from x1
#' @param refilter set to TRUE to filter again after exchange operation (default FALSE). Not tested.
#' @param save_waves if FALSE waves will be returned in a list
#' @param do_plot plot spectrograms of chimeras. Not implremented.
#' @return wav files of chimera pairs for each band in n_bands
#'
#' Copyright Bertrand Delgutte, 1999-2000
#'
#' @source Translated from MATLAB code in Smith, Delgutte & Oxenham (2002).
#' @references Smith, Z. M., Delgutte, B., & Oxenham, A. J. (2002). Chimaeric sounds reveal dichotomies in auditory perception. Nature, 416(6876), 87–90. doi:10.1038/416087a
#' @seealso multi_band_chimera equal_xbm_bands
#' @export

make_band_chimeras <- function(x1, x2 = "noise", f = 1, n_bands = c(1, 8, 32), savepath = ".",
                               bitrate = 16, refilter = FALSE, save_waves = TRUE, do_plot = FALSE){

  # function for getting signal, sample rate and bitrate from input
  get_wave <- function(x){
    if(class(x) == "Wave"){
      x <- tuneR::mono(x)
    }else if(is.vector(x) && is.numeric(x) && length(x) > 0){
      x <- tuneR::Wave(left = x, stereo = FALSE, samp.rate = f, bit = bitrate, pcm = FALSE)
    }else{
      stop(sprintf("%s must be a numeric vector or wave", deparse(substitute(x))))
    }
    return(x)
  }

  # get input as string
  x1_name <- deparse(substitute(x1))

  # get first input
  x1 <- get_wave(x1)
  f <- x1@samp.rate
  bitrate <- x1@bit
  x1 <- tuneR::normalize(x1, as.character(bitrate))

  # get second input
  if(is.character(x2)){
    if(x2 == "noise"){
      x2_name = "noise"
      x2 <- psd_matched_noise(x1)
      #x2 <- tuneR::normalize(x2, unit = as.character(bitrate))
    }
  }else{
    x2_name <- deparse(substitute(x2))
    x2 <- get_wave(x2)
    x2 <- tuneR::normalize(x2, unit = as.character(bitrate))
  }

  # Check sample rates match
  if(x1@samp.rate != x2@samp.rate){
    stop("Sample rates of x1 (%i) and x2 (%i) do not match", x1@samp.rate, x2@samp.rate)
  }

  # Refilter backwards
  if(refilter){
    refilt_code <- "_f2"
  }else{
    refilt_code <- ""
  }

  # Set filterbank limits
  Fmin <- 80
  # upper frequency of filterbank (.8 * Nyquist)
  Fmax <- .4*f

  # Make empty list to output objects if not saving as wav
  if(!save_waves) output <- list()

  # Loop through bands
  for (nb in n_bands){

    # determine band cutoffs equally spaced on basilar membrane
    cutoffs <- equal_xbm_bands(Fmin, Fmax, nb)

    # compute multi-band chimeras
    chimeras <- multi_band_chimera(x1@left, x2@left, f, cutoffs, refilter = FALSE)

    # Create strings for naming files
    chim1_name <- sprintf("%s_env+%s_fts-nb%i%s",
                          x1_name,
                          x2_name,
                          nb,
                          refilt_code)
    chim2_name <- sprintf("%s_env+%s_fts-nb%i%s",
                          x2_name,
                          x1_name,
                          nb,
                          refilt_code)

    # Round values if using bitrate other than 1
    if(bitrate != 1) {
      chimeras <- purrr::map(chimeras, round)
    }

    # Create Wave objects
    chim1_wave <- tuneR::Wave(left = chimeras[[1]], right = chimeras[[1]], samp.rate = f, bit = bitrate)
    chim2_wave <- tuneR::Wave(left = chimeras[[2]], right = chimeras[[2]], samp.rate = f, bit = bitrate)

    # Normalize signal
    chim1_wave <- tuneR::normalize(chim1_wave, unit = as.character(bitrate))
    chim2_wave <- tuneR::normalize(chim2_wave, unit = as.character(bitrate))

    # Plot spectrograms
    if(do_plot){
      sup_title <- sprintf("%i Bands", nb)

      # plot waveforms of original signals
      op <- graphics::par("mar")
      graphics::par(mar=c(2,2,1,1))
      graphics::layout(matrix(c(1,2,4,1,3,5),ncol=2),heights=c(1,3,3))
      graphics::plot.new()
      graphics::text(0.5,0.5,sup_title,cex=2,font=2)
      seewave::spectro(x1@left, f, scale = FALSE)
      graphics::title(sprintf("Original %s Sound", x1_name))
      seewave::spectro(x2@left, f, scale = FALSE)
      graphics::title(sprintf("Original %s Sound", x2_name))
      seewave::spectro(chim1_wave, scale = FALSE)
      graphics::title(sprintf("%s envelope and %s fine structure",
                              x1_name, x2_name))
      seewave::spectro(chim2_wave, scale = FALSE)
      graphics::title(sprintf("%s envelope and %s fine structure",
                              x2_name, x1_name))
      graphics::par(mar = op)
      graphics::layout(c(1,1))
    }

    # Save or assign output
    if(save_waves){
      tuneR::writeWave(chim1_wave, paste0(savepath, "/", chim1_name, ".wav"))
      tuneR::writeWave(chim2_wave, paste0(savepath, "/", chim2_name, ".wav"))
    }else{
      tmp_list <- list(chim1_wave, chim2_wave)
      names(tmp_list) <- c(chim1_name, chim2_name)
      output <- c(output, tmp_list)
      return(output)
    }
  }
}
