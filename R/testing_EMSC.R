foo_spectra = subset(MFM, species == "Pinus" & depth == "A4DL_130-132")
foo_blank = subset(MFM, species == "Blank" & depth == "A4DL_130-132")
sel = c(as.numeric(colnames(foo_spectra$raw)) < 1300)

emsc_spectra = EMSC(foo_spectra$raw[,sel], interferent = colMeans(foo_blank$raw[,sel]), degree = 1)

plot(as.numeric(colnames(emsc_spectra$X)),emsc_spectra$X[1,], type = "l" )
plot(as.numeric(colnames(emsc_spectra$X)),emsc_spectra$corrected[1,], type = "l" )
plot(as.numeric(colnames(emsc_spectra$X)),colMeans(foo_blank$raw[,sel]), type = "l" )

emsc_spectra = EMSC(foo_spectra$raw[,sel], interferent = colMeans(MFM$raw[MFM$species == "Blank",sel]), degree = 1)

plot(as.numeric(colnames(emsc_spectra$X)),emsc_spectra$X[1,], type = "l" )
plot(as.numeric(colnames(emsc_spectra$X)),emsc_spectra$corrected[1,], type = "l" )
plot(as.numeric(colnames(emsc_spectra$X)),colMeans(foo_blank$raw[,sel]), type = "l" )

plot_spectra(MFM, "raw", expression(species=="Blank" & depth == unique(MFM$depth)[1]))

#' check difference in blanks between SPT and acet
#' 
#' Pre-process:
#'
#' 1. linear EMSC of raw spectra with meaned Blanks
#' 2. SG
#' 
#' Analysis:
#' PLSR depth regression or class
#' PCA on all cores 
#' PCA acet vs SPT
#' 
