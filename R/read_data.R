library(here)

raw = read.csv(here("data", "input", "spectra", "DAL_spectra_raw.csv"), check.names = F, colClasses = "character")

spectra_raw = as.matrix((raw[-c(1,2),c(1:765)]))
spectra_raw = apply(spectra_raw, 2, as.numeric)
spectra_meta = raw[,c(766:773)]
