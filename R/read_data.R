library(here)
library(stringi)

raw = read.csv(here("data", "input", "spectra", "DAL_spectra_raw.csv"), check.names = F, colClasses = "character")

spectra_raw = as.matrix((raw[-c(1,2),c(1:765)]))
spectra_raw = apply(spectra_raw, 2, as.numeric)
spectra_meta = raw[-c(1,2),c(766:773)]

ids = stri_rand_strings(nrow(spectra_meta),5)
# Check IDs for duplicates
while (sum(duplicated(ids))!=0) {
  ids = stri_rand_strings(nrow(spectra_meta),5)
}

spectra_meta$ids = ids
rownames(spectra_raw) = ids

DAL = spectra_meta
DAL$raw = spectra_raw
DAL = DAL[,c("ids","type","species","depth","raw")]

saveRDS(DAL, file = here("data","output","DAL_spectra.rds"))
