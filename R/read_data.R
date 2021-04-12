repl = function(data, c, default=""){
  tmp = rep(default, nrow(data))
  for (i in c) {
    tmp[grep(i,data$Filename)] = i
  }
  return(tmp)
}

read_data_fresh = function() {
  raw = read.csv(here("data", "input", "spectra", "fresh_spectra_raw.csv"), 
                 check.names = F, 
                 colClasses = "character")
  spectra_raw = as.matrix((raw[,c(1:765)]))
  spectra_raw = apply(spectra_raw, 2, as.numeric)
  spectra_meta = raw[,c(766:ncol(raw))]
  ids = paste0("FRE_",stri_rand_strings(nrow(spectra_meta),5))
  
  while (sum(duplicated(ids))!=0) {
    ids = paste0(sc,"_",stri_rand_strings(nrow(spectra_meta),5))
  }
  spectra_meta$ids = ids
  spectra_meta$treatment = "fresh"
  spectra_meta$depth = 0
  spectra_meta$age = 2019
  spectra_meta$species = "Pinus"
  spectra_meta$orientation = repl(spectra_meta,c("_side"))
  
  rownames(spectra_raw) = ids
  
  data = spectra_meta
  data$raw = spectra_raw
  
  saveRDS(data, file = here("data","output",paste0("fresh_spectra.rds")))
}

read_data = function(sc){
  raw = read.csv(here("data", "input", "spectra", paste0(sc,"_spectra_raw.csv")), 
                 check.names = F, 
                 colClasses = "character")
  spectra_raw = as.matrix((raw[-c(1,2),c(1:765)]))
  spectra_raw = apply(spectra_raw, 2, as.numeric)
  spectra_meta = raw[-c(1,2),c(766:ncol(raw))]
  sc = strtrim(spectra_meta$`Sample Name`[1], 3)
  ids = paste0(sc,"_",stri_rand_strings(nrow(spectra_meta),5))
  
  # Check IDs for duplicates
  while (sum(duplicated(ids))!=0) {
    ids = paste0(sc,"_",stri_rand_strings(nrow(spectra_meta),5))
  }
  
  spectra_meta$ids = ids
  spectra_meta$species = repl(spectra_meta, c("Blank", "Pinus", "Abies", "BetCor", "Alnus"))
  spectra_meta$treatment = repl(spectra_meta, c("acet"),c("SPT"))
  spectra_meta$orientation = repl(spectra_meta,c("_side"))
  dpth = str_split_fixed(spectra_meta$`Sample Name`, "_", 4)
  if (sc == "MFM") {
    dpth = paste0(dpth[,2],"_",dpth[,3])
  } else {
    dpth = dpth[,2]
  }
  spectra_meta$depth = dpth
  
  rownames(spectra_raw) = ids
  
  data = spectra_meta
  data$raw = spectra_raw
  
  saveRDS(data, file = here("data","output",paste0(sc,"_spectra.rds")))
}
