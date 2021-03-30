pre_processing = function(spectra, ws, cut=T, emsc = F){
  wn = as.numeric(colnames(spectra))
  
  if (emsc){
    foo_emsc = EMSC(spectra, degree = 1)$corrected
    rownames(foo_emsc) = rownames(spectra)
    if (cut) {
      foo_emsc = foo_emsc[,wn<1900 & wn>700]
      colnames(foo_emsc) = wn[wn<1900 & wn>700]
    } else {
      colnames(foo_emsc) = wn
    }    
    
  } else {
    foo_sg = SavitzkyGolay(spectra, poly = 2, width = ws, deriv = 2)
    if (cut) {
      foo_sg = foo_sg[,wn<1900 & wn>700]
      colnames(foo_sg) = wn[wn<1900 & wn>700]
    } else {
      colnames(foo_sg) = wn
    }
    foo_emsc = EMSC(foo_sg, degree = 1)$corrected
    rownames(foo_emsc) = rownames(spectra)
    colnames(foo_emsc) = colnames(foo_sg)
  }
  

  return(foo_emsc)
}

DAL$sg2 = pre_processing(DAL$raw, 5)
TSK$sg2 = pre_processing(TSK$raw, 5)
MFM$sg2 = pre_processing(MFM$raw, 5)
FRE$sg2 = pre_processing(FRE$raw, 5)

DAL$emsc = pre_processing(DAL$raw, 5, T, T)
TSK$emsc = pre_processing(TSK$raw, 5, T, T)
MFM$emsc = pre_processing(MFM$raw, 5, T, T)
FRE$emsc = pre_processing(FRE$raw, 5, T, T)

saveRDS(DAL, here("data","output","DAL_spectra.rds"))
saveRDS(TSK, here("data","output","TSK_spectra.rds"))
saveRDS(MFM, here("data","output","MFM_spectra.rds"))
saveRDS(FRE, here("data","output","FRE_spectra.rds"))
