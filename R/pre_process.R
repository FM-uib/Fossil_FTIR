pre_processing = function(spectra, ws, cut=T){
  wn = as.numeric(colnames(spectra))
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
  return(foo_emsc)
}

DAL$sg2 = pre_processing(DAL$raw, 5)
TSK$sg2 = pre_processing(TSK$raw, 5)
MFM$sg2 = pre_processing(MFM$raw, 5)

saveRDS(DAL, here("data","output","DAL_spectra.rds"))
saveRDS(TSK, here("data","output","TSK_spectra.rds"))
saveRDS(MFM, here("data","output","DAL_spectra.rds"))