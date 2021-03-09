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

plot_spectra = function(data, spec_id, sel, id_vars = c("ids","type","species","treatment","orientation","depth")){
  pdata = subset(data, eval(sel))
  gdata = melt(cbind(pdata[,id_vars],
                       as.data.frame(pdata[,c(spec_id)])),
                 id.vars=id_vars)
  gdata$variable = as.numeric(as.character(gdata$variable))
  ggplot(gdata, aes(variable, value, color = depth, group = ids)) +
    geom_line() + scale_x_reverse() + xlim(1800,900) +
    theme_bw() + theme(legend.position = "bottom")
  #return(gdata)
}
