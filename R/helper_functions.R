pre_processing = function(spectra, ws){
  foo_sg = SavitzkyGolay(spectra, poly = 2, width = 5, deriv = 2)
  foo_emsc = EMSC(foo_sg, degree = 1)$corrected
  rownames(foo_emsc) = rownames(spectra)
  colnames(foo_emsc) = colnames(spectra)
  return(foo_emsc)
}

plot_spectra = function(data, spec_id, sel, id_vars = c("ids","type","species","treatment","orientation","depth")){
  pdata = subset(data, eval(sel))
  gdata = melt(cbind(pdata[,id_vars],
                       as.data.frame(pdata[,c(spec_id)])),
                 id.vars=id_vars)
  gdata$variable = as.numeric(as.character(gdata$variable))
  ggplot(gdata, aes(variable, value, color = depth, group = ids)) +
    geom_line() + scale_x_reverse() +
    theme_bw()
  #return(gdata)
}
