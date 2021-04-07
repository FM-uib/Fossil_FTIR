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

plot_mean_spectra = function(data, subset_sel, spec_id, id.vars = c("ids", "treatment", "Label", "depth", "age", "type"), group = "type"){
  # subset data
  pdata = subset(data, eval(subset_sel))
  
  # melt data  
  pdata = melt(cbind(pdata[,id.vars],
               as.data.frame(pdata[,c(spec_id)])),
         id.vars=id.vars)
  
  # wavenumbers to numeric  
  pdata$variable = as.numeric(as.character(pdata$variable))
  
  #offset
  pdata = offset_spectra(pdata, group, "value", seq(0, by=max(pdata$value)/2, length.out = length(unique(pdata[,group]))))
  
  # mean data over groups
  pdata_mean = pdata %>%
    group_by_at(c(group, "variable")) %>%
    summarise(mean = mean(value))
  
  g = ggplot(pdata, aes_string("variable", "value", color = group))+
    geom_line(aes(group = ids), alpha = .15) + #mean spectra
    geom_line(data = pdata_mean, aes_string("variable", "mean", group = group), inherit.aes = F, size = 1) + #all spectra
    xlim(1800,900) +
    theme_bw() + theme(legend.position = "bottom")  
  
  return(g)
}  

df = data.frame(x = rep(c("a","b"), c(5,10)), y = c(runif(5,3,7),runif(10,)))

offset_spectra = function(data, variable, value, offset) {
  for (i in 1:length(unique(data[, variable]))) {
    data[data[, variable] == unique(data[, variable])[i],value] = data[data[, variable] == unique(data[, variable])[i],value] + offset[i]
  }
  return(data)
}

