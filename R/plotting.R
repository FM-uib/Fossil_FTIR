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
  pdata = offset_spectra(pdata, group, "value", seq(0, by=max(pdata$value)/3, length.out = length(unique(pdata[,group]))))
  
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

offset_spectra = function(data, variable, value, offset) {
  for (i in 1:length(unique(data[, variable]))) {
    data[data[, variable] == unique(data[, variable])[i],value] = data[data[, variable] == unique(data[, variable])[i],value] + offset[i]
  }
  return(data)
}

### creating Plots

DAL_acet = DAL_ %>%
  subset(species == "Pinus") %>%
  subset(depth %in% unique(DAL_[DAL_$treatment == "acet", "depth"]))

TSK_acet = TSK_ %>%
  subset(species == "Pinus") %>%
  subset(depth %in% unique(TSK_[TSK_$treatment == "acet", "depth"]))

MFM_acet = MFM_ %>%
  subset(species == "Pinus") %>%
  subset(depth %in% unique(MFM_[MFM_$treatment == "acet", "depth"]))

l = list()

l[["DAL SPT"]]= plot_mean_spectra(DAL_acet, subset_sel = expression(treatment == "SPT" & !depth == "130"), spec_id = "emsc", group = "depth")

l[["DAL acet"]] = plot_mean_spectra(DAL_acet, subset_sel = expression(treatment == "acet" & !depth == "130"), spec_id = "emsc", group = "depth")


l[["TSK SPT"]] = plot_mean_spectra(TSK_acet, subset_sel = expression(treatment == "SPT"), spec_id = "emsc", group = "depth")

l[["TSK acet"]] = plot_mean_spectra(TSK_acet, subset_sel = expression(treatment == "acet"), spec_id = "emsc", group = "depth")


l[["MFM SPT"]] = plot_mean_spectra(MFM_acet, subset_sel = expression(treatment == "SPT"), spec_id = "emsc", group = "depth")

l[["MFM acet"]] = plot_mean_spectra(MFM_acet, subset_sel = expression(treatment == "acet"), spec_id = "emsc", group = "depth")


l[["FRE"]] = plot_mean_spectra(FRE, subset_sel = expression(Label == "FRE"), spec_id = "emsc", group = "type")

saveRDS(l, here("data","output","mean_plots.rds"))

DAL_blank = DAL_ %>%
  subset(species == "Blank") %>%
  subset(depth %in% unique(DAL_[DAL_$treatment == "acet", "depth"]))

TSK_blank = TSK_ %>%
  subset(species == "Blank") %>%
  subset(depth %in% unique(TSK_[TSK_$treatment == "acet", "depth"]))

MFM_blank = MFM_ %>%
  subset(species == "Blank") %>%
  subset(depth %in% unique(MFM_[MFM_$treatment == "acet", "depth"]))

t = list()

t[["DAL blank SPT"]]= plot_mean_spectra(DAL_blank, subset_sel = expression(treatment == "SPT"), spec_id = "emsc", group = "depth")

t[["DAL blank acet"]] = plot_mean_spectra(DAL_blank, subset_sel = expression(treatment == "acet"), spec_id = "emsc", group = "depth")


t[["TSK blank SPT"]] = plot_mean_spectra(TSK_blank, subset_sel = expression(treatment == "SPT"), spec_id = "emsc", group = "depth")

t[["TSK blank acet"]] = plot_mean_spectra(TSK_blank, subset_sel = expression(treatment == "acet"), spec_id = "emsc", group = "depth")


t[["MFM blank SPT"]] = plot_mean_spectra(MFM_blank, subset_sel = expression(treatment == "SPT"), spec_id = "emsc", group = "depth")

t[["MFM blank acet"]] = plot_mean_spectra(MFM_blank, subset_sel = expression(treatment == "acet"), spec_id = "emsc", group = "depth")

saveRDS(t, here("data","output","blank_mean_plots.rds"))
