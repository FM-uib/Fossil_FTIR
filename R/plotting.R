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
  pdata = offset_spectra(pdata, group, "value", rep(c(0, .2), length(levels(pdata[,group])))) #
  
  # mean data over groups
  pdata_mean = pdata %>%
    group_by_at(c(group, "variable")) %>%
    summarise(mean = mean(value), Label = first(Label))
  
  
  g = ggplot(pdata, aes_string("variable", "value", color = group))+
    geom_line(aes(group = ids), alpha = .15) + facet_wrap(vars(Label), ncol = 2) +#mean spectra
    geom_line(data = pdata_mean, aes_string("variable", "mean", group = group), inherit.aes = F, size = 1) + facet_wrap(vars(Label), ncol = 2) +#all spectra
    scale_x_reverse(name = "wavenumbers",limits = c(1800,900), breaks = seq(1800,900,-100)) +
    scale_y_continuous(name = "Absorbance (arbitrary units)",limits = c(0,0.5), breaks = NULL) +
    scale_color_manual(name = NULL,
                       values = c("#e66101","#ff6c01",
                                  "#ac7d43","#fdb863",
                                  "#b2abd2","#d8d0ff",
                                  "#5e3c99","#9d64ff"),
                       labels = c("DAL non acet.","DAl acet.",
                                  "MFM non acet.","MFM acet.",
                                  "TSK non acet.","TSK acet.",
                                  "Bergen","Innsbruck")) +
    theme_bw() + 
    theme(legend.position = "bottom",
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank()
          ) +
    guides(colour = guide_legend(override.aes = list(alpha = 1)))
  
  return(g)
}  

offset_spectra = function(data, variable, value, offset) {
  off2 = c(0.11,0.11,0.11,0.11,0.22,0.22,.55,.55)
  for (i in 1:length(levels(data[, variable]))) {
    data[data[, variable] == levels(data[, variable])[i],value] = data[data[, variable] == levels(data[, variable])[i],value] + offset[i] - off2[i]
  }
  return(data)
}

### Treatments mean plots over whole core

l = list()

FRE = FRE %>%
  mutate(off = paste(Label, type, sep = "_"))
DAL = DAL %>%
  mutate(off = paste(Label, treatment, sep = "_"))
MFM = MFM %>%
  mutate(off = paste(Label, treatment, sep = "_"))
TSK = TSK %>%
  mutate(off = paste(Label, treatment, sep = "_"))
  
data = rbind(
  FRE,
  DAL,
  MFM,
  TSK
)

data= data%>%
  mutate(off = factor(off, levels = c(
    "DAL_SPT","DAL_acet",
    "MFM_SPT","MFM_acet",
    "TSK_SPT","TSK_acet",
    "FRE_Bergen","FRE_Innsbruck"))) %>%
  arrange(off)
colnames(data$emsc) = colnames(FRE$emsc)

vlines = data.frame(wn = c(1745, 1710, 1605, 1515, 1170, 1040),
                    label = as.character(c(1745, 1710, 1605, 1515, 1170, 1040)),
                    y = rep(0.01, 6))

l = plot_mean_spectra(data, expression(species == "Pinus"), spec_id = "emsc", group = "off", id.vars = c("ids", "treatment", "Label", "depth", "age", "type", "off"))

l = l + #ylim(.05, 2.3) +
  geom_vline(data = vlines, aes(xintercept = wn), linetype = "dashed", alpha = .3) +
  geom_text(data = vlines, aes(wn, y, label = label), angle = 90, inherit.aes = F)

ggsave(here("figures","mean_plots.png"), l, width = 17, height = 17,units = "cm")
saveRDS(l, here("data","output","mean_plots_whole.rds"))

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
