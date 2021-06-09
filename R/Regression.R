# Stepwise Regression

plot_coef = function(obj, comp = 1, coef = T){
  if (coef) {
    tmp = as.data.frame(obj[,,comp])
    n = ncol(tmp)
    tmp$wavenumbers = as.numeric(rownames(obj[,,comp]))
    for (i in 1:n) {
      tmp[,i] = tmp[,i] + i * max(abs(obj[,,comp]))
    }

  } else {
    tmp = as.data.frame(obj[,comp])
    n = ncol(tmp)
    tmp$wavenumbers = as.numeric(rownames(obj[,comp]))
    for (i in 1:n) {
      tmp[,i] = tmp[,i] + i * max(abs(obj[,comp]))
    }
  }

  tmp2 = melt(tmp, id.vars = c("wavenumbers"))
  colnames(tmp2) = c("wavenumbers", "treatment", "coef")
  g = ggplot(tmp2, aes(x = wavenumbers, y = coef, color = treatment)) + 
    geom_line() + xlim(1900,900) + theme_bw() +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank())
    
  return(g)
}

plot_scor = function(obj, meta){
  meta = meta[,c("Label","treatment")]
  tmp = as.data.frame(obj[])
  n = ncol(tmp)
  colnames(tmp) = c("PC1","PC2","PC3","PC4","PC5","PC6")
  tmp = cbind(tmp, meta)
  g = ggplot(tmp, aes_string(x = "PC1", y =  "PC2", color = "Label")) +
    geom_point(aes_string(shape = "treatment"), size = 1) + scale_shape_manual(values = c(17, 19, 15)) + theme_bw()
  return(g)
}

create_layout = function(h,w,x,y,split = 2){
  c = 1
  h = h-1
  w = w-1
  layout = list("t" = rep(0,x*y), "l" =rep(0,x*y), "b" =rep(0,x*y), "r" =rep(0,x*y))
  for (x in 1:x) {
    for (y in 1:y) {
      layout[["t"]][c] = y+(y-1)*(h)
      layout[["l"]][c] =x+(x-1)*(w)
      layout[["b"]][c] =y+h*y
      layout[["r"]][c] =x+w*x
      c = c+1
    }
  }
  attr(layout,  "class") = "patch_area"
  return(layout)
}

library(pls)

FRE$treat = "fresh"
DAL$treat = "fossil"
MFM$treat = "fossil"
TSK$treat = "fossil"

ord_plots = list()

# PLS fresh vs fossil

pls_data = filter(rbind(FRE,DAL,MFM,TSK), species == "Pinus")
pls_data$y = model.matrix(~treat -1, data = pls_data)
colnames(pls_data$sg2) = colnames(DAL$sg2)

results = plsr(y ~sg2, 6, data = pls_data, validation = "LOO")

ord_plots[["allcoresord"]] = plot_scor(results$scores, pls_data)
ord_plots[["allcorescoepc1"]] = plot_coef(results$coefficients)
ord_plots[["allcorescoepc2"]] = plot_coef(results$coefficients, 2)

# PLS SPT vs acet
## all cores
pls_data = filter(
  rbind(
    acet_sel(DAL),
    acet_sel(MFM),
    acet_sel(TSK)
    ), 
  species == "Pinus")
pls_data$y = model.matrix(~Label -1, data = pls_data)
colnames(pls_data$sg2) = colnames(DAL$sg2)

results = plsr(y ~ sg2, 6, data = pls_data, validation = "LOO")
ord_plots[["allcoressptacetord"]] = plot_scor(results$scores, pls_data)
ord_plots[["allcoressptacetpc1"]] = plot_coef(results$coefficients)
ord_plots[["allcoressptacetpc2"]] = plot_coef(results$coefficients,2)

## SPT core only lda on cores
pls_data = filter(
  rbind(
    acet_sel(DAL),
    acet_sel(MFM),
    acet_sel(TSK)
  ), 
  species == "Pinus" & treatment == "SPT")
pls_data$y = model.matrix(~Label -1, data = pls_data)
colnames(pls_data$sg2) = colnames(DAL$sg2)

results = plsr(y ~ sg2, 6, data = pls_data, validation = "LOO")
ord_plots[["allcoressptord"]] = plot_scor(results$scores, pls_data)
ord_plots[["allcoressptpc1"]] = plot_coef(results$coefficients)
ord_plots[["allcoressptpc2"]] = plot_coef(results$coefficients,2)

## acet core only lda on cores
pls_data = filter(
  rbind(
    acet_sel(DAL),
    acet_sel(MFM),
    acet_sel(TSK)
  ), 
  species == "Pinus" & treatment == "acet")
pls_data$y = model.matrix(~Label -1, data = pls_data)
colnames(pls_data$sg2) = colnames(DAL$sg2)

results = plsr(y ~ sg2, 6, data = pls_data, validation = "LOO")
ord_plots[["allcoresacetord"]] = plot_scor(results$scores, pls_data)
ord_plots[["allcoresacetpc1"]] = plot_coef(results$coefficients)
ord_plots[["allcoresacetsp2"]] = plot_coef(results$coefficients,2)

## SPT vs acet
pls_data = filter(
  rbind(
    acet_sel(DAL),
    acet_sel(MFM),
    acet_sel(TSK)
  ), 
  species == "Pinus")
pls_data$y = model.matrix(~ treatment -1, data = pls_data)
colnames(pls_data$sg2) = colnames(DAL$sg2)

results = plsr(y ~ sg2, 6, data = pls_data, validation = "LOO")
ord_plots[["allcorestreatmentord"]] = plot_scor(results$scores, pls_data)
ord_plots[["allcorestreatmentpc1"]] = plot_coef(results$coefficients)
ord_plots[["allcorestreatmentpc"]] = plot_coef(results$coefficients,2)

saveRDS(ord_plots, here("data","output","ordplot.rds"))


#plots

mean_plotsSM = readRDS(here("data","output","mean_plots.rds"))
mean_plots = readRDS(here("data","output","mean_plots_whole.rds"))

g = mean_plotsSM[[7]] + theme(
  axis.title.x = element_blank(),
  axis.ticks.x = element_blank(),
  axis.text.x = element_blank(),
  axis.ticks.y = element_blank(),
  axis.text.y = element_blank(),
  legend.position = "top") + 
  mean_plots[[1]] + theme(
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "none") + 
  mean_plots[[2]] + theme(
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "none") + 
  mean_plots[[3]] + theme(
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "bottom") + 
  plot_layout(design = create_layout(3,2,1,4)) + 
  plot_annotation(tag_levels = c("a"))

ggsave(here("figures","mean_plots.png"), plot = g, width = 17, height = 25, units = c("cm"))

##

plsr_plot = readRDS(here("data","output","ordplot.rds"))

design = "
1111112222
1111112222
1111113333
1111113333
4444445555
4444445555
4444446666
4444446666
"

#create_layout(3,2,2,4)

o = plsr_plot[[1]] +
  plsr_plot[[2]] +
  plsr_plot[[3]] +
  plsr_plot[[13]] +
  plsr_plot[[14]] +
  plsr_plot[[15]] +
  plot_layout(design = design , byrow = T, guides = "collect") + 
  plot_annotation(tag_levels = c("a"))

ggsave(here("figures","ord_plots.png"), plot = o, width = 20, height = 17, units = c("cm"))
