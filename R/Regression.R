# Stepwise Regression

plot_coef = function(obj, comp = 1){
  tmp = as.data.frame(obj[,,comp])
  n = ncol(tmp)
  tmp$wavenumbers = as.numeric(rownames(obj[,,comp]))
  for (i in 1:n) {
    tmp[,i] = tmp[,i] + i * max(abs(obj[,,comp]))
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

library(pls)

FRE$treat = "fresh"
DAL$treat = "fossil"
MFM$treat = "fossil"
TSK$treat = "fossil"

# PLS fresh vs fossil

pls_data = filter(rbind(FRE,DAL,MFM,TSK), species == "Pinus")
pls_data$y = model.matrix(~treat -1, data = pls_data)
colnames(pls_data$sg2) = colnames(DAL$sg2)

results = plsr(y ~sg2, 6, data = pls_data, validation = "LOO")
plot_coef(results$coefficients)
plot_scor(results$scores, pls_data)

# PLS SPT vs acet
## all cores
pls_data = filter(
  rbind(
    acet_sel(DAL),
    acet_sel(MFM),
    acet_sel(TSK)
    ), 
  species == "Pinus")
pls_data$y = model.matrix(~treatment -1, data = pls_data)
colnames(pls_data$sg2) = colnames(DAL$sg2)

results = plsr(y ~sg2, 6, data = pls_data, validation = "LOO")
plot_coef(results$coefficients)

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

results = plsr(y ~sg2, 6, data = pls_data, validation = "LOO")

## DAL
pls_data = filter(
  rbind(
    acet_sel(DAL)
  ), 
  species == "Pinus")
pls_data$y = model.matrix(~treatment -1, data = pls_data)
colnames(pls_data$sg2) = colnames(DAL$sg2)

results = plsr(y ~sg2, 6, data = pls_data, validation = "LOO")
plot(results, plottype = "coefficients")

