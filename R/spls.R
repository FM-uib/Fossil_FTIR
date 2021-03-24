pca_data = rbind(subset(DAL, species == "Pinus"), 
                 subset(TSK, species == "Pinus"), 
                 subset(MFM, species == "Pinus"))

outliers =  c("DAL_rx1GZ", "DAL_2cxp7", "DAL_VzFi0", "DAL_ANdP0", "MFM_av67v", "MFM_OftM2", "MFM_xogJr", "MFM_hoG4y", "MFM_5UNIJ", "TSK_vvgOZ", "DAL_xgYjM", "TSK_m4hN6", "TSK_e04Go")

pca_data_outliers_removed_2 = pca_data[!pca_data$ids %in% outliers,]

pca_wrap = function(pca_data,sel){
  pca_data[pca_data$treatment == "","treatment"] = "SPT"
  if (is.expression(sel)) {
    pca_data = subset(pca_data, eval(sel))
  }
  pca_res = prcomp(pca_data$sg2)
  scores =  as.data.frame(pca_res$x[,1:10]) %>%
    mutate(ids = rownames(.),
           core = pca_data$Label,
           treatment = pca_data$treatment,
           depth = pca_data$depth,
           age = pca_data$age
    )
  loadings = pca_res$rotation
  return(list("scores" = scores, "loadings" = loadings))
}

M_wave = function(M){
  return(as.numeric(as.character(rownames(M))))
}

#PCA on SPT cores samples
#pca_data = subset(pca_data_outliers_removed_2, treatment == "SPT")

scores_cores = pca_wrap(pca_data_outliers_removed_2, expression(treatment == "SPT"))
ggplot(scores_cores[["scores"]], aes(PC1, PC2, color = core)) + geom_point(aes(), size = 4)

pca_data = pca_data_outliers_removed_2[pca_data_outliers_removed_2$depth %in% unique(pca_data_outliers_removed_2[pca_data_outliers_removed_2$treatment == "acet","depth"]),]

scores_treatment = pca_wrap(pca_data, "")

ggplot(scores_treatment, aes(PC1, PC2, color = core)) + geom_point(aes(shape = treatment), size = 4)

plsr_data = subset(pca_data_outliers_removed_2, treatment == "SPT")
M = model.matrix(~ Label -1, plsr_data)

splsFit <- caret:::splsda(plsr_data$sg2, M, 
                          K = 5, eta = .7)
preds = caret:::predict.splsda(splsFit, plsr_data$sg2)
levels(preds) = c("DAL","MFM","TSK")
confusionMatrix(preds,
                as.factor(plsr_data$Label))

predictions = caret:::predict.splsda(splsFit, plsr_data$sg2, type = "class", ncomp = 2)

## acet

plsr_data = pca_data_outliers_removed_2
plsr_data = plsr_data[plsr_data$depth %in% unique(plsr_data[plsr_data$treatment == "acet","depth"]),]
M = model.matrix(~ treatment -1, plsr_data)

splsFit <- caret:::splsda(plsr_data$sg2, M, 
                          K = 5, eta = .8)
preds = caret:::predict.splsda(splsFit, plsr_data$sg2)
levels(preds) = c("acet", "SPT")
confusionMatrix(preds,
                as.factor(plsr_data$treatment))

pls_res = cppls(M ~ plsr_data$sg2, ncomp = 4, validation = "LOO", scale = T)
biplot(pls_res, which = c("y"))


spls.res = plsgenomics::spls(plsr_data$sg2, as.numeric(as.factor(plsr_data$treatment)), .8, 5)
