DAL$Label = "DAL"
TSK$Label = "TSK"
MFM$Label = "MFM"
pca_wrap = function(pca_data,sel){
  pca_data[pca_data$treatment == "","treatment"] = "SPT"
  if (!sel=="None") {
    pca_data = subset(pca_data, Label == sel)
  }
  pca_res = prcomp(pca_data$sg2)
  scores =  as.data.frame(pca_res$x[,1:10]) %>%
    mutate(ids = rownames(.),
           core = pca_data$Label,
           treatment = pca_data$treatment,
           depth = pca_data$depth
           )
  return(scores)
}
pca_data = rbind(subset(DAL, species == "Pinus"), 
                 subset(TSK, species == "Pinus"), 
                 subset(MFM, species == "Pinus"))
 #%>%
#melt(.,id.vars = "ids")
scores = pca_wrap(pca_data,"DAL")
ggplot(scores, aes(PC1, PC2, color = depth)) + geom_point(aes(shape = treatment), size = 4)
pca_data[pca_data$ids %in% scores[scores[,2] < -.05 | scores[,1] > .035,"ids"],1:10]
plot_spectra(DAL,"raw",sel = expression(ids == "DAL_rx1GZ")) 
plot_spectra(DAL,"raw",sel = expression(ids == "DAL_2cxp7"))
plot_spectra(DAL,"raw",sel = expression(ids == "DAL_VzFi0"))
plot_spectra(DAL,"raw",sel = expression(ids == "DAL_ANdP0"))

scores = pca_wrap(pca_data,"MFM")
ggplot(scores, aes(PC1, PC2, color = depth)) + geom_point(aes(shape = treatment), size = 2)
pca_data[pca_data$ids %in% scores[scores[,1] > 0.01 & scores[,2] < 0.0,"ids"],1:10]
plot_spectra(MFM,"raw",sel = expression(ids == "MFM_av67v")) 
plot_spectra(MFM,"raw",sel = expression(ids == "MFM_OftM2"))
plot_spectra(MFM,"raw",sel = expression(ids == "MFM_xogJr"))
plot_spectra(MFM,"raw",sel = expression(ids == "MFM_hoG4y"))
plot_spectra(MFM,"raw",sel = expression(ids == "MFM_5UNIJ"))

scores = pca_wrap(pca_data,"TSK")
ggplot(scores, aes(PC1, PC2, color = depth)) + geom_point(aes(shape = treatment), size = 2)
pca_data[pca_data$ids %in% scores[scores[,2] < -.2,"ids"],1:10] # TSK_NM3bb

plot_spectra(TSK,"raw",sel = expression(ids == "TSK_NM3bb"))
