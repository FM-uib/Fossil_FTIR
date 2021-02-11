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
DAL_outliers = pca_data[pca_data$ids %in% scores[scores[,2] < -.05 | scores[,1] > .035,"ids"],"ids"]

#plot_spectra(DAL,"raw",sel = expression(ids == "DAL_rx1GZ")) 
#plot_spectra(DAL,"raw",sel = expression(ids == "DAL_2cxp7"))
#plot_spectra(DAL,"raw",sel = expression(ids == "DAL_VzFi0"))
#plot_spectra(DAL,"raw",sel = expression(ids == "DAL_ANdP0"))

scores = pca_wrap(pca_data,"MFM")
ggplot(scores, aes(PC1, PC2, color = depth)) + geom_point(aes(shape = treatment), size = 2)
MFM_outliers = pca_data[pca_data$ids %in% scores[scores[,1] > 0.01 & scores[,2] < 0.0,"ids"],"ids"]

#plot_spectra(MFM,"raw",sel = expression(ids == "MFM_av67v")) 
#plot_spectra(MFM,"raw",sel = expression(ids == "MFM_OftM2"))
#plot_spectra(MFM,"raw",sel = expression(ids == "MFM_xogJr"))
#plot_spectra(MFM,"raw",sel = expression(ids == "MFM_hoG4y"))
#plot_spectra(MFM,"raw",sel = expression(ids == "MFM_5UNIJ"))

scores = pca_wrap(pca_data,"TSK")
ggplot(scores, aes(PC1, PC2, color = depth)) + geom_point(aes(shape = treatment), size = 2)
TSK_outliers = pca_data[pca_data$ids %in% scores[scores[,2] < -.2,"ids"],"ids"] # TSK_NM3bb

#plot_spectra(TSK,"raw",sel = expression(ids == "TSK_vvgOZ"))

outliers = c(DAL_outliers, MFM_outliers, TSK_outliers)

pca_data_outliers_removed = pca_data[!pca_data$ids %in% outliers,]
scores = pca_wrap(pca_data_outliers_removed,"DAL")
DAL_outliers_2 = pca_data_outliers_removed[pca_data_outliers_removed$ids 
                          %in% scores[scores[,2] < -.02,"ids"],"ids"]

scores = pca_wrap(pca_data_outliers_removed,"TSK")
TSK_outliers_2 = pca_data_outliers_removed[pca_data_outliers_removed$ids 
                                           %in% scores[scores[,2] < -.02,"ids"],"ids"]

outliers = c(outliers, DAL_outliers_2, TSK_outliers_2)

pca_data_outliers_removed_2 = pca_data[!pca_data$ids %in% outliers,]

scores = pca_wrap(pca_data_outliers_removed_2,"DAL")
scores = pca_wrap(pca_data_outliers_removed_2,"MFM")
scores = pca_wrap(pca_data_outliers_removed_2,"TSK")
