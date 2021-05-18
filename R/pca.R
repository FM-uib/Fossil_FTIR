pca_wrap = function(pca_data,sel){
  pca_data[pca_data$treatment == "","treatment"] = "SPT"
  if (is.expression(sel)) {
    pca_data = subset(pca_data, eval(sel))
  }
  pca_res = prcomp(pca_data$sg2, center = T, scale. = F)
  l = list()
  l[["pca"]] = pca_res
  l[["scores"]] =  as.data.frame(pca_res$x[,1:10]) %>%
    mutate(ids = rownames(.),
           core = pca_data$Label,
           treatment = pca_data$treatment,
           depth = pca_data$depth,
           age = pca_data$age,
           orientation = pca_data$orientation,
           type = pca_data$type
           )
  l[["loadings"]] = as.data.frame(pca_res$rotation[,1:10])
  return(l)
}

acet_sel = function(data){
  data_ = filter(data, (treatment == "acet" & species == "Pinus"))
  data__ = data %>%
    filter(species == "Pinus") %>%
    filter(depth %in% unique(data_$depth))
  return(rbind(data_, data__))
}

bootstrap_pca = function(data, target = "orientation", n = 50){
  res = rep(0,n)
  for (i in 1:n) {
    train = runif(length(data[,target])) <=.7
    while (sum(train) == length(train)) {
      train = runif(length(data[,target])) <=.7
    }
    
    #PCA
    l = pca_wrap(data[train, ],"")
    #LDA
    z = lda(as.formula(paste(target,"~ PC1 + PC2")) , l[["scores"]])
    res[i] =sum(predict(z, as.data.frame(predict(l[["pca"]],data[!train,"sg2"]))[,1:2])$class == data[!train,target])/length(data[!train, target])
    
  }
  
  return(mean(res))
}

#show difference between acetolyzed and non
#show depth differences
#show difference to fresh
#show difference in orientation fresh vs spt vs acet
# pca on each core with lda

#LDAs
#Orientation

# Cores selected for acet and SPT depths and fresh
pca_data = rbind(FRE,
                 acet_sel(DAL),
                 acet_sel(TSK),
                 acet_sel(MFM)
                 )

l = pca_wrap(pca_data, "")
pca_plot = ggplot(l[["scores"]], aes(PC1, PC2, color = core)) + geom_point(aes(shape = treatment), size = 4)

ggplot(l[["loadings"]], aes(as.numeric(rownames(l[["loadings"]])),PC1)) + geom_line()
ggplot(l[["loadings"]], aes(as.numeric(rownames(l[["loadings"]])),PC2)) + geom_line()

# LDA
pca_lda_treatment = data.frame(core = c("DAL","TSK","MFM"),
                               acc = c(bootstrap_pca(filter(DAL, species == "Pinus"), target = "treatment"),
                                       bootstrap_pca(filter(TSK, species == "Pinus"), target = "treatment"),
                                       bootstrap_pca(filter(MFM, species == "Pinus"), target = "treatment")
                               )*100,
                               n = c(nrow(filter(DAL, species == "Pinus")),
                                     nrow(filter(TSK, species == "Pinus")),
                                     nrow(filter(MFM, species == "Pinus"))
                               )
)

pca_lda_orientation = data.frame(core = c("FRE","DAL","DAL","TSK","TSK","MFM","MFM"),
                                 treatment = c("fresh","SPT","acet","SPT","acet","SPT","acet"),
                                 acc = c(bootstrap_pca(FRE),
                                         bootstrap_pca(filter(DAL,treatment == "SPT", species == "Pinus")),
                                         bootstrap_pca(filter(DAL,treatment == "acet", species == "Pinus")),
                                         bootstrap_pca(filter(TSK,treatment == "SPT", species == "Pinus")),
                                         bootstrap_pca(filter(TSK,treatment == "acet", species == "Pinus")),
                                         bootstrap_pca(filter(MFM,treatment == "SPT", species == "Pinus")),
                                         bootstrap_pca(filter(MFM,treatment == "acet", species == "Pinus"))
                                 )*100,
                                 n = c(nrow(FRE),
                                       nrow(filter(DAL,treatment == "SPT", species == "Pinus")),
                                       nrow(filter(DAL,treatment == "acet", species == "Pinus")),
                                       nrow(filter(TSK,treatment == "SPT", species == "Pinus")),
                                       nrow(filter(TSK,treatment == "acet", species == "Pinus")),
                                       nrow(filter(MFM,treatment == "SPT", species == "Pinus")),
                                       nrow(filter(MFM,treatment == "acet", species == "Pinus"))
                                 )
)

pca_lda_depth = data.frame(core = c("DAL","DAL","DAL","TSK","TSK","TSK","MFM","MFM","MFM"),
                           treatment = c("SPT all","SPT sel","acet","SPT all","SPT sel","acet","SPT all","SPT sel","acet"),
                           acc = c(
                             bootstrap_pca(filter(DAL,treatment == "SPT", species == "Pinus"), target = "depth"),
                             bootstrap_pca(filter(acet_sel(DAL),treatment == "SPT", species == "Pinus"), target = "depth"),
                             bootstrap_pca(filter(acet_sel(DAL),treatment == "acet", species == "Pinus"), target = "depth"),
                             bootstrap_pca(filter(TSK,treatment == "SPT", species == "Pinus"), target = "depth"),
                             bootstrap_pca(filter(acet_sel(TSK),treatment == "SPT", species == "Pinus"), target = "depth"),
                             bootstrap_pca(filter(acet_sel(TSK),treatment == "acet", species == "Pinus"), target = "depth"),
                             bootstrap_pca(filter(MFM,treatment == "SPT", species == "Pinus"), target = "depth"),
                             bootstrap_pca(filter(acet_sel(MFM),treatment == "SPT", species == "Pinus"), target = "depth"),
                             bootstrap_pca(filter(acet_sel(MFM),treatment == "acet", species == "Pinus"), target = "depth")
                           )*100,
                           n = c(
                             nrow(filter(DAL,treatment == "SPT", species == "Pinus")),
                             nrow(filter(acet_sel(DAL),treatment == "SPT", species == "Pinus")),
                             nrow(filter(acet_sel(DAL),treatment == "acet", species == "Pinus")),
                             nrow(filter(TSK,treatment == "SPT", species == "Pinus")),
                             nrow(filter(acet_sel(TSK),treatment == "SPT", species == "Pinus")),
                             nrow(filter(acet_sel(TSK),treatment == "acet", species == "Pinus")),
                             nrow(filter(MFM,treatment == "SPT", species == "Pinus")),
                             nrow(filter(acet_sel(MFM),treatment == "SPT", species == "Pinus")),
                             nrow(filter(acet_sel(MFM),treatment == "acet", species == "Pinus"))
                           ),
                           n_class = c(
                             length(unique(filter(DAL,treatment == "SPT", species == "Pinus")$depth)),
                             length(unique(filter(acet_sel(DAL),treatment == "SPT", species == "Pinus")$depth)),
                             length(unique(filter(acet_sel(DAL),treatment == "acet", species == "Pinus")$depth)),
                             length(unique(filter(TSK,treatment == "SPT", species == "Pinus")$depth)),
                             length(unique(filter(acet_sel(TSK),treatment == "SPT", species == "Pinus")$depth)),
                             length(unique(filter(acet_sel(TSK),treatment == "acet", species == "Pinus")$depth)),
                             length(unique(filter(MFM,treatment == "SPT", species == "Pinus")$depth)),
                             length(unique(filter(acet_sel(MFM),treatment == "SPT", species == "Pinus")$depth)),
                             length(unique(filter(acet_sel(MFM),treatment == "acet", species == "Pinus")$depth))
                           )
)

#####################################END###############################################

# more PCA code

# Cores SPT only 
pca_data = rbind(
                 filter(DAL, treatment == "SPT" & species == "Pinus"),
                 filter(TSK, treatment == "SPT" & species == "Pinus"),
                 filter(MFM, treatment == "SPT" & species == "Pinus")
                 )

l = pca_wrap(pca_data, "")
ggplot(l[["scores"]], aes(PC1, PC2, color = core)) + geom_point( size = 4)

ggplot(l[["loadings"]], aes(as.numeric(rownames(l[["loadings"]])),PC1)) + geom_line()
ggplot(l[["loadings"]], aes(as.numeric(rownames(l[["loadings"]])),PC2)) + geom_line()

# Cores acet only 
pca_data = rbind(
                 filter(DAL, treatment == "acet" & species == "Pinus"),
                 filter(TSK, treatment == "acet" & species == "Pinus"),
                 filter(MFM, treatment == "acet" & species == "Pinus")
                 )

l = pca_wrap(pca_data, "")
ggplot(l[["scores"]], aes(PC1, PC2, color = core)) + geom_point(aes(shape = orientation), size = 4)

ggplot(l[["loadings"]], aes(as.numeric(rownames(l[["loadings"]])),PC1)) + geom_line()
ggplot(l[["loadings"]], aes(as.numeric(rownames(l[["loadings"]])),PC2)) + geom_line()

#DAL
pca_data = rbind(
  filter(DAL, species == "Pinus")
)

l = pca_wrap(pca_data, "")
ggplot(l[["scores"]], aes(PC1, PC2, color = depth)) + geom_point(aes(shape = treatment
                                                                     ), size = 4)