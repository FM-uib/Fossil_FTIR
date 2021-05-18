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

# Cores selected for acet and SPT depths and fresh
pca_data = rbind(FRE,
                 acet_sel(DAL),
                 acet_sel(TSK),
                 acet_sel(MFM)
                 )

l = pca_wrap(pca_data, "")
ggplot(l[["scores"]], aes(PC1, PC2, color = core)) + geom_point(aes(shape = treatment), size = 4)

ggplot(l[["loadings"]], aes(as.numeric(rownames(l[["loadings"]])),PC1)) + geom_line()
ggplot(l[["loadings"]], aes(as.numeric(rownames(l[["loadings"]])),PC2)) + geom_line()


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



#show difference between acetolyzed and non
#show depth differences
#show difference to fresh
#show difference in orientation fresh vs spt vs acet
# pca on each core with lda

#LDAs
#Orientation

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

pca_data = filter(DAL,treatment == "acet", species == "Pinus")
train = runif(length(pca_data$orientation)) <=.7

l = pca_wrap(pca_data[train,], "")

z = lda(orientation ~ PC1 + PC2, l[["scores"]])
sum(predict(z, as.data.frame(predict(l[["pca"]],pca_data[!train,"sg2"]))[,1:2])$class == pca_data$orientation[!train])/length(pca_data$orientation[!train])



# LDA
z = lda(orientation ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, l[["scores"]], CV = T)

sum(z$class == l[["scores"]]$orientation)/length(z$class)


#old
pca_data = rbind(subset(DAL, species == "Pinus"), 
                 subset(TSK, species == "Pinus"), 
                 subset(MFM, species == "Pinus"))
 #%>%
#melt(.,id.vars = "ids")
scores = pca_wrap(pca_data, expression(Label == "DAL"))
ggplot(scores, aes(PC1, PC2, color = depth)) + geom_point(aes(shape = treatment), size = 4)
DAL_outliers = pca_data[pca_data$ids %in% scores[scores[,2] < -.05 | scores[,1] > .035,"ids"],"ids"]

#plot_spectra(DAL,"raw",sel = expression(ids == "DAL_rx1GZ")) 
#plot_spectra(DAL,"raw",sel = expression(ids == "DAL_2cxp7"))
#plot_spectra(DAL,"raw",sel = expression(ids == "DAL_VzFi0"))
#plot_spectra(DAL,"raw",sel = expression(ids == "DAL_ANdP0"))

scores = pca_wrap(pca_data, expression(Label == "MFM"))
ggplot(scores, aes(PC1, PC2, color = depth)) + geom_point(aes(shape = treatment), size = 2)
MFM_outliers = pca_data[pca_data$ids %in% scores[scores[,1] > 0.01 & scores[,2] < 0.0,"ids"],"ids"]

#plot_spectra(MFM,"raw",sel = expression(ids == "MFM_av67v")) 
#plot_spectra(MFM,"raw",sel = expression(ids == "MFM_OftM2"))
#plot_spectra(MFM,"raw",sel = expression(ids == "MFM_xogJr"))
#plot_spectra(MFM,"raw",sel = expression(ids == "MFM_hoG4y"))
#plot_spectra(MFM,"raw",sel = expression(ids == "MFM_5UNIJ"))

scores = pca_wrap(pca_data, expression(Label == "TSK"))
ggplot(scores, aes(PC1, PC2, color = depth)) + geom_point(aes(shape = treatment), size = 2)
TSK_outliers = pca_data[pca_data$ids %in% scores[scores[,2] < -.2,"ids"],"ids"] # TSK_NM3bb

#plot_spectra(TSK,"raw",sel = expression(ids == "TSK_vvgOZ"))

outliers = c(DAL_outliers, MFM_outliers, TSK_outliers)

pca_data_outliers_removed = pca_data[!pca_data$ids %in% outliers,]
scores = pca_wrap(pca_data_outliers_removed, expression(Label == "DAL"))
DAL_outliers_2 = pca_data_outliers_removed[pca_data_outliers_removed$ids 
                          %in% scores[scores[,2] < -.02,"ids"],"ids"]

scores = pca_wrap(pca_data_outliers_removed, expression(Label == "TSK"))
TSK_outliers_2 = pca_data_outliers_removed[pca_data_outliers_removed$ids 
                                           %in% scores[scores[,2] < -.02,"ids"],"ids"]

outliers = c(outliers, DAL_outliers_2, TSK_outliers_2)

pca_data_outliers_removed_2 = pca_data[!pca_data$ids %in% outliers,]

scores = pca_wrap(pca_data_outliers_removed_2, expression(Label == "DAL"))
scores = pca_wrap(pca_data_outliers_removed_2, expression(Label == "MFM"))
scores = pca_wrap(pca_data_outliers_removed_2, expression(Label == "TSK"))

ggplot(scores, aes(PC1, PC2, color = depth)) + geom_point(aes(shape = treatment), size = 2)

# All cores
scores = pca_wrap(pca_data_outliers_removed_2, "None")

ggplot(scores, aes(PC1, PC2, color = core)) + geom_point(aes(shape = treatment), size = 2)

scores = pca_wrap(pca_data_outliers_removed_2, expression(!treatment == "acet"))

#PLSR depth regression
library(pls)
plsr_data = subset(pca_data_outliers_removed_2, Label == "DAL")
plsr_data$depth = as.numeric(plsr_data$depth)
pls_res = plsr(depth ~ sg2, data = plsr_data, ncomp = 4, validation = "LOO")
plot(pls_res)

plsr_data = subset(pca_data_outliers_removed_2, Label == "TSK" & treatment == "SPT")
#plsr_data$depth = as.numeric(plsr_data$depth)
pls_res = plsr(age ~ sg2, data = plsr_data, ncomp = 4, validation = "LOO")
plot(pls_res)

plsr_data = subset(pca_data_outliers_removed_2, treatment == "SPT")
M = model.matrix(~ Label -1, plsr_data)
pls_res = plsr(M ~ sg2, data = plsr_data, ncomp = 4, validation = "LOO")
biplot(pls_res, which = c("y"))

#SPLS
library(spls)

sparse.cv = cv.spls(plsr_data$sg2, M, K = c(1:10), eta = seq(.8,.99,.01))
sparse.spsl = spls:::spls(plsr_data$sg2, M, sparse.cv$K.opt, sparse.cv$eta.opt, scale.x = F)

#caret:::splsda

splsFit <- caret:::splsda(plsr_data$sg2, M, 
                          K = 5, eta = .9)
preds = caret:::predict.splsda(splsFit, plsr_data$sg2)
levels(preds) = c("DAL","MFM","TSK")
confusionMatrix(preds,
                as.factor(plsr_data$Label))

## Not run: 
data(mdrr)
set.seed(1)
inTrain <- sample(seq(along = mdrrClass), 450)

nzv <- nearZeroVar(mdrrDescr)
filteredDescr <- mdrrDescr[, -nzv]

training <- filteredDescr[inTrain,]
test <- filteredDescr[-inTrain,]
trainMDRR <- mdrrClass[inTrain]
testMDRR <- mdrrClass[-inTrain]

preProcValues <- preProcess(training)

trainDescr <- predict(preProcValues, training)
testDescr <- predict(preProcValues, test)

## Using spls:
## (As of 11/09, the spls package now has a similar function with
## the same mane. To avoid conflicts, use caret:::splsda to 
## get this version)

splsFit <- caret:::splsda(as.matrix(trainDescr), trainMDRR, 
                          K = 5, eta = .9)

confusionMatrix(caret:::predict.splsda(splsFit, testDescr),
                testMDRR)

## End(Not run)
