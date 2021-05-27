# Stepwise Regression

library(pls)

pca_data = rbind(
                 acet_sel(DAL),
                 acet_sel(TSK),
                 acet_sel(MFM)
)
pca_data$core = model.matrix(~Label -1, data=pca_data)
plsr = plsr(model.matrix(~treatment -1, data=acet_sel(DAL))~sg2,
            6, data = acet_sel(DAL), 
            validation = "LOO")

DAL_ = acet_sel(DAL)
colnames(DAL_$sg2)=colnames(DAL$sg2)
plsr = plsr(model.matrix(~treatment -1, data=DAL_)~sg2,
           6, data = DAL_,
           validation = "LOO", center=F, scale=F)
plot(rownames(plsr$loadings), plsr$loadings[,1], type = "l")
plot(plsr$scores[,1], plsr$scores[,2])


colnames(pca_data$sg2)=colnames(DAL$sg2)
plsr = plsr(model.matrix(~Label -1, data=pca_data)~sg2,
            6, data = pca_data,
            validation = "LOO", center=F, scale=T)
plot(rownames(plsr$loadings), plsr$loadings[,1], type = "l")
plot(plsr$scores[,1], plsr$scores[,2])

reg_data = cbind(pca_data$Label, as.data.frame(pca_data$sg2))
colnames(reg_data)[1] = "core"
reg_data$core = model.matrix(~core -1, data=reg_data)
intercept_only = lm(core ~ 1, data=reg_data)
all = lm(core ~ ., data = reg_data)
both <- step(intercept_only, direction='backward', scope=formula(all))

data(yarn)
## Default methods:
yarn.pcr <- pcr(density ~ NIR, 6, data = yarn, validation = "CV")
