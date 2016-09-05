# Kaggle - Group Bimbo Inventory Demand

library(data.table)
library(xgboost)

train=fread('../input/train_full.csv')
test=fread('../input/test_full.csv')

#train[,V1:=NULL]
#test[,V1:=NULL]
train$id = 0
test$Demanda_uni_equil=0
data=rbind(train,test)
data[, target:=Demanda_uni_equil]
data[, Demanda_uni_equil:=NULL]
rm(val)
rm(train)


# We compute some additional features, which can be used to
# make the categorical IDs "more continuous". Other approaches that take into
# account the target values, such as sorting the IDs by their target mean 
# lead to overfitting.
frec_A=data[,.(frec_A=.N),by=.(Agencia_ID)]
data=merge(data,frec_A,by='Agencia_ID',all.x=T)
#data[,Agencia_ID:=NULL]

frec_R=data[,.(frec_R=.N),by=.(Ruta_SAK)]
data=merge(data,frec_R,by='Ruta_SAK',all.x=T)
data[,Ruta_SAK:=NULL]

frec_C=data[,.(frec_C=.N),by=.(Cliente_ID)]
data=merge(data,frec_C,by='Cliente_ID',all.x=T)

frec_P=data[,.(frec_P=.N),by=.(Producto_ID)]
data=merge(data,frec_P,by='Producto_ID',all.x=T)

# Since the loss function is RMSLE, we apply the convenient
# transformation of the target values.
data$target=log(data$target+1)

data_train=data[Semana==9,]
data_test=data[Semana>9,]
rm(data)

features=names(data_train)[!(names(data_train) %in% c('id',"target",'tst','V1'))] 

# Some rows are used as a validation set
valset=sample(nrow(data_train),50000)
val<-xgb.DMatrix(data=data.matrix(data_train[valset,features,with=FALSE]),
                  label=data.matrix(data_train[valser,target]),missing=NA)

dd = xgb.DMatrix(data=data.matrix(data_train[,features,with=FALSE]),
                 label=data.matrix(data_train[,target]),missing=NA)
rm(data_train)

clf <- xgb.train(params=list(  objective="reg:linear", 
                               booster = "gbtree",
                               eta=0.1, 
                               max_depth=10, 
                               subsample=0.8,
                               colsample_bytree=0.7) ,
                 data = dd, 
                 nrounds = 200, 
                 verbose = 1,
                 early_stopping_rounds    = 3,
                 maximize            = FALSE,
                 eval_metric = 'rmse',
                 watchlist = list(dval=val)
)

# Display the importance of each feature
xgb.importance(feature_names = features, model = clf)

