library(factoextra)

load('ml/data/processed/telcom.Rdata')

gender <- as.data.frame(model.matrix(~gender-1,telcom))
Partner <- as.data.frame(model.matrix(~Partner-1,telcom))
Dependents <- as.data.frame(model.matrix(~Dependents-1,telcom))
PhoneService <- as.data.frame(model.matrix(~PhoneService-1,telcom))
MultipleLines <- as.data.frame(model.matrix(~MultipleLines-1,telcom))
InternetService <- as.data.frame(model.matrix(~InternetService-1,telcom))
OnlineSecurity <- as.data.frame(model.matrix(~OnlineSecurity-1,telcom))
OnlineBackup <- as.data.frame(model.matrix(~OnlineBackup-1,telcom))
DeviceProtection <- as.data.frame(model.matrix(~DeviceProtection-1,telcom))
TechSupport <- as.data.frame(model.matrix(~TechSupport-1,telcom))
StreamingTV <- as.data.frame(model.matrix(~StreamingTV-1,telcom))
StreamingMovies <- as.data.frame(model.matrix(~StreamingMovies-1,telcom))
Contract <- as.data.frame(model.matrix(~Contract-1,telcom))
PaperlessBilling <- as.data.frame(model.matrix(~PaperlessBilling-1,telcom))
PaymentMethod <- as.data.frame(model.matrix(~PaymentMethod-1,telcom))
Churn <- as.data.frame(model.matrix(~Churn-1,telcom))

num_features <- data.frame(telcom['SeniorCitizen'], telcom['tenure'], telcom['MonthlyCharges'], telcom['TotalCharges'])
num_features_scaled <- as.matrix(scale(num_features))


onehot_features <- cbind(
  gender, Partner, Dependents, PhoneService, MultipleLines, InternetService,
  OnlineSecurity, OnlineBackup, DeviceProtection, TechSupport, StreamingMovies,
  StreamingTV, Contract, PaperlessBilling, PaymentMethod, num_features_scaled)

pca_result <- prcomp(onehot_features, scale=T)
summary(pca_result)
fviz_screeplot(pca_result, addlabels = TRUE)
fviz_pca_ind(pca_result, label="none",addEllipses=TRUE, ellipse.level=0.95, 
             palette = c("#999999", "#E69F00"), habillage=telcom$Churn)

dataset <- cbind(onehot_features, Churn)
save(dataset, file = 'data/processed/dataset.Rdata')
