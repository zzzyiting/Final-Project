library(tidyverse)
library(caret)

set.seed(1)

load('ml/data/processed/dataset.Rdata')
dataset <- subset(dataset, select = -ChurnNo)


train_index = createDataPartition(dataset$ChurnYes, times = 1, p = 0.8, list = F)
train_data = dataset[train_index, ]
test_data = dataset[-train_index, ]

prop.table(with(train_data, table(ChurnYes)))
prop.table(with(test_data, table(ChurnYes)))
prop.table(with(dataset, table(ChurnYes)))
save(train_data, file = 'ml/processed/train_data.Rdata')
save(test_data, file = 'ml/data/processed/test_data.Rdata')
