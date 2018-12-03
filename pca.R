load("./zipCode.RData")
load("./zipCodeTestData.RData")

level=array(0,dim=c(90))

for(i in 1:90) {
  if(i < 30) {
    level[i] = 1
  } else if(i < 60) {
    level[i] = 3
  } else {
    level[i] = 8
  }
}

testValueMATRIX = matrix(0, 20, 256)

for(i in 1:20) {
  for(j in 1:256) {
    testValueMATRIX[i, j] = test.X[i, j]
  }
}

trainX<-train.X

PCA <- function(m, n, mar, k) {
  C = 1/n * mar %*% t(mar) # 得到协方差矩阵
  y = eigen(C) 
  yVectors = y$vectors #得到特征向量
  v_r = yVectors[, 1:k] #取降维后的维度
  v_r = t(mar) %*% v_r
  for (i in 1:k) {
    Msqrt = sqrt(sum(v_r[, i]^2))
    v_r[, i] = v_r[, i]/Msqrt
  }
  final_data =  (mar) %*% v_r # 得到降维后的训练向量
  re <- list(final_data, v_r)
  return (re)
}

TrainRow = 90
TrainCol = 256
TrainData = trainX
TrainDim = 26
result = PCA(TrainRow, TrainCol, TrainData, TrainDim)
V_r = result[[2]]
tranData = result[[1]]
cat('当前维度为', TrainDim)

for(h in 1: nrow(testValueMATRIX)) {
  testData = testValueMATRIX[h,] %*%  V_r 
  compareMat = matrix(0, TrainRow, TrainDim)
  for(i in 1:TrainRow) {
    compareMat[i, ] = (tranData[i,] - testData) ^2
  }
  compareArr = matrix(0, TrainRow, 1)
  for(i in 1:TrainRow) {
    sum = 0
    for(j in 1:TrainDim) {
      sum = sum + compareMat[i, j]
    }
    compareArr[i] = sum
  }
  index = 999
  value = 999999
  for(i in 1:TrainRow) {
    if(compareArr[i] < value) {
      value = compareArr[i]
      index = i
    } 
  }
  cat('第', h, '个测试集识别数字为', level[index], "\n")
}



