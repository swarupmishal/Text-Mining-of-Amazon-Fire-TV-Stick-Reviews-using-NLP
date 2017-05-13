install.packages("darch") #one time
library(darch)
model <- darch(Species ~ ., iris, generateWeightsFunction = "generateWeightsNormal",
               weights.mean = .1, weights.sd = .05)
darch <- newDArch(c(2,4,1),batchSize = 2,genWeightFunc = "generateWeights")

??generateWeightsNormal()

>inputs ??? matrix(c(0,0,0,1,1,0,1,1),ncol=2,byrow=TRUE)
>outputs ??? matrix(c(0,1,1,0),nrow=4)

>darch ??? preTrainDArch(darch,inputs,maxEpoch=1000)


getLayerWeights(darch,index=1)

layers ??? getLayers(darch)
>for(i in length(layers):1){
  layers[[i]][[2]] ??? sigmoidUnitDerivative
}
>setLayers(darch) ??? layers
>rm(layers)
?newDarch
