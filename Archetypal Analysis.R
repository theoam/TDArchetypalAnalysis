library("archetypes")
library("readxl")

td <- read_excel("dataset_java_25.xlsx")
td <- data.frame(td)
str(td)
td$project <- factor(td$project)
DifProjects <- levels(td$project)

for (i in 1:length(DifProjects)){
  
  dataset <- td[td$project==paste(DifProjects[i]),]
  
  dataset$sonar_td_minutes.Ranks <- c(rank(dataset$sonar_td_minutes, ties.method="average"))
  dataset$squore_td_minutes.Ranks <- c(rank(dataset$squore_td_minutes, ties.method="average"))
  dataset$cast_td_minutes.Ranks <- c(rank(dataset$cast_td_minutes, ties.method="average"))
  dataset$file <- factor(dataset$file) 
  dat <- dataset[,c("file","sonar_td_minutes.Ranks","squore_td_minutes.Ranks","cast_td_minutes.Ranks")]
  colnames(dat) <- c("file","Sonar","Squore","Cast")
  mat <- as.matrix(subset(dat, select = c(Sonar,Squore,Cast)))
  rownames(mat) <- NULL
  as <- stepArchetypes(mat, k = 1:10,nrep=50)
  rss.vector <- vector("numeric", 10)
  
  a1 <- bestModel(as[[1]])
  a2 <- bestModel(as[[2]])
  a3 <- bestModel(as[[3]])
  a4 <- bestModel(as[[4]])
  a5 <- bestModel(as[[5]])
  a6 <- bestModel(as[[6]])
  a7 <- bestModel(as[[7]])
  a8 <- bestModel(as[[8]])
  a9 <- bestModel(as[[9]])
  a10 <- bestModel(as[[10]])
  
  
  rss.vector <- c(rss(a1),rss(a2),rss(a3),rss(a4),rss(a5),rss(a6),rss(a7), rss(a8), rss(a9), rss(a10))
  rss.vector.reduction <- c(100*(rss.vector[1]/rss.vector[1]),
                            100*(rss.vector[2]/rss.vector[1]),
                            100*(rss.vector[3]/rss.vector[1]),
                            100*(rss.vector[4]/rss.vector[1]),
                            100*(rss.vector[5]/rss.vector[1]),
                            100*(rss.vector[6]/rss.vector[1]),
                            100*(rss.vector[7]/rss.vector[1]),
                            100*(rss.vector[8]/rss.vector[1]),
                            100*(rss.vector[9]/rss.vector[1]),
                            100*(rss.vector[10]/rss.vector[1]))
  
  archetypes.vector <- seq(1:10)
  
  data.RSS <- data.frame(archetypes.vector, rss.vector,rss.vector.reduction)
  
  abest <- bestModel(as[[8]])
  rm(a1,a2,a3,a4,a5,a6,a7,a9,a10)
  
  save.image(file = paste0(DifProjects[i],".RData"))
}
