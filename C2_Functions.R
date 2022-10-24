
#################################################################
#2classes
#################################################################
Metric2 <- function(CM){
  acc = CM[[3]][1]
  sens = CM[[4]][1]
  spec = CM[[4]][2]
  print("Confusion Matrix:")
  print(CM[[2]])
  print("------------------")
  print(acc)
  print("------------------")
  print(sens)
  print("------------------")
  print(spec)
}

##The same function above without the print function, used when i have to 
#procude multiple metrics and save them in a vector 
MetricCross2 <- function(CM){
  acc = CM[[3]][1]
  return(acc)
}

LGMetric <- function(GLMPR) {
  acc <- (GLMPR[[2]][1,1] + GLMPR[[2]][2,2]) / (GLMPR[[2]][1,1] + GLMPR[[2]][2,2] + GLMPR[[2]][1,2] + GLMPR[[2]][2,1])
  sens <- GLMPR[[2]][1,1] / ( GLMPR[[2]][1,1] + GLMPR[[2]][2,1])
  spec <- GLMPR[[2]][2,2] / ( GLMPR[[2]][2,2] + GLMPR[[2]][1,2])
  print("confusion Matrix:")
  print(GLMPR[[2]])
  print("------------------")
  print("Accuracy")
  print(acc)
  print("------------------")
  print("Sensitivity")
  print(sens)
  print("------------------")
  print("specificity")
  print(spec)
}


################################################################################
##Factor Correlation
################################################################################

factorcorr <- function(factor) {
cnamei <- c(colnames(factor))
cnamej <- c(cnamei) 
corrfactor <- c()
namemerge <- c() 
kalulu <- c() ##define the variables

for(j in 1: ncol(factor)) {
  for(i in 1: ncol(factor)) {
    corrfactor <-  append(corrfactor, cramerV(factor[[j]], factor[[i]]))
    namemerge <- append(namemerge, paste(cnamej[[j]], cnamei[[i]]))
  }
} ##nested for in which it is calculated the cramer's V and it is merged the name of the two features
factorcorr <- data.frame("varnames" = namemerge, "corr" = corrfactor) ##creating a dataframe with the variables created
factorst <- factorcorr %>%
  summarise('names' = unique(varnames), 'corr' = corrfactor) %>% arrange(desc(corrfactor))  ##create a standings

factorcol <- c()
factorname <- c()
v = length(cnamei)+1
while(v <= nrow(factorst)) {
  factorcol <- append(factorcol, factorst$corr[v])
  factorname <- append(factorname, factorst$names[v])
  v = v+2
} ##since each correlation is calculated twice (a b and b a), create two variables to delete the repetition
factorstanding <- data.frame("variables" = factorname, "correlation" = factorcol) ##save this variables in a dataframe
return(factorstanding)
}