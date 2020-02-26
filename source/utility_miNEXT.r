#source("./source/species_diversity_method_addBse.r")

library(ggplot2)
library(reshape2)


###if data is dataframe and rowname as col 1####
Fun_data2list<-function(data,type="1"){
  if(type=="1"){
    dat = list()
    name <- as.character(data[,1])
    colname<-colnames(data)
    colname<-colname[-1]
    # print(colname)
    grouplength<-ncol(data)-1
    for(i in 1:grouplength){
      #  print(i)
      j=i+1
      #  print(j)
      dat[[i]] <- data[,j]
    } 
    names(dat) <- colname
    dat <- lapply(dat, function(x){
      names(x)<-name ; x })
    
  }
  else if (type=="2"){
    dat = list()
    name <- rownames(data)
    for(i in 1:ncol(data)) dat[[i]] <- data[,i]
    names(dat) = colnames(data)
    dat <- lapply(dat, function(x){
      names(x)<-name ; x })
    
  }
  dat
}


Fun_createSummary<-function(data,datatype){
  if(length(data) == 1){
    # output <- Fun_Summary4(data[[1]])
    output <- Fun_Datasummary(data[[1]],datatype)
    table <- output
    colnames(table) = names(data)
  }else{
    # output <- Fun_Summary4(data[[1]])
    output <- Fun_Datasummary(data[[1]],datatype)
    
    table <- output
    for(i in 2:length(data)){
      #output <- Fun_Summary4(data[[i]])
      output <- Fun_Datasummary(data[[i]],datatype)
      table <- cbind(table,output)
    }
    colnames(table) = names(data)
  }
  return(table)
}


Fun_Datasummary<-function(datai,datatype="abundance"){
  if(datatype=="abundance"){
    a1 <- matrix(0, 13, 1)
    colnames(a1) <- "values"
    rownames(a1) <- c("n", "S.obs", "Chao1","f1", "f2","f3","f4","f5","f6","f7","f8","f9","f10")
    n<-sum(datai)
    Sobs<-sum(datai != 0)
    f1<-f(1,datai)
    f2<-f(2,datai)
    f0hat<-Chao1_f0(f1,f2,n)
    a1[1, 1] <- n
    a1[2, 1] <- Sobs
    a1[3, 1] <- Sobs+f0hat
    a1[4, 1] <- f1
    a1[5, 1] <- f2
    a1[6, 1] <- f(3,datai)
    a1[7, 1] <- f(4,datai)
    a1[8, 1] <- f(5,datai)
    a1[9, 1] <- f(6,datai)
    a1[10, 1] <- f(7,datai)
    a1[11, 1] <- f(8,datai)
    a1[12, 1] <- f(9,datai)
    a1[13, 1] <- f(10,datai)
    
  }else if(datatype=="incidence_freq"){
    a1 <- matrix(0, 14, 1)
    colnames(a1) <- "values"
    rownames(a1) <- c("T","U", "S.obs", "Chao2","Q1", "Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10")
    TT<-datai[1]
    datai = datai[-1]
    U<-sum(datai)
    Sobs<-c(sum(datai!=0))
    a1[1,1] <- TT
    a1[2,1] <-  U
    a1[3,1] <- Sobs
    Q1<-f(1,datai)
    Q2<-f(2,datai)
    Q0hat<-Chao2_Q0(Q1,Q2,TT)
    a1[4, 1] <- Sobs+Q0hat
    a1[5, 1] <- Q1
    a1[6, 1] <- Q2
    a1[7, 1] <- f(3,datai)
    a1[8, 1] <- f(4,datai)
    a1[9, 1] <- f(5,datai)
    a1[10, 1] <- f(6,datai)
    a1[11, 1] <- f(7,datai)
    a1[12 ,1] <- f(8,datai)
    a1[13 ,1] <- f(9,datai)
    a1[14, 1] <- f(10,datai)
    
  }
  
  return(a1)
}





f <- function(i,data) { sum(data == i) }



##type="bydata" or type=byf"
AFun_bytype <- function(type="bydata",x,f1=NULL,f2=NULL,n=NULL) {
  if (type=="bydata"){
    x <- x[x > 0]
    n <- sum(x)
    f1 <- f(1,x); f2 <- f(2,x)
  }
  else if (type=="byf"){
    if(is.null(f1)|is.null(f2)|is.null(n)) return("please input f1,f2,n")
  }
  if (f2 > 0) {
    A <- 2 * f2 / ((n - 1) * f1 + 2 * f2)
  } else if (f2 == 0 & f1 != 0) {
    A <- 2 * (f2 + 1) / ((n - 1) * (f1 - 1)+ 2 * (f2 + 1))
  } else {
    A <- 1
  }
  return(A)
}


###Chat : sample completeness
###Chat1 : estimator of sample coverage
Chao1_f0<-function(f1,f2,n){
  if (f2 > 0) {
    f0 <- (n - 1) / n * f1^2 / (2 * f2)
    
  } else if (f2 == 0 ) {
    f0<-(n - 1) / n * f1 * (f1 - 1) / 2
  }
  
  return(f0)
}

Chao2_Q0<-function(Q1,Q2,TT){
  if (Q2 > 0) {
    f0 <- (TT - 1) / TT * Q1^2 / (2 * Q2)
    
  } else if (Q2 == 0 ) {
    f0<-(TT - 1) / TT * Q1 * (Q1 - 1) / 2
  }
  
  return(f0)
  
}





MakeTable_Proposed_all = function(data, B, q, conf){
  if(length(data) == 1){
    output<-MakeTable_Proposeprofile(data[[i]],B, q, conf)
    output$Community<-names(data)
    table<-output
    
    
  }
  else{
    output <- MakeTable_Proposeprofile(data[[1]],B, q, conf)
    output$Community<-names(data)[1]
    table<-output
    for(i in 2:length(data)){
      output <- MakeTable_Proposeprofile(data[[i]],B, q, conf)
      output$Community<-names(data)[i]
      table <- rbind(table,output)
    }
    
    
    return(table)
    
  }
  

}




###type="EMP" or "ChaoHsieh2015"
Diversity_q_byEstType <-function(datai,q,type="EMP"){
  x<-datai[datai>0]
  n<-sum(x)
  
  if (type=="ChaoHsieh2015"){
    p<-ChaoHsieh2015_piEstFun(x)
   # print(p)
  }
  else{
    p = x/sum(x)
   # print(p)
  }
  
  Sub = function(q) {
    if (q == 1) {
      exp(-sum(p*log(p)))
    } else {
      (sum(p^q))^(1/(1-q))
    }
  }
  sapply(q, Sub)
}




Fun_createDiversity_Summary<-function(data,q,type){
  if(length(data) == 1){
    output <- Diversity_q_byEstType(data[[1]],q,type)
    
    table <- output
    colnames(table) = names(data)
  }else{
    
    output <- Diversity_q_byEstType(data[[1]],q,type)
    
    table <- output
    for(i in 2:length(data)){
      output <- Diversity_q_byEstType(data[[i]],q,type)
      table <- cbind(table,output)
    }
    colnames(table) = names(data)
    rownames(table) <- q
    #print(table)
  }
  return(table)
}




Plot.p.hist.Tung = function(analistdata) {
  
  aa<-sapply(analistdata,function(i) Fun_pi(i))
  drawdata<-NULL
  for (i in 1:length(aa)){
    tmp<-data.frame(aa[[i]])
    tmp$names<-names(aa)[i]
    drawdata<-rbind(drawdata,tmp)
  }
  
  ggdata<-drawdata
  names(ggdata)[1]<-"p"
 # ylab_ = title_ = "Hill numbers"
 p= ggplot(ggdata)+
    geom_histogram(aes(x = p))+facet_wrap(.~names)
  
  return(p)
}


PlotEmp.q.Tung = function(data,  q) {
  est = sapply(data, function(i) Diversity_q_byEstType(i, q,type="EMP"))
  ans=est
  
  ggdata = melt(ans)
  ggdata[, 1] = rep(q, length(data))
  colnames(ggdata) = c("x", "site", "y")
 # print(ggdata)
  ylab_ = title_ = "Hill numbers"
  p = ggplot(ggdata)+
    geom_line(aes(x = x, y = y, color = site), size=1.5)+
    xlab("q")+
    ylab(ylab_)+theme(text=element_text(size=20),legend.position="bottom")+
    ggtitle(title_)
  return(p)
}


Fun_pi<-function(datai){
  x <- datai[datai > 0]
  n <- sum(x)
  pi <- x / n
  
  return(pi)
}


Fun_Summary4<-function(datai){
  a1 <- matrix(0, 4, 1)
  colnames(a1) <- "values"
  rownames(a1) <- c("n", "S.obs", "f1", "f2")
  f1<-f(1,datai)
  f2<-f(2,datai)
  a1[1, 1] <- sum(datai)
  a1[2, 1] <- sum(datai != 0)
  a1[3, 1] <- f1
  a1[4, 1] <- f2
  return(a1)
}




###if data is dataframe and rowname as col 1####
Fun_modifydataformat<-function(data,type="1"){
  if(type=="1"){
    dat = list()
    name <- as.character(data[,1])
    colname<-colnames(data)
    colname<-colname[-1]
   # print(colname)
    grouplength<-ncol(data)-1
    for(i in 1:grouplength){
    #  print(i)
      j=i+1
    #  print(j)
      dat[[i]] <- data[,j]
    } 
    names(dat) <- colname
    dat <- lapply(dat, function(x){
      names(x)<-name ; x })
    
  }
  else if (type=="2"){
    dat = list()
    name <- rownames(data)
    for(i in 1:ncol(data)) dat[[i]] <- data[,i]
    names(dat) = colnames(data)
    dat <- lapply(dat, function(x){
      names(x)<-name ; x })
    
  }
  dat
}


