####f
f <- function(i,data) { sum(data == i) }

###Shannon entropy
ShannonEntropy<-function(p){-sum(p*log(p))}





#####for MLE#####
#x<-X[,1]
MLE_EstFun <-function(x) { 
  x <- x[x > 0]
  n <- sum(x)
  pi <- x / n
  est <- -sum(pi * log(pi))
  est<-as.numeric(est)
  return(est)
}

#tmp<-MLE_EstFun_se(x,200,MLE_EstFun)

MLE_EstFun_se <-function(x,B,FUNcname) { 
  x <- x[x > 0]
  n <- sum(x)
  pi <- x / n
  est <- -sum(pi * log(pi))
  est<-as.numeric(est)
  se<-Bootstrap_seFun(x, B, FUNcname)
  
  return(c(est,se))
}

#####for MLE bc#####
MLEbc_EstFun <-function(x) { 
  x <- x[x > 0]
  n <- sum(x)
  estmle<-MLE_EstFun(x)
  Shat<-S_ace(x,10)
  est<-estmle+(Shat-1)/2/n
  est<-as.numeric(est)
  return(est)
}


####for Chao Shen 2003#######
ChaoShen_EstFun <-function(x) { 
  x <- x[x > 0]
  n <- sum(x)
  f1<-f(1,x)
  Chat<-1-f1/n
  pi<-Chat*x/n
  est <- -sum(pi * log(pi)/ (1 - (1 - pi)^n))
  return(est)
}



####for Chao Wang2013#######
ChaoWang_EstFun <-function(x) { 
  x <- x[x > 0]
  n <- sum(x)
  A <- AFun(x)
  temp1 <- sum(x / n * (digamma(n) - digamma(x)))
  if(A == 1){
    temp2 <- 0
  } else {
    l <- 1:(n-1)
    temp2 <- f(1,x) / n * (1 - A)^(1-n) * (-log(A) - sum(1 / l * (1-A)^l))
  }
  est <- (temp1 + temp2)
  return(est)
}


####for Chao Hsieh 2015#######
#' DetAbu(x) is a function of estimating detected species relative abundance.
#' @param x a vector of species abundance frequency
#' @return a numerical vector: represent detected species pi hat
#' 

ChaoHsieh2015_ShannonBseFun<-function(x,B){
  x<-x[x>0]
  n<-sum(x)
  pi<-ChaoHsieh2015_piEstFun(x)
 # print(pi)
  X <- rmultinom(B, n, pi)
  estB11<-apply(X, 2, function(y) ChaoHsieh2015_ShannonEstFun(y))
  estBZ<<-estB
  se<-sd(estB)
  return(se)
}



ChaoHsieh2015_ShannonEstNBseFun<-function(x,B){
  x<-x[x>0]
  n<-sum(x)
  pi<-ChaoHsieh2015_piEstFun(x)
  est<-ShannonEntropy(pi)
  X <- rmultinom(B, n, pi)
  estB<-apply(X, 2, function(y) ChaoHsieh2015_ShannonEstFun(y))
  estBZ<<-estB
  se<-sd(estB)
  if (is.na(se)) NAX<<-X
  return(c(est,se))
  
}





ChaoHsieh2015_ShannonEstFun<-function(x){
  pi<-ChaoHsieh2015_piEstFun(x)
  #print(pi)
  
  tryCatch(
    { est<-ShannonEntropy(pi)},
    # 遇到 warning 時的自訂處理函數
    warning = function(msg) {
      message("Original warning message:")
      message(paste0(msg,"\n"))
      message(paste0(pi))
      stop()
    },
    # 遇到 error 時的自訂處理函數
    error = function(msg) {
      message("Original error message:")
      message(paste0(msg,"\n"))
      message(paste0(pi))
      stop()
    }
  ) 
  
  
 # est<-ShannonEntropy(pi)
  return(est)
  
}


ChaoHsieh2015_piEstFun<-function(x){
  pi_det<-ChaoHsieh_DetEstFun(x)
  pi_undet<-ChaoHsieh_UnDetEstFun(x)
  pi_2015<-c(pi_det,pi_undet)
 
  return(pi_2015)
  
}

ChaoHsieh_DetEstFun <-function(x) { 
    x <- x[x > 0]
    n <- sum(x)  
    f1 <- f(1,x)
    f2 <- f(2,x)
    f3 <- f(3,x)
    tmp <- Chat1_f0Fun(f1, f2, n)
    chat1 <- tmp[1] ; f0.hat <- tmp[2]
    A2<-f2 / choose(n, 2) * ((n-2)*f2 / ((n-2)*f2 + 3*max(f3,1)))^2
    theta.solve <- function(theta){
      lambda<-(1-chat1)/sum(x/n*exp(-theta*x))
      out <- sum((x/n * (1 - lambda * exp(-theta*x)))^2) - sum(choose(x,2)/choose(n,2)) + A2
      abs(out)
    }
    
    theta <- tryCatch(optimize(theta.solve, c(0,1))$min, error = function(e) {1})
    lambda<-(1-chat1)/sum(x/n*exp(-theta*x))
    pi_det <- x/n * (1 - lambda * exp(-theta*x))
 
    return(pi_det)
  
}





ChaoHsieh_UnDetEstFun <-function(x) { 
  pi_undet<-NULL
  x <- x[x > 0]
  n <- sum(x)  
  f1 <- f(1,x)
  f2 <- f(2,x)
  f3 <- f(3,x)
  tmp <- Chat1_f0Fun(f1, f2, n)
  chat1 <- tmp[1] ; f0.hat <- tmp[2]
  f0.hat==0
  if(f0.hat==0){
    pi_undet<-NULL
  }
  else{
   chat1_def<-1-chat1
  j <- 1:f0.hat
  ##to prevent R is NaN
  if(f2==0){
    f2_adj <- 1
  }else f2_adj<-f2
  
  #A2<-f2_adj / choose(n, 2) * ((n-2)*f2_adj / ((n-2)*f2_adj + 3*max(f3,1)))^2
  A2<-f2_adj / choose(n, 2) * ((n-2)*f2_adj / ((n-2)*f2_adj + 3*f3))^2
  R <- chat1_def^2/A2
  beta.solve <- function(beta){
    out <- sum(beta^j)^2 / sum((beta^j)^2) - R
    abs(out)
  }
  
  beta <- tryCatch(optimize(beta.solve, c(0,1))$min, error = function(e) {1})
 # beta <- tryCatch(optimize(beta.solve, lower=(R-1)/(R+1), upper=1, tol=1e-5)$min, error = function(e) {(R-1)/(R+1)})
  aplha <- chat1_def / sum(beta^j)
  pi_undet <- aplha*beta^j
  if(f0.hat == 1) pi_undet <- chat1_def
  }
  return(pi_undet)
}



Chat1_f0Fun <-function(f1, f2, n) {
    if (f2 > 0) {
      f0 <- (n - 1) / n * f1^2 / (2 * f2)
      #C<-1-f0*f1/(n*f0+f1)
      #C <- 1 - f1 / n * ((n - 1) * f1 / ((n - 1) * f1 + 2 * f2))
      C <- 1 - f1 / n * (n-1)*f1/((n-1)*f1+2*f2)
      
    } else if (f2 == 0 & f1 != 0) {
      f0 <- (n - 1) / n * f1 * (f1 - 1) / 2
      #C<-1-f0*f1/(n*f0+f1)
      C <- 1 - f1 / n * ((n - 1) * (f1 - 1) / ((n - 1) * (f1 - 1) + 2))
      
    } else {
      f0 <- (n - 1) / n * f1 * (f1 - 1) / 2
      #f0 <- 0
      C <- 1
    }
    f0 <- ceiling(f0)
    return(c(C, f0))
  }

#x<-Xs[,1]
#B=200
#FunName<-MLE_EstFun

Bootstrap_seFun <-function(x, B, FunName) {
    x<-x[x>0]
    n <- sum(x)
    f1 = f(1,x); f2 = f(2,x)
    tmp <- Chat1_f0Fun(f1, f2, n)
    Chat <- tmp[1] ; f0 <- tmp[2]
    lambda <- (1 - Chat) / sum(x / n * (1 - x / n)^n)
    pi <- x / n * (1 - lambda * (1 - x /n)^n)
    pi.star <- c(pi, rep((1 - Chat) / f0, f0))
    #   set.seed(1234)
    X <- rmultinom(B, n, pi.star)
    se <- sd(apply(X, 2, function(x) FunName(x)))
    return(se)
}






Jackknife_EstFun <-function(x) { 
  x <- x[x > 0]
  n <- sum(x)
  estmle<-MLE_EstFun(x)
  SumHihat<-sum ((n-x)   * x / (n - 1) * log(x / (n - 1)))+
    sum ( x[x>1] * (x[x>1]-1) / (n - 1) * log((x[x>1]-1) / (n - 1)))
  SumHihat<--1*SumHihat
  estJackn<-n*estmle-SumHihat*(n-1)/n
  estJackn<-as.numeric(estJackn)
  #varJackn<-(n-1)*SumHihat
  return(estJackn)
}




###for Shat#####
S_ace <- function(data, k){
  
  x <- data[which(data != 0)]
  n <- sum(x)
  DS <- length(x)
  n_rare <- sum(x[which(x <= k)])
  DS_rare <- length(x[which(x <= k)])
  f1<-0
  f1<-f(1,x)
  if (n_rare != 0){
    C_rare_hat <- 1 - f1/n_rare
  } else {
    C_rare_hat = 1
  } 
  n_abun <- n - n_rare
  DS_abun <- length(x[which(x > k)])
  
  j <- c(1:k)
  a1 <- sum(sapply(j, function(j)j*(j - 1)*f(j, x)))
  a2 <- sum(sapply(j, function(j)j*f(j, x)))
  if (C_rare_hat != 0){
    gamma_rare_hat_square <- max(DS_rare/C_rare_hat*a1/(a2*(a2 - 1)) - 1, 0)
    
    gamma_rare_hat_square <- max(DS_rare/C_rare_hat*a1/a2/(a2 - 1) - 1, 0)
 
  }else{
    gamma_rare_hat_square <- 0
    # gamma_rare_1_square <- 0
  }
  CV_rare <- sqrt(gamma_rare_hat_square)

  S_ace <- DS_abun + DS_rare/C_rare_hat + f1/C_rare_hat*gamma_rare_hat_square
  return (S_ace)
}


#####A function
AFun <- function(x) {
  x <- x[x > 0]
  n <- sum(x)
  f1 <- f(1,x); f2 <- f(2,x)
  if (f2 > 0) {
    A <- 2 * f2 / ((n - 1) * f1 + 2 * f2)
  } else if (f2 == 0 & f1 != 0) {
    A <- 2 * (f2 + 1) / ((n - 1) * (f1 - 1)+ 2 * (f2 + 1))
  } else {
    A <- 1
  }
  return(A)
}






