
#Type 1: Linear over time and Linear relationship, deterministic
#Type 1.5: Linear over time and Linear relationship, deterministic (just binary var)
#Type 2: Linear over time and "Linear" relationship, stochastic
#Type 3: Linear over time and "Non linear" relationship, deterministic
#Type 4: Linear over time and "Non linear" relationship, stochastic
#Type 5: Non-Linear over time and linear relationship, deterministic
#Type 6: Non-Linear over time and linear relationship, stochastic
#Type 7: Non-Linear over time and Non-linear relationship, deterministic
#Type 8: Non-Linear over time and Non-linear relationship, stochastic
require(dplyr)
require(splines)
require(lcmm)
require(lme4)
require(stringr)

create.data = function(type=1,n=100,seed=1){
        #set.seed(seed)
        L = 5
        id = rep(1:n,each=L)
        t = seq(1,10,length = L)
        time = rep(t,n)
        
        if(type==1){
                w1 = rep(sample(0:1,n,replace=TRUE),each=L)
                w2 = rep(rnorm(n),each=L) 
                intercept = rep(15  + rnorm(n), each=L)
                slope = 0.5 + 0.25*w1 + 0.3*w2 + rep(rnorm(n,sd = 0.5),each=L)
                y = intercept + slope*time + rnorm(n*L,sd = 2)
        }
        if(type==1.5){
                w1 = rep(sample(0:1,n,replace=TRUE),each=L)
                w2 = rep(rnorm(n),each=L) 
                intercept = rep(15  + rnorm(n), each=L)
                slope = 1 + 1.5*w1 + rep(rnorm(n,sd = 0.5),each=L)
                y = intercept + slope*time + rnorm(n*L,sd = 2)
        }
        if(type==2){
                w1 = sample(0:1,n,replace=TRUE)
                w2 = rnorm(n)
                Weight = exp(cbind(1,w2)%*%t(matrix(c(rep(0,3),c(-1,0,1)),nrow=3)))
                Weight = Weight/rowSums(Weight)
                x <- runif(nrow(Weight))
                cumul.w <- Weight %*% upper.tri(diag(ncol(Weight)), diag = TRUE) / rowSums(Weight)
                i <- rowSums(x > cumul.w) + 1L
                slope <- rep(c(.1,1.1,2.1)[i],each=L)
                intercept = rep(15  + rnorm(n), each=L)
                y = intercept + slope*time + rnorm(n*L,sd = 2)
                w1 = rep(w1,each=L)
                w2 = rep(w2,each=L)
        }       
        if(type==3){
                w1 = rep(sample(0:1,n,replace=TRUE),each=L)
                w2 = rep(rnorm(n),each=L) 
                intercept = rep(15  + rnorm(n), each=L)
                slope = 0.05 + 0.25*w1 -0.1*w2 + 0.2*w2^2+ rep(rnorm(n, sd = 0.5),each=L)
                y = intercept + slope*time + rnorm(n*L,sd = 2)
        }       
        if(type==4){
                w1 = sample(0:1,n,replace=TRUE)
                w2 = rnorm(n)
                Weight = exp(cbind(1,w2)%*%t(matrix(c(c(0,0,0),c(-1,1,0)),nrow=3)))
                Weight = Weight/rowSums(Weight)
                x <- runif(nrow(Weight))
                cumul.w <- Weight %*% upper.tri(diag(ncol(Weight)), diag = TRUE) / rowSums(Weight)
                i <- rowSums(x > cumul.w) + 1L
                slope <- rep(c(.1,1.1,2.1)[i],each=L)
                intercept = rep(15  + rnorm(n), each=L)
                y = intercept + slope*time + rnorm(n*L,sd = 2)
                w1 = rep(w1,each=L)
                w2 = rep(w2,each=L)
        }
        if(type==5){
                w1 = rep(sample(0:1,n,replace=TRUE),each=L)
                w2 = rep(rnorm(n),each=L) 
                intercept = rep(15  + rnorm(n), each=L)
                slope = 0.05 + 0.25*w1 + 0.1*w2 + rep(rnorm(n,sd = .1),each=L)
                slope2 = 0.1*w1 + 0.04*w2 + rep(rnorm(n,sd = .1),each=L)
                y = intercept + slope*time + slope2*time^2 + rnorm(n*L,sd = 2)
        }
        if(type==6){
                w1 = sample(0:1,n,replace=TRUE)
                w2 = rnorm(n)
                Weight = exp(cbind(1,w2)%*%t(matrix(c(rep(0,3),c(-1,0,1)),nrow=3)))
                Weight = Weight/rowSums(Weight)
                x <- runif(nrow(Weight))
                cumul.w <- Weight %*% upper.tri(diag(ncol(Weight)), diag = TRUE) / rowSums(Weight)
                i <- rowSums(x > cumul.w) + 1L
                slope2 <- rep(c(.1,.2,.3)[i],each=L)
                slope <- rep(c(.05,.15,.25)[i],each=L)
                intercept = rep(15  + rnorm(n,sd = 2), each=L)
                y = intercept + slope*time + slope2*time^2 + rnorm(n*L,sd = 2)
                w1 = rep(w1,each=L)
                w2 = rep(w2,each=L)
        } 
        if(type==7){
                w1 = rep(sample(0:1,n,replace=TRUE),each=L)
                w2 = rep(rnorm(n),each=L) 
                intercept = rep(15  + rnorm(n), each=L)
                slope =  0.05 + 0.25*w1 -0.1*w2 + 0.2*w2^2 + rep(rnorm(n,sd = .05),each=L)
                slope2 = 0.05*w1 + 0.04*w2^2 + rep(rnorm(n,sd = .03),each=L)
                y = intercept + slope*time + slope2*time^2 + rnorm(n*L,sd = 2)
        }
        if(type==8){
                w1 = sample(0:1,n,replace=TRUE)
                w2 = rnorm(n)
                Weight = exp(cbind(1,w2)%*%t(matrix(c(c(0,0,0),c(-4,4,0)),nrow=3)))
                Weight = Weight/rowSums(Weight)
                x <- runif(nrow(Weight))
                cumul.w <- Weight %*% upper.tri(diag(ncol(Weight)), diag = TRUE) / rowSums(Weight)
                i <- rowSums(x > cumul.w) + 1L
                slope2 <- rep(c(.1,.2,.3)[i],each=L)
                slope <- rep(c(.05,.15,.25)[i],each=L)
                intercept = rep(15  + rnorm(n,sd = 2), each=L)
                y = intercept + slope*time + slope2*time^2 + rnorm(n*L,sd = 2)
                w1 = rep(w1,each=L)
                w2 = rep(w2,each=L)
        } 
        
        w2cut = cut(w2,breaks=quantile(w2,probs=c(0,0.33,0.66,1)),include.lowest = TRUE)
        ynorm = unlist(tapply(y,id,function(v) v - mean(v)))
        return(data.frame(time,w1,w2,w2cut,y,id,slope,intercept,ynorm))
}


truemean = function(type=1,time,w1,w2,deriv=0){
        
        #write function here to give true means for each type of data (no random errors)
        
        if(type==1){
                intercept =  15
                slope = 1 + 0.5*w1 + (1/3)*w2 
                y = intercept + slope*time 
                if(deriv == 1) y = slope
        }
        if(type==1.5){
                intercept = 15
                slope = 1 + 1.5*w1 
                y = intercept + slope*time 
                if(deriv == 1) y = slope
        }
        if(type==2){
                Weight = exp(cbind(1,w2)%*%t(matrix(c(rep(0,3),c(-4,0,4)),nrow=3)))
                Weight = Weight/rowSums(Weight)
                slope <- c(0,1,2.5)
                intercept = 15 
                y = sum((intercept + slope*time)*Weight)
                if(deriv == 1)  y = sum( slope*Weight)
        }       
        if(type==3){
                intercept = 15
                slope = 2 + .25*w1 + .25*w1^2 - (1/4.5)*w2^2
                y = intercept + slope*time
                if(deriv == 1)  y = slope
        }    
        if(type==4){
                Weight = exp(cbind(1,w2)%*%t(matrix(c(c(0,0,0),c(-4,4,0)),nrow=3)))
                Weight = Weight/rowSums(Weight)
                slope <- c(0,1,2.5)
                intercept = 15
                y = sum((intercept + slope*time)*Weight)
                if(deriv == 1)  y = sum( slope*Weight)
        }
        if(type==5){
                intercept = 15
                slope = -0.3 + .05*w1 + .05*w2
                slope2 = 2/10 + .01*w1 + .03*w2
                y = intercept + slope*time + slope2*time^2
                if(deriv == 1)  y = slope + 2*slope2*time
        }
        if(type==6){
                Weight = exp(cbind(1,w2)%*%t(matrix(c(rep(0,3),c(-4,0,4)),nrow=3)))
                Weight = Weight/rowSums(Weight)
                slope2 <- c(.1,.15,.25)
                slope <- -.1*c(1,3,5)
                intercept = 15
                y = sum((intercept + slope*time + slope2*time^2)*Weight)
                if(deriv == 1)  y = sum((slope + 2*slope2*time)*Weight)
        } 
        if(type==7){
                intercept = 15
                slope = -0.5 + .05*w1 - .03*w2^2
                slope2 = 3/10 + .01*w1  - .03*w2^2
                y = intercept + slope*time + slope2*time^2
                if(deriv == 1)  y = slope + 2*slope2*time
        }
        if(type==8){
                Weight = exp(cbind(1,w2)%*%t(matrix(c(c(0,0,0),c(-4,4,0)),nrow=3)))
                Weight = Weight/rowSums(Weight)
                slope2 <- c(.1,.15,.25)
                slope <- -.1*c(1,3,5)
                intercept = 15
                y = sum((intercept + slope*time + slope2*time^2)*Weight)
                if(deriv == 1)  y = sum((slope + 2*slope2*time)*Weight)
        } 
        return(y)
}




fitModel = function(train,model = "Mixed", funcTime = "Linear", randEf = "None", transf = FALSE, w2VarName = 'w2'){
        
        if(transf){
                form = "ynorm ~"
        }else{ form = "y ~"}
        if(model == "Mixed"){
                if(funcTime == "Linear"){
                        form = paste(form, ' time*w1 + time*',w2VarName,sep='')
                        if(randEf == "Intercept"){
                                form = paste(form, '+ (1|id)')        
                        }
                        if(randEf == "Slope"){
                                if(!transf) form = paste(form, '+ (time|id)')
                                if(transf) form = paste(form, '+ (-1 + time|id)')
                        }
                }
                if(funcTime == "Quadratic"){
                        form = paste(form, '(I(time/10) + I((time/10)^2))*w1 + (I(time/10) + I((time/10)^2))*',w2VarName,sep='')
                        if(randEf == "Intercept"){
                                form = paste(form, '+ (1|id)')        
                        }
                        if(randEf == "Slope"){
                                if(!transf) form = paste(form, '+ (I(time/10) + I((time/10)^2)|id)')      
                                if(transf) form = paste(form, '+ (-1 + I(time/10) + I((time/10)^2)|id)')   
                        }
                }
                if(funcTime == "B-spline"){
                        train = cbind(train,bs(train$time,knots = quantile(train$time,c(.5)),Boundary.knots = range(train$time),intercept=FALSE,degree=2))
                        names(train)[-(1:9)] = paste('T',1:3,sep='')
                        
                        form = paste(form, ' (T1+T2+T3)*w1+(T1+T2+T3)*',w2VarName,sep='')
                        if(randEf == "Intercept"){
                                form = paste(form, '+ (1|id)')        
                        }
                        if(randEf == "Slope"){
                                if(!transf) form = paste(form, '+ (T1|id)')                                  
                                if(transf) form = paste(form, '+ (-1 + T1|id)')  
                        }
                }
                
        mod = try(lmer(as.formula(form), data=train))
        if(class(mod) == 'try-error') return(NULL)
        #print(BIC(mod))
        }
        if(model == "Mixture"){
                corForm = as.formula(paste('~ w1 +',w2VarName))
                #randform = ~ -1
                if(funcTime == "Linear"){
                        form = as.formula(paste(form, ' time'))
                        mixform = ~ time
                        if(randEf == "Intercept"){
                                randform = ~1        
                        }
                        if(randEf == "Slope"){
                                randform = ~time                                
                        }
                }
                if(funcTime == "Quadratic"){
                        form = as.formula(paste(form, '(time + I(time^2))'))
                        mixform = ~ (time + I(time^2))
                        if(randEf == "Intercept"){
                                randform = ~1        
                        }
                        if(randEf == "Slope"){
                                randform = ~time                              
                        }
                }
                if(funcTime == "B-spline"){
                        train = cbind(train,bs(train$time,knots = quantile(train$time,c(.5)),Boundary.knots = range(train$time),intercept=FALSE,degree=2))
                        names(train)[-(1:9)] = paste('T',1:3,sep='')
                        
                        form = as.formula(paste(form, '(T1+T2+T3)'))
                        mixform = ~(T1+T2+T3)
                        if(randEf == "Intercept"){
                                randform = ~ 1
                        }
                        if(randEf == "Slope"){
                                randform = ~T1                                
                        }
                }
                
               
                tmp.bic = Inf 
                for(k in 2:6){
                        if(!exists("randform")){
                                tmp = eval(call('hlme',form, mixture = mixform, classmb = corForm, subject = 'id',ng = k,data=quote(train),maxiter = 100,verbose=FALSE))
                        }else{
                                tmp = eval(call('hlme',form, mixture = mixform, random= randform, classmb = corForm, subject = 'id',ng = k,data=quote(train),maxiter=100,verbose=FALSE))
                        }
                        id <- 1:(3*(k-1))
                        indice <- rep(id * (id + 1)/2)
                        #save(train,tmp,file=paste0('Tmp',j,'.RData'))
                        if(tmp$conv %in% c(2,4,5)){
                                K = k - 1
                                break                                
                        } 
                        if(tmp$BIC > tmp.bic | abs(tmp$BIC - tmp.bic) < 5 | any(tmp$V[indice] < 0)  ){
                                K = k - 1
                                break  
                        }
                        tmp.bic = tmp$BIC
                        mod = tmp
                        K = k     
                }
                if(K==1){return(NULL)}
                #print(mod$BIC)
        }        
        
        
        
        
        
        
        return(list(mod = mod,funcTime = funcTime,w2VarName = w2VarName))
}


predictY = function (x, newdata, var.time, draws = FALSE, na.action = 1,olddata, 
          ...) 
{
  if (missing(newdata)) 
    stop("The argument newdata should be specified")
  if (missing(x)) 
    stop("The argument x should be specified")
  if (!inherits(x, "hlme")) 
    stop("use only with \"hlme\" objects")
  if (!all(x$Xnames2 %in% c(colnames(newdata), "intercept"))) {
    cat("newdata should at least include the following covariates: ", 
        "\n")
    cat(x$Xnames2[-1], "\n")
  }
  if (!all(x$Xnames2 %in% c(colnames(newdata), "intercept"))) 
    stop("see above")
  if (!inherits(newdata, "data.frame")) 
    stop("newdata should be a data.frame object")
  call_fixed <- x$call$fixed[3]
  if (is.null(x$call$random)) {
    call_random <- ~-1
  }
  else call_random <- x$call$random
  if (is.null(x$call$classmb)) {
    call_classmb <- ~-1
  }
  else call_classmb <- x$call$classmb
  if (is.null(x$call$mixture)) {
    call_mixture <- ~-1
  }
  else call_mixture <- x$call$mixture
  if (x$conv == 1 | x$conv == 2) {
    if (x$Xnames2[1] != "intercept") {
      newdata1 <- newdata[, x$Xnames2]
      colnames(newdata1) <- x$Xnames
      newdata1 <- data.frame(newdata1)
    }
    else {
      newdata1 <- cbind(rep(1, length = length(newdata[, 
                                                       1])), newdata[, x$Xnames2[-1]])
      colnames(newdata1) <- c("intercept", x$Xnames2[-1])
      newdata1 <- data.frame(newdata1)
    }
    X1 <- NULL
    X2 <- NULL
    b1 <- NULL
    b2 <- NULL
    if (!(na.action %in% c(1, 2))) 
      stop("only 1 for 'na.omit' or 2 for 'na.fail' are required in na.action argument")
    if (na.action == 1) {
      na.action = na.omit
    }
    else {
      na.action = na.fail
    }
    #olddata <- eval(x$call$data)
    for (v in x$Xnames2[-1]) {
      if (is.factor(olddata[, v])) {
        mod <- levels(olddata[, v])
        if (!(levels(as.factor(newdata1[, v])) %in% mod)) 
          stop(paste("invalid level in factor", v))
        newdata1[, v] <- factor(newdata1[, v], levels = mod)
      }
    }
    z <- all.names(call_fixed)
    ind_factor <- which(z == "factor")
    if (length(ind_factor)) {
      nom.factor <- z[ind_factor + 1]
      for (v in nom.factor) {
        mod <- levels(as.factor(olddata[, v]))
        if (!all(levels(as.factor(newdata1[, v])) %in% 
                 mod)) 
          stop(paste("invalid level in factor", v))
        newdata1[, v] <- factor(newdata1[, v], levels = mod)
      }
    }
    call_fixed <- gsub("factor", "", call_fixed)
    z <- all.names(call_random)
    ind_factor <- which(z == "factor")
    if (length(ind_factor)) {
      nom.factor <- z[ind_factor + 1]
      for (v in nom.factor) {
        mod <- levels(as.factor(olddata[, v]))
        if (!all(levels(as.factor(newdata1[, v])) %in% 
                 mod)) 
          stop(paste("invalid level in factor", v))
        newdata1[, v] <- factor(newdata1[, v], levels = mod)
      }
    }
    call_random <- gsub("factor", "", call_random)
    z <- all.names(call_classmb)
    ind_factor <- which(z == "factor")
    if (length(ind_factor)) {
      nom.factor <- z[ind_factor + 1]
      for (v in nom.factor) {
        mod <- levels(as.factor(olddata[, v]))
        if (!all(levels(as.factor(newdata1[, v])) %in% 
                 mod)) 
          stop(paste("invalid level in factor", v))
        newdata1[, v] <- factor(newdata1[, v], levels = mod)
      }
    }
    call_classmb <- gsub("factor", "", call_classmb)
    z <- all.names(call_mixture)
    ind_factor <- which(z == "factor")
    if (length(ind_factor)) {
      nom.factor <- z[ind_factor + 1]
      for (v in nom.factor) {
        mod <- levels(as.factor(olddata[, v]))
        if (!all(levels(as.factor(newdata1[, v])) %in% 
                 mod)) 
          stop(paste("invalid level in factor", v))
        newdata1[, v] <- factor(newdata1[, v], levels = mod)
      }
    }
    call_mixture <- gsub("factor", "", call_mixture)
    mcall <- match.call()[c(1, match(c("data", "subset", 
                                       "na.action"), names(match.call()), 0))]
    mcall$na.action <- na.action
    mcall$data <- newdata1
    m <- mcall
    m$formula <- formula(paste("~", call_fixed, sep = ""))
    m[[1]] <- as.name("model.frame")
    m <- eval(m, sys.parent())
    na.fixed <- attr(m, "na.action")
    if (!is.null(x$call$mixture)) {
      m <- mcall
      m$formula <- formula(paste("~", call_mixture, sep = ""))
      m[[1]] <- as.name("model.frame")
      m <- eval(m, sys.parent())
      na.mixture <- attr(m, "na.action")
    }
    else {
      na.mixture <- NULL
    }
    if (!is.null(x$call$random)) {
      m <- mcall
      m$formula <- formula(paste("~", call_random, sep = ""))
      m[[1]] <- as.name("model.frame")
      m <- eval(m, sys.parent())
      na.random <- attr(m, "na.action")
    }
    else {
      na.random <- NULL
    }
    if (!is.null(x$call$classmb)) {
      m <- mcall
      m$formula <- formula(paste("~", call_classmb, sep = ""))
      m[[1]] <- as.name("model.frame")
      m <- eval(m, sys.parent())
      na.classmb <- attr(m, "na.action")
    }
    else {
      na.classmb <- NULL
    }
    na.cor <- NULL
    if (length(x$N) > 4) {
      if (x$N[5] > 0) {
        z <- which(x$idcor0 == 1)
        var.cor <- newdata1[, x$Xnames[z]]
        na.cor <- which(is.na(var.cor))
      }
    }
    if (!missing(var.time)) {
      if (!(var.time %in% colnames(newdata))) 
        stop("'var.time' should be included in newdata")
      if (var.time %in% colnames(newdata1)) {
        times <- newdata1[, var.time, drop = FALSE]
      }
      else {
        times <- newdata[, var.time, drop = FALSE]
      }
    }
    else {
      times <- newdata[, 1, drop = FALSE]
    }
    na.action <- unique(c(na.fixed, na.mixture, na.random, 
                          na.classmb, na.cor))
    if (length(na.action)) {
      newdata1 <- newdata1[-na.action, ]
      times <- times[-na.action]
    }
    X_fixed <- model.matrix(formula(paste("~", call_fixed, 
                                          sep = "")), data = newdata1)
    if (colnames(X_fixed)[1] == "(Intercept)") {
      colnames(X_fixed)[1] <- "intercept"
      int.fixed <- 1
    }
    if (!is.null(x$call$mixture)) {
      X_mixture <- model.matrix(formula(paste("~", call_mixture, 
                                              sep = "")), data = newdata1)
      if (colnames(X_mixture)[1] == "(Intercept)") {
        colnames(X_mixture)[1] <- "intercept"
        int.mixture <- 1
      }
      id.X_mixture <- 1
    }
    else {
      id.X_mixture <- 0
    }
    if (!is.null(x$call$random)) {
      X_random <- model.matrix(formula(paste("~", call_random, 
                                             sep = "")), data = newdata1)
      if (colnames(X_random)[1] == "(Intercept)") {
        colnames(X_random)[1] <- "intercept"
        int.random <- 1
      }
      id.X_random <- 1
    }
    else {
      id.X_random <- 0
    }
    if (!is.null(x$call$classmb)) {
      X_classmb <- model.matrix(formula(paste("~", call_classmb, 
                                              sep = "")), data = newdata1)
      colnames(X_classmb)[1] <- "intercept"
      id.X_classmb <- 1
    }
    else {
      id.X_classmb <- 0
    }
    if (length(x$N) > 4) {
      if (x$N[5] > 0) {
        z <- which(x$idcor0 == 1)
        var.cor <- newdata1[, x$Xnames[z]]
      }
    }
    newdata1 <- X_fixed
    colX <- colnames(X_fixed)
    if (id.X_mixture == 1) {
      for (i in 1:length(colnames(X_mixture))) {
        if ((colnames(X_mixture)[i] %in% colnames(newdata1)) == 
            FALSE) {
          newdata1 <- cbind(newdata1, X_mixture[, i])
          colnames(newdata1) <- c(colX, colnames(X_mixture)[i])
          colX <- colnames(newdata1)
        }
      }
    }
    if (id.X_random == 1) {
      for (i in 1:length(colnames(X_random))) {
        if ((colnames(X_random)[i] %in% colnames(newdata1)) == 
            FALSE) {
          newdata1 <- cbind(newdata1, X_random[, i])
          colnames(newdata1) <- c(colX, colnames(X_random)[i])
          colX <- colnames(newdata1)
        }
      }
    }
    if (id.X_classmb == 1) {
      for (i in 1:length(colnames(X_classmb))) {
        if ((colnames(X_classmb)[i] %in% colnames(newdata1)) == 
            FALSE) {
          newdata1 <- cbind(newdata1, X_classmb[, i])
          colnames(newdata1) <- c(colX, colnames(X_classmb)[i])
          colX <- colnames(newdata1)
        }
      }
    }
    if (length(x$N) > 4) {
      if (x$N[5] > 0) {
        if (x$idg0[z] == 0 & x$idea0[z] == 0 & x$idprob0[z] == 
            0) {
          newdata1 <- cbind(newdata1, var.cor)
          colnames(newdata1) <- c(colX, x$Xnames[z])
          colX <- colnames(newdata1)
        }
      }
    }
    placeV <- list()
    placeV$commun <- NA
    for (i in 1:x$ng) {
      placeV[paste("class", i, sep = "")] <- NA
    }
    kk <- 0
    for (k in 1:length(x$idg0)) {
      if (x$idg0[k] == 1) {
        X1 <- cbind(X1, newdata1[, k])
        place <- x$N[1] + kk
        b1 <- c(b1, x$best[place + 1])
        placeV$commun <- c(placeV$commun, place + 1)
        kk <- kk + 1
      }
      if (x$idg0[k] == 2) {
        X2 <- cbind(X2, newdata1[, k])
        place1 <- x$N[1] + kk + 1
        place2 <- x$N[1] + kk + x$ng
        b2 <- rbind(b2, x$best[place1:place2])
        for (i in 1:x$ng) {
          placeV[[paste("class", i, sep = "")]] <- c(placeV[[paste("class", 
                                                                   i, sep = "")]], x$N[1] + kk + i)
        }
        kk <- kk + x$ng
      }
    }
    Y <- matrix(0, length(newdata1[, 1]), x$ng)
    for (g in 1:x$ng) {
      if (length(b1) != 0) {
        Y[, g] <- X1 %*% b1
      }
      if (length(b2) != 0) {
        Y[, g] <- Y[, g] + X2 %*% b2[, g]
      }
    }
    if (draws == TRUE) {
      Vbeta <- matrix(0, x$N[2], x$N[2])
      npm <- length(x$best)
      indice <- 1:npm * (1:npm + 1)/2
      indtmp <- indice[(x$N[1] + 1):(x$N[1] + x$N[2])]
      indtmp <- cbind(indtmp - 0:(length(indtmp) - 1), 
                      indtmp)
      indV <- NULL
      for (i in 1:nrow(indtmp)) {
        indV <- c(indV, seq(indtmp[i, 1], indtmp[i, 2]))
      }
      Vbeta[upper.tri(Vbeta, diag = TRUE)] <- x$V[indV]
      Vbeta <- t(Vbeta)
      Vbeta[upper.tri(Vbeta, diag = TRUE)] <- x$V[indV]
      lower <- matrix(0, nrow(Y), ncol(Y))
      upper <- matrix(0, nrow(Y), ncol(Y))
      colnames(lower) <- paste("lower.class", 1:x$ng, sep = "")
      colnames(upper) <- paste("upper.class", 1:x$ng, sep = "")
      if (x$ng == 1) {
        varpred <- apply(X1, 1, function(x) matrix(x, 
                                                   nrow = 1) %*% Vbeta %*% matrix(x, ncol = 1))
        lower[, 1] <- Y[, 1] - 1.96 * sqrt(varpred)
        upper[, 1] <- Y[, 1] + 1.96 * sqrt(varpred)
      }
      else {
        for (g in 1:x$ng) {
          ind <- na.omit(c(placeV[["commun"]], placeV[[paste("class", 
                                                             g, sep = "")]]))
          X12 <- cbind(X1, X2)
          X12 <- X12[, order(ind)]
          varclass <- Vbeta[sort(ind) - x$N[1], sort(ind) - 
                              x$N[1]]
          varpred <- diag(X12 %*% varclass %*% t(X12))
          lower[, g] <- Y[, g] - 1.96 * sqrt(varpred)
          upper[, g] <- Y[, g] + 1.96 * sqrt(varpred)
        }
      }
    }
    if (draws == TRUE) {
      res <- cbind(Y, lower, upper)
      if (x$ng == 1) 
        colnames(res) <- c("pred", "lower.pred", "upper.pred")
      if (x$ng > 1) 
        colnames(res) <- c(paste("pred_class", 1:x$ng, 
                                 sep = ""), paste("lower.pred_class", 1:x$ng, 
                                                  sep = ""), paste("upper.pred_class", 1:x$ng, 
                                                                   sep = ""))
      res.list <- NULL
      res.list$pred <- res
      res.list$times <- times
    }
    if (draws == FALSE) {
      if (x$ng == 1) {
        colnames(Y) <- c("Ypred")
      }
      if (x$ng > 1) {
        colnames(Y) <- c(paste("Ypred_class", 1:x$ng, 
                               sep = ""))
      }
      res.list <- NULL
      res.list$pred <- Y
      res.list$times <- times
    }
  }
  else {
    cat("Predictions can not be computed since the program stopped abnormally. \n")
    res.list <- list(pred = NA, times = NA)
  }
  class(res.list) <- "predictY"
  return(res.list)
}

CalcMSE = function(mod,test,model = "Mixed",transf = FALSE,w2VarName = 'w2',train){
        if(is.null(mod)) {return(NA)}
        test = cbind(test,bs(test$time,knots = quantile(train$time,c(.5)),Boundary.knots = range(train$time),intercept=FALSE,degree=2))
        #save(train,file='output.RData')
        names(test)[-(1:9)] = paste('T',1:3,sep='')
        if(model == 'Mixed'){
                predY =  model.matrix(formula(mod,fixed.only=TRUE),test)%*%fixef(mod)
                if(!transf){
                        mse = mean((predY - test$y)^2)
                }else{
                       mse =  mean((predY - test$ynorm)^2)
                }
        }
        if(model == 'Mixture'){
                K = mod$ng
                corForm = as.formula(paste('~ w1 +', w2VarName))
                W = model.matrix(corForm,test)
                pp = exp(W%*%t(matrix(mod$best[1:(ncol(W)*(K-1))],nrow=K-1)))/(1+rowSums(exp(W%*%t(matrix(mod$best[1:(ncol(W)*(K-1))],nrow=K-1)))))
                pp = cbind(pp,1-rowSums(pp))
                predY = rowSums(predictY(mod,newdata=test,var.time = 'time',olddata=train)$pred*pp) 
                if(!transf){
                       mse =  mean((predY- test$y)^2)
                     
                }else{
                      mse =   mean((predY - test$ynorm)^2)
                }
                
        }   
        return(mse)
}


CalcBIC = function(mod,model = "Mixed"){
        if(is.null(mod)) {return(NA)}
        if(model == 'Mixed'){
                bic = BIC(mod)
        }
        if(model == 'Mixture'){
                bic = mod$BIC
        }   
        return(bic)
}






priorp = function(mod,newdata){
        X_classmb <- model.matrix(formula(mod$call$classmb), data = newdata)
        mat = matrix(mod$best[1:mod$N[1]],nrow = mod$ng - 1)
        p = exp(cbind(X_classmb%*%t(mat),0))
        p = p/rowSums(p)
        p
}

CalcDeriv = function(mod,model='Mixed',dattype,train,funcTime = 'Linear',w2VarName,foo2){
        foo2$w2cut2 = as.numeric(foo2$w2cut) == 2 
        foo2$w2cut3 = as.numeric(foo2$w2cut) == 3
        if(funcTime == 'Linear'){
                if(model == 'Mixed' & w2VarName == 'w2') return(cbind(0,1,0,0,foo2$w1,foo2$w2)%*%fixef(mod))
                if(model == 'Mixed' & w2VarName == 'w2cut') return(cbind(0,1,0,0,0,foo2$w1,foo2$w2cut2,foo2$w2cut3)%*%fixef(mod))
                if(model == 'Mixture') return(rowSums(cbind(0,rep(1,nrow(foo2))) %*%t(matrix(mod$best[(mod$N[1]+1): sum(mod$N[1:2])],ncol=2)) *priorp(mod,newdata=foo2)) )

        }
        
        if(funcTime == 'Quadratic'){
                if(model == 'Mixed'& w2VarName == 'w2') return(cbind(0,1/10,2/100*foo2$time,0,0,foo2$w1/10,2*foo2$w1/100*foo2$time,foo2$w2/10,2*foo2$w2/100*foo2$time)%*%fixef(mod))
                if(model == 'Mixed'& w2VarName == 'w2cut') return(cbind(0,1/10,2/100*foo2$time,0,0,0,foo2$w1/10,2*foo2$w1/100*foo2$time,foo2$w2cut2/10,2*foo2$w2cut2/100*foo2$time,foo2$w2cut3/10,2*foo2$w2cut3/100*foo2$time)%*%fixef(mod))
                if(model == 'Mixture') return(rowSums(cbind(0,1,2*foo2$time) %*%t(matrix(mod$best[(mod$N[1]+1): sum(mod$N[1:2])],ncol=3)) *priorp(mod,newdata=foo2)) )
        }
        
        if(funcTime == 'B-spline'){
                knots = 9.5
                degree = 2
                ord = degree + 1
                Boundary.knots = range(train$time,na.rm=TRUE)
                Boundary.knots <- sort(Boundary.knots)
                Aknots <- sort(c(rep(Boundary.knots, ord), knots))
                foo2 = cbind(foo2,spline.des(Aknots,foo2$time,ord = ord,derivs=1)$design[,-1L]) 
                names(foo2)[-(1:9)] = paste('T',1:3,sep='')
                
                
                if(model == 'Mixed'& w2VarName == 'w2') return(cbind(0,foo2$T1,foo2$T2,foo2$T3,0,0,foo2$T1*foo2$w1,foo2$T2*foo2$w1,foo2$T3*foo2$w1,foo2$T1*foo2$w2,foo2$T2*foo2$w2,foo2$T3*foo2$w2)%*%fixef(mod))
                if(model == 'Mixed'& w2VarName == 'w2cut') return(cbind(0,foo2$T1,foo2$T2,foo2$T3,0,0,0,foo2$T1*foo2$w1,foo2$T2*foo2$w1,foo2$T3*foo2$w1,foo2$T1*foo2$w2cut2,foo2$T2*foo2$w2cut2,foo2$T3*foo2$w2cut2,foo2$T1*foo2$w2cut3,foo2$T2*foo2$w2cut3,foo2$T3*foo2$w2cut3)%*%fixef(mod))
                if(model == 'Mixture') return(rowSums(cbind(0,foo2$T1,foo2$T2,foo2$T3) %*%t(matrix(mod$best[(mod$N[1]+1): sum(mod$N[1:2])],ncol=4)) *priorp(mod,newdata=foo2)) )
        }
        
}

CalcGrid = function(mod,model = "Mixed",dattype,train,funcTime,w2VarName){
        if(is.null(mod)) { return(NA) }
        t = seq(1.5,10,by=1)
        w1 = 1
        b = unique(as.numeric(unlist(str_split(str_replace_all(levels(train$w2cut),'\\[|\\(|\\]',''),','))))
        w2 = seq(-2,2,length=10)
        w2cut = cut(w2,breaks=b,include.lowest = TRUE)
        foo = expand.grid(t,w1,w2,1,w2cut,1,1)
        names(foo) = c('time','w1','w2','id','w2cut','y','ynorm')
        foo2 = foo
        
        foo = cbind(foo,bs(foo$time,knots = 9.5,Boundary.knots = range(train$time),intercept=FALSE,degree=2))
        names(foo)[-(1:7)] = paste('T',1:3,sep='')
        
        
        #write('gridfunc1',file='output.txt')
        
        #BREAKS = seq(min(train$y),max(train$y),length=11)
        #BREAKS2 = seq(0,5,length=11)
        
        if(model == 'Mixed'){
                predY =  model.matrix(formula(mod,fixed.only=TRUE),foo)%*%fixef(mod)
                predD =  CalcDeriv(mod,model,dattype,train,funcTime = funcTime,w2VarName,foo2) 
        }
        if(model == 'Mixture'){
                predY = rowSums(predictY(mod,newdata=foo,var.time = 'time',olddata=train)$pred*priorp(mod,newdata=foo))
                predD = CalcDeriv(mod,model,dattype,train,funcTime = funcTime,w2VarName,foo2)
                #write('gridfunc2',file='output.txt')
                
        }
        Y = apply(foo[,1:3],1,function(v) truemean(type=dattype,v[1],v[2],v[3]))
        D = apply(foo[,1:3],1,function(v) truemean(type=dattype,v[1],v[2],v[3],deriv=1))

        
        
        maerror = mean(abs(Y-predY))
        mavelerror = mean(abs(D - predD))
        #return(list(predY=predY,predY2=predY2,maerror=maerror,mavelerror=mavelerror))
        return(list(maerror,mavelerror))
}


GraphGrid = function(mod,model = "Mixed",dattype,Vel=TRUE){
        if(is.null(mod)) { return(NA) }
        t = seq(1.5,10,by=1)
        w1 = 1
        b = unique(as.numeric(unlist(str_split(str_replace_all(levels(train$w2cut),'\\[|\\(|\\]',''),','))))
        w2 = seq(-2,2,length=10)
        foo = expand.grid(t,w1,w2,1)
        names(foo) = c('time','w1','w2','id')
        foo$w2cut = cut(foo$w2,breaks=b,include.lowest = TRUE)
        
        foo2 = foo
        foo2$time = foo$time - .5
        
        foo = cbind(foo,bs(foo$time,knots = quantile(train$time,c(.5)),Boundary.knots = range(train$time),degree = 2))
        names(foo)[-(1:5)] = paste('T',1:4,sep='')
        foo2 = cbind(foo2,bs(foo2$time,knots = quantile(train$time,c(.5)),Boundary.knots = range(train$time),degree = 2))
        names(foo2)[-(1:5)] = paste('T',1:4,sep='')
        
        BREAKS = seq(min(train$y),max(train$y),length=11)
        BREAKS2 = seq(0,5,length=11)
        
        if(model == 'Mixed'){
                predY = predict(mod,newdata=foo,allow.new.levels = TRUE)
                predY2 = predict(mod,newdata=foo2,allow.new.levels = TRUE)
        }
        if(model == 'Mixture'){
                predY = rowSums(predictY(mod,newdata=foo,olddata=train)$pred*priorp(mod,newdata=foo))
                predY2 = rowSums(predictY(mod,newdata=foo2,olddata=train)$pred*priorp(mod,newdata=foo2))
        }
        Y = apply(foo[,1:3],1,function(v) truemean(type=dattype,v[1],v[2],v[3]))
        Y2 = apply(foo2[,1:3],1,function(v) truemean(type=dattype,v[1],v[2],v[3]))
        
        
        #maerror = mean(abs(Y-predY))
        #mavelerror = mean(abs((Y-Y2)-(predY-predY2)))
        if(Vel){ image(t,w2,matrix((Y2-Y)-(predY2-predY),9,10))
        }else{ image(t,w2,matrix(Y-predY,9,10)) }
}

CalcK = function(mod){
        if(is.null(mod)) { return(NA) }
        mod$ng
}