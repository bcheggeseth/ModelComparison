################################################
#
#       Simulation to Compare Methods
#       Date Created: Sept 2, 2016
#       Date Updated: Nov 10, 2016
#       Notes: 
#       Update truemean for the ynorm transformation
#       Summarize results nicely and then...
#       some with intercept and baseline correlated... (variation here)
#     
#       Start with Nonlinear ones        
#       Trying confidence intervals via bootstrapping and 3D graphing.

############################################








#Mixed Effects Model

m1 = fitModel(train,funcTime = 'Linear',randEf = 'Intercept')
m2 = fitModel(train,funcTime = 'Quadratic',randEf = 'Intercept')
m3 = fitModel(train,funcTime = 'B-spline',randEf = 'Intercept')
cat('.')


m4 = fitModel(train,funcTime = 'Linear',randEf = 'Slope')
m5 = fitModel(train,funcTime = 'Quadratic',randEf = 'Slope')
m6 = fitModel(train,funcTime = 'B-spline',randEf = 'Slope')
cat('.')
 write(paste0('here1_',j),file=paste0('output',L,'.txt'),append=TRUE)

m7 = fitModel(train,funcTime = 'Linear',randEf = 'Intercept',w2VarName = 'w2cut')
m8 = fitModel(train,funcTime = 'Quadratic',randEf = 'Intercept',w2VarName = 'w2cut')
m9 = fitModel(train,funcTime = 'B-spline',randEf = 'Intercept',w2VarName = 'w2cut')
cat('.')

m10 = fitModel(train,funcTime = 'Linear',randEf = 'Slope',w2VarName = 'w2cut')
m11 = fitModel(train,funcTime = 'Quadratic',randEf = 'Slope',w2VarName = 'w2cut')
m12 = fitModel(train,funcTime = 'B-spline',randEf = 'Slope',w2VarName = 'w2cut')
cat('.')

 write(paste0('here2_',j),file=paste0('output',L,'.txt'),append=TRUE)

#m1.2 = fitModel(train,funcTime = 'Linear',randEf = 'Intercept',transf = TRUE)
#m2.2 = fitModel(train,funcTime = 'Quadratic',randEf = 'Intercept',transf = TRUE)
#m3.2 = fitModel(train,funcTime = 'B-spline',randEf = 'Intercept',transf = TRUE)
#cat('.')

#m4.2 = fitModel(train,funcTime = 'Linear',randEf = 'Slope',transf = TRUE)
#m5.2 = fitModel(train,funcTime = 'Quadratic',randEf = 'Slope',transf = TRUE) 
#m6.2 = fitModel(train,funcTime = 'B-spline',randEf = 'Slope',transf = TRUE)
#cat('.')


 write(paste0('here3_',j),file=paste0('output',L,'.txt'),append=TRUE)


#m7.2 = fitModel(train,funcTime = 'Linear',randEf = 'Intercept',w2VarName = 'w2cut',transf = TRUE)
#m8.2 = fitModel(train,funcTime = 'Quadratic',randEf = 'Intercept',w2VarName = 'w2cut',transf = TRUE)
#m9.2 = fitModel(train,funcTime = 'B-spline',randEf = 'Intercept',w2VarName = 'w2cut',transf = TRUE)
#cat('.')

#m10.2 = fitModel(train,funcTime = 'Linear',randEf = 'Slope',w2VarName = 'w2cut',transf = TRUE)
#m11.2 = fitModel(train,funcTime = 'Quadratic',randEf = 'Slope',w2VarName = 'w2cut',transf = TRUE) 
#m12.2 = fitModel(train,funcTime = 'B-spline',randEf = 'Slope',w2VarName = 'w2cut',transf = TRUE)
#cat('.')


 write(paste0('here4_',j),file=paste0('output',L,'.txt'),append=TRUE)



#m21 = fitModel(train,model='Mixture',funcTime = 'Linear')
#m22 = fitModel(train,model='Mixture',funcTime = 'Quadratic')
#m23 = fitModel(train,model='Mixture',funcTime = 'B-spline')
cat('.')

m24 = fitModel(train,model='Mixture',funcTime = 'Linear',randEf = 'Intercept')
m25 = fitModel(train,model='Mixture',funcTime = 'Quadratic',randEf = 'Intercept')
m26 = fitModel(train,model='Mixture',funcTime = 'B-spline',randEf = 'Intercept')
cat('.')


m27 = fitModel(train,model='Mixture',funcTime = 'Linear',randEf = 'Slope') 
m28 = fitModel(train,model='Mixture',funcTime = 'Quadratic',randEf = 'Slope')
m29 = fitModel(train,model='Mixture',funcTime = 'B-spline',randEf = 'Slope')
cat('.')


#m30 = fitModel(train,model='Mixture',funcTime = 'Linear',w2VarName = 'w2cut') 
#m31 = fitModel(train,model='Mixture',funcTime = 'Quadratic',w2VarName = 'w2cut')
#m32 = fitModel(train,model='Mixture',funcTime = 'B-spline',w2VarName = 'w2cut')
cat('.')


m30 = fitModel(train,model='Mixture',funcTime = 'Linear',randEf = 'Intercept',w2VarName = 'w2cut')
m31 = fitModel(train,model='Mixture',funcTime = 'Quadratic',randEf = 'Intercept',w2VarName = 'w2cut')
m32 = fitModel(train,model='Mixture',funcTime = 'B-spline',randEf = 'Intercept',w2VarName = 'w2cut')
cat('.')

m33 = fitModel(train,model='Mixture',funcTime = 'Linear',randEf = 'Slope',w2VarName = 'w2cut')
m34 = fitModel(train,model='Mixture',funcTime = 'Quadratic',randEf = 'Slope',w2VarName = 'w2cut')
m35 = fitModel(train,model='Mixture',funcTime = 'B-spline',randEf = 'Slope',w2VarName = 'w2cut')
cat('.')


 write(paste0('here5_',j),file=paste0('output',L,'.txt'),append=TRUE)


#m21.2 = fitModel(train,model='Mixture',funcTime = 'Linear',transf = TRUE)
#m22.2 = fitModel(train,model='Mixture',funcTime = 'Quadratic',transf = TRUE)
#m23.2 = fitModel(train,model='Mixture',funcTime = 'B-spline',transf = TRUE)
cat('.')

#m24.2 = fitModel(train,model='Mixture',funcTime = 'Linear',randEf = 'Intercept',transf = TRUE)
#m25.2 = fitModel(train,model='Mixture',funcTime = 'Quadratic',randEf = 'Intercept',transf = TRUE)
#m26.2 = fitModel(train,model='Mixture',funcTime = 'B-spline',randEf = 'Intercept',transf = TRUE)
cat('.')

#m27.2 = fitModel(train,model='Mixture',funcTime = 'Linear',randEf = 'Slope',transf = TRUE) #
#m28.2 = fitModel(train,model='Mixture',funcTime = 'Quadratic',randEf = 'Slope',transf = TRUE) #
#m29.2 = fitModel(train,model='Mixture',funcTime = 'B-spline',randEf = 'Slope',transf = TRUE)
cat('.')


#m30.2 = fitModel(train,model='Mixture',funcTime = 'Linear',w2VarName = 'w2cut',transf = TRUE)
#m31.2 = fitModel(train,model='Mixture',funcTime = 'Quadratic',w2VarName = 'w2cut',transf = TRUE)
#m32.2 = fitModel(train,model='Mixture',funcTime = 'B-spline',w2VarName = 'w2cut',transf = TRUE)
cat('.')

#m33.2 = fitModel(train,model='Mixture',funcTime = 'Linear',randEf = 'Intercept',w2VarName = 'w2cut',transf = TRUE)
#m34.2 = fitModel(train,model='Mixture',funcTime = 'Quadratic',randEf = 'Intercept',w2VarName = 'w2cut',transf = TRUE)
#m35.2 = fitModel(train,model='Mixture',funcTime = 'B-spline',randEf = 'Intercept',w2VarName = 'w2cut',transf = TRUE)
cat('.')

#m36.2 = fitModel(train,model='Mixture',funcTime = 'Linear',randEf = 'Slope',w2VarName = 'w2cut',transf = TRUE)
#m37.2 = fitModel(train,model='Mixture',funcTime = 'Quadratic',randEf = 'Slope',w2VarName = 'w2cut',transf = TRUE)
#m38.2 = fitModel(train,model='Mixture',funcTime = 'B-spline',randEf = 'Slope',w2VarName = 'w2cut',transf = TRUE)
cat('.')


 write(paste0('here6_',j),file=paste0('output',L,'.txt'),append=TRUE)


train = cbind(train,bs(train$time,knots = quantile(train$time,c(.5)),Boundary.knots = range(train$time),degree=2,intercept=FALSE))
names(train)[-(1:9)] = paste('T',1:3,sep='')

results = matrix(NA,ncol = 5,nrow = 24)


for(i in 1:12){
        results[i,1] = CalcBIC(eval(parse(text=paste('m',i,sep='')))$mod)
        results[i,2] = CalcMSE(eval(parse(text=paste('m',i,sep='')))$mod,test,w2VarName = ifelse(i<7,'w2','w2cut'),train = train)
        results[i,3:4] = unlist(CalcGrid(eval(parse(text=paste('m',i,sep='')))$mod,model='Mixed',dattype=L,train =train,funcTime = eval(parse(text=paste('m',i,sep='')))$funcTime,w2VarName = eval(parse(text=paste('m',i,sep='')))$w2VarName))
}


write(paste0('here8_',j),file=paste0('output',L,'.txt'),append=TRUE)


#for(i in 1:12){
#        results[12+i,1] = CalcBIC(eval(parse(text=paste('m',i,'.2',sep=''))))
#        results[12+i,2] = CalcMSE(eval(parse(text=paste('m',i,'.2',sep=''))),test,w2VarName = #ifelse(i<7,'w2','w2cut'),transf=TRUE,train =train)
#        results[12+i,3:4] = unlist(CalcGrid(eval(parse(text=paste('m',i,'.2',sep=''))),dattype=L,train =train))
#}


  write(paste0('here9_',j),file=paste0('output',L,'.txt'),append=TRUE)
 

for(i in 24:35){
  
        results[i-11,1] = CalcBIC(eval(parse(text=paste('m',i,sep='')))$mod,model='Mixture')
        #write('here1',file='output.txt',append=TRUE)
        results[i-11,2] = CalcMSE(eval(parse(text=paste('m',i,sep='')))$mod,test,model='Mixture',w2VarName = ifelse(i<30,'w2','w2cut'),train =train)
        #write('here2',file='output.txt',append=TRUE)
        results[i-11,3:4] = unlist(CalcGrid(eval(parse(text=paste('m',i,sep='')))$mod,dattype=L,model='Mixture',train =train,funcTime = eval(parse(text=paste('m',i,sep='')))$funcTime,w2VarName = eval(parse(text=paste('m',i,sep='')))$w2VarName))
        #write('here3',file='output.txt',append=TRUE)
        results[i-11,5] = CalcK(eval(parse(text=paste('m',i,sep='')))$mod)
        #write('here4',file='output.txt',append=TRUE)
}


write(paste0('here10_',j),file=paste0('output',L,'.txt'),append=TRUE)

#for(i in 21:38){
 #       results[22+i,1] = CalcBIC(eval(parse(text=paste('m',i,'.2',sep=''))),model='Mixture')
 #       results[22+i,2] = CalcMSE(eval(parse(text=paste('m',i,'.2',sep=''))),test,model='Mixture',w2VarName = #ifelse(i<30,'w2','w2cut'),transf=TRUE,train =train)
  #      results[22+i,3:4] = unlist(CalcGrid(eval(parse(text=paste('m',i,'.2',sep=''))),dattype=L,model='Mixture',train =train))
   #     results[22+i,5] = CalcK(eval(parse(text=paste('m',i,sep=''))))
#}

write(paste0('here11_',j),file=paste0('output',L,'.txt'),append=TRUE)


results = as.data.frame(results)
names(results) = c("BIC",'MSE','Grid','VelGrid','K')
results = results %>% 
        mutate(Model = c(rep("Mixed",12),rep("Mixture",12))) %>%
        mutate(RE = c(rep(c(rep("Random Intercept",3),rep("Random Slope",3)),2),rep(c(rep("Random Intercept",3),rep("Random Slope",3)),2)) ) %>%                      
        mutate(W = c(rep(c("Con","Cat"),each=6),rep(c("Con","Cat"),each=6) )) %>%    
        mutate(Mean = rep(c("Linear",'Quad',"Bspline"),8) )  %>%
        mutate(DataType = L)



#Add rankings.... to get a sense of which model is best at all three of these measures (but separate out raw and transformed)
#Anything else?
