# Programa extraído do site: https://www.ime.usp.br/~giapaula/textoregressao.htm
# Créditos: Prof. Dr. Gilberto Alvarenga Paula
# Adaptado por Caio L. N. Azevedo
# source("E:\\windows\\Unicamp\\Disciplinas\\1_semestre_2016\\ME 720 MLG\\Programas\\envel_gama.r")


envelgama <- function(fit.model,ligacao,estphi){
  # fit.model: objeto com o resultado do ajuste do MLG obtido através da função glm
  # ligacao: função de ligação (mesmo nome usado pela função glm (colocar entre aspas)
  # estphi: método de estimação para o parâmetro phi
  #         1 - Máxima verossimilhança 
  #         2 - Método dos Momentos 
  #         3 - Default do R
  
  #par(mfrow=c(1,1))
  X <- model.matrix(fit.model)
  n <- nrow(X)
  p <- ncol(X)
  w <- fit.model$weights
  W <- diag(w)
  H <- solve(t(X)%*%W%*%X)
  H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
  h <- diag(H)
  ro <- resid(fit.model,type="response")
  #fi <- (n-p)/sum((ro/(fitted(fit.model)))^ 2)
  #library(MASS)
  #fi <- gamma.shape(fit.model)$alpha
  if (estphi == 1)
  {
    library(MASS)
    fi <- gamma.shape(fit.model)$alpha
  }
  else if (estphi == 2)
  {
    #ro <- resid(fit.model,type="response")
    fi <-(n-p)/sum((ro/(fitted(fit.model)))^ 2)}
  else if (estphi == 3)
  {
    fi <- 1/summary(fit.model)$dispersion
  }
  td <- resid(fit.model,type="deviance")*sqrt(fi/(1-h))
  #
  e <- matrix(0,n,100)
  #
  for(i in 1:100){
    resp <- rgamma(n,fi)
    resp <- (fitted(fit.model)/fi)*resp
    fit <- glm(resp ~ X, family=Gamma(link=ligacao))
    w <- fit$weights
    W <- diag(w)
    H <- solve(t(X)%*%W%*%X)
    H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
    h <- diag(H)
    ro <- resid(fit,type="response")
    #phi <- (n-p)/sum((ro/(fitted(fit)))^ 2)
    #library(MASS)
    #fi <- gamma.shape(fit.model)$alpha
    if (estphi == 1)
    {
      library(MASS)
      phi <- gamma.shape(fit.model)$alpha
    }
    else if (esphi == 2)
    {
      #ro <- resid(fit.model,type="response")
      phi <-(n-p)/sum((ro/(fitted(fit.model)))^ 2)}
    else if (espphi == 3)
    {
      phi <- 1/summary(fit.model)$dispersion
    }
    
    e[,i] <- sort(resid(fit,type="deviance")*sqrt(phi/(1-h)))}
  #
  e1 <- numeric(n)
  e2 <- numeric(n)
  #
  for(i in 1:n){
    eo <- sort(e[i,])
    e1[i] <- (eo[2]+eo[3])/2
    e2[i] <- (eo[97]+eo[98])/2}
  #
  med <- apply(e,1,mean)
  faixa <- range(td,e1,e2)
  #par(pty="s")
  qqnorm(td, xlab="Percentil da N(0,1)",
         ylab="Resíduo Componente do Desvio", ylim=faixa, pch=16, main="",cex=1.1,cex.axis=1.1,cex.lab=1.1)
  par(new=T)
  #
  qqnorm(e1,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=1, main="")
  par(new=T)
  qqnorm(e2,axes=F,xlab="",ylab="", type="l",ylim=faixa,lty=1, main="")
  par(new=T)
  qqnorm(med,axes=F,xlab="", ylab="", type="l",ylim=faixa,lty=2, main="")
  #------------------------------------------------------------#                      
}