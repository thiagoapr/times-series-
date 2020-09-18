library(tidyverse)

# Função que simula um AR(1)
# phi: valor do parâmetro
# n: númerio de observações
# amostra: número de amostras 

ar1.sim <- function(phi, n, amostra) {

    db <- NULL
    
    for(j in 1:amostra){
    
      e <- rnorm(n-1, mean=0, sd = 1)
      y <- 0
      
      for(i in 1:n-1){
        value <- phi*tail(y, n=1) + e[i]
        y <- rbind(y, value)
      }
      
    db0 <- data.frame(Amostra = j, y = y)
    db <- rbind(db, db0)
    
    }

    db
}

# Executa a função (phi = 1)

db <- ar1.sim(phi = 1, n = 100, amostra = 1000)

par <- NULL
for (i in 1:1000){
  par0 <- lm(data = filter(db, Amostra == i), y ~ lag(y) -1)$coef
  par <- rbind(par, par0)
}

hist(par, breaks = 30)

colMeans(par)

# Executa a função (phi = 0.9)

db <- ar1.sim(phi = 0.9, n = 100, amostra = 1000)

par <- NULL
for (i in 1:1000){
  par0 <- lm(data = filter(db, Amostra == i), y ~ lag(y) -1)$coef
  par <- rbind(par, par0)
}

hist(par, breaks = 30)

colMeans(par)

# Executa a função (phi = 0.5)

db <- ar1.sim(phi = 0.5, n = 100, amostra = 1000)

par <- NULL
for (i in 1:1000){
  par0 <- lm(data = filter(db, Amostra == i), y ~ lag(y) -1)$coef
  par <- rbind(par, par0)
}

hist(par, breaks = 30)

colMeans(par)
