# Inferencia II

Disciplina de inferencia II

## Questão 01
~~~r

pop1 <- c(40.1, 45.0, 39.1, 43.9, 45.8, 44.2, 37.4, 44.7, 45.2,
       41.2, 40.7, 43.1, 44.1, 42.6, 40.6, 41.8, 42.9, 45.8,
       43.4, 45.5, 44.8, 42.3, 40.4, 41.9, 42.1, 44.4, 43.7,
       43.9, 42.6, 45.5, 41.5, 45.2, 43.6, 42.8, 43.3, 45.7)

pop1 <- as.matrix(pop1)

n1 <- c(8 + 6 + 0 + 1)

set.seed(180008601)

x1 <- sample(pop1,n1)

mu <- 42
alpha <- 0.05
sigma <- sqrt(var(pop1))
xbarra <- mean(x1)

# Como temos a media populacional, e desvio padrao populacional, usaremos o teste
# para média e desvio padrao populacionais conhecidos, tal que :

zc <- (xbarra - mu)/(sigma/sqrt(n)) # Perceba que zc > 0
zt <- qnorm(1-alpha)

c('z tabelado (zt)' = zt, 'z calculado (zc)' = zc) ## zt = 1.64 // zc = 1.93

zt < zc

# Pela Funcao, temos
t.test(x1, mu=mu, alternative = "greater")[3] > alpha 
# alpha = 0.05 // p-valor = 0.09
# p-value maior que alpha, entao nao rejeitamos H0

# Logo, não rejeitamos a hipotese de que a media populacional é inferior a 42 .

~~~
