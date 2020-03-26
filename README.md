# Inferencia II

Prova 03

Aluno : Manuel Ferreira Junior
Matricula : 20180008601
Disciplina : Inferencia II

~~~r
library(TeachingDemos) 
library(OneTwoSamples)
~~~

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

## Questão 02

~~~r

pop2 <- c(1200, 1100, 900, 1250, 1300, 1290,  1100, 1060, 1180, 1120, 1160,
          1140, 1190, 1110, 1100, 1220)

pop2 <- as.matrix(pop2)
sd(pop2)
n2 <- c(2*(0 + 1))

set.seed(180008601)

x2 <- sample(pop2, n2)

sigma2 <- (1e5)**(2)
alpha <- 0.1

qc <-((n2 - 1)*var(x2))/sigma2 
qt <- qchisq(alpha,n2-1)

c('q tabelado (qt)' = qt, 'q calculado (qc)' = qc) # qt = 0.0158 // qc = 0.00000018

qt > qc # Regiao inferior da curva

# Pela funcao, temos :
sigma.test(x2,sigmasq=sigma2,conf.level = 0.90)[3] < alpha
# p-value = 0.000677 // alpha = 0.1
# p-value menor que alpha, entao rejeitamos H0 

# Verifica-se que , rejeitamos a hipotese de que a variancia populacional
# é superior a (100.000 horas)^2, ou seja, rejeitamos a hipotese de que 
# o desvio padrão do tempo de vida das lampadas é superior ou igual a 100.000
# horas, a um nivel de confianca de 90 % .
~~~
