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

\[H_{0} : \mu \le 42\]
\[H_{1} : \mu >  42\]
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
## Questão 03
~~~r

pop31 <- c(2.19, 2.39, 2, 7.99, 1.98, 4.99, 1.79, 1.69, 2.19, 1.99)
pop32 <- c(1.35, 1.69, 2.49, 5.99, 1.29, 3.69, 1.33, 1.49, 1.49, 1.59)

pop31 <- as.matrix(pop31)
pop32 <- as.matrix(pop32)

n3 <- 8 # estou usando o seguinte digito da matricula 2018000 8 601

set.seed(180008601)

x3 <- sample(pop31,n3)
y3 <- sample(pop32,n3)

alpha <- 0.01
mu31 <- mean(pop31) # Média populacional da populacao 32
mu32 <- mean(pop32)
xbarra31 <- mean(x3)
ybarra32 <- mean(y3)

sp <- sqrt( ((n3 - 1)*var(x3) + (n3 - 1)*var(y3))/(n3 + n3 - 2) )

tc <- (xbarra31 - ybarra32)/(sp*sqrt((1/n3) + (1/n3))) # tc < 0
tt <- qt(alpha,df = n3 + n3 - 2)

c('t tabelado (tt)' = tt, 't calculado (tc)' = tc) # tt = -2.6245 // tc = -0.0711

tt < tc

# Pela funcao, temos :
t.test(x3,y3, alternative = "greater", conf.level = 0.99)[3] > alpha 
# p-value = 0.5520911 // alpha = 0.01
# p-value maior que alpha, entao nao rejeitamos H0

# Não rejeitamos a hipotese de que a media dos precos é mais alta
# no Whole Foods, a um nivel de confiança de 99%

~~~

## Questão 04

~~~r

# A)

pop41 <- c(4.55,4.5,4.4,4.38,4.38)

pop42 <- c(4.94,4.9,4.85,4.85,4.85)

n4 <- 4

alpha <- 0.1

set.seed(180008601)

x4 <- sample(pop41,n4)
xbarra4 <- mean(x4)
s41 <- sd(x4)

y4 <- sample(pop42,n4)
ybarra4 <- mean(y4)
s42 <- sd(y4)

tt <- c(qf(alpha/2,n4-1,n4-1), qf(1 - (alpha/2), n4-1,n4-1))
tc <- (s42/s41)**(2)

c('t tabelado (tt)' = tt, 't calculado (tc)' = tc) 
# tt = [0.1077978 ,  9.2766282] // tc = 0.2897078

# Utilizando a funcao 

var.test(x4,y4)[3] > alpha
# p-value = 0.3360 // alpha = 0.1
# p-value maior que alpha, entao nao rejeitamos H0

# Verifica-se que , a um nivel de significancia de 1%, temos que 
# nao rejeitamos a hipotese de que as variancias dos tipos de 
# investimentos sao diferntes .

# B)

alpha <- 0.1

sp <- sqrt(((n4 - 1)*(s41)**(2) + (n4 - 1)*(s42)**(2))/(n4 + n4 - 2))

tc <- (xbarra4 - ybarra4)/(sp*sqrt((1/n4) + (1/n4))) # Note que tc < 0
tt <- c(qt((alpha/2), df= n1 + n2 - 2),qt(1 -(alpha/2), df= n1 + n2 - 2))

c('t tabelado (tt)' = tt, 't calculado (tc)' = tc)
# tt = [-1.7530504  , 1.7530504] // tc = -9.296591

tt[1] < tc &  tc < tt[2]

# utilizanto a função t.test

t.test(x4,y4)[3] < alpha
# p-value = 0.0003727682 // alpha = 0.1
# p-value menor que alpha, entao rejeitamos H0

# Verificamos que , a um nivel de confiança  de 1%, rejeitamos a de que a diferença
# entre as medias dos tipos de investimento e igual de 0 .

~~~

## Questão 05

~~~r

pop5 <- c(4.0,3.5,6.1,5.8,5.4,4.4,4.9,3.9,5.1,5.3,4.1,4.2,4.8,4.7,3.8,4.8,5.3,
          5.5,3.6,3.5,4.7,3.3,3.7,6.3,5.7,3.9,4.6,4.7,4.1,4.3)

pop5 <- as.matrix(pop5)

n5 <- c(8 + 6 + 0 + 1)

set.seed(180008601)

x5 <- sample(pop5,n5)
xbarra5 <- mean(x5)

alpha <- 0.05
mu <- 5

tc <- (xbarra5 - mu)/(sd(x5)*sqrt(n5)) # Note que tc < 0
tt <- qnorm(alpha)

c('t tabelado (tt)' = tt, 't calculado (tc)' = tc)
# tt = -1.64485363  //  tc = -0.06621417 

# Pela funcao temos
t.test(x5, mu= 5, alternative ='less')[3] > alpha
# p-value = 0.1687318 // alpha = 0.05
# sendo o p-valor superior ao meu alpha,  nao rejeitamos H0

# Verifica-se que , a um nivel de confianca de 95 %,  nao ha motivos para rejeitar
# a hipotese de que a proporcao de pacientes para os quais o tempo de recao apos a 
# utilizacao desse medicamento maior que 5 minutos .
~~~

## Questão 06

~~~r

set.seed(180008601)

alpha <- 0.05

n1 <- 400
x1 <- 220
p1 <- x1/n1

n2 <- 400
x2 <- 192
p2 <- x2/n2

zc <- (p1 - p2)/sqrt((p1*(1-p1)/n1) + (p2*(1-p2)/n2)) # zc > 0
zt <- qnorm(1 - alpha)

c('z tabelado (zt)' = zt, 'z calculado (zc)' = zc)
#  zt = 1.644854    // zc =  1.985666 

zt < zc

# Pela funcao, temos

num_positive = c(x1, x2)
num_total = c(n1, n2)
prop.test(x = num_positive, num_total, alternative ="greater")[3] < alpha
# p-value =  0.02806352  //  alpha = 0.05
# p-value  menor que alpha, entao rejeitamos H0

# Verifica-se que , a um nivel de confianca de 5%, rejeitamos a hipotese de que 
# a proporcao de europeus que estao mais otimistas quanto ao panorama economico
# futuro e menor que a proporcao de  americanos que responderam afirmativamente,
# entao, podemos afirmar que  os europeus estao mais otimistas quanto ao panorama
# economico futuro
~~~

