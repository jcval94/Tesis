library(VGAM)
library(ggplot2)
set.seed(1)       # for reproducible example
alfa=1;beta=1;mu=0;sigma=1

p1 <- ggplot(data = data.frame(x = c(-3, 3)), aes(x), colour = "blue") +
  stat_function(fun = dbetanorm, n = 101, 
                args = list(shape1=alfa,shape2=beta,mean=mu,sd=sigma)) + 
  ylab("") + scale_y_continuous(breaks = NULL) +
  ggtitle(paste0("Parámetros: alpha = ",alfa,", beta = ",beta
                 ,", mu = ",mu,", sigma = ",sigma))

p1


alfa=1.5;beta=.5;mu=.7;sigma=1

p1 <- ggplot(data = data.frame(x = c(-3, 6)), aes(x), colour = "blue") +
  stat_function(fun = dbetanorm, n = 101, 
                args = list(shape1=alfa,shape2=beta,mean=mu,sd=sigma)) + 
  ylab("") + scale_y_continuous(breaks = NULL) +
  ggtitle(paste0("Parámetros: alpha = ",alfa,", beta = ",beta
                 ,", mu = ",mu,", sigma = ",sigma))

p1

alfa=.1;beta=.1;mu=0;sigma=.5

p1 <- ggplot(data = data.frame(x = c(-3, 3)), aes(x), colour = "blue") +
  stat_function(fun = dbetanorm, n = 101, 
                args = list(shape1=alfa,shape2=beta,mean=mu,sd=sigma)) + 
  ylab("") + scale_y_continuous(breaks = NULL) +
  ggtitle(paste0("Parámetros: alpha = ",alfa,", beta = ",beta
                 ,", mu = ",mu,", sigma = ",sigma))

p1

alfa=.25;beta=.25;mu=0;sigma=.5

p1 <- ggplot(data = data.frame(x = c(-3, 3)), aes(x), colour = "blue") +
  stat_function(fun = dbetanorm, n = 101, 
                args = list(shape1=alfa,shape2=beta,mean=mu,sd=sigma)) + 
  ylab("") + scale_y_continuous(breaks = NULL) +
  ggtitle(paste0("Parámetros: alpha = ",alfa,", beta = ",beta
                 ,", mu = ",mu,", sigma = ",sigma))
p1


alfa=.01;beta=.1;mu=0;sigma=.5

p1 <- ggplot(data = data.frame(x = c(-13, 6)), aes(x), colour = "blue") +
  stat_function(fun = dbetanorm, n = 101, 
                args = list(shape1=alfa,shape2=beta,mean=mu,sd=sigma)) + 
  ylab("") + scale_y_continuous(breaks = NULL) +
  ggtitle(paste0("Parámetros: alpha = ",alfa,", beta = ",beta
                 ,", mu = ",mu,", sigma = ",sigma))

p1

alfa=.5;beta=10;mu=1;sigma=1

p1 <- ggplot(data = data.frame(x = c(-3, 3)), aes(x), colour = "blue") +
  stat_function(fun = dbetanorm, n = 101, 
                args = list(shape1=alfa,shape2=beta,mean=mu,sd=sigma)) + 
  ylab("") + scale_y_continuous(breaks = NULL) +
  ggtitle(paste0("Parámetros: alpha = ",alfa,", beta = ",beta
                 ,", mu = ",mu,", sigma = ",sigma))

p1

alfa=10;beta=.5;mu=0;sigma=1

p1 <- ggplot(data = data.frame(x = c(-1, 6)), aes(x), colour = "blue") +
  stat_function(fun = dbetanorm, n = 101, 
                args = list(shape1=alfa,shape2=beta,mean=mu,sd=sigma)) + 
  ylab("") + scale_y_continuous(breaks = NULL) +
  ggtitle(paste0("Parámetros: alpha = ",alfa,", beta = ",beta
                 ,", mu = ",mu,", sigma = ",sigma))

p1

alfa=10;beta=.1;mu=0;sigma=.1

p1 <- ggplot(data = data.frame(x = c(-0, 1)), aes(x), colour = "blue") +
  stat_function(fun = dbetanorm, n = 101, 
                args = list(shape1=alfa,shape2=beta,mean=mu,sd=sigma)) + 
  ylab("") + scale_y_continuous(breaks = NULL) +
  ggtitle(paste0("Parámetros: alpha = ",alfa,", beta = ",beta
                 ,", mu = ",mu,", sigma = ",sigma))

p1
