
ggplot(data = data.frame(x = c(70, 80)), aes(x)) +
       stat_function(fun = dnorm, n = 101, args = list(mean = 75, sd = 2)) + ylab("") +
       scale_y_continuous(breaks = NULL)+
  geom_vline(aes(xintercept=75),linetype="dashed", size=1)

# TT<-t.test(rnorm(10000,75,1),conf.level = .995)
# 
# quantile(rnorm(10000,75,1),.05)
