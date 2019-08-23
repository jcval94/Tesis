set.seed(31109)
R<-c(rexp(500,4),rweibull(500,5,3))
q<-quantile(R,c(.975,.025))
R<-R[R>q[2] & R<q[1]]
ra<-quantile(R)
int<-ra[4]-ra[2]
  RI<-c(ra[2]-int,ra[4]+int)

sum(R>RI[2] & R<RI[1])

R_int<-R[R>RI[2] & R<RI[1]]


plot(density(R))
library(ADGofTest)

X<-R;gen=1;Cont=TRUE;plot=FALSE;p.val_min=-1;criteria=2;DPQR=T
{
  DIS<-list(Nombres=c("exp","pois","beta","gamma","lnorm","norm","weibull","nbinom","hyper","cauchy","binom","t"),
            p=c(stats::pexp,stats::ppois,stats::pbeta,stats::pgamma,stats::plnorm,stats::pnorm,stats::pweibull,stats::pnbinom,stats::phyper,stats::pcauchy,stats::pbinom,stats::pt),
            d=c(stats::dexp,stats::dpois,stats::dbeta,stats::dgamma,stats::dlnorm,stats::dnorm,stats::dweibull,stats::dnbinom,stats::dhyper,stats::dcauchy,stats::dbinom,stats::dt),
            q=c(stats::qexp,stats::qpois,stats::qbeta,stats::qgamma,stats::qlnorm,stats::qnorm,stats::qweibull,stats::qnbinom,stats::qhyper,stats::qcauchy,stats::qbinom,stats::qt),
            r=c(stats::rexp,stats::rpois,stats::rbeta,stats::rgamma,stats::rlnorm,stats::rnorm,stats::rweibull,stats::rnbinom,stats::rhyper,stats::rcauchy,stats::rbinom,stats::rt),
            d_c=c(1,0,1,1,1,1,1,0,0,1,0,1),
            indicadora=c("0","0","01","0","0","R","0","0","0","R","0","R")
  )
  DIS<-purrr::map(DIS,~subset(.x, DIS$d_c==as.numeric(Cont)))
  DIS_0<-purrr::map(DIS,~subset(.x, DIS$indicadora=="0"))
  DIS_R<-purrr::map(DIS,~subset(.x, DIS$indicadora=="R"))
  DIS_01<-purrr::map(DIS,~subset(.x, DIS$indicadora=="01"))
  if(sum(purrr::map_dbl(DIS_0,~length(.x)))==0){DIS_0<-NULL}
  if(sum(purrr::map_dbl(DIS_R,~length(.x)))==0){DIS_R<-NULL}
  if(sum(purrr::map_dbl(DIS_01,~length(.x)))==0){DIS_01<-NULL}
  bt<-X
  despl<-0
  escala<-1
  eps<-1E-15
  if (sum(X<0)>0){
    if (sum(X<0)/length(X)<0.03){
      bt<-ifelse(X<0,eps,X)
      b_0<-bt
    }else{
      b_0<-bt-min(bt)+eps
      despl<- min(bt)
    }
  }else{
    b_0<-bt
  }
  if(max(X)>1){
    escala<-max(bt)
    b_01<-(bt-despl)/(escala-despl)
  }else{
    b_01<-bt
  }
  fit_b<-function(bt,dist="",Cont.=Cont){
    if(is.null(dist)){return(NULL)}
    Disc<-!Cont
    aju<-list()
    if(!dist %in% DIS_01$Nombres){
      suppressWarnings(aju[[1]]<-try(fitdistrplus::fitdist(bt,dist,method = "mle",discrete = Disc),silent = TRUE))
    }
    suppressWarnings(aju[[2]]<-try(fitdistrplus::fitdist(bt,dist,method = "mme",discrete = Disc),silent = TRUE))
    suppressWarnings(aju[[3]]<-try(fitdistrplus::fitdist(bt,dist,method = c("mge"),discrete = Disc),silent = TRUE))
    suppressWarnings(aju[[4]]<-try(MASS::fitdistr(bt,dist),silent = TRUE))
    if(!assertthat::is.error(aju[[4]])){aju[[4]]$distname<-dist}
    if(assertthat::is.error(aju[[1]]) & assertthat::is.error(aju[[2]]) &
       assertthat::is.error(aju[[3]]) & assertthat::is.error(aju[[4]])){
      return(list())
    }
    funcionales<-!purrr::map_lgl(aju,~assertthat::is.error(.x))
    names(aju)<-c("mle","mme","mge","mlg2")
    aju<-aju[funcionales]
    return(aju)
  }
  suppressWarnings(try(aju_0<-purrr::map(DIS_0$Nombres,~fit_b(b_0,.x)),silent = TRUE))
  suppressWarnings(try(aju_R<-purrr::map(DIS_R$Nombres,~fit_b(bt,.x)),silent = TRUE))
  suppressWarnings(try(aju_01<-purrr::map(DIS_01$Nombres,~fit_b(b_01,.x)),silent = TRUE))
  AAA<-list(aju_0,aju_R,aju_01)
  descate<-purrr::map(AAA,~length(.x))!=0
  AAA<-AAA[descate]
  bts<-list(b_0,bt,b_01)[descate]
  num<-0
  Compe<-data.frame()
  
  
  for (aju_ls in 1:length(AAA)) {
    aju<-AAA[[aju_ls]]
    aju<-aju[purrr::map_lgl(aju,~length(.x)>0)]
    bs<-bts[[aju_ls]]
    for (comp in 1:length(aju)) {
      for (ress in 1:length(aju[[comp]])) {
        num<-num+1
        if(length(aju[[comp]])!=0){evaluar<-aju[[comp]][[ress]]}
        else{evaluar<-NULL}
        if (is.null(evaluar) | length(evaluar)==0 |
            c(NA) %in% evaluar$estimate | c(NaN) %in% evaluar$estimate) {next()}
        distname<-evaluar$distname
        method<-names(aju[[comp]])[[ress]]
        dist_pfun<-try(get(paste0("p",distname)),silent = TRUE)
        dist_rfun<-try(get(paste0("r",distname)),silent = TRUE)
        if(assertthat::is.error(dist_rfun)){next()}
        
        argumentos<-formalArgs(dist_pfun)
        argumentos<-argumentos[argumentos %in% names(evaluar$estimate)]
        num_param<-length(argumentos)
        evaluar$estimate<-evaluar$estimate[names(evaluar$estimate) %in% argumentos]
        if(num_param==1){
          EAD<-try(AD<-ADGofTest::ad.test(bs,dist_pfun,evaluar$estimate[1]),silent = TRUE)
          if (Cont) {KS<-try(KS<-stats::ks.test(bs,dist_pfun,evaluar$estimate[1]),silent = TRUE)}
          else{KS<-data.frame(p.value=0)}
          if(assertthat::is.error(EAD) | assertthat::is.error(KS)){next()}
          if(is.na(KS$p.value)){next()}
          Chs<-data.frame(p.value=0)
        }
        if(num_param==2){
          suppressWarnings(
            Err_pl<-try(AD<-ADGofTest::ad.test(bs,dist_pfun,evaluar$estimate[1],evaluar$estimate[2]),silent = TRUE))
          
          if (assertthat::is.error(Err_pl)) {
            Err_pl<-try(AD<-ADGofTest::ad.test(bs,dist_pfun,evaluar$estimate[1],,evaluar$estimate[2]),silent = TRUE)
          }
          if (Cont) {Err_pl2<-try(KS<-stats::ks.test(bs,dist_pfun,evaluar$estimate[1],evaluar$estimate[2]),silent = TRUE)}
          else{Err_pl2<-KS<-data.frame(p.value=0)}
          if(assertthat::is.error(Err_pl) | assertthat::is.error(Err_pl2)){next()}
          if(is.na(Err_pl2$p.value)){next()}
          suppressWarnings(
            EE_Chs<-try(dst_chsq<-dist_rfun(length(bs),evaluar$estimate[1],evaluar$estimate[2]))
          )
          if(assertthat::is.error(EE_Chs) | prod(is.na(EE_Chs))==1){
            dst_chsq<-dist_rfun(length(bs),evaluar$estimate[1],,evaluar$estimate[2])
          }
          Chs<-data.frame(p.value=0)
        }
        pvvv<-p.val_min
        if(criteria==1){
          crit<-AD$p.value>pvvv | KS$p.value>pvvv | Chs$p.value >pvvv
        }else{
          crit<-AD$p.value>(pvvv) & KS$p.value>(pvvv)
        }
        
        if(crit){
          print(distname)
          if(aju_ls %in% 3){
            estimate3=despl
            estimate4=escala
          }else if(aju_ls==1){
            estimate3=despl
            estimate4=1
          }else{
            estimate3=0
            estimate4=1
          }
          Compe<-rbind(Compe,data.frame(Dist=distname,method=method,AD_p.v=AD$p.value,KS_p.v=KS$p.value,
                                        Chs_p.v=Chs$p.value,
                                        estimate1=evaluar$estimate[1],estimate2=evaluar$estimate[2],
                                        estimateLL1=estimate3,estimateLL2=estimate4
          ))
        }else{
          next()
        }
        
      }
    }
  }
  if (nrow(Compe)==0) {
    warning("No fit")
    return(NULL)
  }
  Compe$PV_S<-rowSums(Compe[,2:4])
  WNR<-Compe[Compe$PV_S %in% max(Compe$PV_S),][1,]
  distW<-WNR$Dist
  paramsW<-WNR[1,names(Compe)[startsWith(names(Compe),"estim")]]
  paramsW<-paramsW[,!is.na(paramsW)]
  if(gen<=0){gen<-1}
  generadora_r<-function(n=gen,dist=distW,params=paramsW){
    fn<-get(paste0("r",dist))
    formals(fn)[1]<-n
    for (pr in 1:(length(params)-2)) {
      formals(fn)[pr+1]<-as.numeric(params[pr])
    }
    fn()*params[,length(params)]+params[,length(params)-1]
  }
  if(DPQR){
    generadoras<-function(x,tipo,dist=distW,params=paramsW){
      fn<-get(paste0(tipo,dist))
      formals(fn)[1]<-x
      for (pr in 1:(length(params)-2)) {
        formals(fn)[pr+1]<-as.numeric(params[pr])
      }
      class(fn)<-"gl_fun"
      fn
    }
    rfit<-generadora_r
    class(rfit)<-"gl_fun"
    pfit<-generadoras(1,"p")
    qfit<-generadoras(1,"q")
    dfit<-generadoras(1,"d")
  }
  MA<-generadora_r()
  paramsAUX<-c()
  paramsW2<-data.frame()
  for(cl in 1:nrow(paramsW)){
    paramsW2<-rbind(paramsW2,round(paramsW[1,],3))
  }
  if(paramsW2[,length(paramsW2)]!=1 | paramsW2[,length(paramsW2)-1]!=0){
    distribu<-paste0(WNR$Dist,"(",paste0(paramsW2[,1:(length(paramsW2)-2)],collapse = ", "),")*",paramsW2[,length(paramsW2)],"+",paramsW2[,length(paramsW2)-1])
  }else{
    distribu<-paste0(WNR$Dist,"(",paste0(paramsW2[,1:(length(paramsW2)-2)],collapse = ", "),")")
  }
  p<-c()
  if(plot){
    DF<-rbind(data.frame(A="Fit",DT=MA),
              data.frame(A="Real",DT=X))
    p <- ggplot2::ggplot(DF,ggplot2::aes(x=DF$DT,fill=DF$A)) + ggplot2::geom_density(alpha=0.4) +ggplot2::ggtitle(distribu)
  }
}
Compe
write.csv(Compe,"resultados1.csv")



##########Separación de la muestra
set.seed(1)
R1<-kmeans(R,2)

#Separamos la muestra
Muestras<-purrr::map(1:max(R1$cluster),~R[R1$cluster==.x])

x11();par(mfrow=c(1,3))
plot(density(R))
plot(density(Muestras[[2]]),main=paste0("Len ",length(Muestras[[2]])))
plot(density(Muestras[[1]]),main=paste0("Len ",length(Muestras[[1]])))

