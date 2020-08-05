cll<-function () 
{
  ctx <- try(rstudioapi::getActiveDocumentContext(), silent = TRUE)
  if (!inherits(ctx, "try-error")) {
    contenido <- ctx[["contents"]]
    file_w <- file(paste0(getwd(),"/new.R"))
    writeLines(((contenido)), file_w)
    writeLines(as.character(parse("new.R")), file_w)
    utils::file.edit(paste0(getwd(),"/new.R"))
  }
  return(invisible())
}



x <- c(rnorm(1000),rnorm(1000,3))
y <- density(x)
Y = c(y[['y']])
X = c(y[['x']])

#  crr <- min(y[['x']])
#  X <- c(y[['x']]-crr)
#  X <- c(X, X + max(X))

p <- TSA::periodogram(Y, plot = F)
dds <- data.frame(freq = 1/p$freq, spec = p$spec, orden = 1:length(p$spec))
dds <- head(dds[order(-dds$spec), ], 10)

periodos <- round(dds[['freq']],0)

for (p in periodos) {
  print(X[p])
}

library(TSA)
library(forecast)
y.dc <- decompose(ts(Y, frequency = 171))
plot(y.dc)

frequency(Y)

plot(y)



N = length(Y)
I = abs(fft(Y)/sqrt(N))^2
P = (4/N) * I
f = (0:floor(N/2))/N
plot(f, I[1:((N/2) + 1)], type = "h", xlab = "frequency", ylab = "", main = "Periodogram of CO2 data after trend removal", col = "blue")


D_F = data.frame(Freq = f, Spec = I[1:((N/2) + 1)])
1/head(D_F[order(-D_F$Spec),'Freq'])

###########################


periodicidad <- function(ts, place = 10) {
  ddT <- data.frame(freq = c(), spec = c(), orden = c())
  ords <- floor(length(ts) * 0.7):length(ts)
  for (lu in ords) {
    p <- TSA::periodogram(ts[1:lu], plot = F)
    dds <- data.frame(freq = 1/p$freq, spec = p$spec, orden = 1:length(p$spec))
    dds <- head(dds[order(-dds$spec), ], place)
    ddT <- rbind(ddT, dds)
  }
  ddT <- ddT[order(-ddT$spec), ]
  Maxi <- max(ddT$spec)
  ddT <- head(ddT[ddT$orden > 2, ], 15)
  ddT$Freq_Orden <- paste0(ddT$freq, "_", ddT$orden)
  ddT <- suppressWarnings(reshape2::dcast(ddT, Freq_Orden ~ ., max, value.var = "spec"))
  ddT$. <- ddT$./Maxi
  ddT <- ddT[order(-ddT$.), ]
  return(list(unique(as.numeric(do.call("rbind", strsplit(ddT$Freq_Orden, "_"))[, 1])), ddT))
}
library(timeSeries)
data(LPP2005REC)
df <- LPP2005REC
periodicidad(df, place = 10)
length(df)
