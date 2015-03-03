# This file contains the function for the calculation of
# the measures of TS data. For more information see: 
# http://www.r-bloggers.com/measuring-time-series-characteristics/
#

#' Maps values from [0,infinity) to [0,1]
#'
#' @param x The value to be mapped
#' @param a A constant to be chosen
#' @param b A constant to be chosen
#' @return The mapped value
#' @references \url{http://link.springer.com/article/10.1007/s10618-005-0039-x/fulltext.html}
#' @export 
#' 
f1 <- function(x,a,b)
{
  if(is.nan(x))
    return(NA)
  eax <- exp(a*x)
  if (eax == Inf)
    f1eax <- 1
  else
    f1eax <- (eax-1)/(eax+b)
  return(f1eax)
}

#' Maps values from [0,1] onto [0,1]
#'
#' @param x The value to be mapped
#' @param a A constant to be chosen
#' @param b A constant to be chosen
#' @return The mapped value
#' @references \url{http://link.springer.com/article/10.1007/s10618-005-0039-x/fulltext.html}
#' @export 
#' 
f2 <- function(x,a,b)
{
  eax <- exp(a*x)
  ea <- exp(a)
  return((eax-1)/(eax+b)*(ea+b)/(ea-1))
}

#' Decompose the data (detrend & deseasonal)
#'
#' @details We needed a measure of the strength of trend and
#' the strength of seasonality, and to do this we 
#' decomposed the data into trend, seasonal and 
#' error terms (irregular components).
#'
#' Because not all data could be decomposed additively, 
#' we first needed to apply an automated Box-Cox transformation.
#'
#' Automatic selection of Box Cox transformation parameter
#' with Guerrero's (1993) method implemented 
#' in BoxCox.lambda(). 
#' 
#' Lambda is choosen such that minimizes the coefficient of variation 
#' for subseries of x. 
#' @param x The time series
#' @param transform A logical parameter. If TRUE
#' a Box-Cox transformation is applied to x.
#' @return The mapped value
#' @references \url{http://www.r-bloggers.com/measuring-time-series-characteristics/}
#' @export 
#' 
decomp <- function(x,transform=TRUE)
{
  require(forecast)
  # Transform series
  #
  if(transform & min(x,na.rm=TRUE) >= 0)
  {
    lambda <- BoxCox.lambda(na.contiguous(x))
    # BoxCox() returns a transformation of the 
    # input variable using a Box-Cox transformation.
    #
    x <- BoxCox(x,lambda)
  }
  else
  {
    lambda <- NULL
    transform <- FALSE
  }
  # Seasonal data
  #
  # For seasonal time series, we decomposed the 
  # transformed data using an stl decomposition 
  # with periodic seasonality.
  #
  # STL is an acronym for 
  # 'Seasonal and Trend decomposition using Loess', while 
  # 'Loess' is a method for estimating nonlinear relationships. 
  # 'See: https://www.otexts.org/fpp/6/5
  #
  if(frequency(x)>1)
  {
    x.stl <- stl(x,s.window="periodic",na.action=na.contiguous)
    trend <- x.stl$time.series[,2]
    season <- x.stl$time.series[,1]
    remainder <- x - trend - season
  }
  # Nonseasonal data
  #
  # For non-seasonal time series, we estimated the trend of 
  # the transformed data using penalized regression splines
  #
  else 
  {
    require(mgcv)
    tt <- 1:length(x)
    trend <- rep(NA,length(x))
    # fitted() is a generic function which extracts 
    # fitted values from objects returned by 
    # modeling functions
    #
    trend[!is.na(x)] <- fitted(gam(x ~ s(tt)))
    season <- NULL
    remainder <- x - trend
  }
  return(list(x=x,trend=trend,season=season,remainder=remainder,transform=transform,lambda=lambda))
}

#' Find frequency (or period) from time series data. 
#'
#' @details The method used in the paper was based on local peaks 
#' and troughs in the autocorrelation function. 
#' A better approach was developed using an estimate of 
#' the spectral density.
#'
#' The goal of spectral density estimation (SDE) is 
#' to estimate the spectral density of a random signal 
#' from a sequence of time samples of the signal. 
#' Intuitively speaking, the spectral density characterizes 
#' the frequency content of the signal. 
#' One purpose of estimating the spectral density is to detect 
#' any periodicities in the data, by observing peaks 
#' at the frequencies corresponding to these periodicities.
#' Source: \url{http://en.wikipedia.org/wiki/Spectral_density_estimation}
#'
#' @param x It is a TS object
#' @return The frequency 
#' @references \url{http://www.r-bloggers.com/measuring-time-series-characteristics/}
#' @export 
#'
findFreq <- function(x)
{
  n <- length(x)
  spec <- spec.ar(c(na.contiguous(x)),plot=FALSE)
  if(max(spec$spec)>10) # Arbitrary threshold chosen by trial and error.
  {
    period <- round(1/spec$freq[which.max(spec$spec)])
    if(period==Inf) # Find next local maximum
    {
      j <- which(diff(spec$spec)>0)
      if(length(j)>0)
      {
        nextmax <- j[1] + which.max(spec$spec[j[1]:500])
        if(nextmax <= length(spec$freq))
          period <- round(1/spec$freq[nextmax])
        else
          period <- 1
      }
      else
        period <- 1
    }
  }
  else
    period <- 1
  
  return(period)
}


#' Computes all measures 
#' 
#' @param x The time series (as a numeric type)
#' @return a 13 element list in the following order: 
#' Periodicity, Trend, Seasonality, Serial Correlation,
#' Non-linearity, Skewness, Kurtosis, Self-similarity, Chaos,
#' TSA Serial Correlation, TSA Non-linearity,TSA Skewness,
#' TSA Kurtosis 
#' @references \url{http://www.r-bloggers.com/measuring-time-series-characteristics/}
#' @export 
#' 
measures <- function(x)
{
  tryCatch(
  { 
    require(forecast)
    require(fracdiff)
    require(tseries)
  
    N <- length(x)
    freq <- findFreq(x)
    fx <- c(frequency=(exp((freq-1)/50)-1)/(1+exp((freq-1)/50)))
    #The function ts() is used to create time-series objects
    #
    x <- ts(x,f=freq)
    
    # Decomposition
    #
    # When a title has less than two periods (or no periods) 
    # we can't calculate the trend and seasonality.
    # For more information see:
    # http://www.analyticbridge.com/profiles/blogs/time-series-analysis-using-r-forecast-package
    #
    decomp.x <- decomp(x)
    
    # Adjust data
    #
    if(freq > 1)
      fits <- decomp.x$trend + decomp.x$season
    else # Nonseasonal data
      fits <- decomp.x$trend
    adj.x <- decomp.x$x - fits + mean(decomp.x$trend, na.rm=TRUE)
    
    # Backtransformation of adjusted data
    #
    if(decomp.x$transform)
      #InvBoxCox() reverses the BoxCox transformation
      #
      tadj.x <- InvBoxCox(adj.x,decomp.x$lambda)
    else
      tadj.x <- adj.x
    
    # Trend and seasonal measures
    #
    # avoids the divide by zero problem by testing 
    # if the variances are close to zero first
    #
    v.adj <- var(adj.x, na.rm=TRUE)
    if(freq > 1)
    {
      detrend <- decomp.x$x - decomp.x$trend
      deseason <- decomp.x$x - decomp.x$season
      trend <- ifelse(var(deseason,na.rm=TRUE) < 1e-10, 0, 
                      max(0,min(1,1-v.adj/var(deseason,na.rm=TRUE))))
      season <- ifelse(var(detrend,na.rm=TRUE) < 1e-10, 0,
                       max(0,min(1,1-v.adj/var(detrend,na.rm=TRUE))))
    }
    else #Nonseasonal data
    {
      trend <- ifelse(var(decomp.x$x,na.rm=TRUE) < 1e-10, 0,
                      max(0,min(1,1-v.adj/var(decomp.x$x,na.rm=TRUE))))
      season <- 0
    }
    
    m <- c(fx,trend)
    
    # Measures on original data
    #
    xbar <- mean(x,na.rm=TRUE)
    s <- sd(x,na.rm=TRUE)
    
    # Serial correlation
    #
    Q <- Box.test(x,lag=10)$statistic/(N*10)
    fQ <- f2(Q,7.53,0.103)
    
    # Nonlinearity
    #
    p <- terasvirta.test(na.contiguous(x))$statistic
    fp <- f1(p,0.069,2.304)
    
    # Skewness
    require(moments)
    # sk <- abs(mean((x-xbar)^3,na.rm=TRUE)/s^3)
    sk <- skewness(x, na.rm = TRUE)
    fs <- f1(sk,1.510,5.993)
    
    # Kurtosis
    #k <- mean((x-xbar)^4,na.rm=TRUE)/s^4
    k <- kurtosis(x, na.rm=TRUE)
    fk <- f1(k,2.273,11567)
    
    # Hurst=d+0.5 where d is fractional difference.
    # Self-similarity
    #
    H <- fracdiff(na.contiguous(x),0,0)$d + 0.5
    
    # Lyapunov Exponent (Chaos)
    #
    if(freq > N-10)
      stop("Insufficient data")
    Ly <- numeric(N-freq)
    for(i in 1:(N-freq))
    {
      idx <- order(abs(x[i] - x))
      idx <- idx[idx < (N-freq)]
      j <- idx[2]
      Ly[i] <- log(abs((x[i+freq] - x[j+freq])/(x[i]-x[j])))/freq
      if(is.na(Ly[i]) | Ly[i]==Inf | Ly[i]==-Inf)
        Ly[i] <- NA
    }
    Lyap <- mean(Ly,na.rm=TRUE)
    fLyap <- exp(Lyap)/(1+exp(Lyap))
    
    m <- c(m,fQ,fp,fs,fk,H,fLyap)
    
    # Measures on adjusted data
    #
    xbar <- mean(tadj.x, na.rm=TRUE)
    s <- sd(tadj.x, na.rm=TRUE)
    
    # Serial
    #
    Q <- Box.test(adj.x,lag=10)$statistic/(N*10)
    fQ <- f2(Q,7.53,0.103)
    
    # Nonlinearity
    #
    p <- terasvirta.test(na.contiguous(adj.x))$statistic
    fp <- f1(p,0.069,2.304)
    
    # Skewness
    # sk <- skewness(tadj.x, na.rm = TRUE)
      sk <- abs(mean((tadj.x-xbar)^3,na.rm=TRUE)/s^3)
    
    fs <- f1(sk,1.510,5.993)
    
    # Kurtosis
    #k <- mean((tadj.x-xbar)^4,na.rm=TRUE)/s^4
    k <- kurtosis(tadj.x, na.rm=TRUE)
    fk <- f1(k,2.273,11567)
    
    m <- c(m,fQ,fp,fs,fk)
    #   names(m) <- c("frequency", "trend","seasonal",
    #                 "autocorrelation","non-linear","skewness","kurtosis","Hurst","Lyapunov",
    #                 "dc autocorrelation","dc non-linear","dc skewness","dc kurtosis")
    
    return(m)}, 
    error = function(err) { 
      print(paste("MY_ERROR:  ",err))
      return(NA)})
 
}
