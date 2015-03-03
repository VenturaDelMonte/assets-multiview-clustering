# To load and manage Symbols
#
library(quantmod)
source("TSmeasure.R")
library(parallel)

#' for each asset, store in measures.Rdata 
#' the 13 measures related to each 
#' variable.
#' 
#' @param symbols A list of assets symbols
#' @param from A string representing the start date
#'        of each TS. The format of the date is
#'        yyyy-mm-dd.
#' @param to A string representing the end date
#'        of each TS. The format of the date is
#'        yyy-mm-dd.
#' @return void 
#' @export 
#' 
extractTS <- function(symbols, class, sector,
                      from="2014-01-01", 
                      to="2015-01-01")
{
  label                    <- c("Periodicity", "TSA Trend",
                                "Serial Corr",
                                "Non-linearity","Skewness",
                                "Kurtosis","Self-sim",
                                "Chaos", "TSA Serial Corr",
                                "TSA Non-linearity",
                                "TSA Skewness",
                                "TSA Kurtosis")
  
  # number of the symbols
  n                        <- length(symbols)  
  # number of the measures
  nf                       <- length(label)                    
  # dataframe which contains:
  #  - for each asset, the measures calculated;
  #  - a vector of symbols on which the 
  #    computation of the measures is failed
  ret                      <- NULL
  # dataframe which contains all the measures 
  # for each variables
  meas                     <- NULL  
  # vector which contains the indices of the
  # not inserted assets
  not.inserted             <- NULL
  #temporary variables
  tmp                      <- NULL 
  #closing.price.adjusted   <- NA
  #m.closing.price.adjusted <- matrix(NA, nrow = n, ncol = nf)
  range                    <- NA
  m.range                  <- matrix(NA, nrow = n, ncol = nf)
  y                        <- NA
  yield                    <- NA
  m.yield                  <- matrix(NA, nrow = n, ncol = nf)
  volatility               <- NA
  m.volatility             <- matrix(NA, nrow = n, ncol = nf)
  volume                   <- NA
  m.volume                 <- matrix(NA, nrow = n, ncol = nf)
  
  # Load symbols
  # This way is faster than getSymbols(sym)
  #
  for(i in 1:n) 
  {
    print(paste("download asset #", i, "--", symbols[i], sep=" "))
    getSymbols(symbols[i], src = "yahoo", from = as.Date(from), to = as.Date(to))
  }
    
  # for each asset,
  # calulate the measures 
  # on our variables
  for (i in 1:n)
  {
    print(paste("calculate measure for #", 
                i, "asset --", symbols[i], sep=" "))
    
    # Select sym.High, sym.Low and sym.Adjusted 
    # sym.closing.price.adjusted and sym.volume
    # of the i-th asset
    #
    tmp                    <- as.data.frame((get(symbols[i])))[,c(2,3,5,6)]
    
    # calculate the measures 
    # related to the variables
    #closing.price.adjusted <- as.numeric(measures(tmp[4][[1]]))   
    range                  <- as.numeric(measures(tmp[1][[1]] - tmp[2][[1]])) 
    y                      <- calculateYield(tmp)
    yield                  <- as.numeric(measures(y)) 
    volatility             <- as.numeric(measures(abs(y))) 
    volume                 <- as.numeric(measures(tmp[3][[1]])) 
    
    # if we can't calculate a measure on
    # an asset, we do not place that asset
    # in our data
    if( is.na(range) ||
         is.na(yield) || is.na(volatility) || is.na(volume))
    {
      not.inserted = c(not.inserted, i)
      print(paste("assert not inserted #", 
                  i, "asset --", symbols[i], sep=" "))
    }
    else
    {
      #m.closing.price.adjusted[i,] <- closing.price.adjusted
      m.range[i,]                  <- range
      m.volume[i,]                 <- volume
      m.yield[i,]                  <- yield
      m.volatility[i,]             <- volatility
    }    
    
  }
  
  #scale the matrix if
  #some measure on some assets
  #are not calculated
  if (!is.null(unique(not.inserted)))
  {
    ret$symbols                              <- symbols[-not.inserted]
    ret$class                                <- class  [-not.inserted]
    ret$sector                               <- sector [-not.inserted]
    
    meas$yield                               <- m.yield[-not.inserted,]
    meas$range                               <- m.range[-not.inserted,]
    meas$volatility                          <- m.volatility[-not.inserted,]
    meas$volume                              <- m.volume[-not.inserted,]
    #meas$closing.price.adjusted              <- m.closing.price.adjusted[-not.inserted,]
    
  }
  else
  {
    ret$symbols                              <- symbols
    ret$class                                <- class
    ret$sector                               <- sector
    
    meas$yield                               <- m.yield
    meas$range                               <- m.range
    meas$volatility                          <- m.volatility
    meas$volume                              <- m.volume
    #meas$closing.price.adjusted              <- m.closing.price.adjusted
  }


  # set row names and col names 
  # for each matrix of the dataframe
  #rownames(meas$closing.price.adjusted)       <- symbols
#   rownames(meas$range)                        <- symbols
#   rownames(meas$yield )                       <- symbols
#   rownames(meas$volatility)                   <- symbols
#   rownames(meas$volume)                       <- symbols
#   #colnames(meas$closing.price.adjusted)       <- label
#   colnames(meas$range)                        <- label
#   colnames(meas$yield )                       <- label
#   colnames(meas$volatility)                   <- label
#   colnames(meas$volume)                       <- label
 
  
  
  ret$meas                                    <- meas
  

  return(ret)
  
}