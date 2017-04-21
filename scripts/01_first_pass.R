library(xts)

symbols <- c('SPY', 'XLB', 'XLE', 'XLF', 'XLI', 'XLK', 'XLP', 'XLU', 'XLV', 'XLY', 'XME')
dates <- c('2017-01-03', '2017-01-04', '2017-01-05', '2017-01-06', '2017-01-09', '2017-01-10', '2017-01-11', '2017-01-12', '2017-01-13', '2017-01-17', '2017-01-18', '2017-01-19', '2017-01-20')
all_data <- NULL

for (d in dates) {
  data <- NULL
  
  for (symbol in symbols) {
    filename <- paste0("../data/", symbol, "/", d, ".csv")
    a <- read.csv(filename, header=TRUE)
    alpha <- .05
    
    a$datetime <- as.POSIXct(a$datetime, tz="America/NewYork")
    
    a <- xts(a[, 'close'], order.by=a$datetime)
    start <- as.POSIXct(paste0(d, ' ', '09:31:00'), tz="America/NewYork")
    end <- as.POSIXct(paste0(d, ' ', '16:00:00'), tz="America/NewYork")
    idx <- seq(from=start, to=end, by=60)
    
    idx <- xts(rep(NA, length(idx)), order.by=idx)
    
    a <- na.locf(merge(idx, a))[index(idx)]
    colnames(a) <- c('dummy', 'close')
    
    a <- cbind(a, NA)
    ema_col <- paste0('ema.', symbol)
    colnames(a)[ncol(a)] <- ema_col
    
    ema <- NULL
    
    for (i in 1:dim(a)[1]) {
      if (is.null(ema)) {
        ema <- as.numeric(a[i, 'close'])
      } else {
        ema <- as.numeric((1 - alpha) * ema + alpha * a[i, 'close'])
      }
      
      a[i, ema_col] <- ema
    }
    
    a <- cbind(a, NA)
    forward_col <- paste0('forward.', symbol)
    colnames(a)[ncol(a)] <- forward_col
    
    for (i in 1:dim(a)[1]) {
      if (i + 10 <= dim(a)[1]) {
        a[i, forward_col] <- as.numeric(a[i + 10, 'close'])
      }
    }
    
    if (is.null(data)) {
      data <- a[, -1]
      colnames(data) <- c(paste0('close.', symbol), ema_col, forward_col)
    } else {
      colnames(a) <- c('dummy', paste0('close.', symbol), ema_col, forward_col)
      data <- merge(data, a[, -1])
    }
  }
  
  if (is.null(all_data)) {
    all_data <- data
  } else {
    all_data <- rbind(all_data, data)
  }
}


formula <- 'I(forward.SPY - close.SPY) ~ '

for (symbol in symbols[1:length(symbols)]) {
  formula <- paste0(formula, 'I(close.', symbol, ' - ema.', symbol, ') + ')
}

formula <- paste0(formula, ' - 1')

result <- lm(formula, data=all_data)

print(summary(result))