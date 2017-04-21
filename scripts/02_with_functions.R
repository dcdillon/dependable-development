library(xts)

get_symbol_data <- function(symbol, date) {
  filename <- paste0("../data/", symbol, "/", date, ".csv")
  raw_data <- read.csv(filename, header=TRUE)
  alpha <- .05
  
  raw_data$datetime <- as.POSIXct(raw_data$datetime, tz="America/NewYork")
  
  raw_data <- xts(raw_data[, 'close'], order.by=raw_data$datetime)
  start <- as.POSIXct(paste0(date, ' ', '09:31:00'), tz="America/NewYork")
  end <- as.POSIXct(paste0(date, ' ', '16:00:00'), tz="America/NewYork")
  idx <- seq(from=start, to=end, by=60)
  
  idx <- xts(rep(NA, length(idx)), order.by=idx)
  
  raw_data <- na.locf(merge(idx, raw_data))[index(idx)]
  colnames(raw_data) <- c('dummy', 'close')
  
  raw_data <- cbind(raw_data, NA)
  ema_col <- paste0('ema.', symbol)
  colnames(raw_data)[ncol(raw_data)] <- ema_col
  
  ema <- NA
  
  for (i in 1:dim(raw_data)[1]) {
    if (is.na(ema)) {
      ema <- as.numeric(raw_data[i, 'close'])
    } else {
      ema <- as.numeric((1 - alpha) * ema + alpha * raw_data[i, 'close'])
    }
    
    raw_data[i, ema_col] <- ema
  }
  
  retval <- raw_data[, c('close', ema_col)]
  colnames(retval) <- c(paste0('close.', symbol), ema_col)
  retval
}

get_day_data <- function(date, symbols) {
  retval <- NULL
  
  for (symbol in symbols)
  {
    data <- get_symbol_data(symbol, date)
    forward_col <- paste0('forward.', symbol)
    close_col <- paste0('close.', symbol)
    data <- cbind(data, NA)
    colnames(data)[ncol(data)] <- forward_col
    
    for (i in 1:dim(data)[1]) {
      if (i + 10 <= dim(data)[1]) {
        data[i, forward_col] <- as.numeric(data[i + 10, close_col])
      }
    }
    
    if (is.null(retval)) {
      retval <- data
    } else {
      retval <- merge(retval, data)
    }
  }
  
  retval
}

get_all_data <- function(dates, symbols) {
  
  retval <- NULL
  
  for (date in dates) {
    data <- get_day_data(date, symbols)
    
    if (is.null(retval)) {
      retval <- data
    } else {
      retval <- rbind(retval, data)
    }
  }
  
  retval
}

fit_model <- function(symbol, driver_symbols, dates) {
  forward_col <- paste0('forward.', symbol)
  close_col <- paste0('close.', symbol)
  
  driver_close_cols <- paste0('close.', driver_symbols)
  driver_ema_cols <- paste0('ema.', driver_symbols)
  
  formula <- paste0('I(', forward_col, ' - ', close_col, ') ~ ')
  
  for (i in 1:length(driver_symbols)) {
    formula <- paste0(formula, 'I(', driver_close_cols[i], ' - ', driver_ema_cols[i], ') + ')
  }
  
  formula <- paste0(formula, ' -1')
  
  all_data <- get_all_data(dates, unique(c(symbol, driver_symbols)))
  result <- lm(formula, data=all_data)
  result
}

symbols <- c('SPY', 'XLB', 'XLE', 'XLF', 'XLI', 'XLK', 'XLP', 'XLU', 'XLV', 'XLY', 'XME')
dates <- c('2017-01-03', '2017-01-04', '2017-01-05', '2017-01-06', '2017-01-09', '2017-01-10', '2017-01-11', '2017-01-12', '2017-01-13', '2017-01-17', '2017-01-18', '2017-01-19', '2017-01-20')

result <- fit_model('SPY', symbols, dates)

print(summary(result))