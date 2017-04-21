library(xts)

sample_equity_data_minute <- function(data, date) {
  start <- as.POSIXct(paste0(date, ' ', '09:31:00'), tz="America/NewYork")
  end <- as.POSIXct(paste0(date, ' ', '16:00:00'), tz="America/NewYork")
  idx <- seq(from=start, to=end, by=60)
  idx <- xts(rep(NA, length(idx)), order.by=idx)
  
  data <- na.locf(merge(idx, data))[index(idx), -1]
  data
}

get_raw_symbol_data <- function(symbol, date, timestamp_col, data_cols, base_dir) {
  filename <- paste0(base_dir, '/', symbol, '/', date, '.csv')
  raw_data <- read.csv(filename, header=TRUE, stringsAsFactors = FALSE)
  raw_data <- raw_data[, c(timestamp_col, data_cols)]
  
  raw_data[, timestamp_col] <- as.POSIXct(raw_data[, timestamp_col], tz="America/NewYork")
  
  raw_data <- xts(raw_data[, data_cols], order.by=raw_data[, timestamp_col])
  raw_data <- sample_equity_data_minute(raw_data, date)
  colnames(raw_data) <- c(paste0(data_cols, ".", symbol))
  raw_data
}

get_raw_day_data <- function(symbols, date, timestamp_col, data_cols, base_dir) {
  retval <- NULL
  
  for (symbol in symbols)
  {
    data <- get_raw_symbol_data(symbol, date, timestamp_col, data_cols, base_dir)

    if (is.null(retval)) {
      retval <- data
    } else {
      retval <- merge(retval, data)
    }
  }
  
  retval
}

calculate_ema <- function(data, col, alpha)
{
  ema <- NA
  
  ema_col <- paste0(col, ".ema")
  
  data <- cbind(data, NA)
  colnames(data)[ncol(data)] <- ema_col
  
  for (i in 1:dim(data)[1]) {
    if (is.na(ema)) {
      ema <- as.numeric(data[i, col])
    } else {
      ema <- as.numeric((1 - alpha) * ema + alpha * data[i, col])
    }
    
    data[i, ema_col] <- ema
  }
  
  data
}

calculate_forward <- function(data, col, lag) {
  forward_col <- paste0(col, ".forward")
  
  data <- cbind(data, NA)
  colnames(data)[ncol(data)] <- forward_col
  
  for (i in 1:dim(data)[1]) {
    if (i + lag <= dim(data)[1]) {
      data[i, forward_col] <- data[i + lag, col]
    }
  }
  
  data
}

my_model_get_data <- function(symbols, dates, alpha, lag, data_dir) {
  retval <- NULL
  
  for (date in dates) {
    data <- get_raw_day_data(symbols, date, 'datetime', 'close', data_dir)
    
    for (symbol in symbols) {
      data_col <- paste0('close.', symbol)
      data <- calculate_forward(data, data_col, lag)
      data <- calculate_ema(data, data_col, alpha)
    }
    
    if (is.null(retval)) {
      retval <- data
    } else {
      retval <- rbind(retval, data)
    }
  }
  
  retval
}

my_model_fit <- function(symbol, driver_symbols, data) {
  forward_col <- paste0('close.', symbol, '.forward')
  close_col <- paste0('close.', symbol)
  
  driver_close_cols <- paste0('close.', driver_symbols)
  driver_ema_cols <- paste0('close.', driver_symbols, '.ema')
  
  formula <- paste0('I(', forward_col, ' - ', close_col, ') ~ ')
  
  for (i in 1:length(driver_symbols)) {
    formula <- paste0(formula, 'I(', driver_close_cols[i], ' - ', driver_ema_cols[i], ') + ')
  }
  
  formula <- paste0(formula, ' -1')
  
  result <- lm(formula, data=data)
  result
}

symbols <- c('SPY', 'XLB', 'XLE', 'XLF', 'XLI', 'XLK', 'XLP', 'XLU', 'XLV', 'XLY', 'XME')
dates <- c('2017-01-03', '2017-01-04', '2017-01-05', '2017-01-06', '2017-01-09', '2017-01-10', '2017-01-11', '2017-01-12', '2017-01-13', '2017-01-17', '2017-01-18', '2017-01-19', '2017-01-20')
alpha <- .05
lag <- 10

data <- my_model_get_data(symbols, dates, alpha, lag, '../data/')
result <- my_model_fit('SPY', symbols, data)

print(summary(result))
