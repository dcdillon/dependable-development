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
