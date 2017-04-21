library(stat385data)
library(stat385tester)

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

r_prediction <- unname(predict(result, data["2017-01-03", ]) + as.numeric(data["2017-01-03", "close.SPY"]))
c_prediction <- stat385tester::prod_predict(coredata(data["2017-01-03", 1:11]))
all.equal(r_prediction, c_prediction)
