#Define moving average function
ma <- function(arr, n){
  res = arr
  for(i in (n-1):length(arr)){
    res[i] = mean(as.numeric(arr[(i-n+1):i]))
  }
  for(i in 1:(n-1)){
    res[i] = NA
  }
  res
}

#Define EWS averaging function:
ewsAverage <- function(R0arr,arr,n, lag = 1){
	m <-ma(arr,n)
	v <- ma(as.numeric(arr)*as.numeric(arr),n) -m*m #y <- ma(a, input$bins)

	arr_shifted <- c(0*1:lag  , arr[1:(length(arr)-lag)])
	cfunc <- ma(as.numeric(arr)*as.numeric(arr_shifted),n)
	m_s <-ma(arr_shifted,n)
	v_s <- ma(as.numeric(arr_shifted)*as.numeric(arr_shifted),n) -m_s*m_s #y <- ma(a, input$bins)
	AC = (cfunc - m*m_s)/sqrt(v*v_s)
	CT <- -lag/log(abs(AC))
	df <- data.frame(R0arr,m,v,sqrt(v)/m, v/m, AC,CT)
	names(df) <- c("R0","Mean", "Variance","Coefficient of variation", "Index of dispersion", "Autocorrelation", "Correlation time")
	return(df)
	}


#Ylim function:
ewsYlim <- function(t,x, tl, tu){
	xr <- as.numeric(x[which(t > tl & t < tu)])
	is.na(xr) <- do.call(cbind,lapply(xr, is.infinite))
	return(range(xr, na.rm = TRUE))
}


