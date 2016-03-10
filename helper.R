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

#Define stationary theoretical EWS array:
ewsStationary <- function(R0arr){
  df <-data.frame(R0arr,ifelse(R0arr < 1, 1/(1-R0arr), NA),ifelse(R0arr < 1, 1/((1-R0arr)^2), NA),ifelse(R0arr < 1, 1, NA), 1/(abs(1-R0arr)) )
  names(df) <- c("R0","Mean", "Variance","Coefficient of variation", "Index of dispersion")
  return(df)
}


#Define finite speed theoretical EWS array (note step size may be large for infrequent data. see ewsFinite_acc):
ewsFinite <- function(R0arr){

	m = R0arr	
	v = R0arr
	m[1] = 1
	v[1] = 1

	for(i in 2:length(R0arr)){
		m[i] = ((R0arr[i-1]-1)*m[i-1] + 1) + m[i-1]
		v[i] = (2*(R0arr[i-1]-1)*v[i-1] + (R0arr[i-1]+1)*m[i-1] + 1) + v[i-1]
		}
	df <- data.frame(R0arr,m,v,sqrt(v)/m,v/m)
	names(df) <- c("R0","Mean", "Variance","Coefficient of variation", "Index of dispersion")
	return(df)
}

#Define more accurate finite speed array. Same calculation as ewsFinite, but steps variable sets the number of timesteps between each data point:
ewsFinite_acc <- function(R0arr, steps = 100){

	m = R0arr	
	v = R0arr
	m[1] = 1
	v[1] = 1
	DR0 = (R0arr[2]-R0arr[1])/steps
	
	for(i in 2:length(R0arr)){
		temp_m = m[i-1]
		temp_v = v[i-1]
		for(j in 0:(steps-1)){
			temp_v = temp_v + (1/steps)*(2*(R0arr[i-1] +DR0*j -1)*temp_v + (R0arr[i-1] +DR0*j+1)*temp_m + 1) 
			temp_m = temp_m + (1/steps)*((R0arr[i-1]+DR0*j-1)*temp_m + 1) 
			}
			v[i] = temp_v
			m[i] = temp_m
		}
	df <- data.frame(R0arr,m,v,sqrt(v)/m,v/m)
	names(df) <- c("R0","Mean", "Variance","Coefficient of variation", "Index of dispersion")
	return(df)
}

#returns Euler derivative (x_i - x_(i-1))(y_i-y_(i-1)):
ewsDerivative <- function(y, x){
	return(c(NA,diff(x)/diff(y)))
}


#Ylim function:
ewsYlim <- function(t,x, tl, tu){
	xr <- as.numeric(x[which(t > tl & t < tu)])
	is.na(xr) <- do.call(cbind,lapply(xr, is.infinite))
	return(range(xr, na.rm = TRUE))
}


ewsProbTimeseries <- function(t,x, w){
  x <- as.numeric(x)
  prob <- matrix(data = NA, nrow = length(t), ncol = max(x)-min(x)+1)
  colnames(prob) <- min(x):max(x)
  rownames(prob) <- t
  prob[w,] <- 0
  for(i in 1:(w+1)){
  prob[w,x[i]] <- prob[w,x[i]] + 1/w
  }
  for(i in (w+1):length(t)){
    prob[i,] <- prob[i-1,]
    prob[i,x[i]] <- prob[i,x[i]] + 1/w
    prob[i,x[i-w-1]] <- prob[i,x[i-w-1]] - 1/w
  }
return(prob)
}

ewsEntropy <- function(prob){
  ent <- 1:nrow(prob)
  for(i in 1:(nrow(prob))){
    ent[i] <- -sum(prob[i,]*log(prob[i,]), na.rm = TRUE)
  }
  return(ent)
}

informationContentTheory <- function(s,r){-dnbinom(s, 1/r,r)*log(dnbinom(s,1/r,r))}

entropyTheory <- function(k_max,r){sum(informationContentTheory(0:k_max,r), na.rm=TRUE)}
#y <- sapply(0.05*1:19, ent, k_max = 100000)
#x <- 0.05*1:19
#plot(x,y)



