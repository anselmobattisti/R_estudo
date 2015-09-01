add2 <- function(x, y){
  x+y
}

above10 <- function (x){
  use <- x < 10
  x[use]
}

above <- function(vet, n = 10){
  use <- vet < n
  x[use]
}

functionmean <- function (data, removeNA = TRUE){
    
  nc <- ncol(data);
  
  means <- numeric(nc);
  
  for(i in 1:nc){
    means[i] <- mean(data[,i], na.rm = removeNA);
  }
  means
}


minimo <- function(data){
  
  nc <- ncol(data)
  nr <- nrow(data);
  
  mins <- numeric(nc);
  
  for(i in 1:nc){
    for(j in 1:nr){
      
      #if(mins[j] <= data[i,j]){
       # mins[i] <- data[i,j];
      #} 
      print(data[i,j]);
    }
  }
  mins
}




minvet <- function (data){
  
  cr  = nrow(data)
  
  for(i in 1:cr){
    print(data[i])
  }
}

facumulada <- function(data){
  a <- cbind(round(prop.table(table(data))*100))
  b <- cbind(table(data));
  
  # number of rows, categories
  nr = nrow(a);
  ac <- 0
  vetac <- numeric(nr);
  
  fac <- 0
  vetfac <- numeric(nr);
  
  for(i in 1:nr){
    ac <- ac + b[i]
    vetac[i] <- ac;
    
    fac <- fac + a[i]
    vetfac[i] <- fac;
  }
  # concatenate 2 vectors in a frame
  ret <- data.frame(a,vetfac, vetac);
  names(ret)[1] <- "Value"
  names(ret)[2] <- "Accumulated"
  names(ret)[3] <- "Frequency Accumulated"
  
  ret
}


