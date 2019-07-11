collatz <- function(start_val, show_me, odd_even) {
  if(missing(show_me)){
    show_me <- start_val
  }
  if(missing(odd_even)){
    odd_even <- 0
  }
  
  n <- start_val
  comp <- c()
  moving_mean <- c()
  moving_mean_even <- c()
  moving_mean_odd <- c()
  moving_mean_prime <- c()
  moving_median <- c()
  j <- 0
  for(i in 1:n){
    steps <- 0
    j <- j + 1
    while(i!=1) {
      #print(n)
      if(i%%2 == 0){
        i <- i/2
      } else i <- (3*i) + 1
      steps <- steps + 1
    }
    comp[j] <- steps
    moving_mean[j] <- mean(comp)
    
    #moving_median[j] <- median(comp)
    #print(comp)
    #print(moving_mean)
  }
  numbers <- c(1:n)
  par(mfrow=c(1,2))
  
  if(odd_even == 1){
    
    #This code sniipet comes from https://stackoverflow.com/questions/3789968/generate-a-list-of-primes-up-to-a-certain-number
    
    sieve <- function(p)
    {
      p <- as.integer(p)
      if(p > 1e8) stop("p too large")
      primes <- rep(TRUE, p)
      primes[1] <- FALSE
      last.prime <- 2L
      fsqr <- floor(sqrt(p))
      while (last.prime <= fsqr)
      {
        primes[seq.int(2L*last.prime, p, last.prime)] <- FALSE
        sel <- which(primes[(last.prime+1):(fsqr+1)])
        if(any(sel)){
          last.prime <- last.prime + min(sel)
        }else last.prime <- fsqr+1
      }
      which(primes)
    }
    
    
    primes <- sieve(n)
    odds <- numbers[c(TRUE, FALSE)]
    evens <- numbers[-odds]
    even_comp <- comp[evens]
    odd_comp <- comp[odds]
    prime_comp <- comp[primes]
    plot(odd_comp~odds, xlab="Starting Number", ylab="Steps to Reach 1", pch=19, col="red", cex=0.2)
    points(even_comp ~evens, pch=19, col="green", cex=0.2)
    points(prime_comp~primes, col="purple", pch=19, cex=0.2)
    
    for(k in 1:(n/2)){
      moving_mean_even[k] <- mean(even_comp[1:k])
      moving_mean_odd[k] <- mean(odd_comp[1:k])
    }
    
    for(v in 1:length(primes)){
      moving_mean_prime[v] <- mean(prime_comp[1:v])
    }
    
    plot(moving_mean~numbers, xlab="Starting Number", ylab="Moving Mean of Steps to Reach 1", pch=19, col="blue", cex=0.2)
    points(moving_mean_odd~odds, col="red", pch=19, cex=0.2)
    points(moving_mean_even~evens, col="green", pch=19, cex=0.2)
    points(moving_mean_prime~primes, col="purple", pch=10, cex=0.2)
    legend("bottomright", inset=.02, legend=c("Odd", "Prime", "All", "Even"), col=c("red", "purple", "blue", "green"), pch=19, cex=0.8)
    
    #print(moving_mean_odd)
    #print(moving_mean_even)
  
  
    
  } else plot(comp~numbers, xlab="Starting Number", ylab="Steps to Reach 1", pch=19, col="orange", cex=0.2) & plot(moving_mean~numbers, xlab="Starting Number", ylab="Moving Mean of Steps to Reach 1", pch=19, col="blue", cex=0.2)
  
  
  
  
  #points(moving_median~numbers, col="red", pch=19, cex=0.2)
  paste(show_me, "took", comp[show_me], "steps to converge to 1")
} 

