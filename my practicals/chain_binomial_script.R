


## inference ##

loglik_measles <- function(a,b,c,d) {
  
  function(p){
    q <- 1 - p
    a*log(q^2) + b*log(p*q^2) +c*log(2*p^2*q) + d*log(p^2)
  }
}

loglik_dat <- loglik_measles(34,25,36,239)

p <- seq(0, 1, 0.005)
plot(p, loglik_dat(p), type = "l")

# find maximum
p_hat <- p[loglik_dat(p) >= max(loglik_dat(p))] 
# 0.79

q_hat <- 1 - p_hat
# 0.21

n <- 334

n*q_hat^2
n*2*p_hat*q_hat^2
n*2*p_hat^2*q_hat
n*p_hat^2



## simulation ##

#e.g. http://epirecip.es/epicookbook/chapters/ob18/c3/r


# Reed-Frost
sir_cb <- function(q = NA,
                   R0 = NA,
                   N,
                   MAXTIME = 100,
                   I0 = 1,
                   S0 = N - I0){
  if (is.na(q))
    q <- 1 - R0/N  #pairwise probability of avoiding potentially infectious contact
  
  # initial conditions
  I <- I0
  S <- S0
  
  for(t in seq_len(MAXTIME)){
    
    I <- c(I, rbinom(n = 1,
                     size = S[t],
                     prob = 1 - q^I[t]))
    
    S <- c(S, S[t] - I[t + 1])
    
    if(I[t + 1] == 0) break
    
  }
  
  return(
    data.frame(t = 0:t,
               I = I,
               S = S))
}

## single household
sir_cb(q = 0.5, N = 3)


## lecture measles example
res <- list()
for (i in 1:334){
  
  res[[i]] <- sir_cb(q = 0.21, N = 3)
}

library(purrr)
xx <- map(res, "I")
xx <- map_chr(xx, paste, collapse = ",")
table(xx)


# plots ------------------------------------------------------------------


