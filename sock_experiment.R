sock <- setRefClass("sock", fields = list(color = "character", LR = "character", id = "numeric", pair = "numeric"))
# create a class of objects called sock, not all features are used
#sock
#greenL <- sock(color = "Green", LR= "L", id = 1, pair = 2)
#greenR <- greenL$copy()
#greenR$id <- 2
#greenR$pair <- 1
#greenR
##greenL
#rbind(greenR, greenL)
#sample(10, replace = F)
#green <- sock(color = c("Green", "Green"), LR = c("L","R"), id = c(1,2), pair = c(2,1))
#no.pairs = 10
#no.socks = 2*no.pairs
### time to build the problem, test it out
# find max size for given number of socks
find.max <- function(no.pairs, out.opt = NA, user.shuff = NA){
  #input number of pairs, out option (optional), and input shuffle (optional)
  #output depends on out option
    # 1: max of stochastic process
    # 2: permutation shuffling matrix (rows are steps, columns are {0,1} depending if sock is chosen and out without pair)
    # 3: return the order the socks were drawn
    # 4: return the path of the stochastic process
  
  if(!(out.opt %in% c(1,2,3,4)) && !is.na(out.opt)){
    return(stop("Invalid Input"))
  }
  no.socks <- 2 * no.pairs
  if(!is.na(user.shuff) && length(user.shuff)!=no.socks){
    return(stop("Invalid Shuffle"))
  }
  
  M <- matrix( data = 0, nrow = no.socks, ncol = no.socks) #permutation matrix to switch even and even + 1 element (pair)
  for ( i in 1:no.pairs){
    j <- 2*i
    M[j-1,j] <- 1
    M[j,j-1] <-1
  }
  M
  ID <- 1:no.socks
  pair.map <- as.numeric(M %*% ID) #which sock is pair for sock_i
  #cbind(ID,pair.map) # see the pair and grouping, can map them together
  sock.name <- matrix(data = "sock", nrow = no.socks, ncol = 1)
  sock.type <- as.character(floor((2:(no.socks+1)/2)))
  sock.foot <- as.character(matrix(data = c("L","R"), nrow = no.socks, ncol = 1))
  paste(sock.name, sock.type, sock.foot)
  sock.set <- sock(color = rep("Green", no.socks), LR = sock.foot, id = ID, pair = pair.map)
  #sock.set$pair
  
  
  M.init <- matrix(data = 0, nrow = no.socks, ncol = no.socks)
  if(is.na(user.shuff[1])){
    shuff <- sample(no.socks, replace = F) #create random ordering of sock pulls
  }else{
    shuff <- user.shuff #use predetermined ordering of sock pulls
  }
  for ( i in 1:no.socks){
    if( i == 1 ){
      M.init[,shuff[i]] <- 1
    } else{
      map <- sock.set$pair[shuff[i]]
      curr <- shuff[i]
      if (M.init[i-1,map] == 1){
        M.init[i:no.socks,curr] <- 0
        M.init[i:no.socks,map] <- 0
      }else{
        M.init[i:no.socks,curr] <- 1
      }
    }
    
  }
  if(out.opt == 1 || is.na(out.opt)){#max of stochastic process
    res <- norm(M.init, type = "I")
  }else if(out.opt == 2) {#another option to return permutation shuff
    res <- M.init
  }else if (out.opt ==3){ #return shuffling
    res <- shuff
  }else if (out.opt == 4) { #path of stochastic process
    res <- apply(M.init, 1, sum)
  }
  return(res)
}
find.max(5,2)

N <- 15000
dist <- rep(0,N)
for( i in 1:N ){
  dist[i] <- find.max(150)
}
hist(dist,150)
mean(dist)-(150/2)
sd(dist)

find.max(10)
permutations <- function(n){
  if(n==1){
    return(matrix(1))
  } else {
    sp <- permutations(n-1)
    p <- nrow(sp)
    A <- matrix(nrow=n*p,ncol=n)
    for(i in 1:n){
      A[(i-1)*p+1:p,] <- cbind(i,sp+(sp>=i))
    }
    return(A)
  }
}

m <- 10
perms <- permutations(m)
det_res <- cbind(perms, 0)
for(i in 1:length(det_res[,m+1])){
  det_res[i,m+1] <-find.max(m/2,1, det_res[i,1:m])
}
det_res
hist(det_res[,7])
library(dplyr)

det_res <- as.data.frame(det_res)
actdist_3p <- det_res %>% group_by(V7) %>% tally()
actdist_3p$n / sum(actdist_3p$n)
# 4 pair actual
m <- 8
perms <- permutations(m)
det_res <- cbind(perms, 0)
for(i in 1:length(det_res[,m+1])){
  det_res[i,m+1] <-find.max(m/2,1, det_res[i,1:m])
}
det_res
hist(det_res[,9])

det_res <- as.data.frame(det_res)
actdist_4p <- det_res %>% group_by(V9) %>% tally()
actdist_4p$n / sum(actdist_4p$n)


#plot a few
plot(  find.max(100,4), type = "l",  xlim = c(0,200), ylim=c(0,70))
for(i in 1:20){
  points(find.max(100,4), type="l")
}
