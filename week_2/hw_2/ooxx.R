triples <- list(c(1,2,3),c(4,5,6),c(7,8,9),c(1,4,7),c(2,5,8),c(3,6,9),c(1,5,9),c(3,5,7))
winner <- F
state <- as.character(1:9)

display <- function(state){
  cat(" ",state[1],"|",state[2],"|",state[3],"\n")
  cat("______","\n")
  cat(" ",state[4],"|",state[5],"|",state[6],"\n")
  cat("______","\n")
  cat(" ",state[7],"|",state[8],"|",state[9],"\n")
}

update <- function(state, who, pos){
  if(who==1){state[pos]<- "x"}
  else if(who==2){state[pos]<-"o"}
  return(state)
}

check_winner <- function(state){
  for(i in 1:length(triples)){
    if(sum(triples[[i]] %in% which(state=="x"))>=3){cat("x win")}
  }
  for(i in 1:length(triples)){
    if(sum(triples[[i]] %in% which(state=="o"))>=3){cat("o win")}
  }
}























