## a decision support tool for the stratified decision-making model 

##INPUTS
# N is the number of status or number of utility matrices.
# Insert the number of status (at least 2):
N <- 2

# M is the number of outcomes.
# Insert the number of outcomes (at least 2):
M <- 2

# B is the number of strategies.
# Insert the number of strategies (at least 2):
B <- 2

# W is the vector of probability of current state.
# We've got N*M states.
# We assumed the probabilities in W are equal. 
#You may decide to insert your own W vector:
W <- matrix(1/(N*M),nrow=1,ncol=N*M)



# Matrix P is a status transition probability matrix that is a N*N matrix.
# We've made the matrix P randomly from values within [0,1] knowing that sum of each row must be equal to 1.
# You may disregard this part and provide your own N*N matrix P.

P<-matrix(0,nrow=N,ncol=N)

for (j in 1:N) { 
  if (j > 1){
    if (j<N) {
      
      second1_or_above_rows <- matrix(0, nrow=1, ncol=N)
      
      
      y <- diff(c(0, sort(runif(j-1)), 1))
      for (k in 1:j){
        second1_or_above_rows[1,k] <- y[k]
        
      }
      
      P[j,] <- second1_or_above_rows
      
    }else{
      second2_or_above_rows <- matrix(0, nrow=1, ncol=N)
      x <- diff(c(0, sort(runif(j-1)), 1))
      for (l in 1:j){
        second2_or_above_rows[1,l] <- x[l]
      }
      P[j,] <- second2_or_above_rows
    }
    
  } else {
    
    first_row <- matrix(0, nrow=1, ncol=N)
    for (i in 2:N) { 
      first_row[1,1] <- 1
      first_row[1,i] <- 0
    }
    P[1,] <- first_row 
    
  }
  
}

# Matrix Q is an outcome transition probability matrix that is a M*M matrix.
# We've made the matrix Q randomly from values within [0,1] knowing that sum of each row must be equal to 1.
# You may disregard this part and provide your own M*M matrix Q.

Q<-matrix(0,nrow=M,ncol=M)

for (j in 1:M) { 
  if (j > 1){
    if (j<M) {
      
      second1_or_above_rows <- matrix(0, nrow=1, ncol=M)
      
      
      y <- diff(c(0, sort(runif(j-1)), 1))
      for (k in 1:j){
        second1_or_above_rows[1,k] <- y[k]
        
      }
      
      Q[j,] <- second1_or_above_rows
      
    }else{
      second2_or_above_rows <- matrix(0, nrow=1, ncol=M)
      x <- diff(c(0, sort(runif(j-1)), 1))
      for (l in 1:j){
        second2_or_above_rows[1,l] <- x[l]
      }
      Q[j,] <- second2_or_above_rows
    }
    
  } else {
    
    first_row <- matrix(0, nrow=1, ncol=M)
    for (i in 2:M) { 
      first_row[1,1] <- 1
      first_row[1,i] <- 0
    }
    Q[1,] <- first_row 
    
  }
  
}

# N utility matrices (U) are required which are shown in arrays.
# We've generated utility matrices (U) by producing randomly generated values from 0 to 1.
# However, if you have already collected your N utility matrices, you may disregard this part.

U <- array(matrix(0,nrow=B,ncol=M), dim=c(B,M,N))
for(i in 1:N){
  U[,,i]<-matrix(runif(B*M, min=0, max=1),nrow=B,ncol=M)
  #print(U[,,i])
}


##OUTPUTS:
# The matrix S (state transition probability matrix) will be generated based on your inputs:


S<-matrix(0,nrow=N*M,ncol=N*M)

for (l in 1:N){
  for (k in 1:N){
    v<-k*M-M+1
    a<-k*M
    for(i in v:a){
      w<-l*M-M+1
      z<-l*M
      for(j in w:z){
        vv<-i-k*M+M
        xx<-j-l*M+M
        S[i,j]<-P[k,l]*Q[vv,xx]
      }
      
    }
    
  }
}


# UTS (Utility Status) matrix has got B rows and N*M columns.
# Pulling out utility values of strategies and making the UTS matrix. 
UT<-matrix(0,nrow=N,ncol=M)
UTS<-matrix(0,nrow=B,ncol=N*M)
for(j in 1:B){
  for(i in 1:N){
    UT[i,]<-U[j,,i]
  }
  
  UTS[j,]<-as.vector(t(UT))
}


# The after transition pay off matrix (ATPFM) will be generated:

ATPFM<-matrix(0,nrow=B,ncol=N*M)
for(i in 1:B){
  for(j in 1:(N*M)){
    ATPFM[i,j]<-crossprod(UTS[i,],S[j,])
  }
}

# Calculating the EMV and presenting the order of strategies:
EMV<-matrix(0,nrow=B,ncol=1)
for(i in 1:B){
  EMV[i,1]<-crossprod(ATPFM[i,],W[1,])
}

EMV
print("Order of strategies is:")
order(EMV, decreasing=TRUE)

