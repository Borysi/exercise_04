
ReturnCoins = function(M){
  r = M
  pd = r %/% 50
  r = r - 50 * pd
  dc = r%/%20
  r = r - 20 * dc
  ds = r%/%10
  r = r - 10 * ds
  p = r%/%5
  r = r - 5 * p
  d = r%/%2
  j = r - 2 * d
  return (c(pd,dc,ds,p,d,j))
}

print(ReturnCoins(8))


UniversalReturnCoins = function(M,den_list){
  out <<- append(out,M%/%den_list[N])

  N<<- N+1

  if (N > length(den_list)){
    return(out)
  }

  UniversalReturnCoins(M-(den_list[N-1]*out[N-1]),den_list)
}

out <<- c()
N <<- 1
print(UniversalReturnCoins(40,c(25,20,10,5,1)))

Chocolate_rec = function (M,r,c){
  if (r == nrow(M)){
    return(M[r,c])
  }
  else{
    bars = M[r,c]
    down = Chocolate(M,r+1,c)
    diagonal = Chocolate(M,r+1,c+1)
    return (max(down,diagonal) + bars)
  }
}


A = matrix(c(1,6,6,4,5,6),nrow = 2,ncol = 3)
A

print(Chocolate_rec(A,1,1))

Chocolate_iter = function (M,r,c){
  n_r = 0
  n_c = 0
  bars = 0
  while(n_r < nrow(M)-1){ # why -1 ???
    if (M[r+1,c] > M[r+1,c+1]){
      n_r = n_r + 1
      bars = bars + M[r + n_r,c+ n_c]
    }
    else{
      n_r = n_r+1
      n_c = n_c + 1
      bars = bars + M[r + n_r,c+ n_c]}
    }
  return(bars)
}

A = matrix(c(1,6,2,6,1,1),nrow = 2,ncol = 3)
print(Chocolate_iter(A,1,1))
A


HanoiTowers = function(n,fromPeg,toPeg){
  if (n ==1){
    print(paste("Move discr from ", fromPeg, " to ", toPeg))
    return()
  }
  emptyPeg = 6 - fromPeg - toPeg
  HanoiTowers(n-1,fromPeg,emptyPeg)
  print(paste("Move discr from ", fromPeg, " to ", toPeg))
  HanoiTowers(n-1,emptyPeg,toPeg)
  return

}

print(HanoiTowers(5,1,3))