


euc<-function(a,b){

  if (!is.numeric(a) | !is.numeric(b)) stop('Wrong input')

  while (b!=0){
    x=b
    b=a%%b
    a=x
  }
  return(a)
}
