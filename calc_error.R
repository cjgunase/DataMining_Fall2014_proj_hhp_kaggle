calc_error <- function(act,pred)
{
  aact <- as.matrix(act)
  ppred <- as.matrix(pred)
  
  if(nrow(aact) == nrow(ppred)){
    return (sqrt(colSums((log(ppred+1) - log(aact+1)) ^ 2) / nrow(aact)))
  } else {
    return (-99)
  }
  
}




