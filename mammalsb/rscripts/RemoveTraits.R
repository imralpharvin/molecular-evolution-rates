
RemoveTraits <- function(dt) {
  temp = 4
  columns = c()
  
  while(temp < ncol(dt))
  {
    name <- colnames(dt)[temp]
    print(name)
    b <- GetTraitInfo(dfTraits[[temp]])
    if(b == 1)
    {
      columns <- c(columns, temp)
    }
    temp = temp + 1
    
  }
  print("Columns")
  print(columns)
  return(columns)
}
