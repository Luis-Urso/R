###################################################################
# R Basics by Luis Urso                                           #
# Loops & Conditions                                              #
###################################################################

## Important to notice that the names are case sensitive. 

## Using the FOR loop statment (assume: for each value of the 
## vector var1 feed into index)

var1 <- c(1:10)

for (index in var1)
{
  
  multip <- index * 7 
  
  print(multip)
  
  if(multip==42) 
    {
    print("Bingo")
    }
  
}





