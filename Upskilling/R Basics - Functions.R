###################################################################
# R Basics by Luis Urso                                           #
# Functions                                                       #
###################################################################

## Important to notice that the names are case sensitive. 

## Creating the function 

calc_x <- function(x)
{

    y <- 100+(2*x)^2
    
    return(y)
  
}

## Using the function calc_x
 
x <- c(1:100)

y <- calc_x(x)

## Ploting the values

plot(x,y)


## Function Calculate Money Change

calc_change <- function(amount)
{
  avail <- c(200,100,50,20,10,5,2,1,0.5,0.25)
  distrib <- amount
  resp <-""
  
  for (i in avail)
  {
    qtd <- as.integer(distrib/i)
    
    if(distrib>=i)
    {
      resp=paste(resp,qtd," bills(s) of: ",i," ; ")
      distrib <- distrib - (qtd*i)
    }
    
  }
  return(resp)
}


## Using the function calc_change

print(calc_change(110))


