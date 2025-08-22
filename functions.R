myR0 <- function(a,b){
  R0=a/b
  return(R0)
} 

myR0(3,5)

myIncidence <- function(numCases, Pop){
  
  incidenceRate <- numCases/Pop*1000
  
  return(incidenceRate)
}



myIncidence(6900234, 20000000)


myBMI <- function(height, weight){
  
  BMI <- weight/height*height
  
  
  return(BMI)
}
myBMI(5,9)

myBMI <- function(height, weight) {
  BMI <- weight / (height^2)
  return(BMI)
}
myBMI(9,8)


