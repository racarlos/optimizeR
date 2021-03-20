# Quadratic Spline Interpolation

QuadraticSplineInterpolation <- function(a,b,approx){
  
  x = as.numeric(a)
  y = as.numeric(b)
  approx = as.numeric(approx)
  
  print("Left")
  print(x)
  print("Right")
  print(y)
  print("Approx")
  print(approx)
  
  # Getting points and intervals
  n = length(x)                         # data points
  intervs = n - 1                       # intervals
  
  unknowns = (3*intervs)-1 				        # 3 unknowns for each interval
  mat = matrix(0,unknowns,unknowns+1)     # matrix for storing equations  
  rhs = unknowns+1
  
  row = 1       # 1 equation per row, append per equation
  col = 0      
  
  # Condition 1 : Ax^2 + Bx + C , Inner points
  
  for(i in 2:intervs){
                  
    mat[row,col] = x[i]^2
    mat[row,col+1] = x[i]^1
    mat[row,col+2] = x[i]^0
    mat[row,rhs] = y[i]
    
    col = col + 3    # gaps every 3 columns  
  
    row = row + 1   
    mat[row,col] = x[i]^2
    mat[row,col+1] = x[i]^1
    mat[row,col+2] = x[i]^0
    mat[row,rhs] = y[i] 
    
    row = row + 1
  }
 
  #Condition 2: Ax^2 + Bx + C , Outer points
  
  col = 0
  mat[row,col] = x[1]^2         # First Point
  mat[row,col+1] = x[1]^1
  mat[row,col+2] = x[1]^0
  mat[row,rhs] = y[1]
  
  col = rhs
  row = row + 1
  
  mat[row,col-3] = x[n]^2         # Last Point
  mat[row,col-2] = x[n]^1
  mat[row,col-1] = x[n]^0
  mat[row,col] = y[n]
  
  row = row + 1
  col = 0
  
  #Condition 3: First derivative of interior knots must be equal, 2Ax + b = 2Ax + b 
  
  for(i in 2:intervs){
  
    mat[row,col] = 2*x[i]             # Left Side
    mat[row,col+1] = x[i]^0
    
    col = col + 3                     # gaps every 3 columns 
    
    mat[row,col] = (2*x[i])*-1       # Right Side, Negated
    mat[row,col+1] = (x[i]^0)*-1
    
    row = row+1
  }
  
  #Vector of variables 
  variables = c()
  
  for(i in 2:intervs){              # A variables
    term = "a"
    term = paste(term,i,sep = "")
    variables = c(variables,term)
  }
  
  for(i in 1:intervs){              # B variables
    term = "b"
    term = paste(term,i,sep = "")
    variables = c(variables,term)
  }
  
  for(i in 1:intervs){              # c variables 
    term = "c"
    term = paste(term,i,sep = "")
    variables = c(variables,term)
  }
  
  # data for the gaussian method 
  solution = list(augcoeffmatrix=mat,variables=variables)
  
  # Using gaussian Method of Elimination to solve system of Equations  
  answer = GaussianMethod(solution)
  solset = c(0,answer$solutionSet)      # getting the solutionset 

  # List of parsed functions   
  funclist = list()
  funcs = c()
  # Creating functions
  for(i in 1:intervs){
    
    func = "function(x) "
    pos = i*3
    
    func = paste(func,solset[pos-2],sep ="")
    func = paste(func,"* x^2 + ")
    func = paste(func,solset[pos-1],sep ="")
    func = paste(func,"* x + ")
    func = paste(func,solset[pos],sep ="")
    
    funcs[i] = func
    strfunc = eval(parse(text=func))  
    funclist[[i]] = strfunc
  }
  
  n = as.numeric(approx)
  datalist = data.frame(x=x,y=y)
  coeffs = c("a1",answer$variables)   # adds a1 to the coefficients
  index = 0
  # get the function to use the given approx 
  for (i in 1:intervs){
    # if the approximate value is between the interval and the next
    if(x[i] <= n && n <= x[i+1]){
      index = i
    }
  }
  
  intervfunc = funclist[[index]]
  answer = intervfunc(n)
  
  # Feeding the approximate to the obtained function   
  print(answer)
  
  final = list(functions=funclist,funcs = funcs,coefficients=coeffs,data = datalist,approximate = answer)
  
  return(final)
}

# Gaussian elimination takes in agument coefficient matrix 
GaussianMethod <- function(AugCoeffMatrix){
  
  test = AugCoeffMatrix$augcoeffmatrix
  variables = AugCoeffMatrix$variables
  
  # For the row and column of the matrix
  row = nrow(test)
  col = ncol(test)
  
  #Solution set 
  solutionset = c(matrix(0,1,row))
  
  # Loop for Forward Elimination
  for(i in 1:(row-1)){ 
    
    #Getting the Pivot Element
    pivot =  max(abs(test[i:row,i]))
    
    if(pivot == 0){     #if there is no solution
      return(test)
    }
    
    # gets pivot row
    pivotrow = which.max(abs(test[i:row,i]))+i-1
    
    # swaps the rows
    temp = test[i,]
    test[i,] = test[pivotrow,]
    test[pivotrow,] = temp
    
    # normalizing the rows to create and upper triangularix
    for(j in (i+1):row){
      pivotelement = test[i,i]
      multiplier = (test[j,i])/(pivotelement)
      normalizedrow = multiplier * test[i,]
      test[j,] = test[j,] - normalizedrow
    }
  }
  
  # Gets the RHS values 
  rhs = test[,col]
  
  # Back Substiution
  for (i in row:1){
    for (j in i:1){
      numerator = rhs[i] - (sum(test[i,j:row] * solutionset[j:row]))
      denominator = test[i,i]
      solutionset[j] = numerator / denominator
    }
  }
  
  # labelled list to be returend
  final = list(augcoeffmatrix = test,variables = variables, solutionSet = solutionset)
  
  return(final)
}