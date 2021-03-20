# Author: Carlos Robie A.
# Student No: 2018 - 03026



# Gauss Jordan Function for solving the system of equations
GaussJordanMethod <- function(matrix){
  
  # Gets the augcoeff matrix and the variables
  test = matrix
  
  # For the row and column of the matrix
  row = nrow(test)
  col = ncol(test)
  
  #Solution Set for the Equation
  solutionSet = c()
  
  # Loop for Gauss Jordan
  for (i in 1:row){
    if(i != row){
      
      #Getting the Pivot Element
      pivot = max(abs(test[i:row,i]))
      
      # Gettting the Row of the Pivot Element
      cols = abs(test[,i])
      index = order(cols,decreasing = TRUE)[1]
      pivotrow = test[index,]
      
      if(pivot == 0){     #if there is no solution
        print("No Unique Solution Exists.")
        return(NA)
      } else {            # swaps the rows
        swaprow = test[i,]
        test[i,] = pivotrow
        test[index,] = swaprow
      }
    }
    
    
    # Normalizing the rows to create an upper triangular matrix
    test[i,] = test[i,]/test[i,i]
    
    # Putting all solutions to the RHS and creating and creating a main diagonal of 1
    for(j in 1:row){
      
      if(i == j){
        next  
      }
      
      # Getting the normalized row
      normalizedrow = test[j,i]*test[i,]
      test[j,] = test[j,] - normalizedrow
    }
    
  }
  
  for(i in 1:row){
    solutionSet[i] = test[i,col]
  }  
  
  
  matrixanswer = test
  
  #returning the list 
  return(list(solutionSet = solutionSet,matrix = matrixanswer))
}


#Function 
PolynomialRegression <- function(x,y,n){
  
  
  # Gets the left part
  a = as.numeric(x)
  print("Left: ")
  print(a)
  #Gets the right part
  b = as.numeric(y)
  print("Right ")
  print(b)
  
  
  #creating augcoeff matrix 
  matrix = matrix(NA,nrow = n+1,ncol= n+2)
  
  row = nrow(matrix)
  col = ncol(matrix)
  length = length(a)
  rip = length+1
  
  
  # populating the matrix 
  for (i in 1:row) {    # Rows 
    for(j in 1:col){  # Columns
      x = i+j
      z = x-2
      
      a = as.numeric(a)
      z = as.numeric(z)
      
      matrix[i,j] = sum(a^z)
      
      # For the RHS 
      if(j == col){
        sumz = 0
        
        # Loop to get the summation of the left and right parts
        for (k in 1:length(a)) {
          
          ak = as.numeric(a[k])
          bk = as.numeric(b[k])
          sumz = sumz + (( ak^(i-1) ) * bk)
        }
        
        # add sum to the rhs 
        matrix[i,j] = sumz
      }
    }
  }
  
  # Calling Gauss Jordan for Answers
  solution = GaussJordanMethod(matrix)
  set = solution$solutionSet
  mat = solution$matrix 
  
  # Parsing the solution set
  strsum = "function(x) "
  strsum = paste(strsum,set[1])
  strsum = paste(strsum,"+ ")
  count = 1
  

  # Loop for getting the String Equivalent
  for(i in 2:length(set)){
    parsed = set[i]
    equate = ""
    equate = paste(equate,parsed,sep="")
    equate = paste(equate," * x^",sep="")
    equate = paste(equate,count, sep="")
    
    if(i != length(set))
      equate = paste(equate," + ",sep="")
    
    strsum = paste(strsum,equate,sep="")
    count = count + 1
  }
  
  # Creating a Function 
  func = eval(parse(text = strsum))
  
  # Final list 
  final = list(augcoeffmatrix = matrix,unknowns = set,polynomial_string = strsum,polynomial_function = func)

  return(final)
}

