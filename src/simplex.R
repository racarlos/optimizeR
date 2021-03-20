
# simplex function takes in matrix as input 
simplex <- function(mat){

  orig = mat
  print("Simplex Starting: ")
  print(mat)
  # Get the row and columns of the matrix 
  row = nrow(mat)
  col = ncol(mat)
  
  iterations = 0
  outputs = list()
  
  # Keep iterating while any of the elements in the bottom row is negative
  while(any(mat[row,1:col-1] < 0)){
      # Get Most negative number in bottom row 
      pivotcol = which.min(mat[row,])
    
      testratio = c()
      
      # Get the test ratio for the pivot column
      for (i in 1:row-1) {
        testratio[i] =  mat[i,col] / mat[i,pivotcol]   # lastcol / pivotcol
      }
      
      # Set all negatives and 0's to infinity
      testratio[testratio <= 0 ] = Inf
      
      # Pivot row is the least positive test ratio
      pivotrow = which.min(testratio)
      
      # Pivot element is the intersection of pivot row and pivot col
      pivotelement = mat[pivotrow,pivotcol]
      
      # Normalize Pivot Row
      for(i in 1:col){
        mat[pivotrow,i] = mat[pivotrow,i] / pivotelement  
      }
      
      pivotelement = mat[pivotrow,pivotcol]
      
      # Zero out pivot col
      for(i in 1:row){
    
        subtracter = c()  
    
        if(i!= pivotrow){   # Getting vector to subtract    
          for(j in 1:col){
            subtracter[j] = mat[i,pivotcol] * mat[pivotrow,j]
          }
    
          for(j in 1:col){  # Actual subtraction
            mat[i,j] = mat[i,j] - subtracter[j]
          }
        }
      }
      
      print(mat)
  
      # Incrementing Iterations and adding current matrix to list of outputs 
      iterations = iterations + 1
      outputs[[iterations]] = mat
      print(iterations)
      
      # Stahp na at 201 iterations
      if(iterations > 200){
        print(iterations)
        break
      }
      
  } # End of simplex method
  
  if(iterations > 200){
    final = list(matrix = orig,answer = -1,iterations = -1,outputs = outputs)
  } else {
    # Gets the answer from last col of the last Row
    answer = mat[row,col]
    final = list(matrix = mat,answer = answer,iterations = iterations,outputs = outputs)
  }
  
  return(final)
}






