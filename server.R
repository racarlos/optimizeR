# Sourcing functions from solvers
source("src/regres.R")
source("src/qsi.R")
source("src/simplex.R")

# Importing libaries 
library(shiny)
library(shinydashboard)
library(rhandsontable)

# Simplex Initial Input 
init1 = c(100,5,5,5,5,5)
init2 = c(100,5,5,5,5,5)
init3 = c(100,5,5,5,5,5)
init4 = c(0,20,20,20,20,20)

rownames = c("Denver","Phoenix","Dallas","Demands")
colnames = c("Supply","W-1","W-2","W-3","W-4","W-5")

mat = matrix(0,nrow =4,ncol = 6,byrow = TRUE)

colnames(mat) = colnames
rownames(mat) = rownames

mat[1,] = init1
mat[2,] = init2
mat[3,] = init3
mat[4,] = init4

# Simplex Initial Output 
init1 = c(0,0,0,0,0,0)
init2 = c(0,0,0,0,0,0)
init3 = c(0,0,0,0,0,0)
init4 = c(0,0,0,0,0,0)

rownames = c("Denver","Phoenix","Dallas","Total/W")
colnames = c("W-1","W-2","W-3","W-4","W-5","Total/P")

out = matrix(0,nrow =4,ncol = 6,byrow = TRUE)

colnames(out) = colnames
rownames(out) = rownames

out[1,] = init1
out[2,] = init2
out[3,] = init3
out[4,] = init4


# Function for adding slack
slacker <- function(final,simple){
  
  # Slack Variables 
  slack1 = c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  slack2 = c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  slack3 = c(0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0)
  slack4 = c(0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0)
  slack5 = c(0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0)
  slack6 = c(0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0)
  slack7 = c(0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0)
  slack8 = c(0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0)
  slack9 = c(0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0)
  slack10 =c(0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0)
  slack11 =c(0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0)
  slack12 =c(0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0)
  slack13 =c(0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0)
  slack14 =c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0)
  slack15 =c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0)
  slack16 =c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)
  
  # Adds slack the matrix 
  final[,1] = simple[,1]
  final[,2] = simple[,2]
  final[,3] = simple[,3]
  final[,4] = simple[,4]
  final[,5] = simple[,5]
  final[,6] = simple[,6]
  final[,7] = simple[,7]
  final[,8] = simple[,8]
  final[,9] = slack1
  final[,10] = slack2
  final[,11] = slack3
  final[,12] = slack4
  final[,13] = slack5
  final[,14] = slack6
  final[,15] = slack7
  final[,16] = slack8
  final[,17] = slack9
  final[,18] = slack10
  final[,19] = slack11
  final[,20] = slack12
  final[,21] = slack13
  final[,22] = slack14
  final[,23] = slack15
  final[,24] = slack16
  final[,25] = simple[,9]	#rhs 
 
  
  return(final)
}


#Server 
server <- function(input,output,session){
  
  # Initial values for reactive values
  optimal = NULL
  iterations = 0
  tableaus = list()
  

	# Polynomial Regression function 
	output$prfunc = renderValueBox({

		prfile = input$prfile
		degree = input$prdegree

		#Warning value boxes
		nocsv = valueBox("Error","No CSV File Yet",icon = icon("exclamation-triangle"),color="red",width = 7)
		nodeg = valueBox("Error","No Degree Yet",icon = icon("exclamation-triangle"),color="red",width = 7)
		wrongdeg = valueBox("Error","Invalid Degree",icon = icon("exclamation-triangle"),color="yellow",width = 7)
		
		# Checks if CSV is not empty
		if(is.null(prfile)){ 
			return(nocsv) 
		}
		
		# Checks if the degrees is not empty
		if(is.na(degree)){
		  return(nodeg)
		} 

		# Reads csv file and assigns left and right side to variables
		prcsv = read.csv(prfile$datapath,header = FALSE)
		a = prcsv$V1
		b = prcsv$V2
		
		# Degree must be between 1 and degree n-1
		if(degree > 0 && degree < length(a)){
			pr = PolynomialRegression(a,b,degree)
			prfunc = pr$polynomial_string
		} else {
			return(wrongdeg)
		}

		final = valueBox("Function: ",prfunc,icon = icon("database"),color="blue",width = 7)
		return(final)
	})

	# Polynomial Regression function 
	output$prapprox = renderValueBox({  
	  prfile = input$prfile
	  degree = input$prdegree
	  approx = input$prapprox

	  #Warning value boxes
    nocsv = valueBox("Error","No CSV File Yet",icon = icon("exclamation-triangle"),color="red",width = 7)
    noapp = valueBox("Error","No Estimate Yet",icon = icon("exclamation-triangle"),color="red",width = 7)
    nodeg = valueBox("Error","No Degree Yet",icon = icon("exclamation-triangle"),color="red",width = 7)
    wrongdeg = valueBox("Error","Invalid Degree",icon = icon("exclamation-triangle"),color="yellow",width = 7)
    
    # Checks if CSV is not empty
    if(is.null(prfile)){ 
      return(nocsv) 
    }
    
    # Checks if the degrees is not empty
    if(is.na(degree)){
      return(nodeg)
    } 
    
    # Checks if the approx is not empty
    if(is.na(approx)){
      return(noapp = valueBox("Error","No Estimate Yet",icon = icon("exclamation-triangle"),color="red",width = 7))
    } 
    
    # Reads csv file and assigns left and right side to variables
    prcsv = read.csv(prfile$datapath,header = FALSE)
    a = prcsv$V1
    b = prcsv$V2
    print(degree)
    
    # Degree must be between 1 and degree n-1
    if(degree > 0 && degree < length(a)){
      pr = PolynomialRegression(a,b,degree)
      prfunc = pr$polynomial_function
    } else {
      return(wrongdeg)
    }
    
    # Feeds the approxinate to the function 
    prapprox = prfunc(approx)
    
    final = valueBox("Estimate: ",prapprox,icon = icon("signal"),color="green",width = 7)
	  return(final)
	})


	# ValueBox - Quadratic Spline Interpolation Approximate 
	output$qsanswer = renderValueBox({

		qsfile = input$qsfile
		approx = input$qsapprox
		
		#Warning Value Boxes
		nocsv = valueBox("Error","No CSV File Yet.",icon = icon("exclamation-triangle"),color="red",width = 7)
		noap = valueBox("Error","No Estimate Yet.",icon = icon("exclamation-triangle"),color="red",width =7)
		wrongap = valueBox("Error","Current Estimate is invalid.",icon = icon("exclamation-triangle"),color="red",width =7)

		# Checks if CSV is not empty
		if(is.null(qsfile)){ 
		  return(nocsv) 
		}
		
		# Checks if the approx is not empty
		if(is.na(approx)){
		  return(noap)
		} 
		
		# Reads csv file and assigns left and right side to variables
		qsicsv = read.csv(qsfile$datapath,header = FALSE)
		a = as.numeric(qsicsv$V1)
		b = as.numeric(qsicsv$V2)
		
		print(a)
		print(b)
		#If the approximate is within the bounds of the intervals 
		if(approx > a[1] && approx < a[length(a)]){
		  qsi = QuadraticSplineInterpolation(a,b,approx)
		  answer = qsi$approximate
		} else {
		  return(wrongap)
		}
		
		
		final = valueBox(answer,"Estimate :",icon = icon("signal"),color="blue",width = 7)
		return(final)
	})
	
	#functions for the QSI
	output$qsfuncs = renderTable({

		qsfile = input$qsfile
		approx = input$qsapprox
		
		# Checks if CSV is not empty
		if(is.null(qsfile)){ 
		  return(NULL) 
		}
		
		# Checks if the approx is not empty
		if(is.na(approx)){
		  return(NULL)
		} 
		
		# Reads csv file and assigns left and right side to variables
  	qsicsv = read.csv(qsfile$datapath,header = FALSE)
  	a = qsicsv$V1
  	b = qsicsv$V2

  	qsi = QuadraticSplineInterpolation(a,b,approx)
 	  funclist = qsi$funcs

	 	funcs = c()
	 	intervs = c()
    
	 	# for the intervals 
	 	for (i in 1:length(funclist)+1){
	 	  
	 	 	gap = " "
	 		gap = paste(gap,a[i-1],sep="")
	 		gap = paste(gap," <= x <= ",sep="")
	 		gap = paste(gap,a[i],sep ="")
	 		
	 		intervs[i-1] = gap
	 		funcs[i-1] = funclist[i-1]
	 	}

	 	print(funcs)
	 	print(intervs)
	 	
	 	final = data.frame(Interval = intervs,Function = funcs)
	 	return(final)
	},bordered = TRUE,align ="c")


  # Defining Reactive values
  values = reactiveValues(input = mat,output = out,optimal = optimal,iterations = iterations,tableaus = tableaus)

  # Observes change for input matrix
  observeEvent(input$simplexinput$changes$changes,{
  	values$input = hot_to_r(input$simplexinput)
  })

  #Observes change for output matrix 
  observeEvent(input$simplexoutput$changes$changes,{
  	values$output = hot_to_r(input$simplexoutput)
  })

  
  #Table for Simplex Outputs - simplex output
  output$simplexoutput = renderRHandsontable({
     
    #Get the values from the matrix
  
    # Total for each plant , right most column
	  values$output[1,6] = sum(values$output[1,1:5])
	  values$output[2,6] = sum(values$output[2,1:5])
	  values$output[3,6] = sum(values$output[3,1:5])

	  # Total for each warehouse - bottom row 
	  values$output[4,1] = sum(values$output[1:3,1])
	  values$output[4,2] = sum(values$output[1:3,2])
	  values$output[4,3] = sum(values$output[1:3,3])
	  values$output[4,4] = sum(values$output[1:3,4])
	  values$output[4,5] = sum(values$output[1:3,5])

	  final = rhandsontable(values$output,readOnly = TRUE)

    return(final)
  })


  #Table for Simplex Inputs - simplexinput
  output$simplexinput = renderRHandsontable({
    
      # Row and column names
      plants = c("Denver","Phoenix","Dallas","Demands")
      wares = c("Supply","W-1","W-2","W-3","W-4","W-5")

      #Get the matrix of costs
      cost = as.matrix(values$input[1:3,2:6])
      print("Costs :")
      print(cost)
      
      # Get the vector of supply 
      supply = as.vector(values$input[1:3,1])
      totalsupply = sum(supply)
      supply = supply *-1
      print("Supply :")
      print(supply)

      print(totalsupply)
      
      #Get the vector of demands 
      demands = as.vector(values$input[4,2:6])
      print("Demands :")
      print(demands)
      
      totaldemands = sum(demands)
      print(totaldemands)
      
      
      #if the demands is greater than the supply then there is no feasible solution , return fail 
      if(totaldemands > totalsupply){
        fail = matrix(values$input,nrow = 4,ncol =6,dimnames = list(plants,wares))
        values$optimal = -1
        return(fail)
      }
      
      #Initial tableau
      simple = matrix(0,nrow = 9,ncol=16,byrow = TRUE)
 
      v1 = c(-1,	0,	0, -1,	0,  0, -1,	0,	0, -1,  0,	0, -1,	0,	0,	supply[1])    # Supply Constraints
      v2 = c(0 , -1,  0,  0, -1,	0,  0, -1,	0,	0, -1,  0,	0, -1,	0,	supply[2])
      v3 = c(0 ,	0, -1,	0,	0, -1,	0,  0, -1,	0,	0, -1,  0,	0, -1,	supply[3])    # Demands constraints
      v4 = c(1, 	1,	1,  0,	0,	0,	0,	0,  0,	0,	0,	0,	0,  0,	0,	demands[1])       
      v5 = c(0 ,	0,	0,	1,  1,	1,	0,	0,	0,  0,	0,	0,	0,	0,  0,	demands[2])
      v6 = c(0 ,	0,	0,	0,	0,	0,	1,	1,	1,	0,	0,	0,	0,	0,	0,	demands[3])       
      v7 = c(0 ,	0,	0,	0,	0,	0,	0,	0,	0,	1,	1,	1,	0,	0,	0,	demands[4])
      v8 = c(0 ,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	1,	1,	demands[5])   
      v9 = c(cost[1:nrow(cost),1:ncol(cost)] ,0)  # LHS = 0
      
      # Gives all rows to simple
      simple[1,] = v1
      simple[2,] = v2
      simple[3,] = v3
      simple[4,] = v4
      simple[5,] = v5
      simple[6,] = v6
      simple[7,] = v7
      simple[8,] = v8
      simple[9,] = v9
    
      print(simple)
      
      # Transpose matrix and negate last row 
      simple = t(simple)
      simple[16,] = simple[16,]*-1


      # Adding slack variables
      final = matrix(0,nrow=16,ncol =25,byrow=TRUE)
      final = slacker(final,simple)

      # Using simplex to get the answers
      answer = simplex(final)
      
      solmat = answer$matrix
      
      # Update Otpimal Values
      values$optimal = answer$answer
      
      #Update Iterations
      values$iterations = answer$iterations
      iters = answer$iterations
      
      # Update Output Matrix
      pass = matrix(solmat[16,9:23],nrow =3,ncol=5,byrow = FALSE)
      values$output[1:3,1:5] = pass
      
      #Update Tableau
      values$tableaus = answer$outputs

      #Update tableau selection
      updateSelectInput(session,"choice", label = "Select Iteration to Display",choices = c("Hide", c(1:iters)))
      
      data = matrix(values$input,nrow=4,ncol=6)
      rownames(data) = plants
      colnames(data) = wares
      a = rhandsontable(data)
      return(a)
  })
  
  # Value box for the optimal cost
  output$optimalvalue = renderValueBox({
 
    # if there is a feasible solution 
    if(values$optimal > 0){
      final = valueBox(values$optimal,"Optimal Value: ",icon = icon("dollar-sign"),color = "blue",width = 4)  
    } else {
      final  = valueBox("Warning : ","There is no feasible solution",icon = icon("exclamation-triangle"),color = "red",width = 3)  
    }
    return(final)
  })
  
  # outputs the total number of iterations
  output$totaliterations = renderValueBox({
    
    if(values$iterations > 0 ){
      final = valueBox(values$iterations,"Total Iterations: ",icon = icon("clock"),color = "green",width = 4) 
    } else {
      final = valueBox("Warning : ","There is no feasible solution",icon = icon("exclamation-triangle"),color = "red",width = 3)  
    }
    
    return(final)
  })
  
  #Select which tableau to display
  output$tableaulist = renderTable({
    if(input$choice != "Hide"){
      cols = c()
      rows = c()
      choice = as.integer(input$choice)
      mat = as.matrix(values$tableaus[[choice]])
      return(mat)
    } else {
      return(NULL)
    }
  })
  
}