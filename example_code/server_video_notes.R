##The server function

#The server function controls the relationship between inputs and outputs

##Server function example

server <- function(input, output){ ##Only takes the argument

	##Create scatterplot boject

	output$scatterplot <- renderPlot({ ##Specifies how it should be updated
		                               ##Notice how the input comes from the parameters in the ui
		ggplot(data = movies,aes_string(x = input$x,y = input$y)) + 
		       geom_point()
	})
}


##Rules of server functions
# 1. Save objects to display to output$xx
# 2. Build objects to display with render*() (renderPlot for instance)
# 3. Use input values with input$xx

#Reactivity

#Shiny automatically updates objects, such as plots, when the inputs change