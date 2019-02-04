##Reactive flow

##Example
#Set alpha level

sliderInput(
	inputId = "alpha",
	label = "Alpha: ",
	min = 0, 
	max = 1,
	value = 0.5
)

#input$alpha is the input. This is the variable that "carries" the value of this input
#Reactivity 101: Reactivity automatically occurs when an input value is used to render an output object

