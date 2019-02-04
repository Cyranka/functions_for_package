

#Anatomy of a Shiny app - Video 1

library(shiny) #Loads the package

ui <- fluidPage() #User interface: controls the appearance

server <- function(input, output){} ##Contains the instructions

shinyApp(ui, server = server) #Creates the shiny app object



##
#The data is loaded before the app itself is loaded, so it can be used in both.


#Building an user interface - Video 2

#Role of the UI
#Defines the inputs, where users can make changes
#Controls how the output of the app is displayed to the end user

#Role of the server function
#This is where outputs are calculated. Moreover, functions and other calculations that are necessary
##are also controlled here

ui <- fluidPage() ##Creates a layout consisting of rows and columns

##Within the fluidPage() call, you define the layout
##sidebarLayout defines the layout (sidebar in this case)
##Sidebarpanel will receive the input controls
##selectInput:
## 1st) Argument: inputId (the input value that the app will internally use to access the value chosen by the user)
## 2nd) Argument: label (the display label that the user will see),
## 3rd) Argument: the choices that will be available to the user (this is a named vector)
                   ##Use a vector with names to specify labels for the choices
## 4th) Argument: the value that will be specified as a default from the list

##mainPanel: creates a main panel containing output elements that get created in the server function

ui <- fluidPage(
	sidebarLayout(
		sidebarPanel(
			selectInput(
				)
		),

	 #Output: Show scatterplot
	 mainPanel(

	  )
	)

)


##Adding new inputs
##Example: checkboxInput
##Imagine you would like to add a checkboxInput that contains an if condition (if checkbox is checked, then..)
### 1st step) ui: add an inuput widget that the user can check/uncheck the box

checkboxInput(
	inputId = "show_data",
	label = "Show data table",
	value = TRUE ##initial value (here the box is initially checked)
	)

### 2nd step) ui: add an output defining where the data table should appearance
##Within mainPanel
	DT::dataTableOutput(outputId = "moviestable")

### 3rd step): Add a reactive expression that creates the data table if the checkbox is checked
output$moviestable <- DT::renderDataTable({
	if(input$show_data){
		DT::datatable(data = movies %>% select(1:7),
			          options = list(pageLength = 10),
			          rownames = FALSE
		)
	}
	})


############################################################################################################
#Rendering functions
#renderTable: Table with numbers

#Steps
# 1. Add a table beneath the plot displaying summary statistics for a new variable
# 1st) Calculate thew new variable to start running the app
	movies <- movies %>%
			mutate(score_ratio = audience_score/critics_score)

# 2nd) Add an ui that the user can interact with to check boxes for selected title types
checkboxGroupInput(
	inputId = "selected_title_type",
	label = "Select title type:",
	choices = levels(movies$title_type), ##Directly using the information from the dataset
	selected = levels(movies$title_type) ##All of them
	)
# 3rd) Add an output in the ui defining where the summary table should appear
mainPanel(
	plotOutput(outputId = "scatterplot"),
	tableOutput(outputId = "summarytable") ##under the first one
	)
# 4th) In the server, add a reactive expression that creates the table
output$summarytable <- renderTable({
	movies %>%
	  	filter(title_type %in% input$selected_title_type) %>%
	  	group_by(mpaa_rating) %>%
	  	summarise(Mean = mean(score_ratio),
	  		      SD = sd(score_ratio),
	  	          n = n()), ##Calculate the table
	  },
	  striped = TRUE, spacing = "l", align = "lccr",digits = 4,
	  caption = "Score ratio (audience/critics'score) summary statistics by MPAA rating" 
	)

##Striped = do we want alternate color rows?
##spacing = do we want large row heights
##align = define alignment
##width = width of the table output


#Shiny has a variety of render* functions with corresponding *output functions to create and display outputs
#render functions can take multiple arguments, the first being the expression for the desired output
#The expression in the render function should be wrapped in curly braces


renderSomething(
{

	}, ##Create the expression
	caption = "Whatever")  ##Pass "prettying arguments after the curly brackets"


############################################################################################################
#UI outputs
#plotOutput

#Objective: select points on the plot via brushing, and report the selected points in a data table underneath the plot
#1. ui: Add functionality to plotOutput to select points via brushing
plotOutput(outputId = "scatterplot", brush = "plot_brush")

#2. ui: Add an output defining where the data table should appear
DT::dataTableOutput(outputId = "moviestable")

#3. server: add a reactive expression that creates the data table for selected points
output$moviestable <- DT::renderDataTable({
	brushedPoints(movies,input$plot_brush) %>%
	select(title, audience_score, critis_score)
	})
