library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinythemes::shinytheme("superhero"),
    titlePanel("Regression"), 
    sidebarLayout(
        sidebarPanel(
            selectInput('var','Select Variable',c('carat','table','depth','x','y','z')),
            selectInput('degree','Select Degree',1:10)
            
        ),
        mainPanel(
            plotOutput('plot'),
            h3(textOutput('r2')),
            h3(textOutput('rmse'))
        )
    )
)

server <- function(input, output) {
    output$r2 = renderText({
        library(ggplot2)
        model = lm(reformulate(termlabels = input$var, response='price'),data=diamonds)
        r2 = summary(model)$r.squared
        r2_round = formatC(r2,digits = 2,format = 'f')
        paste('R-squared = ',r2_round)
    })        
    
    output$rmse = renderText({
        library(ggplot2)
        model = lm(reformulate(termlabels = input$var, response='price'),data=diamonds)
        rmse = sqrt(mean((predict(model) - diamonds$price)^2))
        rmse_round = formatC(rmse, digits = 2,format='f')
        paste('RMSE = ',rmse_round)
    })        
    
    output$plot = renderPlot({
        library(ggplot2)
        ggplot(data=diamonds, aes_string(x=input$var, y='price'))+
            geom_point(size=0.4)+
            geom_smooth(method='lm',formula = y~poly(x,as.integer(input$degree)), size=1.2,color='steelblue', se=F)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
