# server.R

source("external/serverHead.R",local=TRUE)
# shinyServer(function(input, output) source("external/app.R", local = TRUE))

shinyServer(function(input, output, session) {
    
#     dat <- reactive({
#         if(!is.null(input$dat.name)){
#             if(input$dat.name=="vowel.train"){
#                 dat <- vowel.train
#             } 
#             
#             if(input$dat.name=="mtcars"){
#                 dat <- mtcars
#             }
#             
#             if(input$dat.name=="Orange"){
#                 dat <- Orange
#             }
#             else dat <- NULL
#         } else dat <- NULL
#         dat
#     })
#     
#     outcome <- reactive({
#         if(!is.null(input$dat.name)){
#             if(input$dat.name=="vowel.train"){
#                 outcome <- "y"
#             } 
#             
#             if(input$dat.name=="mtcars"){
#                 outcome <- "mpg"
#             }
#             
#             if(input$dat.name=="Orange"){
#                 outcome <- "circumference"
#             }
#             else outcome <- NULL
#         } else outcome <- NULL
#         outcome
#     })
#     
#     rf1 <- reactive({
#         rf<-randomForest(as.formula(paste(outcome(),"~",".")),data=dat())
#         vm<-varImp(rf)
#         vm$names <- row.names(vm)
#         rf1<-vm[order(-vm$Overall),]
#         rf1
#     })
    
    plot.df<-reactive({
        if (input$dat.name == "vowel.train - Vowel Recognition (Deterding data)") {
            # vowel.train
            rf<-randomForest(y~.,data=vowel.train)
            vm<-varImp(rf)
            vm$names <- row.names(vm)
            rf1<-vm[order(-vm$Overall),]
            resp_var<-"y"
        }
        
        if (input$dat.name == "imports85 - The Automobile Data") {
            # imports85
            rf<-randomForest(numOfDoors~.,data=imports85)
            vm<-varImp(rf)
            vm$names <- row.names(vm)
            rf1<-vm[order(-vm$Overall),]
            resp_var<-"numOfDoors"
        }
        
        if (input$dat.name == "iris3 - Edgar Anderson's Iris Data") {
            # iris3
            rf<-randomForest(Species~.,data=iris)
            vm<-varImp(rf)
            vm$names <- row.names(vm)
            rf1<-vm[order(-vm$Overall),]
            resp_var<-"Species"
        }
        
        # re-order by variable Overall
        # the variables are re-orderd in the order of the win
        rf1$names <- reorder( rf1$names,  rf1$Overall)
        plot_title<-paste("Variable Importance for Response Variable '",resp_var,"' Using the Random Forest",sep="")
        
        
        g <- ggplot(data=rf1,aes(x=names, y=Overall, fill=names)) +
            geom_bar(aes(x=names),data=rf1, stat="identity") + 
            coord_flip() + xlab("Variable Names") + ylab("Importance") +
            guides(fill=FALSE) + 
            ggtitle(plot_title)
        
        print(g)
        
    }
    )

    
    output$plot <- renderPlot({
       plot.df()
    },height = 600, width = 800)
    
})