#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(GA)
library(lpSolve)


ui <- fluidPage(
    titlePanel("Trabalho de PO"),
    
    tabsetPanel(
        tabPanel("Dados" ,
                 fluidRow(
                     column(6,fileInput("arquivo", "Selecione o arquivo:",multiple = FALSE,accept = c(".csv"))),
                     column(6,actionButton("Carregar","Carregar dados"))
                 ),
                 fluidRow(
                     column(3,h3(textOutput("TQuantidade"))),
                     #column(3,h3(textOutput("VTotal"))),
                     #column(3,h3(textOutput("TVolumetotal"))),
                     #column(3,h3(textOutput("TValor")))
                 ),
                 #fluidRow(
                 #    column(12, tableOutput("Processamento"))
                 #),
                 hr(),
                 fluidRow(
                     column(12, tableOutput("Dados"))
                 )
        ),
        tabPanel("Otmizar",
                 fluidRow(
                     column(3,numericInput("investimento", "Informe o valor disponível para investir",value = 0)) ,
                     column(3,numericInput("volume", "Informe a capacidade do estoque",value = 0)),
                     #column(3,numericInput("iteracoes", "Informe a quantidade de Iterações",value = 0)),
                     column(3,actionButton("Otimizar","Otimizar") )
                 ),
                 fluidRow(
                     column(3,h3(textOutput("RFuncaoObj"))),
                     column(3,h3(textOutput("investimento"))),
                     #column(3,h3(textOutput("RValor")))
                     
                 ) ,
                 fluidRow(
                     column(12, tableOutput("DadosP"))
                     
                 )
        )
    )
)

server <- function(input, output) {
    
    observeEvent(input$Carregar, {
        
        file1 <- input$arquivo
        itens <<-  read.csv(file1$datapath, sep=";")
        z <<- nrow(itens)
        
        output$Dados <- renderTable(itens)
        output$TQuantidade = renderText({paste0("Quantidade de Itens: ",z)})
        #output$VTotal = renderText({paste0("Valor Total do estoque: ", sum(itens))})
        #output$TVolumetotal = renderText({ paste0("Volume Total: ", sum(itens$VOLUME ))  })
        #output$TValor = renderText({ paste0("Valor Total: ", sum(itens$VALOR ))  })
        
    })
    
    
    observeEvent(input$Otimizar, {
        maxvolume =  input$volume
        investimento = input$investimento
        
       
        output$DadosP <- renderTable(itens)
        output$RFuncaoObj = renderText({paste0("Resultado da função objetiva: ", z)})
        output$maxvolume = renderText({paste0("Quantidade de Itens: ",maxvolume)})
        output$investimento = renderText({paste0("Quantidade de Itens: ",investimento)})
         
        # Modelo
        funcao_obj <- c()
        restricoes <-matrix(c(2,1,
                              1,2,
                              1,3), ncol = 2, byrow = TRUE)
        
        restricoes_sinal   <-c("<=", "<=", "<=")
        restricoes_valores <-c(16, 11, 15)
        
        res_modelo <-lp("max",
                        funcao_obj,
                        restricoes,
                        restricoes_sinal,
                        restricoes_valores,
                        all.int = TRUE,
                        compute.sens=TRUE)
        
        res_modelo$solution
        
        options("scipen"=100, digits = 1)
        
        res_modelo        

    }) 
    
    
    observeEvent(input$SEProcessar, {
        maxvolume =  input$sobravolume 
        maxpeso = input$sobrapeso
        
        f <-function(x)
        {
            valor = 0
            peso = 0
            volume = 0
            
            for (i in 1:z)
            {
                
                if (x[ i ] != 0)
                {
                    
                    valor = valor + itens[i,3]
                    peso = peso +  itens[i,2]
                    volume = volume +  itens[i,4]
                    
                }
            }
            if ( volume > maxvolume | peso > maxpeso )
                valor = 0
            return(valor)
        }
        
        #algoritmo genetico
        resultado = ga("binary", fitness = f, nBits = z,popSize = 10, maxiter = input$iteracoes)
        result = t(as.data.frame( summary(resultado)$solution))
        
        result = itens[result[,1]==1,]
        
        output$Rfinal <- renderTable({result})
        
        output$RQuantidade = renderText({  paste0("Quantidade Final: ", nrow(result)  )})
        output$RPesototal = renderText({  paste0("Peso Final: ", sum(result$PESO ))  })
        output$RVolumetotal = renderText({  paste0("Volume Final: ", sum(result$VOLUME ))  })
        output$RValor = renderText({  paste0("Valor Total: ", sum(result$VALOR ))  })
        
    }) 
    
}


shinyApp(ui = ui, server = server)


