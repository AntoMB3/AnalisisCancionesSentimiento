library(syuzhet)
library(RColorBrewer)
library(wordcloud)
library(tm)
library(reticulate)
library(hash)
library(jsonlite)
#setwd("D:/Proyectos/AnalisisCancionesSentimiento")
use_virtualenv("venv/")
source_python("Scrapping.py")


library(shiny)


cargar_diccionario <- function(){
  out <- tryCatch(
    {
      palabras <- fromJSON("diccionario.json")
      print("Diccionario cargado")
      return (palabras)
    }, error = function(e){
      palabras <- list()
      return (palabras)
      print("Diccionario creado")
    }
  )
  
  return (out)
}
diccionario <- cargar_diccionario()
conectores_adversos <- c("pero","niego","No","a pesar de","al contrario","excepto","salvo","contra","sin","falso")
conectores_aumentan <- c("Muy","Demasiado","Sumamente","bastante","mucho","más","Tan")
advers_connectors <- c("not", "dont", "never", "no", "without", "unless", "except", "but", "however", "yet", "although", "despite", "in spite of", "even though", "while", "whereas")
increasing_connectors <- c("a lot", "much", "very", "extremely", "highly", "greatly", "significantly", "tremendously", "exceedingly", "intensely", "immensely", "enormously", "vastly", "remarkably", "exceptionally", "considerably")


ui <- fluidPage(
  
  titlePanel("Análisis de sentimientos de letras de canciones"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput("cancion", "Seleccionar archivo txt con la letra de tu cancion",
                accept = c(".txt")
                ),
      selectInput("idioma",label = "Selecciona el idioma",
                  choices = list("Español" = "spanish", "Inglés"="english"),
                  selected = "None"),
      actionButton("botonaccion","Generar análisis",class = "btn btn-primary"),
      
      selectInput("emocion",label ="Seleccionar las frases: ",
                  choices = list("Enojo"="anger","Esperado"="anticipation","Disgusto"="disgust","Miedo"="fear",
                                 "Alegría"="joy","Tristeza"="sadness","Sorpresa"="surprise","Confianza"="trust",
                                 "Paz"="peace","Placer"="pleasure","Frases negativas"="negative","Frases positivas"="positive")),
      actionButton("botonemocion","Obtener Frases",class = "btn btn-secundary"),
      
    ),
    mainPanel(
      tags$h1("Gráfica de resultados"),
      plotOutput("resultados",height = "800px"),
      tags$h2("Lyrics:"),
      textOutput("prueba_letra"),
      tags$h2("Frases seleccionadas"),
      textOutput("Emociones")
      
      
    )
    
  )
  
)

server <- function(input, output, session) {
  sentimientos_sin_duplicadosG <- reactiveVal("")
  oracionesG <- reactiveVal("")
  
  observeEvent(input$botonaccion,{

    
    letra <- scan(file = input$cancion$datapath,fileEncoding = "UTF-8" 
                           ,what = character(),sep = "\n",allowEscapes = TRUE)
    
    oraciones <- get_sentences(letra)
    sentimientos_df <- get_nrc_sentiment(oraciones,language = input$idioma)
    sentimientos_pre <- sentimientos_df[,]
    sentimientos_pre$peace <- rep(0,length(sentimientos_df[,1]))
    sentimientos_pre$pleasure <- rep(0,length(sentimientos_df[,1]))
    
    mejorar_resultados <- function(sentimientos_pre){
      
      for(i in 1:nrow(sentimientos_pre)){
        
        palabras_en_frase <- get_tokens(oraciones[i])
        
        for(w in 1:length(palabras_en_frase)){
          sentimiento <- get_nrc_sentiment(palabras_en_frase[w], language = input$idioma)
          palabra_actual <- palabras_en_frase[w]
          if (sum(sentimiento) == 0 && nchar(palabras_en_frase[w]) > 3 && input$idioma == "spanish"){
            
            if(palabra_actual %in% names(diccionario)){
              palabra_actual <- diccionario[[palabra_actual]]
            }
            else{
              
              palabra_nueva <- BuscarPalabra(palabras_en_frase[w])
              if(!is.null(palabra_nueva)){
                diccionario[[palabra_actual]] <- palabra_nueva
                diccionario[[palabra_nueva]] <- palabra_nueva
                palabra_actual <- palabra_nueva
              }
              else{
                diccionario[[palabra_actual]] <- palabra_actual
              }
              
              
            }
            
            nuevo_sentimiento <- get_nrc_sentiment(palabra_actual,language = input$idioma)
            sentimientos_pre[i,] = sentimientos_pre[i,1:10] + nuevo_sentimiento
          }
        }
        
        
        for(h in 1:length(conectores_adversos)){
          con <- conectores_adversos[h]
          if(grepl(paste0("^(",con,"|\\b",con,"\\b)"),oraciones[i],ignore.case = TRUE)){
            bandera = FALSE
            
            valor <- sentimientos_pre[i,1]
            if(valor == 0){bandera <- TRUE}
            sentimientos_pre[i,1] <- sentimientos_pre[i,1] - valor
            sentimientos_pre[i,11] <- sentimientos_pre[i,11] + valor
            if(bandera){
              valor <- sentimientos_pre[i,11]
              sentimientos_pre[i,11] <- sentimientos_pre[i,11] - valor
              sentimientos_pre[i,1] <- sentimientos_pre[i,1] + valor
              bandera <- FALSE
            }
            
            valor <- sentimientos_pre[i,3]
            if(valor == 0){bandera <- TRUE}
            sentimientos_pre[i,3] <- sentimientos_pre[i,3] - valor
            sentimientos_pre[i,12] <- sentimientos_pre[i,12] + valor
            if(bandera){
              valor <- sentimientos_pre[i,12]
              sentimientos_pre[i,12] <- sentimientos_pre[i,12] - valor
              sentimientos_pre[i,3] <- sentimientos_pre[i,3] + valor
              bandera <- FALSE
            }
            
            valor <- sentimientos_pre[i,4]
            if(valor == 0){bandera <- TRUE}
            sentimientos_pre[i,4] <- sentimientos_pre[i,4] - valor
            sentimientos_pre[i,8] <- sentimientos_pre[i,8] + valor
            if(bandera){
              valor <- sentimientos_pre[i,8]
              sentimientos_pre[i,8] <- sentimientos_pre[i,8] - valor
              sentimientos_pre[i,4] <- sentimientos_pre[i,4] + valor
              bandera <- FALSE
            }
            
            valor <- sentimientos_pre[i,5]
            if(valor == 0){bandera <- TRUE}
            sentimientos_pre[i,5] <- sentimientos_pre[i,5] - valor
            sentimientos_pre[i,6] <- sentimientos_pre[i,6] + valor
            if(bandera){
              valor <- sentimientos_pre[i,6]
              sentimientos_pre[i,6] <- sentimientos_pre[i,6] - valor
              sentimientos_pre[i,5] <- sentimientos_pre[i,5] + valor
              bandera <- FALSE
            }
            
            valor <- sentimientos_pre[i,9]
            if(valor == 0){bandera <- TRUE}
            sentimientos_pre[i,9] <- sentimientos_pre[i,9] - valor
            sentimientos_pre[i,10] <- sentimientos_pre[i,10] + valor
            
            if(bandera){
              valor <- sentimientos_pre[i,10]
              sentimientos_pre[i,10] <- sentimientos_pre[i,10] - valor
              sentimientos_pre[i,9] <- sentimientos_pre[i,9] + valor
              bandera <- FALSE
            }
            
          }
        }
        
        for(h in 1:length(conectores_aumentan)){
          con <- conectores_aumentan[h]
          if(grepl(paste0("^(",con,"|\\b",con,"\\b)"),oraciones[i],ignore.case = TRUE)){
            for(k in 1:ncol(sentimientos_pre)){
              sentimientos_pre[i,k] <- sentimientos_pre[i,k]*2
            }
          }
        }
        
      }
      dictJson <- toJSON(diccionario)
      write(dictJson, file = "diccionario.json")
      return (sentimientos_pre)
    }
    mejorar_duplicados <- function(sentimientos_mejorado){
      
      for(i in 1:(nrow(sentimientos_mejorado)-1)){
        for(j in i+1:nrow(sentimientos_mejorado)){
          if(!is.na(oraciones[i]) && !is.na(oraciones[j]) && oraciones[i] == oraciones[j]){
            valor1 <- sum(sentimientos_mejorado[i,])
            valor2 <- sum(sentimientos_mejorado[j,])
            
            if(valor1 == valor2){
              break
            }
            else if(valor1 < valor2){
              sentimientos_mejorado <- subset(sentimientos_mejorado, rownames(sentimientos_mejorado) != j)
            }
            else{
              sentimientos_mejorado <- subset(sentimientos_mejorado, rownames(sentimientos_mejorado) != i)
            }
            
          }
        }
      }
      return(sentimientos_mejorado)
    }
    improve_results <- function(sentimientos_pre){
      
      for(i in 1:nrow(sentimientos_pre)){
        
        for(h in 1:length(advers_connectors)){
          con <- advers_connectors[h]
          if(grepl(paste0("^(",con,"|\\b",con,"\\b)"),oraciones[i],ignore.case = TRUE)){
            print(con)
            bandera = FALSE
            
            valor <- sentimientos_pre[i,1]
            if(valor == 0){bandera <- TRUE}
            sentimientos_pre[i,1] <- sentimientos_pre[i,1] - valor
            sentimientos_pre[i,11] <- sentimientos_pre[i,11] + valor
            if(bandera){
              valor <- sentimientos_pre[i,11]
              sentimientos_pre[i,11] <- sentimientos_pre[i,11] - valor
              sentimientos_pre[i,1] <- sentimientos_pre[i,1] + valor
              bandera <- FALSE
            }
            
            valor <- sentimientos_pre[i,3]
            if(valor == 0){bandera <- TRUE}
            sentimientos_pre[i,3] <- sentimientos_pre[i,3] - valor
            sentimientos_pre[i,12] <- sentimientos_pre[i,12] + valor
            if(bandera){
              valor <- sentimientos_pre[i,12]
              sentimientos_pre[i,12] <- sentimientos_pre[i,12] - valor
              sentimientos_pre[i,3] <- sentimientos_pre[i,3] + valor
              bandera <- FALSE
            }
            
            valor <- sentimientos_pre[i,4]
            if(valor == 0){bandera <- TRUE}
            sentimientos_pre[i,4] <- sentimientos_pre[i,4] - valor
            sentimientos_pre[i,8] <- sentimientos_pre[i,8] + valor
            if(bandera){
              valor <- sentimientos_pre[i,8]
              sentimientos_pre[i,8] <- sentimientos_pre[i,8] - valor
              sentimientos_pre[i,4] <- sentimientos_pre[i,4] + valor
              bandera <- FALSE
            }
            
            valor <- sentimientos_pre[i,5]
            if(valor == 0){bandera <- TRUE}
            sentimientos_pre[i,5] <- sentimientos_pre[i,5] - valor
            sentimientos_pre[i,6] <- sentimientos_pre[i,6] + valor
            if(bandera){
              valor <- sentimientos_pre[i,6]
              sentimientos_pre[i,6] <- sentimientos_pre[i,6] - valor
              sentimientos_pre[i,5] <- sentimientos_pre[i,5] + valor
              bandera <- FALSE
            }
            
            valor <- sentimientos_pre[i,9]
            if(valor == 0){bandera <- TRUE}
            sentimientos_pre[i,9] <- sentimientos_pre[i,9] - valor
            sentimientos_pre[i,10] <- sentimientos_pre[i,10] + valor
            
            if(bandera){
              valor <- sentimientos_pre[i,10]
              sentimientos_pre[i,10] <- sentimientos_pre[i,10] - valor
              sentimientos_pre[i,9] <- sentimientos_pre[i,9] + valor
              bandera <- FALSE
            }
            
          }
        }
        
        for(h in 1:length(increasing_connectors)){
          con <- increasing_connectors[h]
          if(grepl(paste0("^(",con,"|\\b",con,"\\b)"),oraciones[i],ignore.case = TRUE)){
            for(k in 1:ncol(sentimientos_pre)){
              sentimientos_pre[i,k] <- sentimientos_pre[i,k]*2
            }
          }
        }
      }
      
      return (sentimientos_pre)
    }
    
    if(input$idioma == "spanish"){
    
    sentimientos_mejorado <- mejorar_resultados(sentimientos_pre)
    sentimientos_sin_duplicados <- mejorar_duplicados(sentimientos_mejorado)
    }
    else{
      print("Hola")
      sentimientos_sin_duplicados <- improve_results(sentimientos_pre)
    }
    
    nombre_cancion <- input$cancion$name
    nombre_cancion <- gsub("\\.txt$","",nombre_cancion)
    
    output$resultados <- renderPlot(barplot(
      colSums(prop.table(sentimientos_sin_duplicados[,])),
      space = 0.2,
      horiz = FALSE,
      las = 1,
      cex.names = 0.7,
      col = brewer.pal(n = 8, name = "Set3"),
      main = nombre_cancion,
      sub = "Analisis de letras",
      xlab="emociones", ylab = NULL))
    
    sentimientos_sin_duplicadosG(sentimientos_sin_duplicados)
    oracionesG(oraciones)
    output$prueba_letra <- renderText(letra)
    
    
  })
  
  observeEvent(input$botonemocion, {
    emocion <- input$emocion
    elementos <- which(sentimientos_sin_duplicadosG()[,emocion]>0)
    print(oracionesG()[6])
    inicial <-""
    for(i in elementos){
      inicial <- paste(inicial,"\n","-",oracionesG()[i],sep = "\n")
    }
    output$Emociones <- renderText(inicial)
  })
  
}

shinyApp(ui, server)
