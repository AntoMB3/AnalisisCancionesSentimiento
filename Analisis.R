library(syuzhet)
library(RColorBrewer)
library(wordcloud)
library(tm)
library(reticulate)
library(hash)
library(jsonlite)
setwd("D:/Proyectos/AnalisisCancionesSentimiento")
use_virtualenv("venv/")
source_python("Scrapping.py")

#letra <- scan(file = "D:/Proyectos/AnalisisCancionesSentimiento/Tu mirada.txt",fileEncoding = "UTF-8" 
#              ,what = character(),sep = "\n",allowEscapes = TRUE)

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

oraciones <- get_sentences(letra)



#Obtener sentimientos
sentimientos_df <- get_nrc_sentiment(oraciones,language = "spanish")
summary(sentimientos_df)

sentimientos_pre <- sentimientos_df[,]
sentimientos_pre$peace <- rep(0,length(sentimientos_df[,1]))
sentimientos_pre$pleasure <- rep(0,length(sentimientos_df[,1]))



oraciones_felicidad <- oraciones[sentimientos_pre$positive> 0]
oraciones_felicidad
oraciones_felicidad<- sort(table(unlist(oraciones_felicidad)), decreasing = TRUE)
head(oraciones_felicidad)

#Posibles c("aunque",)
conectores_adversos <- c("pero","niego","No","a pesar de","al contrario","excepto","salvo","contra","sin","falso")
conectores_aumentan <- c("Muy","Demasiado","Sumamente","bastante","mucho","más","Tan")

mejorar_resultados <- function(sentimientos_pre){
  
  for(i in 1:nrow(sentimientos_pre)){
    
    palabras_en_frase <- get_tokens(oraciones[i])
    
    for(w in 1:length(palabras_en_frase)){
      sentimiento <- get_nrc_sentiment(palabras_en_frase[w], language = "spanish")
      palabra_actual <- palabras_en_frase[w]
      if (sum(sentimiento) == 0 && nchar(palabras_en_frase[w]) > 3){
        
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
        
        nuevo_sentimiento <- get_nrc_sentiment(palabra_actual,language = "spanish")
        sentimientos_pre[i,] = sentimientos_pre[i,1:10] + nuevo_sentimiento
      }
    }
    
    
    for(h in 1:length(conectores_adversos)){
    con <- conectores_adversos[h]
    if(grepl(paste0("^(",con,"|\\b",con,"\\b)"),oraciones[i],ignore.case = TRUE)){
      print(oraciones[i])
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
  #write(dictJson, file = "D:/Proyectos/AnalisisCancionesSentimiento/diccionario.json")
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

sentimientos_mejorado <- mejorar_resultados(sentimientos_pre)
sentimientos_sin_duplicados <- mejorar_duplicados(sentimientos_mejorado)

barplot(
  colSums(prop.table(sentimientos_sin_duplicados[,])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.7,
  col = brewer.pal(n = 8, name = "Set3"),
  main = "Mientes tan bien sin bandera",
  sub = "Analisis de letras",
  xlab="emociones", ylab = NULL)


indices <- which(apply(sentimientos_sin_duplicados[sentimientos_sin_duplicados$peace> 0,],1,function(x) x["peace"]>0))
indices <-names(indices)
indices <- as.integer(indices)


oraciones[indices]
indices



################################################
install.packages("reticulate")
library(reticulate)
setwd("D:/Proyectos/AnalisisCancionesSentimiento")
use_virtualenv("venv/")
source_python("Scrapping.py")

print(BuscarPalabra("Perdí"))

palabra <- "gfdgfdgd"

sentimiento <- get_nrc_sentiment(palabra, language = "spanish")
if (sum(sentimiento) == 0 && nchar(palabra) > 3){
  nuevo_sentimiento <- get_nrc_sentiment(BuscarPalabra(palabra),language = "spanish")
  print(nuevo_sentimiento)
}

print(BuscarPalabra("gfsdgfdgd")) #Regresa un NULL
