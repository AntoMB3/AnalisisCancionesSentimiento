library(syuzhet)
library(RColorBrewer)
library(wordcloud)
library(tm)

letra <- scan(file = "D:/Proyectos/AnalisisCancionesSentimiento/Amorfoda.txt",fileEncoding = "UTF-8" 
              ,what = character(),sep = "\n",allowEscapes = TRUE)

oraciones <- get_sentences(letra)
oraciones
oraciones[11]


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

conectores_adversos <- c(" pero "," niego "," a pesar de "," al contrario "," aunque "," excepto "," salvo "," contra "," sin "," falso ")
conectores_aumentan <- c(" Muy "," Demasiado "," Sumamente "," bastante "," mucho "," más "," Tan ")

mejorar_resultados <- function(dataframe_sentimientos){
  
  for(i in 1:nrow(sentimientos_pre)){
    have_feelings <- sum(sentimientos_pre[i-1,])
    
    for(h in 1:length(conectores_adversos)){
    
    if(grepl(conectores_adversos[h],oraciones[i],ignore.case = TRUE)){
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
      if(grepl(h,oraciones[i])){
        for(k in 1:ncol(sentimientos_pre)){
          sentimientos_pre[i,k] <- sentimientos_pre[i,k]*2
        }
      }
    }
    
    for (j in 1:ncol(sentimientos_pre)){
      
      if(i != 1 & i!= max(nrow(sentimientos_pre)) ){
        #print(paste(round((sentimientos_pre[i,j]+sentimientos_pre[i+1,j]+sentimientos_pre[i-1,j])/3,0),columnas[j]))
      #sentimientos_pre[i,j] <- (sentimientos_pre[i,j]+sentimientos_pre[i+1,j]+sentimientos_pre[i-1,j])/3
      }
    }

  }
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


indices <- which(apply(sentimientos_sin_duplicados[sentimientos_sin_duplicados$joy> 0,],1,function(x) x["joy"]>0))
indices <-names(indices)
indices <- as.integer(indices)
oraciones[indices]
indices
