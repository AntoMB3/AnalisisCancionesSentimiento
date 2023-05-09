library(syuzhet)
library(RColorBrewer)
library(wordcloud)
library(tm)

letra <- scan(file = "D:/Proyectos/AnalisisCancionesSentimiento/Tu Piel.txt",fileEncoding = "UTF-8" 
              ,what = character(),sep = "\n",allowEscapes = TRUE)

oraciones <- get_sentences(letra)
oraciones
oraciones[11]


#Obtener sentimientos
sentimientos_df <- get_nrc_sentiment(oraciones,language = "spanish")
summary(sentimientos_df)

barplot(
  colSums(prop.table(sentimientos_df[, 1:8])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.7,
  col = brewer.pal(n = 8, name = "Set3"),
  main = "Mientes tan bien sin bandera",
  sub = "Analisis de letras",
  xlab="emociones", ylab = NULL)

oraciones_felicidad <- oraciones[sentimientos_df$fear> 0]
oraciones_felicidad
oraciones_felicidad<- sort(table(unlist(oraciones_felicidad)), decreasing = TRUE)
head(oraciones_felicidad)
