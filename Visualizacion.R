library(ggplot2)
library(plotly)
# Grafico de barras

qplot(mtcars$cyl, geom = 'bar',
      fill = I('blue'), colour=I('red'),
      xlab = 'Cylinders', ylab = 'Number of vehicles',
      main = 'Cylinders in mtcars')

#  Historgramas

qplot(mtcars$hp, geom = 'histogram',
      bandwidth= 25, colour=I('black'),
      xlab = 'Horsepower', ylab = 'Number of the cars',
      alpha=I(0), main = 'Horsepower')


#  Grafico de pastel o pie chart

bar<-ggplot(mtcars, aes(x=1, y=sort(mtcars$carb),
            fill = sort(mtcars$carb)))+
      geom_bar(stat = 'identity')+
     print(bar)
bar<- bar + coord_polar(theta='y')

bar<-bar + theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_blank())+
    labs(y='Carburators')
print(bar)


# Graficos de dispersión

qplot(mpg, wt, data=mtcars)

ggplot(mtcars, aes(x=mpg, y=wt))+geom_point(shape=19, colour='blue')

mtcars$cylFactor<-factor(mtcars$cyl)
ggplot(mtcars, aes(x=mpg, y=wt, shape=cylFactor))+
      geom_point()  


ggplot(mtcars, aes(x=mpg, y=wt, col=cyl))+geom_point(shape=19)

ggplot(mtcars, aes(x=mpg, y=wt, col=cylFactor))+
       geom_point(shape=19)+
       labs(colour='Cylinders')+
       xlab('Miles per Gallon')+
       ylab('Weight')+
       ggtitle('Scatterplot')

#  Regresión lineal 
stock<-as.data.frame(EuStockMarkets)
head(stock)

ggplot(stock, aes(x=c(1:nrow(stock)), y=DAX))+
       geom_line(size=1.5, colour='light blue')+
      labs(x='Time', y='Stocks')

all_stocks<-ggplot()+
    geom_line(data = stock, aes(x=c(1:nrow(stock)),y=DAX), size=1.5, colour='light blue')+
    geom_line(data = stock, aes(x=c(1:nrow(stock)),y=SMI), size=1.5, colour='red')+
    geom_line(data = stock, aes(x=c(1:nrow(stock)),y=CAC), size=1.5, colour='purple')+
    geom_line(data = stock, aes(x=c(1:nrow(stock)),y=FTSE), size=1.5, colour='green')+
    labs(x='Time', y='Stocks') 
print(all_stocks)

legend_stocks<-all_stocks +
               xlab('Days')+
               ylab('Price')+
               ggtitle('EU Stocks')
print(legend_stocks)               

ggplot(mtcars, aes(x=mpg, y=wt))+
       geom_point(shape=19)+
      geom_smooth(method = 'lm', se=FALSE, color='red')

#  Confidence level
ggplot(mtcars, aes(x=mpg, y=wt, col=cylFactor))+
  geom_point(shape=19)+
  geom_smooth(method = 'lm', se=TRUE, color='red')

ggplot(mtcars, aes(x=mpg, y=wt, col=cylFactor))+
  geom_point(shape=19)+
  geom_smooth(method = 'lm' colour ='red')

#  Linear Regression

ggplot(mtcars, aes(x=mpg, y=wt, col=cylFactor))+
  geom_point(shape=19)+
  geom_smooth(method = 'lm', se=TRUE, color='red')+
  xlab('Miles por Gallon')+
  ylab('Weight')+
  labs(colour='Cylinders')+
  ggtitle('Linear Regression')

#  Gaussian Regression

ggplot(mtcars, aes(x=mpg, y=wt, col=cylFactor))+
  geom_point(shape=19)+
  geom_smooth(method = 'auto', se=TRUE, color='blue')+
  xlab('Miles por Gallon')+
  ylab('Weight')+
  labs(colour='Cylinders')+
  ggtitle('Gaussian Regression')


#  Wordcloud
#  Primero se creamos la carpeta donde se va a guardo el archivo a analizar en esta caso
#  Es un .txt

dir.create('C:/Users/Admin/Desktop/Riesgo_Maestría/wordcloud')
download.file('http://ibm.box.com/shared/static/cmid70rpa7xe4ocitcga1bve7r0kqnia.txt',
              destfile = 'C:/Users/Admin/Desktop/Riesgo_Maestría/wordcloud/churchill_speeches.txt', quiet = TRUE)

#  Instalamos los siguientes paquetes
install.packages('tm')
library(tm)
install.packages('wordcloud')
library(wordcloud)
library(wordcloud2)

dirPath<-'C:/Users/Admin/Desktop/Riesgo_Maestría/wordcloud'
speech<-Corpus(DirSource(dirPath))
inspect(speech)
#  En este paso quitamos los guiones 
speech<-tm_map(speech, content_transformer(tolower)) 
#  En este paso removemos los números 
speech<-tm_map(speech, removeNumbers)
#  En esta parte quitamos los artículos 
speech<-tm_map(speech, removeWords, stopwords('english'))
#  Removemos puntuación
speech<-tm_map(speech, removeWords, c('floccinaucinihilipitification', 'squirrelled')) 
speech<-tm_map(speech, removePunctuation)
speech<-tm_map(speech, stripWhitespace)

#  Creamos el documentos de términos
dtm<-TermDocumentMatrix(speech)
#  Convetimos a matriz
m<-as.matrix(dtm)
# Acomodamos para que nos muestre las palabras más usadas
v<-sort(rowSums(m), decreasing = TRUE)
#  Lo tranformamos a data frame
d<-data.frame(word=names(v), freq=v)
head(d,10)

#  Generamos la nube de palabras sencilla
wordcloud(words = d$word, freq = d$freq)
#  Especificamos que realice la mínima frecuencia 
wordcloud(words = d$word, freq = d$freq, min.freq = 1)
#  Ahora limitamos al número de palabras deseadas
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words = 250)
#  Ahora le ponemos color 
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words = 250,
          colors = brewer.pal(8,'Dark2'))
#  Ahora quitamos el orden aleatorio
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words = 250,
          colors = brewer.pal(8,'Dark2'), random.order = FALSE)

#  Crear un gráfico de radar 
# Primero instalamos los paquetes siguientes
install_github("ricardo-bion/ggradar")
library(dplyr)
library(scales)
# Seleccionamos nuesto dataset
mtcars %>%
  # Atributos a variables
  add_rownames(var = 'group')%>%
  # Asignamos a cada variable para relacionarlas
  mutate_each(funs(rescale), -group) %>%
  # Sleccionamos los datos para graficas
  head(3) %>% select(1:10)->mtcars_radar
  

options(warn = -1)
ggradar(mtcars_radar)

#  Grafico de Waffle

library(ggplot2)
install.packages('waffle')
library(waffle)

expenses<-c('Health ($43,212)'=43212,
            'Education ($11,412)'=113412,
            'Transportation ($20,231)'=20231,
            'Entertainement ($28,145)'=28145)

waffle(expenses/1235, rows=5, size=0.3,
       colors='#c7d4b6','#a3aabd','#a0d0de','#97b5cf',
       title='Imaginary Household Expenses Each Year',
       xlab='l square = $934')
  
#  Garficos de cajas

set.seed(1234)
  
set_a<-rnorm(200, mean = 1, sd=0)
set_b<-rnorm(200, mean = 0, sd=1)
  
df<-data.frame(label=factor(rep(c('A','B'), each=200)),
                            value=c(set_a, set_b))
library(ggplot2)
library(plotly)  
library(rlang)  
ggplot(df, aes(x=label, y=value))+geom_boxplot()
ggplotly()
  
summary(mtcars)

cars<-ggplot(mtcars, aes(factor(cyl), mpg))
cars+geom_boxplot()


#  Elaboración de mapas en R
# Instalamos la libreria 
install.packages('leaflet')
library(leaflet)

map<-leaflet()
map<-leaflet()%>%addTiles()
map<-leaflet()%>%addTiles()%>%
  addMarkers(lng = -73.9851, lat = 40.7589)
map<-leaflet()%>%addTiles()%>%
  addMarkers(lng = -73.9851, lat = 40.7589, popup = 'Times Square')
map<-leaflet()%>%addProviderTiles('Stamen.Watercolor')%>%
  addMarkers(lng = 2.2945, lat = 48.8584, popup = 'Iffel Tower')
map


head(quakes)
map<-leaflet(quakes)%>% addTiles()%>%
    addCircleMarkers(lng=quakes$long, lat = quakes$lat)

library(IRdisplay)
map<-leaflet(quakes)%>% addTiles()%>%
     addMarkers(clusterOptions = markerClusterOptions())
     saveWidget(map, file='map.html', selfcontained=FALSE)
     display_html(paste('<iframe src=°', 'map.html',"'width='100%' 
                        height='300'",'/>'))
map<-leaflet(quakes)%>% addTiles()%>%
  addCircleMarkers(lng=86.92, lat = 27.99, popup='Mount Everest')%>%
  addRectangles(86.9, 27.95, 87,28.05)
  

     
     
     map

 
     
     
     
