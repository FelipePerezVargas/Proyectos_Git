#############################################
###                                       ###
###    Data Visualizatión Using ggplot2   ###
###                                       ###
#############################################

library(dslabs)
data(murders)
head(murders)

# Función unique y length()
data(heights)
x <- heights$height
unique(x)
length(unique(x))

# Tabla de frecuencias

data(heights)
x <- heights$height
table(x)
tab<-table(x)

# Sumar todos los que tengan valor de 1

data(heights)
tab <- table(heights$height)
sum(tab==1)

head(heights)


prop.table(table(heights$sex))
prop.table(table(heights$height)) #Solo categoricas, este no jala 

hist(heights$height)
plot(heights)

d<-density(heights$height)
plot(d)
polygon(d, col="red", border="blue")

# Porcentajes de alturas
male <- heights$height[heights$sex=="Male"]
female <- heights$height[heights$sex=="Female"]

female_percentiles <- quantile(female, seq(0.1, 0.9, 0.2))
male_percentiles <- quantile(male, seq(0.1, 0.9, 0.2))

df <- data.frame(female = female_percentiles, male = male_percentiles)
df





# Selección de sexo con conteo
male <- heights$height[heights$sex=="Male"]
female <- heights$height[heights$sex=="Female"]
length(male)
length(female)
print(paste('The numbre of Male is',length(male), 'and number of Female is', length(female)))

#  Construcción de una función de distribución acumulada

a <- seq(min(my_data), max(my_data), length = 100)    # define range of values spanning the dataset
cdf_function <- function(x) {    # computes prob. for a single value
  mean(my_data <= x)
}
cdf_values <- sapply(a, cdf_function)
plot(a, cdf_values)











