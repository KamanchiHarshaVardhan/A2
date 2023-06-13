library(tidyverse)
library(rvest)
library(imager)
#part a
data(iris)
species <- 0
for(i in 1:150)
{
  if(iris$Species[i]=='setosa'){
    species[i]=1}
  else if(iris$Species[i]=='versicolor'){
    species[i]=2}
  else if(iris$Species[i]=='virginica'){
    species[i]=3}
}
species
plot(iris$Sepal.Length, iris$Petal.Length,xlab='sepal length',ylab='petal length',pch=16,col=species,main='Scatterplot')

#part b
img <- load.image("dog.jpeg")
plot(img)
dim1 <- dim(img)[1]
dim2 <- dim(img)[2]
img <- as.array(img)
img[2,1,1,1]
plot(img)
flip <- function(img)
{
  for(i in 1:dim2)
  {
    for(j in 1:((dim1)/2))
    {
        t <- img[j,i,1,] 
        img[j,i,1,] <- img[dim1-j+1,i,1,]
        img[dim1-j+1,i,1,] <- t
    }
    print(i)
  }
  return(img)
}
plot(as.cimg(flip(img)))

#part c
library(MASS)
k <- 0
data("ships")
for(i in 1:40)
{
  if(ships$type[i]=='A')
    k[i]=1
  else if(ships$type[i]=='B')
    k[i]=2
  else
    k[i]=3
}
plot(ships$service,ships$incidents,xlab='service',ylab='incidents',col=k,pch=16,main='plot',type='o')
# I disprove because B has more services compared to others.

#part d
html <- read_html("https://stats.stackexchange.com/questions?tab=Votes")
Title <- html%>%html_nodes("#questions .s-link")%>%html_text()
No_ofviews <- 0
No_ofvotes <- 0
No_ofanswers <- 0
yes <- html%>%html_nodes(".s-post-summary--stats-item-number")%>%html_text()
for(i in 0:14)
{
  No_ofvotes <- append(No_ofvotes,yes[3*i+1])
  No_ofanswers <- append(No_ofanswers,yes[3*i+2])
  No_ofviews <- append(No_ofviews,yes[3*i+3])
}
No_ofanswers <- No_ofanswers[2:16]
No_ofviews <- No_ofviews[2:16]
No_ofvotes <- No_ofvotes[2:16]
data <- data.frame(Title,No_ofviews,No_ofanswers,No_ofvotes) 

#part e
yes <- 0
value <- 0
for(i in 1:500)
{
  q <- 1
  for(j in 1:100)
  {
    y <- sample(0:1,size=1,prob=c(1-q,q))
    q=q-(1/100)
    if(y==0)
      break
    else
      yes=yes+1
      
  }
  value[i] <- yes
  yes <- 0
}
value
mean(value)
