## by Luis Urso 

## Following the TUTORIAL in the file: ggplot2_tutorial.pdf

## Install package GGPLOT2 in R Environment

install.packages("ggplot2")

## Call Library GGPLOT2 for usage.

library("ggplot2")

## Read dataset IRIS (this is a free dataset on promisse available on R environment)

data(iris)


## Basic Plot IRIS Dataset

IrisPlot <- ggplot(iris,aes(Sepal.Length, Petal.Length, colour=Species)) +
  geom_point() +
  ggtitle("Petals & Sepals")
print(IrisPlot)

## Changing Labels & Titles Programmatically

print(IrisPlot + labs(y="Petal Length(cm)", x="Sepal Length (cm)") +
  ggtitle("Petal & Sepal Length of Iris"))


## Add in chart legend / plot markers

IrisPlot + annotate("text",x=6,y=6,label="Sample of Text at 6,6") +

## Add rectangle in the chart 

IrisPlot + annotate("rect",xmin=5, xmax=7,ymin=4,ymax=6,alpha=.4)

## Add a Segment in the chart

IrisPlot + annotate("segment",x=5,xend=7,y=4,yend=5,colour="black")


