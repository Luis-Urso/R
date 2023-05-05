library(plotly)

## Basic Line Plot

dat1 <- data.frame(
  sex = factor(c("Female","Female","Male","Male")),
  time = factor(c("Lunch","Dinner","Lunch","Dinner"), levels=c("Lunch","Dinner")),
  total_bill = c(13.53, 16.81, 16.24, 17.42)
)

p <- ggplot(data=dat1, aes(x=time, y=total_bill, group=sex)) +
  geom_line() +
  geom_point()

fig <- ggplotly(p)

fig


## Adding Points

# Map sex to different point shape, and use larger points
p <- ggplot(data=dat1, aes(x=time, y=total_bill, group=sex, shape=sex)) +
  geom_line() +
  geom_point()

fig <- ggplotly(p)

fig


## Styles and Themes

p <- ggplot(data=dat1, aes(x=time, y=total_bill, group=sex, shape=sex, colour=sex)) +
  geom_line(aes(linetype=sex), size=1) +     # Set linetype by sex
  geom_point(size=5) +         # Use larger points, fill with white
  scale_colour_hue(name="Sex",      # Set legend title
                   l=30)  +                  # Use darker colors (lightness=30)
  scale_shape_manual(name="Sex",
                     values=c(22,21)) +      # Use points with a fill color
  scale_linetype_discrete(name="Sex") +
  xlab("Time of day") + ylab("Total bill") + # Set axis labels
  ggtitle("Average bill for 2 people") +     # Set title
  theme_bw()

fig <- ggplotly(p)

fig


## Continuous 

datn <- read.table(header=TRUE, text='
supp dose length
  OJ  0.5  13.23
  OJ  1.0  22.70
  OJ  2.0  26.06
  VC  0.5   7.98
  VC  1.0  16.77
  VC  2.0  26.14
')

p <- ggplot(data=datn, aes(x=dose, y=length, group=supp, colour=supp)) +
  geom_line() +
  geom_point()

fig <- ggplotly(p)

fig
