library(plotly)

set.seed(12345)
Date <- seq(as.Date("2010/1/1"), as.Date("2014/1/1"), "week")
Date

Y <- rnorm(n=length(Date), mean=100, sd=1)
Y
df <- data.frame(Date, Y)

df$Year <- format(df$Date, "%Y")
df$Month <- format(df$Date, "%b")
df$Day <- format(df$Date, "%d")

df$MonthDay <- format(df$Date, "%d-%b")

df$CommonDate <- as.Date(paste0("2000-",format(df$Date, "%j")), "%Y-%j")
head(df)
ggplot(data = df,
       mapping = aes(x = CommonDate, y = Y, shape = Year, colour = Year)) +
  geom_point() +
  geom_line() +
  facet_grid(facets = Year ~ .) +
  scale_x_date(labels = function(x) format(x, "%d-%b"))

ggplotly()


### Tooltips
seq <- 1:10
name <- c(paste0("company",1:10))
value <- c(250,125,50,40,40,30,20,20,10,10)
d <- data.frame(seq,name,value)


require(ggplot2)
ggplot(data = d,aes(x = seq, 
                    y = value,
                    group = 1,
                    text = paste('name: ', name,
                                 '</br>name_again: ', name)
))+
  geom_line() + 
  geom_point()

require(plotly)
ggplotly(tooltip = c("x", "y", "text"))
