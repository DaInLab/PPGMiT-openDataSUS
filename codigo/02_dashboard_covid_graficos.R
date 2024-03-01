# load obitos

df_dashboard <- read.csv('./dados/DASHBOARD.csv', sep=";") 

df_sexo <- aggregate(df_dashboard$CS_SEXO,
                     by=list(CS_SEXO=df_obitos$CS_SEXO), FUN=length) 

for (k in 1:nrow(df_sexo)) {
  if(df_sexo$CS_SEXO[k] == "M") df_sexo$label[k] <- "Masculino"
  if(df_sexo$CS_SEXO[k] == "F") df_sexo$label[k] <- "Feminino"
  if(df_sexo$CS_SEXO[k] == "I") df_sexo$label[k] <- "Ignorado"
}


df_sexo

# Grafico Sexo
install.packages("plotly")
library(plotly)

colors <- c("pink", "orange", "blue")

# set the marker properties, including the colors and line width
marker <- list(colors = colors)

# define the text and hover information to be displayed on the chart
textinfo <- "label+percent"
hoverinfo <- "text"


chart <- plot_ly(df_sexo, labels = ~label, values = ~x, type = "pie",
                 hole = 0.5, marker = marker,
                 text = ~paste(CS_SEXO, x, sep = ": "),
                 textinfo = textinfo, hoverinfo = hoverinfo)

# customize the layout of the chart, including the title and legend
layout(chart, 
       legend = list(x = 0, y = 1, font = list(width = 6, color = "black")))


# load library
library(ggplot2)

# Create test data.
data <- data.frame(
  category=c("A", "B", "C"),
  count=c(10, 60, 30)
)

# Compute percentages
data$fraction <- data$count / sum(data$count)

# Compute the cumulative percentages (top of each rectangle)
data$ymax <- cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n=-1))

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
data$label <- paste0(data$category, "\n value: ", data$count)

# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=category), size=6) + # x here controls label position (inner / outer)
  scale_fill_brewer(palette=3) +
  scale_color_brewer(palette=5) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")

library(ggplot2)
library(gridExtra)
data <- data.frame(
  Category = c("A", "B", "C", "D"),
  Value = c(20, 30, 40, 10)
)

pie_chart <- ggplot(data, aes(x = "", y = Value, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste(Category, round(Value/sum(Value) * 100), "%")), 
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Dark2") +
  theme_void()

donut_chart <- pie_chart + 
  geom_bar(data = data, aes(x = "", y = Value), stat = "identity", 
           width = 0.5, fill = "white") +
  geom_text(data = data, aes(label = paste(Category, round(Value/sum(Value) * 100), "%")), 
            position = position_stack(vjust = 0.5), color = "black") +
  theme_void()

donut_chart

donut_chart <- pie_chart + 
  geom_bar(data = data, aes(x = "", y = Value), stat = "identity", 
           width = 0.3, fill = "white") +
  geom_text(data = data, aes(label = paste(Category, round(Value/sum(Value) * 100), "%")), 
            position = position_stack(vjust = 0.5), color = "white") + # changed color to white
  theme_void()

grid.arrange(pie_chart, donut_chart, ncol = 2, widths = c(3, 3))


install.packages("plotly")
library(plotly)

# create a data frame with category labels and corresponding values
data <- data.frame(
  category = c("A", "B", "C", "D", "E"),
  value = c(20, 25, 15, 10, 30)
)

# define the colors for each category
colors <- c("red", "orange", "yellow", "green", "blue")

# set the marker properties, including the colors and line width
marker <- list(colors = colors)

# create the pie chart with a hole in the center
plot_ly(data, labels = ~category, values = ~value, type = "pie",
        hole = 0.5, marker = marker)


