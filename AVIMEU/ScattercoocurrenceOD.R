base <- fread("metrobikedep.txt")
library(parsedate)
Base <- filter(base, base$`Ending Station ID`==3030)
Base$`End Time` <- parse_date(Base$`End Time`)
Base$hour <- hour(Base$`End Time`)


dataResum <- Base%>%group_by(`Starting Station ID`, hour) %>% 
  summarise(Countrip = n())

dataResum <- filter(dataResum, hour !=0)

tickvals0 = seq(0, 345, 60)
hour = seq(1:24)
horas <- cbind(tickvals0,hour)

dataResum <- merge(dataResum,horas, by="hour")
dataResum$`Starting Station ID` <- as.character(dataResum$`Starting Station ID`)
library(plotly)



pal <- c("red", "blue", "green")
p <- plot_ly(data=dataResum,
             type = 'scatterpolar',
             r = ~Countrip,
             theta = ~tickvals0,
             size = ~Countrip,
             sizes = c(100, 300),
             mode = 'markers',
             color=~dataResum$`Starting Station ID`
) %>%
  layout(
    showlegend = TRUE,
    polar = list(
      angularaxis = list(
        showticklabels = TRUE,
        # remove grid lines and ticks
        showgrid = TRUE,
        ticks = '',
        # if you want the axis to go the other way around
        direction = 'clockwise',
        tickmode="array",
        tickvals = seq(0, 345, 15),
        ticktext = seq(1:24)
      ),
      radialaxis = list(
        tickmode="array"
        #tickvals = c(0, 1, 2, 3, 4, 5, 6, 7)
        #ticktext = c('', "One", "Two", "Three", "Four", "Five", "Six", "Seven")
      )
    )
  )
ggplotly(p)






pal <- c("red", "blue", "green")
p <- plot_ly(data=dataResum,
             type = 'scatterpolar',
             r = ~Countrip,
             theta = ~tickvals0,
             size = ~Countrip,
             sizes = c(100, 300),
             mode = 'markers',
             color=~dataResum$`Starting Station ID`,
             opacity = 1
) %>%
  layout(
    showlegend = FALSE,
    polar = list(
      angularaxis = list(
        showticklabels = TRUE,
        # remove grid lines and ticks
        showgrid = TRUE,
        ticks = '',
        # if you want the axis to go the other way around
        direction = 'clockwise',
        tickmode="array",
        tickvals = tickvals0,
        ticktext =seq(1:24)
      ),
      radialaxis = list(
        tickvals = tickvals0,
        ticktext = seq(1:24)
        #ticktext = c('', "One", "Two", "Three", "Four", "Five", "Six", "Seven")
      )
    )
  )
ggplotly(p)


})