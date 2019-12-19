radar <- function(Base2, rango){

  
 dimensions <- list()
  
 for(i in seq(2:dim(df)[2])){   
 dimensions[[i]] <-
    list(range = c(min(df[,i]),max(df[,i])),
         label = as.character(i), values = df[,i])
 }

p <- plot_ly(
    width = 1000, height = 600)
  for(i in seq(1:dim(df)[2])){
    p <-add_trace(p, type = 'parcoords',line = list(color = df[,5],
                            #colorscale = 'Jet',
                            showscale = TRUE),
                            #reversescale = TRUE),
                            #cmin = -4000,
                            #cmax = -100),
                dimensions = list(
                  list(range = c(~min(df[,i]),~max(df[,i])),
                       #constraintrange = c(100000,150000),
                       label = i, values = df[,i])))
  }
      
      
      
      
      %>%
      layout(
        title = "Mean of interest vars",
        polar = list(
          radialaxis = list(
            visible = T,range = rango)
        )
      )
  }
  assign("radarplot", p, environment<-.GlobalEnv)
  
}



dimensions = list(
  list(range = c(2,4.5),
       label = names(Base), values = ~sepal_width),
  list(range = c(4,8),
       constraintrange = c(5,6),
       label = 'Sepal Length', values = ~sepal_length),
  list(range = c(0,2.5),
       label = 'Petal Width', values = ~petal_width),
  list(range = c(1,7),
       label = 'Petal Length', values = ~petal_length)
)



df <- read.csv("https://raw.githubusercontent.com/bcdunbar/datasets/master/iris.csv")

p <- df %>%
  p <- plot_ly(type = 'parcoords',
               line = list(color = ~species_id,
                           colorscale = list(c(0,'red'),c(0.5,'green'),c(1,'blue'))),
               
               for(i in seq(1:dim(Base2)[1])){
                 p= add_trace(p,
                              r = unname(unlist(Base2[i,2:dim(Base2)[2]])),
                              theta = names(Base2)[2:dim(Base2)[2]],
                              name = Base2[i,1],
                              fill="toself"
                              
                              
                              
                              
                              dimensions = list(
                                list(range = c(2,4.5),
                                     label = 'Sepal Width', values = ~sepal_width),
                                list(range = c(4,8),
                                     constraintrange = c(5,6),
                                     label = 'Sepal Length', values = ~sepal_length),
                                list(range = c(0,2.5),
                                     label = 'Petal Width', values = ~petal_width),
                                list(range = c(1,7),
                                     label = 'Petal Length', values = ~petal_length)
                              )
                 )
                 
                 
                 