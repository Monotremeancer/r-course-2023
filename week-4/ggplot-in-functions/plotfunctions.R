require(tidyverse)
require(openxlsx)

weather_data <- read.xlsx("data-in/ottenby_2022_weather.xlsx", na.strings = "-")
weather_data$month <- format(as.Date(weather_data$date_time), "%m")
str(weather_data)

bat_data <- read.xlsx("data-in/ottenby_2022_tidy.xlsx", na.strings = "-")

# Used to create a nice ceiling round (up) to nearest multiple of accuracy
# Useful for calculating upper boundaries of axis
round_any <- function(x, accuracy, f=ceiling){f(x/ accuracy) * accuracy}

# Plot a boxplot on top of a violin plot to get an idea of data distribution
box_vio_plot <- function(data,param='', x, y){
  ggplot(data = data,aes(y=.data[[y]], x=.data[[x]])) +
    theme_classic()+
    geom_violin(fill='gray88')+
    geom_boxplot(width=0.04)+
    labs(x=param)+
    guides(x= guide_axis(n.dodge = 2))+
    theme(text = element_text(size = 12))
    #coord_cartesian(ylim = c(0, 1)) 
}


# A simple count histogram 
hist_plot<-function(data, x, title='', 
                    fill_col='gray88', 
                    border_col='black', 
                    xlabel=''){
  ggplot(data=data, aes(x=.data[[x]]))+
    theme_classic()+
    geom_histogram(stat = "count")+
    ggtitle(title)+
    ylab('Species')+
    xlab(xlabel)+
    theme(plot.title = element_text(hjust = 0.5), 
          text = element_text(size = 12))
}

scatterplot<-function(data,param='', x, y, title=''){
  ggplot(data = data,aes(y=.data[[y]], x=.data[[x]])) +
    theme_classic()+
    geom_point()+
    geom_smooth(method = 'loess')+
    labs(x=param, title = title)+
    theme(text = element_text(size = 12),plot.title = element_text(size=12))
}

box_vio_plot(data = weather_data, 
             param = "Month", 
             x = "month", 
             y = "air_temp_c")

mean_sd_se_plot(data = weather_data,
                x = "month",
                y = "air_temp_c")

hist_plot(data = bat_data, 
          x = "taxa_ok")

scatterplot(data = weather_data[0:200,], 
            x= "time", 
            y = "air_temp_c")
