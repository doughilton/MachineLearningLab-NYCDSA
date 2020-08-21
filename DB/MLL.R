library(tidyverse)
library(lubridate)
library(ggplot2)
library(RColorBrewer)

orders = read_csv('Orders.csv')
returns = read_csv('Returns.csv')

str(orders)

#remove $ and , from Profit and Sales columns and convert dates to DateTime
clean_amount_str <- function( amount_str ){
  cleaned_str <- gsub( pattern = '[$,]', replacement = '', x = amount_str)
  as.numeric(cleaned_str)
}

clean_date_str <- function( date_str ){
  require( lubridate )
  parse_date_time( date_str, orders = '%m/%d/%y' )
}

#clean df with the functions
orders = orders %>% mutate_at( c('Profit', 'Sales'), clean_amount_str  ) %>% 
                    mutate_at(c('Order.Date', 'Ship.Date'), clean_date_str)

#Looking at seasonal trends in inventory
orders_cat = orders %>% 
                    group_by(Category, month = floor_date(Order.Date, 'month')) %>% 
                    summarise(items_sold = sum(Quantity))
orders_cat %>% 
  ggplot(aes(x = month, y = items_sold, color = Category)) +
              geom_line() +
              facet_wrap(~Category)

