library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(patchwork)

df=read.csv("Tesla.csv")

 #MAIN FIGURE
 df$Date=as.Date(df$Date, format = "%m/%d/%Y") #Change the data type to date.
 df$vol_scaled=df$Volume/10^5 #Volume was scaled to fit with the graph.
 max_date <- df$Date[which(df$High == max(df$High))] #We find the date when the Tesla stock price was at its highest.
 df$ROC= ((diff(df$Close))/df$Close)*100 #Rate of change: (https://www.investopedia.com/terms/r/rateofchange.asp )
Warning message:
In (diff(df$Close))/df$Close :
  longer object length is not a multiple of shorter object length
 df$ROC_Dir= ifelse(df$ROC > 0,"#2ab04d", "#e06565") #ifelse function to assign green color to positive rate of change and red to negative rate of change.
 
 
 palette <- c("High Price"="#cc0000","Low Price"="#212121")#Tesla Motors Palette: https://www.color-hex.com/color-palette/42354; 
 
 global <- ggplot(df, aes(x=Date)) +
     geom_bar(aes(y=vol_scaled), stat="identity", alpha=0.5, colour="#d9d9d9")+  #Bar plot of the amount of stocks traded. 
     geom_line(aes(y=High, colour="High Price")) + #Line plot of the higher price.
     geom_line(aes(y=Low, colour="Low Price"), alpha=0.6) + #Line plot of the lowest price.
     geom_bar(aes(y=ROC), color=df$ROC_Dir, stat="identity", alpha=0.5)+ #Bar plot of the price rate of change
     scale_color_manual(values = palette, name="Stock Price")+ #We manually add the colors in order to have the legend of the lines. 
     scale_x_date(date_breaks = "1 year", date_labels = "%Y")+ #x-axis parametrization.
     scale_y_continuous(limits = c(-25, 400), breaks = seq(0, 400, by = 50))+ #y-axis setting.
     theme(panel.background = element_rect(fill="#f2f2f2"), #Background setting.
           panel.grid.major = element_blank(), #Grid format
           panel.grid.minor = element_blank(),
           plot.title= element_text(size=18,color='#212121',face='bold'), #Title setting.
           axis.title.x = element_text(size=12,color='#212121',face='bold'),#x-axis setting.
           axis.text.x = element_text(size=10,face="bold"),
           axis.title.y = element_text(size=12,color='#212121',face='bold'), #y-axis fsetting.
           axis.text.y = element_text(size=10,face="bold"),
           legend.title = element_text(face="bold"))+ #Legend setting.
     geom_vline(xintercept = as.Date(c("2010-06-29")), linetype="dashed", color = "#212121", size=1)+ #We add a vertical line at the first date of the dataset
     annotate(geom = "text", x = as.Date(c("2010-05-20")), y = 200, label = "Tesla Initial public offering (06/29/2010): 19.0$", size=4, color = "#212121",
              angle = 90)+ #We add text to specify the meaning of the first date with the initial Tesla stock price
     annotate(geom="point", x=as.Date(max_date), y=max(df$High), size=13, shape=21, fill="transparent", color="#212121") + #We add a circle to highlight the highest Tesla share price
     annotate(geom = "text", x = as.Date(max_date), y = max(df$High)+50, label = "Highest Value of  \n Tesla's stock: 291.4$", size=4, color = "#212121")+ #We add text to specify the highest Tesla share price.
     annotate(geom = "text", x = as.Date("2016-12-01"), y = -10, label = "Price Rate of Change", size=4, color = "#212121")+ #We add text to annotate where the bar graph of the rate of price change is located.
     labs( #We change the title label, and the x-axis and y-axis labels. 
         title = "Evolution of Tesla's stock price from its initial public offering (IPO) to March 17, 2017",
         subtitle = "The lines represent the higher and lowest price of the day, the bar plot represent the amount of stocks traded during that day (1e05).",
         y= "Tesla's Stock Price ($)",
         x= "Date (years)"
     )
 global 
 
 
 df$year=format(as.Date(df$Date, format="%m/%d/%Y"),"%Y") #Extract the year of the date
 
 #SECOND FIGURE: Evolution of the Price through the year
 year_price=df %>% #Create a dataframe
     group_by(year) %>%
     summarise(High=mean(High)) #Extract the highest average share price per year 
 
 year_price$year=as.factor(year_price$year) #Change data type of the year variable to factor.
 yearly= ggplot(year_price, aes(x=year, y=High))+ #Creation of the bar plot 
     geom_bar(stat="identity", fill= "#cc0000", alpha=0.6)+
     labs(
         title = "Average Tesla's stock price per year",
         y= "Tesla's stock price ($)",
         x= "Date (years)"
     )+
     theme(panel.background = element_blank(),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           plot.title= element_text(size=14,color='#212121',face='bold'),
           axis.title.x = element_text(size=12,color='#212121',face='bold'),
           axis.text.x = element_text(size=10,face="bold"),
           axis.title.y = element_text(size=12,color='#212121',face='bold'),
           axis.text.y = element_text(size=10,face="bold"),)
 yearly
 
 
 #THIRD FIGURE: Evolution of the Price Rate of Change
 df$yearmonth=format(as.Date(df$Date, format="%m/%d/%Y"),"%Y-%m") #We extracte the "year" and "month" of the date, but concatenated. 
 roc_month = df %>% 
     group_by(yearmonth) %>%
     summarise(Close=mean(Close)) #To calculate the price rate of change, we will use the Tesla's stock close price. We extract the average closing price of Tesla's stock per year and month. 
 
 roc_month$ROC= ((diff(roc_month$Close))/roc_month$Close)*100 #We calculate the price rate of change.

 roc_month$ROC_Dir= ifelse(roc_month$ROC > 0,"#2ab04d", "#e06565") #ifelse function to assign green color to positive rate of change and red to negative rate of change.
 roc_monthly= ggplot(roc_month, aes(x=yearmonth, y=ROC))+
     geom_bar(stat="identity", fill=roc_month$ROC_Dir, alpha=0.7)+
     labs(
         title = "Evolution of the Price Rate of Change",
         y= "Price Rate of Change (%)",
         x= "Date (Year-Month)"
     )+
     theme(panel.background = element_blank(),
           panel.grid.major.x = element_line( size=.1, linetype="dashed", color="#c4c4c4" ),
           panel.grid.major.y = element_blank(),
           panel.grid.minor = element_blank(),
           plot.title= element_text(size=14,color='#212121',face='bold'),
           axis.title.x = element_text(size=12,color='#212121',face='bold'),
           axis.text.y = element_text(face="bold"),
           axis.title.y = element_text(size=12,color='#212121',face='bold'),
           axis.text.x = element_text(size= 9, angle = 90, vjust = 0.5, hjust=1))
 roc_monthly
 
 dash <- (global) / (((yearly )+ roc_monthly)+ #We use the patchwork package to create a dashboard where the first column of the second row is shorter than the second column. 
                         plot_layout(widths = c(1, 3)))
 dash
 
 tiff(file = "Dashboard.tiff", compression="lzw", width = 6000, height = 4000, res = 300) #We save the dashboard in tiff.format.
 dash
 dev.off()