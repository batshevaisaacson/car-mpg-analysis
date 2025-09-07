#read in the file call-center.csv
call_data <- read.csv(file.choose())
View(call_data)
summary(call_data)

#Question 1: Why do customers call?
#count how many calls there were for each reason
reasons <- table(call_data$Reason)
reasons

count_of_reasons <- c(23462,4749,4730)

#pie chart showing percentage of calls for each reason
piepercent <- round(100*count_of_reasons/sum(count_of_reasons),2)
pie(count_of_reasons, labels = piepercent,
    main = "Reasons for Calls",
    col = rainbow(length(count_of_reasons)))
legend("topright",c("Billing Question","Payments","Service Outage"),
       fill=rainbow(length(count_of_reasons)))

#Question 2: Where do most customers call from?
#count number of calls per state and list them in descending order
states <- sort(table(call_data$State),decreasing=TRUE)
states

#display the top 5 states where calls are coming from
states[1:5]

#Question 3: How do customers rate our service?
ratings <- table(call_data$Sentiment)
#create a bar chart showing customer satisfaction
barplot(ratings, main="Consumer Sentiment", col="blue")
