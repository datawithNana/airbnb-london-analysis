##Title: Airbnb Data Analysis in London, United Kingdom (2022)

##Summary & Objective:
#The objective of this project is to conduct a comprehensive analysis of Airbnb listings in London using 2022 data. 
#The analysis involves detailed exploratory data analysis (EDA) to uncover key trends and factors influencing rental prices. 
#The project builds and evaluates two linear regression and two multiple regression models, 
#with the primary goal of predicting listing prices based on a variety of features such as location, property type, and review scores. 
#Through this predictive analysis, the project aims to identify the most significant variables affecting price, 
#provide actionable insights for hosts and policymakers, and demonstrate effective use of regression techniques in real-world business contexts.

#Load packages
library(tidyverse)
library(broom)
library(lubridate)
library("skimr")
library("kableExtra")
library("RColorBrewer")
library("readr")
library("janitor")
library("data.table")
library("geosphere")
library("scales")
install.packages("plotrix")
library(plotrix)
library("Hmisc")
install.packages("corrplot")
library("corrplot")
install.packages("plotly")
library("plotly")
install.packages("DT",type = "binary")
library(DT)
#Load Data
airbnb_ldn<-read.csv("~/Desktop/listings.csv", encoding = "UTF-8", stringsAsFactors = F, na.strings = c(""))
cat("Number of Rows For Unclceaned Dataset:", nrow(airbnb_ldn), "\n")
cat("Blank cells count For Uncleaned Dataset:", sum(!complete.cases(airbnb_ldn)))

colSums(is.na(airbnb_ldn))

#view the head and tail of the data
DT::datatable(head(airbnb_ldn, 5,5, ellipsis = R), rownames = F)
DT::datatable(tail(airbnb_ldn, 5,5, ellipsis = R), rownames = F)

options(scipen = 100)
cat("Dimension of the data", "\n")
dim(airbnb_ldn)
cat("Summary of the data","\n")
summary(airbnb_ldn)

sapply(airbnb_ldn, class)

#Delete unnecessary columns - id, host_id, neighbourhood_group, license
delete_names<-c("id", "host_id", "neighbourhood_group", "license")
airbnb_ldn[delete_names]=NULL

#Transform character columns to factor columns - host_name, neighbourhood, room_type
names_factor<-c("host_name", "neighbourhood", "room_type")
airbnb_ldn[names_factor]=map(airbnb_ldn[names_factor], as.factor)

airbnb_ldn[c("last_review")] = airbnb_ldn[c("last_review")] %>%
  map(~lubridate::ymd(.x))

glimpse(airbnb_ldn)

missing_df <- airbnb_ldn %>% summarise_all(~(sum(is.na(.))/n()))
missing_df <- gather(missing_df, key = "variables", value = "percent_missing")
missing_df <- missing_df[missing_df$percent_missing > 0.0, ] 
ggplot(missing_df, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "darkgreen", aes(color = I('white')), linewidth = 0.3)+
  xlab('variables')+
  coord_flip() + 
  ggtitle("Missing Data") +
  xlab("Column name") +
  ylab("Percentage missing") 

ggplot(airbnb_ldn, aes(price)) +
  geom_histogram(bins = 30, aes(y = ..density..), fill = "skyblue") + 
  geom_density(alpha = 0.2, fill = "darkgreen") +
  ggtitle("Price Distribution - with log10 transformation of x-axis") +
  #theme(axis.title = element_text(), axis.title.x = element_text()) +
  geom_vline(xintercept = round(mean(airbnb_ldn$price), 2), size = 2, linetype = 3) +
  scale_x_log10() +
  annotate("text", x = 1800, y = 0.75,label = paste("Mean price = ", paste0(round(mean(airbnb_ldn$price), 2), "GBP")),
           color =  "darkred", size = 5)


room_values<-c(43076, 221, 28258, 383)
labels<-c("Entire home/apt", "Hotel", "Private room", "Shared room")
colors<-c("lightgreen", "gold", "orange", "lightblue")

percentages <- paste0(round(100 * room_values / sum(room_values), 2), "%")
pie3D(room_values, labels = percentages, col = colors, explode = 0.2, main = "Percentage of Listings by Room Type")
legend("topright", legend = labels, fill = colors, cex = 0.5)

ggplot(airbnb_ldn, aes(x = room_type, y = price)) +
  geom_boxplot(aes(fill = room_type)) + scale_y_log10() +
  xlab("Room type") + 
  ylab("Price") +
  ggtitle("Price by Room Type") +
  geom_hline(yintercept = mean(airbnb_ldn$price), color = "cadetblue", linetype = 3)


neighbourhood_airbnb<-airbnb_ldn%>% count(neighbourhood, sort = TRUE)
y<-neighbourhood_airbnb$n
y_max <- ceiling(max(y) / 4000) * 4000
barplot( height=neighbourhood_airbnb$n, names=neighbourhood_airbnb$neighbourhood, 
         density=c(5,10,20,30,7) , angle=c(0,45,90,11,36) , col="brown",  
         las=1,
         ylim = c(0, y_max),
         xlab="Neighbourhood", 
         ylab="Number of Listings", 
         main="Distribution of Listing by Neighbourhood",
         cex.names=0.4,las=2.5)

neighbourhood_room_type_count <- airbnb_ldn %>% 
  filter(neighbourhood %in% top10_neighborhoods$neighbourhood) %>% 
  group_by(neighbourhood, room_type) %>% 
  summarise(listing_count = n()) %>% 
  ungroup()

ggplot(neighbourhood_room_type_count, aes(x = neighbourhood, y = listing_count, fill = room_type)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("Entire home/apt" = "violet", "Private room" = "lightblue", "Shared room" = "lightgreen", "Hotel room" = "orange")) +
  labs(title = "Listings by top 10 Neighbourhoods",
       x = "Neighbourhood",
       y = "Number of Listings") +
       theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

plot(airbnb_ldn$availability_365, airbnb_ldn$price, 
     main="Price vs. Availability",
     xlab="Availability (in days)", 
     ylab="Price (in GBP)", 
     pch=20, 
     col="darkcyan",
     cex=0.5,
     ylim=c(0, 10000))
abline(lm(price~availability_365, data=airbnb_ldn), col="firebrick")

plot(airbnb_ldn$number_of_reviews, airbnb_ldn$price, 
     main="Price vs. Number of Reviews",
     xlab="Number of Reviews", 
     ylab="Price (in GBP)", 
     pch=20, 
     col="darkcyan",
     cex=0.5,
     ylim=c(0,10000))

abline(lm(airbnb_ldn$price ~ airbnb_ldn$number_of_reviews), col="red")

filtered_airbnb_london <- subset(airbnb_ldn, room_type %in% c("Entire home/apt", "Private room"))

t.test(price ~ room_type, data = filtered_airbnb_london, var.equal = FALSE)

ggplot(filtered_airbnb_london, aes(x=room_type, y=price, fill=room_type)) + 
  geom_boxplot() + 
  ylim(0,200) +
  labs(title="Distribution of Prices by Room Type", 
       x="Room Type", y="Price") + 
  theme(plot.title = element_text(hjust = 0.5))

set.seed(123)
sample_data <- airbnb_ldn %>%
  filter(!is.na(room_type) & !is.na(reviews_per_month) & neighbourhood %in% c("Westminster", "Tower Hamlets") & room_type == "Entire home/apt") %>%
  sample_n(100)

ggplot(sample_data, aes(x = neighbourhood, y = reviews_per_month, fill = neighbourhood)) +
  geom_boxplot() +
  labs(x = "Borough", y = "Reviews per Month", title = "Box Plot of Reviews per Month by Borough for Entire Home/Apt Listings") +
  theme_bw()

t.test(reviews_per_month ~ neighbourhood, data = sample_data)

set.seed(123)
airbnb_ldn_cor<- airbnb_ldn[, sapply(airbnb_ldn, is.numeric)]
airbnb_ldn_cor<-airbnb_ldn_cor[complete.cases(airbnb_ldn_cor),]
correlation_matrix<- cor(airbnb_ldn_cor, method = "spearman", use = "complete.obs")

#correlation_matrix
corrplot(correlation_matrix, method = "color")

#create matrix of correlation coefficients and p-values
rcorr(as.matrix(airbnb_ldn_cor))

airbnb_train <- airbnb_ldn %>% sample_frac(.7) %>% filter(price > 0)

# sanity check
nrow(airbnb_train) + nrow(airbnb_test) == nrow(airbnb_ldn %>% filter(price > 0))

# Perform linear regression on price and other variables in the dataset
lm_model_1 <- lm(price ~ latitude + longitude + minimum_nights + number_of_reviews + 
                 reviews_per_month + calculated_host_listings_count + availability_365 +
                 number_of_reviews_ltm, data = airbnb_ldn)

# Summarize the linear regression results
summary(lm_model_1)

plot(lm_model_1)

improve_model<-airbnb_train %>% filter(price < quantile(airbnb_train$price, 0.9) & price >
                                 quantile(airbnb_train$price, 0.1)) %>% tidyr::drop_na()
lm_model_2<- lm(log(price) ~ room_type + neighbourhood + latitude +
                   longitude
                 + number_of_reviews + availability_365
                 + reviews_per_month +
                   calculated_host_listings_count + minimum_nights, data = improve_model)
summary(lm_model_2)

plot(lm_model_2)

airbnb_test<- airbnb_test %>% filter(price <= quantile(airbnb_train$price, 0.9) & price >= quantile(airbnb_train$price, 0.1)) %>% tidyr::drop_na()
pred_regression<- predict(lm_model_2, newdata = airbnb_test)
pred_regression<- exp(pred_regression)

RMSE_regression<- sqrt(mean( (airbnb_test$price - pred_regression)**2 ))
RMSE_regression
SSE= sum((airbnb_test$price - pred_regression)**2)

SSR= sum((pred_regression - mean(airbnb_test$price)) ** 2)

R2<- 1 - SSE/(SSE + SSR)
R2

regression_results= tibble(
  obs = airbnb_test$price, pred = pred_regression, diff = pred - obs,
  abs_diff = abs(pred - obs),
  neighbourhood = airbnb_test$neighbourhood,
  name = airbnb_test$name,
  type = airbnb_test$room_type
)
regression_plot<- regression_results %>%
  ggplot(aes(obs, pred)) +
  geom_point(alpha = 0.1, aes(text = paste("Name:", name,
                                           "\nType:", type,
                                           "\nPrice diff = ", diff))) +
  th + scale_x_log10() + scale_y_log10() +
  ggtitle("Observed vs predicted",
          subtitle = "Linear regression model") + geom_abline(slope = 1, intercept =
                                                                0, color = "green", linetype = 2) + facet_wrap(~type)
ggplotly(regression_plot)

# Metrics for testing set: R2 = 0.34 and RMSE = 53.80
model_mul1<- lm(price ~ longitude+latitude, data = airbnb_ldn)
summary(model_mul1)
plot(model_mul1)

model_mul2<- lm(price ~ number_of_reviews+availability_365, data =
                  airbnb_ldn)
summary(model_mul2)
plot(model_mul2)

cat("\n","Simple:Line Regression","\n")
confint(lm_model_2, level=0.95)
cat("\n","Multiple Line Regression for price, longitude and latitude:","\n")
confint(model_mul1, level=0.95)
cat("\n","Multiple Line Regression for price, number of reviews and availability
for 365 days:","\n")
confint(model_mul2, level=0.95)

rmarkdown::render("final milestone.R")
