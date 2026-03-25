################################################
# INSTALL PACKAGES (ONLY IF NOT INSTALLED)
################################################

if(!require(tidyverse)) install.packages("tidyverse")  #for data manipulation and plloting
if(!require(corrplot)) install.packages("corrplot")    #for correlation heatmap
if(!require(cluster)) install.packages("cluster")      #for clustering algorithms
if(!require(factoextra)) install.packages("factoextra")  #for cluster visualization
if(!require(rpart)) install.packages("rpart")            #for decision tree
if(!require(rpart.plot)) install.packages("rpart.plot")  #for decision tree plotting

################################################
# LOAD LIBRARIES
################################################

library(tidyverse)
library(corrplot)
library(cluster)
library(factoextra)
library(rpart)
library(rpart.plot)

################################################
# DATASET LOADING
################################################

cat("\n----- LOADING DATASET -----\n")

data <- read.csv("D:/Projects/Customer purchase behaviour analysis/CPA/shopping_trends.csv")

cat("\nFirst 6 rows:\n")
print(head(data))

################################################
# DATASET STRUCTURE
################################################

cat("\n----- DATASET STRUCTURE -----\n")
str(data)

################################################
# DATA SUMMARY
################################################

cat("\n----- DATA SUMMARY -----\n")
summary(data)

################################################
# DATA CLEANING
################################################

cat("\n----- CHECKING MISSING VALUES -----\n")

missing_values <- colSums(is.na(data))
print(missing_values)

# Remove missing values
data <- na.omit(data)

# Remove duplicates
data <- unique(data)

cat("\nData cleaned successfully\n")

################################################
# DATA TRANSFORMATION
################################################

cat("\n----- DATA TRANSFORMATION -----\n")

data$Gender <- as.factor(data$Gender)
data$Category <- as.factor(data$Category)
data$Season <- as.factor(data$Season)
data$Subscription.Status <- as.factor(data$Subscription.Status)

cat("Categorical variables converted to factors\n")

################################################
# DATA REDUCTION (CORRELATION ANALYSIS)
################################################

cat("\n----- CORRELATION ANALYSIS -----\n")

num_data <- data %>% select_if(is.numeric)

cor_matrix <- cor(num_data)

print(cor_matrix)

corrplot(
  cor_matrix,
  method="color",
  type="upper",
  addCoef.col="black",
  tl.cex=0.8,
  number.cex=0.7
)

################################################
# EXPLORATORY DATA ANALYSIS
################################################

#EDA helps identify patterns, trends, and outliers visually

# AGE DISTRIBUTION - Shows how ages are distributed
ggplot(data, aes(x=Age)) +
  geom_histogram(fill="skyblue", bins=20) +
  ggtitle("Age Distribution of Customers")

# GENDER DISTRIBUTION - Count of males vs females
ggplot(data, aes(x = Gender)) +
  geom_bar(fill = "orange") +
  ggtitle("Gender Distribution")

# PURCHASE AMOUNT DISTRIBUTION - Spending distribution
ggplot(data, aes(x = Purchase.Amount..USD.)) +
  geom_histogram(fill = "purple", bins = 20) +
  ggtitle("Purchase Amount Distribution")


# SALES BY CATEGORY - Which product category is most popular
ggplot(data, aes(x = Category)) +
  geom_bar(fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 45)) +
  ggtitle("Sales by Product Category")


# SALES BY SEASON - Seasonal trends
ggplot(data, aes(x = Season)) +
  geom_bar(fill = "green") +
  ggtitle("Sales by Season")


# AGE VS PURCHASE AMOUNT - Relationship between age & spending
ggplot(data, aes(x = Age, y = Purchase.Amount..USD.)) +
  geom_point(color = "red") +
  ggtitle("Age vs Spending")


# SPENDING BY GENDER - shows median, mean, range, outliers
ggplot(data, aes(x = Gender, y = Purchase.Amount..USD., fill = Gender)) +
  geom_boxplot() +
  ggtitle("Spending Distribution by Gender")


# SPENDING BY CATEGORY - Compare category-wise spending
ggplot(data, aes(x = Category, y = Purchase.Amount..USD., fill = Category)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45)) +
  ggtitle("Spending Distribution by Category")

################################################
# CUSTOMER SEGMENTATION (K-MEANS CLUSTERING)
################################################

cat("\n----- CUSTOMER SEGMENTATION -----\n")

cluster_data <- data %>%
  select(Age, Purchase.Amount..USD.)

# SCALE DATA (important for K-means)
cluster_data_scaled <- scale(cluster_data)

cluster_data
cluster_data_scaled

set.seed(123)

kmodel <- kmeans(cluster_data_scaled, centers=3)

data$Cluster <- kmodel$cluster

cat("\nCluster Centers:\n")
print(kmodel$centers)

cat("\nCluster Sizes:\n")
print(kmodel$size)

# CLUSTER VISUALIZATION
fviz_cluster(
  kmodel,
  data = cluster_data_scaled,
  ellipse.type = "convex",
  palette = "jco",
  ggtheme = theme_minimal(),
  main = "Customer Segmentation"
)
################################################
# CLASSIFICATION (DECISION TREE)
################################################

cat("\n----- DECISION TREE MODEL -----\n")

model <- rpart(
  Category ~ Age + Gender + Previous.Purchases + Purchase.Amount..USD.,
  data=data
)

print(model)

rpart.plot(
  model,
  type=2,
  extra=104,
  fallen.leaves=TRUE,
  box.palette="Blues",
  shadow.col="gray",
  nn=TRUE
)
################################################
# OLAP ANALYSIS
################################################

# TOTAL SALES BY CATEGORY
sales_category <- data %>%
  group_by(Category) %>%
  summarise(Total_Sales = sum(Purchase.Amount..USD.))

print(sales_category)

ggplot(sales_category, aes(x=Category, y=Total_Sales)) +
  geom_bar(stat="identity", fill="blue") +
  theme(axis.text.x = element_text(angle=45)) +
  ggtitle("Total Sales by Category")

# AVERAGE SPENDING BY GENDER
gender_spending <- data %>%
  group_by(Gender) %>%
  summarise(Average_Spending = mean(Purchase.Amount..USD.))

print(gender_spending)

ggplot(gender_spending, aes(x=Gender, y=Average_Spending)) +
  geom_bar(stat="identity", fill="orange") +
  ggtitle("Average Spending by Gender")

# TOTAL SALES BY SEASON
season_sales <- data %>%
  group_by(Season) %>%
  summarise(Total_Sales = sum(Purchase.Amount..USD.))

print(season_sales)

ggplot(season_sales, aes(x=Season, y=Total_Sales)) +
  geom_bar(stat="identity", fill="darkgreen") +
  ggtitle("Total Sales by Season")

################################################
# TOP PRODUCT CATEGORY ANALYSIS
################################################

top_products <- data %>%
  group_by(Category) %>%
  summarise(Total_Sales = sum(Purchase.Amount..USD.)) %>%
  arrange(desc(Total_Sales))

print(top_products)

ggplot(top_products,
       aes(x=reorder(Category, Total_Sales),
           y=Total_Sales)) +
  geom_bar(stat="identity", fill="darkblue") +
  coord_flip() +
  ggtitle("Top Performing Product Categories")

################################################
# FINAL DATA SAMPLE
################################################

cat("\n----- FINAL DATA SAMPLE WITH CLUSTERS -----\n")
print(head(data))

################################################
# BUSINESS INSIGHTS
################################################

cat("\n----- PROJECT INSIGHTS -----\n")

cat("Average Customer Spending:",
    mean(data$Purchase.Amount..USD.), "\n")

cat("\nTop Revenue Generating Category:\n")
print(sales_category[which.max(sales_category$Total_Sales),])

cat("\nSeason with Highest Sales:\n")
print(season_sales[which.max(season_sales$Total_Sales),])

cat("\nGender with Highest Average Spending:\n")
print(gender_spending[which.max(gender_spending$Average_Spending),])

cat("\nCustomer segmentation identified three major groups based on age and spending behaviour.\n")

cat("\nDecision tree analysis shows how age, gender, and purchase behaviour influence product category purchases.\n")

################################################
# INTERACTIVE BUSINESS QUERY SYSTEM
################################################

cat("\n----- CUSTOMER PURCHASE QUERY SYSTEM -----\n")

repeat{
  
  cat("\nChoose a question:\n")
  cat("1. What is the total sales?\n")
  cat("2. Which product category has highest sales?\n")
  cat("3. Which season has highest sales?\n")
  cat("4. Which gender spends more on average?\n")
  cat("5. What is the average customer spending?\n")
  cat("6. Exit\n")
  
  choice <- as.integer(readline(prompt="Enter your choice: "))
  
  if(choice == 1){
    
    total_sales <- sum(data$Purchase.Amount..USD.)
    cat("\nTotal Sales:", total_sales, "\n")
    
  } else if(choice == 2){
    
    top_category <- sales_category[which.max(sales_category$Total_Sales),]
    cat("\nHighest Selling Category:\n")
    print(top_category)
    
  } else if(choice == 3){
    
    top_season <- season_sales[which.max(season_sales$Total_Sales),]
    cat("\nSeason with Highest Sales:\n")
    print(top_season)
    
  } else if(choice == 4){
    
    top_gender <- gender_spending[which.max(gender_spending$Average_Spending),]
    cat("\nGender with Highest Average Spending:\n")
    print(top_gender)
    
  } else if(choice == 5){
    
    avg_spending <- mean(data$Purchase.Amount..USD.)
    cat("\nAverage Customer Spending:", avg_spending, "\n")
    
  } else if(choice == 6){
    
    cat("\nExiting Query System...\n")
    break
    
  } else{
    
    cat("\nInvalid choice. Try again.\n")
    
  }
  
}

