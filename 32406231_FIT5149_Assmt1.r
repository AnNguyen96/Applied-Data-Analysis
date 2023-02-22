# Remove the warning message
options(warn=-1)

# Add all necessary libraries
library(tidyverse)
library(reshape)
library(gridExtra)
library(PerformanceAnalytics)
library(corrplot)
library(caret)
library(Metrics)
library(broom)
library(rpart)
library(rpart.plot)
library(car)

# Read csv file
data <- read.csv("./FIT5149_Assessment1.csv")

# Display first 6 rows
head(data)

# Check null values from the original data
data %>% summarise_all(funs(sum(is.null(data))))

# Check the internal structure of data
str(data)

# Quickly compute summary statistics of data
summary(data)

# Reshape the dataframe 
melt_data <- melt(data)

# Plot the boxplot using ggplot() 
ggplot(melt_data, aes(factor(variable), value)) +
     geom_boxplot(alpha = 0.8, color = "black", aes(fill = variable)) + 
     facet_wrap(~variable, scale="free") +
     ggtitle("Data distribution in each factor") +
     xlab("Variables") +
     theme_bw() +
     theme(plot.title = element_text(hjust = 0.5))

# Create new df for every factor to draw plots
num_rooms <- data %>% select(num_rooms) %>% group_by(num_rooms) %>% summarise(value = n())
num_people <- data %>% select(num_people) %>% group_by(num_people) %>% summarise(value = n())
is_ac <- data %>% select(is_ac) %>% group_by(is_ac) %>% summarise(value = n())
is_tv <- data %>% select(is_tv) %>% group_by(is_tv) %>% summarise(value = n())
is_flat <- data %>% select(is_flat) %>% group_by(is_flat) %>% summarise(value = n())
num_children <- data %>% select(num_children) %>% group_by(num_children) %>% summarise(value = n())
is_urban <- data %>% select(is_urban) %>% group_by(is_urban) %>% summarise(value = n())
month <- data %>% select(month) %>% group_by(month) %>% summarise(value = n())

room_barchart <- ggplot(num_rooms, aes(x = num_rooms, y = value)) +
                 geom_bar(stat = "identity", aes(fill = value)) +
                 scale_fill_gradient('n', low = 'skyblue', high = 'dodgerblue3') +
                 theme_bw() +
                 labs(title = "Count per num_rooms", y = "") +
                 theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 11)) 

people_barchart <- ggplot(num_people, aes(x = num_people, y = value)) +
                   geom_bar(stat = "identity", aes(fill = value)) +
                   scale_fill_gradient('n', low = 'lightpink', high = 'firebrick2') +
                   theme_bw() +
                   labs(title = "Count per num_people", y = "") +
                   theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 11)) 
is_ac_barchart <- ggplot(is_ac, aes(x = is_ac, y = value)) +
                  geom_bar(stat = "identity", aes(fill = value)) +
                  scale_fill_gradient('n', low = 'lightgreen', high = 'green4') +
                  theme_bw() +
                  labs(title = "Count per AC", y = "") +
                  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 11)) 

is_tv_barchart <- ggplot(is_tv, aes(x = is_tv, y = value)) +
                  geom_bar(stat = "identity", aes(fill = value)) +
                  scale_fill_gradient('n', low = 'skyblue', high = 'dodgerblue3') +
                  theme_bw() +
                  labs(title = "Count per TV", y = "") +
                  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 11))

is_flat_barchart <- ggplot(is_flat, aes(x = is_flat, y = value)) +
                    geom_bar(stat = "identity", aes(fill = value)) +
                    scale_fill_gradient('n', low = 'lightpink', high = 'firebrick2') +
                    theme_bw() +
                    labs(title = "Count per num_flat", y = "") +
                    theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 11))

num_children_barchart <- ggplot(num_children, aes(x = num_children, y = value)) +
                         geom_bar(stat = "identity", aes(fill = value)) +
                         scale_fill_gradient('n', low = 'lightgreen', high = 'green4') +
                         theme_bw() +
                         labs(title = "Count per num_children", y = "") +
                         theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 11))

is_urban_barchart <- ggplot(is_urban, aes(x = is_urban, y = value)) +
                     geom_bar(stat = "identity", aes(fill = value)) +
                     scale_fill_gradient('n', low = 'skyblue', high = 'dodgerblue3') +
                     theme_bw() +
                     labs(title = "Count per urban", y = "") +
                     theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 11))

month_barchart <- ggplot(month, aes(x = month, y = value)) +
                  geom_bar(stat = "identity", aes(fill = value)) +
                  scale_fill_gradient('n', low = 'lightpink', high = 'firebrick2') +
                  theme_bw() +
                  labs(title = "Count per month", y = "") +
                  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 11)) +
                  scale_x_continuous(breaks = c(1:10))


# Using 'grid.arrange()' method to arrange the order of the charts
grid.arrange(room_barchart, people_barchart, is_ac_barchart, is_tv_barchart, 
             is_flat_barchart, num_children_barchart, is_urban_barchart, month_barchart,
             ncol = 3, nrow = 3, layout_matrix = rbind(c(1,2,3), c(4,5,6), c(7,8,8)))

housearea_hist <- ggplot(data, aes(x=housearea)) + 
                  geom_histogram(aes(y=..density..), colour="black", fill="lightgreen") +
                  geom_density(alpha=.1, fill="#FF6666") +
                  theme_bw() +
                  labs(title = "The number of records per House area, Average monthly income and Amount paid range",
                       x = " House Area") +
                  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 11))

ave_monthly_income_hist <- ggplot(data, aes(x=ave_monthly_income)) + 
                           geom_histogram(aes(y=..density..), colour="black", fill="goldenrod1") +
                           geom_density(alpha=.1, fill="#FF6666") +
                           theme_bw() +
                           labs(x = "Average monthly income")

amount_paid_hist <- ggplot(data, aes(x=amount_paid)) + 
                    geom_histogram(aes(y=..density..), colour="black", fill="cyan") +
                    geom_density(alpha=.1, fill="#FF6666") +
                    theme_bw() +
                    labs(x = "Amount Paid")

# Using 'grid.arrange()' method to arrange the order of the charts
grid.arrange(housearea_hist, ave_monthly_income_hist, amount_paid_hist, nrow = 3)

paid_per_room <- data %>% select(num_rooms, amount_paid)
paid_per_people <- data %>% select(num_people, amount_paid)
paid_per_ac <- data %>% select(is_ac, amount_paid) 
paid_per_tv <- data %>% select(is_tv, amount_paid)
paid_per_flat <- data %>% select(is_flat, amount_paid)
paid_per_child <- data %>% select(num_children, amount_paid)
paid_per_urban <- data %>% select(is_urban, amount_paid)
paid_per_month <- data %>% select(month, amount_paid)

f1 <- ggplot(paid_per_room, aes(x = num_rooms, y = amount_paid, group = num_rooms)) + 
      geom_boxplot(color="black", fill="red", alpha=0.8) + 
      theme_bw() +
      labs(title = "Amount paid per no_rooms", y = "") +
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10))

f2 <- ggplot(paid_per_people, aes(x = num_people, y = amount_paid, group = num_people)) + 
      geom_boxplot(color="black", fill="orange", alpha=0.8) +
      theme_bw() +
      labs(title = "Amount paid per no_people", y = "") +
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10))

f3 <- ggplot(paid_per_ac, aes(x = is_ac, y = amount_paid, group = is_ac)) + 
      geom_boxplot(color="black", fill="gold1", alpha=0.8) +
      theme_bw() +
      labs(title = "Amount paid per is_AC", y = "") +
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10))

f4 <- ggplot(paid_per_tv, aes(x = is_tv, y = amount_paid, group = is_tv)) + 
      geom_boxplot(color="black", fill="deepskyblue", alpha=0.8) +
      theme_bw() +
      labs(title = "Amount paid per is_TV", y = "") +
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10))

f5 <- ggplot(paid_per_flat, aes(x = is_flat, y = amount_paid, group = is_flat)) + 
      geom_boxplot(color="black", fill="yellowgreen", alpha=0.8) +
      theme_bw() +
      labs(title = "Amount paid per is_flat", y = "") +
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10))

f6 <- ggplot(paid_per_child, aes(x = num_children, y = amount_paid, group = num_children)) + 
      geom_boxplot(color="black", fill="mediumpurple", alpha=0.8) +
      theme_bw() +
      labs(title = "Amount paid per no_children", y = "") +
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10))

f7 <- ggplot(paid_per_urban, aes(x = is_urban, y = amount_paid, group = is_urban)) + 
      geom_boxplot(color="black", fill="deeppink", alpha=0.8) +
      theme_bw() +
      labs(title = "Amount paid per is_urban", y = "") +
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10))

f8 <- ggplot(paid_per_month, aes(x = month, y = amount_paid, group = month)) + 
      geom_boxplot(color="black", fill="cyan1", alpha=0.8) +
      theme_bw() +
      labs(title = "Amount paid per month", y = "") +
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10))

# Using 'grid.arrange()' method to arrange the order of the charts
grid.arrange(f1, f2, f3, f4, f5, f6, f7, f8,
             ncol = 3, nrow = 3)

round(cor(data),3)

# Plot the correlation graph
chart.Correlation(data, histogram = TRUE, method = "pearson")

# Plot the correlogram with palette of colors 
library(RColorBrewer)
corrplot(cor(data), type="upper", order="hclust",
         col=brewer.pal(n=6, name="RdYlBu"))

evaluation <- function(model, test.data, test.label){
    
    # Predict results based on model and test data
    prediction = predict(model, newdata = test.data)
    
    # Use error functions based on test labels and predicted variable
    rss <- sum((prediction - test.label) ^ 2)  ## residual sum of squares
    tss <- sum((test.label - mean(test.label)) ^ 2)  ## total sum of squares
    RSquare <- 1 - rss/tss

    MSE <- round(mse(test.label, prediction),7) 
    RMSE <- round(rmse(test.label, prediction),7) 
    MAE <- round(mae(test.label, prediction),7)
    
    # Return a list of criteria to evaluate
    return (c(RSquare, MSE, RMSE, MAE))
}

# Set index and split data into train.set and test.set
index = 1:800
train.set <- data[index, ]
test.set <- data[-index,]

# Set the data anomalies with NA in both train.set and test.set
train.set[train.set$num_rooms <= 0, 'num_rooms'] <- NA
train.set[train.set$num_people <= 0, 'num_people'] <- NA 
train.set[train.set$ave_monthly_income < 0, 'ave_monthly_income'] <- NA

test.set[test.set$num_rooms <= 0, 'num_rooms'] <- NA
test.set[test.set$num_people <= 0, 'num_people'] <- NA
test.set[test.set$ave_monthly_income < 0, 'ave_monthly_income'] <- NA

# Replace the NA with median of that column
train.set$num_rooms[is.na(train.set$num_rooms)] <- median(train.set$num_rooms, na.rm = TRUE)
train.set$num_people[is.na(train.set$num_people)] <- median(train.set$num_people, na.rm = TRUE)
train.set$ave_monthly_income[is.na(train.set$ave_monthly_income)] <- median(train.set$ave_monthly_income, na.rm = TRUE)

test.set$num_rooms[is.na(test.set$num_rooms)] <- median(test.set$num_rooms, na.rm = TRUE)
test.set$num_people[is.na(test.set$num_people)] <- median(test.set$num_people, na.rm = TRUE)
test.set$ave_monthly_income[is.na(test.set$ave_monthly_income)] <- median(test.set$ave_monthly_income, na.rm = TRUE)

# Split train.set into data and label 
train.data <- train.set[, -11]
train.label <- as.matrix(train.set[, 11])

# Split test.set into data and label
test.data <- test.set[, -11]
test.label <- as.matrix(test.set[, 11])

plot_actual_pred <- function(model, train.label, test.data, test.label, color, name){
    
    # Predict results based on model and test data
    prediction = predict(model, newdata = test.data)
    
    # Make a dataframe with x-axis is Actual value and y-axis is Predicted value
    # then using ggplot() to draw the plot
    data.frame(Actual = test.label, Predicted = prediction) %>%
    ggplot(aes(x = Actual, y = Predicted)) +
       geom_point(alpha = 0.6, color = color) +
       geom_smooth(method = "loess", formula = "y ~ x") +
       geom_abline(intercept = 0, slope = 1, linetype = 2) +
       theme_bw() +
       labs(title = name) +
       theme(plot.title = element_text(hjust = 0.5, size = 10))
}

set.seed(123)
cv <- trainControl(
  method = "cv",
  number = 10,
)

MLR_model <- lm(amount_paid ~ ., data = train.set)
summary(MLR_model)

MLR_step <- step(MLR_model)

summary(MLR_step)

par(mfcol = c(2,2))
plot(MLR_step, col = 'orange')

influencePlot(MLR_step, scale=5, 
              id.method="noteworthy", 
              main = "Influence Plot", 
              sub = "Circle size is proportial to Cook's Distance",
              col = "coral")

# Copy a training set into new dataframe
train.set.no.outlier <- data.frame(train.set)

# Function to detect the outlier using Interquatile rule
detect_outliers <- function(outlier) {

    Q1 <- quantile(outlier, probs=.25)
    Q3 <- quantile(outlier, probs=.75)
    IQR = Q3-Q1

    upper_inner_fence = Q3 + (IQR * 1.5)
    lower_inner_fence = Q1 - (IQR * 1.5)

    outlier > upper_inner_fence | outlier < lower_inner_fence
}

# Function to remove outlier from the dataframe
remove_outliers <- function(df, cols = names(df)) {
    for (i in cols) {
        df <- df[!detect_outliers(df[[i]]),]
    }
    df
}

# Get the new training set without outlier
train.set.no.outlier <- remove_outliers(train.set.no.outlier, c('num_people', 'housearea','ave_monthly_income', 'amount_paid'))

# Train the model without outlier and then using step() to optimize the model
MLR_no_outlier <- lm(amount_paid ~ ., data = train.set.no.outlier)
MLR_no_outlier <- step(MLR_no_outlier)

summary(MLR_no_outlier)

# Draw the Influence Plot
influencePlot(MLR_no_outlier, scale=5, 
              id.method="noteworthy", 
              main = "Influence Plot", 
              sub = "Circle size is proportial to Cook's Distance",
              col = "yellowgreen")

# Tuning lambda parameter
lambda <- expand.grid(
  .lambda = seq(0, 0.5, by = 0.02)
)

# Training Ridge model with best lambda and using Cross validation
Ridge_model <- train(
  amount_paid ~ .,
  data = train.set,
  method = 'ridge',
  preProcess = c("center", "scale"),
  trControl = cv,
  tuneGrid = lambda
)

f1 <- plot(Ridge_model, main = "Lambda vs RMSE")
f2 <- plot(varImp(Ridge_model), main = "Important Factor in Ridge model")
grid.arrange(f1, f2, ncol = 1)

Ridge_model

# Tuning fraction parameter
frac <- expand.grid(
  .fraction = seq(0, 1, by = 0.1)
)

# Training Ridge model with best fraction and using Cross validation
Lasso_model <- train(
  amount_paid ~ .,
  data = train.set,
  method = 'lasso',
  preProcess = c("center", "scale"),
  trControl = cv,
  tuneGrid = frac
)

f1 <- plot(Lasso_model, col='red', main = "Fraction vs RMSE")
f2 <- plot(varImp(Lasso_model), col = 'red', main = "Important Factor in Lasso model")
grid.arrange(f1, f2, ncol = 1)

Lasso_model

Poly_model <− lm(amount_paid ~ polym(num_people, housearea, is_ac, 
                                     is_tv, is_flat, ave_monthly_income,
                                     num_children, is_urban, degree=3, raw=TRUE),
                data = train.set)

glance(Poly_model)

par(mfcol = c(2,2))
plot(Poly_model, col = 'cyan3')

# Training regression tree model with Cross validation and tuneLength is 7
DT_model <- train(
    amount_paid ~ ., 
    data = train.set, 
    method = "rpart",
    preProcess = c("center", "scale"),
    trControl = cv,
    tuneLength = 7,
    metric = "RMSE"
)

print(DT_model)

DT_model2 <- train(
    amount_paid ~ ., 
    data = train.set, 
    method = "rpart",
    tuneGrid = expand.grid(cp = seq(from = 0, to = 0.1, by = 0.01)),
    trControl = cv,
    metric = "RMSE"
)
print(DT_model2)

f1 <- plot(DT_model)
f2 <- plot(DT_model2)
f3 <- plot(varImp(DT_model), main = "Unprune")
f4 <- plot(varImp(DT_model2), main = "Prune")
grid.arrange(f1, f2, f3, f4, nrow = 2)

rpart.plot(DT_model$finalModel, box.palette = c("springgreen", "springgreen1", "springgreen2", "springgreen3", "springgreen4"))

# Tuning the hyperparameters
tuning <- expand.grid(
    n.trees = c(50, 100, 150, 200, 500),
    interaction.depth = c(1, 2, 3),
    shrinkage = 0.1,
    n.minobsinnode = 10
)

# Training Boosted Tree model
BT_model <- train(
    amount_paid ~ .,
    data = train.set,
    method = 'gbm',
    preProcess = c("center", "scale"),
    trControl = cv,
    tuneGrid = tuning,
    verbose = FALSE
)
BT_model

plot(BT_model)

summary(BT_model)

f1 <- plot_actual_pred(MLR_step, train.label, test.data, test.label, "aquamarine1", "Multiple Linear Regression")
f2 <- plot_actual_pred(MLR_no_outlier, train.label, test.data, test.label, "orange", "MLR without Outlier")
f3 <- plot_actual_pred(Ridge_model, train.label, test.data, test.label, "burlywood2", "Ridge Regression")
f4 <- plot_actual_pred(Lasso_model, train.label, test.data, test.label, "darkturquoise", "Lasso Regression")
f5 <- plot_actual_pred(Poly_model, train.label, test.data, test.label, "pink", "Polynomial Regression")
f6 <- plot_actual_pred(DT_model, train.label, test.data, test.label, "darkorchid2", "Regression Tree")
f7 <- plot_actual_pred(BT_model, train.label, test.data, test.label, "seagreen1", "Boosted Regression Tree")

grid.arrange(f1, f2, f3, f4, f5, f6, f7, nrow = 3)

columns <- c("R-Squared", "MSE", "RMSE", "MAE") 
rows <- c("Multiple Linear Regression", "Multiple Linear Regression without Outlier",
          "Ridge Regression", "Lasso Regression", 
          "Polynomial Regression", "Decision Tree Regression", "Boosted Regression Tree")

# Create a dataframe to store the test error in each model 
df <- data.frame(matrix(nrow = length(rows), ncol = length(columns))) 
colnames(df) <- columns
rownames(df) <- rows

# Append to each row the list of test error
df[1,] <- evaluation(MLR_step, test.data, test.label)
df[2,] <- evaluation(MLR_no_outlier, test.data, test.label)
df[3,] <- evaluation(Ridge_model, test.data, test.label)
df[4,] <- evaluation(Lasso_model, test.data, test.label)
df[5,] <- evaluation(Poly_model, test.data, test.label)
df[6,] <- evaluation(DT_model, test.data, test.label)
df[7,] <- evaluation(BT_model, test.data, test.label)

df

final_predict = predict(MLR_step, test.data)
head(final_predict)

# Create dataframe to store actual and predicted value from row 800 to 1000 in data 
final_df <- data.frame(Actual = test.label, Predicted = final_predict)
head(final_df,10)

important <- data.frame(varImp(MLR_model))
important <- data.frame(factor = rownames(important), 
                        overall = important$Overall)
important[order(important$overall, decreasing = TRUE),]


