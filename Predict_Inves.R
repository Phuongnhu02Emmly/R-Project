#Name: Nguyễn Ngọc Phương Nhu
#Student ID: K204141924

########################### IMPORT LIBRARY ##########################

#import library
library(readxl)
library(ggplot2)
library(tidyverse)  #for dataframe manipulation
library(scales)
library(readxl)
library(dplyr)
library(tseries)
library(lmtest)

#TASK 3

#data collection and input 
data <- read_excel('Data.xlsx')


# Chuyển đổi cột "Date" sang định dạng ngày-tháng-năm
data$Date <- as.Date(data$Date, format = "%d/%m/%Y")

# Lọc dữ liệu chỉ lấy từ năm 2020 về trước và đặt tên tập dữ liệu là "before"
before <- data[data$Date <= as.Date("2019-12-31"), ]

# Lọc dữ liệu chỉ lấy sau năm 2020 và đặt tên tập dữ liệu là "after"
after <- data[data$Date > as.Date("2019-12-31") & data$Date <= as.Date("2022-12-31"), ]

# Tính toán số liệu mô tả cho toàn bộ kỳ
summary_all <- summary(data[, c("Equity", "NAGR", "EBT", "ROAA", "Investment")])
# Tính toán số liệu mô tả cho toàn bộ kỳ
summary_before <- summary(before[, c("Equity", "NAGR", "EBT", "ROAA", "Investment")])
# Tính toán số liệu mô tả cho toàn bộ kỳ
summary_after <- summary(after[, c("Equity", "NAGR", "EBT", "ROAA", "Investment")])

# In kết quả
print("Toàn bộ kỳ:")
print(summary_all)
print("Kỳ trước:")
print(summary_before)
print("Kỳ sau:")
print(summary_after)


'---------------Task 4: Plot ------------------'
##### before
# Vẽ biểu đồ whisker plot cho cột Investment
boxplot(before$Investment, main = "Whisker plot for (before) Investment", ylab = "Investment")

# Vẽ biểu đồ histogram cho cột Investment
hist(before$Investment, main = "Histogram for (before) Investment", xlab = "Investment", ylab = "Frequency")

#### after
# Vẽ biểu đồ whisker plot cho cột Investment
boxplot(after$Investment, main = "Whisker plot for (after) Investment", ylab = "Investment")

# Vẽ biểu đồ histogram cho cột Investment
hist(after$Investment, main = "Histogram for (after) Investment", xlab = "Investment", ylab = "Frequency")

#### Entire
# Vẽ biểu đồ whisker plot cho cột Investment
boxplot(data$Investment, main = "Whisker plot for (entire) Investment", ylab = "Investment")

# Vẽ biểu đồ histogram cho cột Investment
hist(data$Investment, main = "Histogram for (entire) Investment", xlab = "Investment", ylab = "Frequency")


'-----------------TASK 5: MODEL---------------------------'

# Lọc dữ liệu chỉ với các biến và target của đề tài
df <- select(data, Equity, NAGR, EBT, ROAA, Investment)

# Thực hiện hồi quy bội với tất cả các biến riêng lẻ
model1 <- lm(Investment ~ ., data = df)

# Xem kết quả của mô hình hồi quy
summary(model1)

# Creating the Covid-19 dummy variable
data$CovidDummy <- ifelse(data$Date >= as.Date("2019-12-31"), 1, 0)

# Fitting Model 2
model2 <- lm(Investment ~ Equity + NAGR + EBT + ROAA + CovidDummy + Equity:CovidDummy + NAGR:CovidDummy + EBT:CovidDummy + ROAA:CovidDummy, data = data)

# Printing the summary of Model 2
summary(model2)

# Predicting the value of AssignedTopic for all quarters using Model 1
predicted_values <- predict(model1, newdata = data)

# Printing the predicted values
print(predicted_values)


'----------------TASK 6: ARIMA----------'
library(forecast)

# Tạo đối tượng phân rã dữ liệu
time_series <- ts(data$Investment, start = c(2012, 1), end = c(2022,4), frequency = 4)
# Phân rã chuổi thời gian
decompose_results <- decompose(time_series, type = "multiplicative")
# Vẽ biểu đồ kết quả 
plot(decompose_results)

### Kiểm định tính dừng
# Kiểm định tính dừng của chuỗi thời gian Investment
result_ <- adf.test(data$Investment)
print(result_)

# Chia tập dữ liệu
train <- data[1:40,]
test <- data[41:44,]
model_3 <- arima(train$Investment, order = c(2, 1, 0))
model_3

# Chọn mô hình với auto arima
acf(train$Investment)
pacf(train$Investment)
model_4 <- arima(train$Investment, order = c(2, 1, 0))
model_4

# Dự đoán 4 quý đầu năm 2022
forecast <- predict(model_4, n.ahead = 4)
# Hiển thị dự báo
print(forecast$pred)

# So sánh dự báo với dữ liệu thực tế
real_data <- data$Investment[41:44]  # Dữ liệu thực tế cho 4 quý trong năm 2022
comparison <- data.frame(Real_Data = real_data, Forecast = forecast$pred)
print(comparison)

