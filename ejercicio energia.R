data <- read_xlsx("transaction_data.xlsx")

# variable transaction amount
data$`Transaction Amount` <- as.numeric(data$`Transaction Amount`)
summary(data)
mobile <- data %>% filter(`Device Used`=="Mobile")
desktop <- data %>% filter(`Device Used`=="Desktop")
summary(mobile)
summary(desktop)
sd(mobile$`Transaction Amount`,na.rm=T)
sd(desktop$`Transaction Amount`)

# agrupacion




# cuantos elementos unicos
length(unique(data$`Transaction ID`))
length(unique(data$`Sender Account ID`))
length(unique(data$`Transaction Type`))
length(unique(data$`Transaction Status`))
length(unique(data$`Network Slice ID`))

# tipo de dato que deberia corresponderle a cada variable
str(data)
data$`Transaction Type` <- as.factor(data$`Transaction Type`)
data$Timestamp <- as.Date(data$Timestamp,)
data$`Transaction Status` <- as.factor(data$`Transaction Status`)
data$`Fraud Flag` <- as.logical(data$`Fraud Flag`) # tambien se podria hacer factor
data$`Device Used` <- as.factor(data$`Device Used`)
data$`Network Slice ID` <- as.factor(data$`Network Slice ID`)