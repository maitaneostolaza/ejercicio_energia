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


#
################################# DATA CLEANING
nrow(data) # como los registros son mas que 50, utilizamos el test de lillie

# ------ TRANSACTION AMOUNT
n <- lillie.test(data$`Transaction Amount`)
n$p.value<0.05 # como es menor que 0.05, no es una distribucion normal

# ------- Latency
b <- lillie.test(data$`Latency (ms)`)
b$p.value<0.05 # es un p valor bajo, por lo que no es una distribucion normal

# ------ slice bandwidth
c <- lillie.test(data$`Slice Bandwidth (Mbps)`)
c$p.value<0.05 # es un p valor bajo, por lo que no es una distribucion normal


# ---------- DETECCION DE OUTLIERS
# TRANSACTION AMOUNT
rango <- c(quantile(data$`Transaction Amount`,0.05,na.rm=T),
           quantile(data$`Transaction Amount`,0.95,na.rm=T))
# los que se salen del rango (129.855  y 1432.945)

posibles_outliers_TM <- ifelse(data$`Transaction Amount`< 129.855, data$`Transaction Amount`,
                               ifelse(data$`Transaction Amount`>1432.945, data$`Transaction Amount`,NA))
posibles_outliers_TM <- posibles_outliers_TM[!is.na(posibles_outliers_TM)]
sort(posibles_outliers_TM)
max(posibles_outliers_TM)
min(posibles_outliers_TM)
# estos dos valores 5233.00 2500.00  y el minimo parecen ser outliers --> sustituirlo por NA

data <- data %>% arrange(desc(`Transaction Amount`))
data <- data[-c(1,2),]
data <- data %>% arrange(`Transaction Amount`)
data <- data[-1,]

# LATENCY 
rango_LA <- c(quantile(data$`Latency (ms)`,0.05,na.rm=T),
              quantile(data$`Latency (ms)`,0.95,na.rm=T))
# los que se salen del rango (3 y 20)
posibles_outliers_LA <- ifelse(data$`Latency (ms)`< 3, data$`Latency (ms)`,
                               ifelse(data$`Latency (ms)`> 20,data$`Latency (ms)`,NA))
posibles_outliers_LA <- posibles_outliers_LA[!is.na(posibles_outliers_LA)]
# no hay nignun valor que se salga del rango

# SLICE BANWIDTH
rango_SB <- c(quantile(data$`Slice Bandwidth (Mbps)`,0.05,na.rm=T),
              quantile(data$`Slice Bandwidth (Mbps)`,0.95,na.rm=T))
# los que se salen del rango (62 y 240)
posibles_outliers_SB <- ifelse(data$`Slice Bandwidth (Mbps)`< 62, data$`Slice Bandwidth (Mbps)`,
                               ifelse(data$`Slice Bandwidth (Mbps)`> 240,data$`Slice Bandwidth (Mbps)`,NA))
posibles_outliers_SB <- posibles_outliers_SB[!is.na(posibles_outliers_SB)]

sort(posibles_outliers_SB) # no hay nignun valor que destace mucho
# jasdja

########### VALORES MISSINGS
miss_case_summary(data)
miss_var_summary(data)

vis_miss(data)

# imputacion de variables
data_imputado <- kNN(data=data,variable=c("Sender Account ID","Receiver Account ID",
                                          "Transaction Amount","Transaction Status",
                                          "Geolocation (Latitude/Longitude)",
                                          "Latency (ms)",
                                          "Slice Bandwidth (Mbps)",
                                          "PIN Code"),
                     dist_var = c("Device Used","Network Slice ID",
                                  "Transaction Type"),k=5)