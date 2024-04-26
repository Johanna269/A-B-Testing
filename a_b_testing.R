# Data Preperation 

data <- read.csv("totals.csv")
library(reshape2)
library(ggplot2)
library(plotly)


# Graph
reshapeddata <- reshape(data,
                        idvar = c("device","date","day"),
                        timevar = "condition",
                        direction = "long",
                        varying = c("N_A",
                                    "purchases_A",
                                    "registeraccount_A",
                                    "N_B",
                                    "purchases_B",
                                    "registeraccount_B"),
                        sep="_")


# Melt the data
plotdata <- melt(reshapeddata, id.vars = c("device", "date", "day", "condition"))
plotdata1 <- plotdata %>%
  group_by(variable, condition) %>%
  mutate(Average = mean(value))

#Visualize
p <- ggplot(plotdata1, aes(x = date, y = value, group = variable, fill = variable)) +
  geom_bar(stat = "identity") +
  facet_grid(device ~ condition) +
  theme(axis.text.x = element_text(angle = 90)) 
ggplotly(p) 
p


# Statistical signifigance
data$cN_A <- ave(data$N_A, data$device, FUN = cumsum)
data$cN_B <- ave(data$N_B, data$device, FUN = cumsum)
data$cpurchases_A <- ave(data$purchases_A, data$device, FUN = cumsum)
data$cpurchases_B <- ave(data$purchases_B, data$device, FUN = cumsum)
data$cregisteraccount_A <- ave(data$registeraccount_A, data$device, FUN=cumsum)
data$cregisteraccount_B <- ave(data$registeraccount_B, data$device, FUN=cumsum)


#Mobile device:
row1column1 <- 29170 #Number of participants in condition A (mobile)
row2column1 <- 33418 #Number of participants in condition B (mobile)
# The expected values under perfect random assignment = average of the two observed values
expected <- (row1column1 + row2column1) / 2
row1column2 <- expected #Number of expected participants in condition A (mobile), given random assignment
row2column2 <- expected #Number of expected participants in condition B (mobile), given random assignment
testtable <- cbind(matrix(c(row1column1, row1column2, row2column1, row2column2), nrow=2, ncol=2))
chisq.test(testtable)

#Desktop:
row1column1 <- 14347 #Number of participants in condition A (desktop)
row2column1 <- 13569 #Number of participants in condition B (desktop)
# The expected values under perfect random assignment = average of the two observed values
expected <- (row1column1 + row2column1) / 2
row1column2 <- expected #Number of expected participants in condition A (desktop), given random assignment
row2column2 <- expected #Number of expected participants in condition B (desktop), given random assignment
testtable <- cbind(matrix(c(row1column1, row1column2, row2column1, row2column2), nrow=2, ncol=2))
chisq.test(testtable)
 

row1column1 <- sum(data$N_A)
row2column1 <- sum(data$N_B)
row1column2 <- (sum(data$N_A) + sum(data$N_B)) / 2
row2column2 <- (sum(data$N_A) + sum(data$N_B)) / 2

# Create matrix
testtable <- cbind(matrix(c(row1column1, row1column2, row2column1, row2column2), nrow = 2, ncol = 2))
# Perform Chi Square test
chisq.test(testtable)


# Run for loop
for(i in 1:nrow(data)){
  row1column1 <- data[i, "cpurchases_A"]
  row2column1 <- data[i, "cpurchases_B"] 
  row1column2 <- mean(c(row1column1, row2column1)) 
  row2column2 <- mean(c(row1column1, row2column1))
  testtable <- cbind(matrix(c(row1column1, row1column2, row2column1, row2column2), nrow = 2, ncol = 2))
  data$p_purchases[i] <- chisq.test(testtable)$p.value
}
# Plot statistical significance over time
ggplot(data, aes(x = date, y = p_purchases, group = device, color = device)) +
  geom_line() +
  ggtitle("Statistical Significance over Time") +
  theme(axis.text.x = element_text(angle = 90))


# Confidence intervals
data$cconversion_A <- data$cpurchases_A/data$cN_A
data$cconversion_B <- data$cpurchases_B/data$cN_B
data$cSEpurchases_A<-sqrt((data$cpurchases_A/data$cN_A)*(1 - (data$cpurchases_A/data$cN_A))/data$cN_A)
data$cSEpurchases_B<-sqrt((data$cpurchases_B/data$cN_B)*(1 - (data$cpurchases_B/data$cN_B))/data$cN_B)
ggplot(data, aes(x=date, y=cSEpurchases_A)) + #Create a visualization of the SE
  geom_line(aes(group = 1))
ggplot(data, aes(x=date, y=cSEpurchases_B)) + #Create a visualization of the SE
  geom_line(aes(group = 1))

#Visualize
reshapeddata <- reshape(data,
                        timevar = "condition",direction = "long",
                        varying = c("cSEpurchases_A","cSEpurchases_B",
                                    "cconversion_A","cconversion_B"),
                        sep = "_",
                        drop = c("N_A","N_B","purchases_A","purchases_B",
                                 "registeraccount_A","registeraccount_B",
                                 "cN_A","cN_B","cpurchases_A","cpurchases_B",
                                 "cregisteraccount_A","cregisteraccount_B")
)
# Melt the data
ggplot(reshapeddata, aes(x = date, y = cconversion, group = condition,fill = condition,
                         color = condition))+geom_line()+facet_grid(device ~ .)+
  geom_ribbon(aes(ymin = cconversion - 1.96 * cSEpurchases, ymax = cconversion + 1.96 * cSEpurchases))+
  theme(axis.text.x = element_text(angle = 90))

