library(readxl)
or = read_excel('Online Retail.xlsx')
head(or)
str(or)

# Removing the invoices with missing ID numbers

length(unique(or$CustomerID))
sum(is.na(or$CustomerID))
or = subset(or, !is.na(or$CustomerID))

data = subset(or, Country="United Kingdom")
data$item.return = grepl("C", data$InvoiceNo, fixed=T)
data$purchase.invoice = ifelse(data$item.return==T,0,1)
str(data)

# recency
customers = as.data.frame(unique(data$CustomerID))
names(customers) = "CustomerID"
data$recency = as.Date("2011-12-10") - as.Date(data$InvoiceDate)
temp = subset(data, purchase.invoice==1)
recency = aggregate(recency~CustomerID, data=temp, FUN=min, na.rm=T)
remove(temp)
customers = merge(customers, recency, by="CustomerID", all=T, sort=T)
remove(recency)

# frequency
customer.invoices = subset(data, select = c("CustomerID","InvoiceNo","purchase.invoice"))
customer.invoices = customer.invoices[!duplicated(customer.invoices),]
customer.invoices = customer.invoices[order(customer.invoices$CustomerID),]
row.names(customer.invoices) = NULL

# No of invoices per year
annual.invoices = aggregate(purchase.invoice~CustomerID, data=customer.invoices, FUN=sum, na.rm=T)
names(annual.invoices)[names(annual.invoices)=="purchase.invoice"] = "frequency"

# Add no of invoices to customer data
customers = merge(customers, annual.invoices, by="CustomerID", all=T, sort=T)
range(customers$frequency)
table(customers$frequency)

# Remove customers who have not made any purchases in the past year
customers = subset(customers, frequency>0)


## Monetary Value of Customers

#Total spent on each item on an invoice
data$Amount = data$Quantity*data$UnitPrice

# Aggregated total sales to customers
annual.sales = aggregate(Amount ~ CustomerID, data=data, FUN=sum, na.rm=T)
names(annual.sales)[names(annual.sales)=="Amount"] = "monetary"

# Add monetary value to customers dataset
customers = merge(customers, annual.sales, by="CustomerID", all.x=T, sort=T)

# Identify customers with negative monetary value numbers, as they were presumably returning purchased goods
hist(customers$monetary)
customers$monetary = ifelse(customers$monetary<0,0,customers$monetary) # reset negative with 0
hist(customers$monetary)

# 80/20
customers = customers[order(-customers$monetary),]

# Apply Pareto Principle(80/20 Rule)
pareto.cutoff = .8*sum(customers$monetary)                
customers$pareto = ifelse(cumsum(customers$monetary) <= pareto.cutoff,"Top20%", "Bottom80%")
customers$pareto = factor(customers$pareto, levels=c("Top20%", "Bottom80%"), ordered=T)
levels(customers$pareto)
remove(pareto.cutoff)
customers = customers[order(customers$CustomerID),]


