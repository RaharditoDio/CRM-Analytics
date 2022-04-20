# load dataset
dataset <- read.csv('Data-Customer.csv', stringsAsFactors = T)
# show table
View(dataset)

# show data type
str(dataset)

# data conversion from categorical to numeric
dataset_matrix <- data.matrix(dataset[c("Gender","Employment","House.Type")])

# join data conversion to dataset
dataset <- data.frame(dataset, dataset_matrix)
dataset

# value normalization to get better SSE
dataset$Expenditure <- dataset$Expenditure / 1000000
View(dataset)

# create data master
Employment <- unique(dataset[c("Employment", "Employment.1")])
Gender <- unique(dataset[c("Gender", "Gender.1")])
House.Type <- unique(dataset[c("House.Type", "House.Type.1")])

# select specific variable
selected_field <- c("Gender.1","Age","Employment.1","House.Type.1","Expenditure")

# show selected_field
dataset[selected_field]

# K-Means
set.seed(100)

# create K-Means fucntion with 3 cluster & 25 scenario
segmentation <- kmeans(x=dataset[selected_field], centers=3, nstart=25)
segmentation

# join clustering result
segmentation$cluster

dataset$Cluster <- segmentation$cluster
View(dataset)
str(dataset)

# Filtering cluster-1
which(dataset$Cluster == 1)
length(which(dataset$Cluster == 1))
dataset[which(dataset$Cluster == 1),]

# Filtering cluster-2
which(dataset$Cluster == 2)
length(which(dataset$Cluster == 2))
dataset[which(dataset$Cluster == 2),]

# Filtering cluster-3
which(dataset$Cluster == 3)
length(which(dataset$Cluster == 3))
dataset[which(dataset$Cluster == 3),]

# Show cluster means from object 
segmentation$centers

# Compare 2 cluster kmeans (2 and 5)
set.seed(100)
kmeans(x=dataset[selected_field], centers=2, nstart=25)
set.seed(100)
kmeans(x=dataset[selected_field], centers=5, nstart=25)

segmentation$withinss
segmentation$cluster
segmentation$tot.withinss

#  Sum of Squares (SS)
sse <- sapply(1:10, function(param_k){kmeans(x=dataset[selected_field], param_k, nstart=25)$tot.withinss})
sse

#  create elbow effect chart
library(ggplot2)
cluster_max <- 10
ssdata = data.frame(cluster=c(1:cluster_max),sse)
ggplot(ssdata, aes(x=cluster,y=sse)) + geom_line(color="red") + geom_point() +
  ylab("Within Cluster Sum of Squares") + xlab("Number of Cluster") +
  geom_text(aes(label=format(round(sse, 2), nsmall = 2)), hjust=-0.2, vjust=-0.5) + scale_x_discrete(limits=c(1:cluster_max))

# create label segment
Segment.Dataset <- data.frame(cluster = c(1,2,3,4,5), Label.Segment = c("Silver Youth Gals","Diamond Senior Member","Gold Young Professional","Diamond Professional","Silver Mid Professional"))

# Label.Cluster
Identity.Cluster <- list(Employment=Employment, Gender=Gender, House.Type=House.Type, segmentation=segmentation, Segment.Dataset=Segment.Dataset, selected_field=selected_field)
saveRDS(Identity.Cluster,"cluster.rds")

# test dataset
datanew <- data.frame(Customer.ID="CUST-100",
                       Cust.Name="Ade Suyanto",
                       Age=20,
                       Gender="Female",
                       Employment="Student",
                       House.Type="Cluster",
                       Expenditure=3.5)
datanew

Identity.Cluster <- readRDS(file="cluster.rds")
Identity.Cluster

# join data
datanew <- merge(datanew, Identity.Cluster$Customer.ID)
datanew <- merge(datanew, Identity.Cluster$Cust.Name)
datanew <- merge(datanew, Identity.Cluster$Age)
datanew <- merge(datanew, Identity.Cluster$Gender)
datanew <- merge(datanew, Identity.Cluster$Employment)
datanew <- merge(datanew, Identity.Cluster$House.Type)
datanew <- merge(datanew, Identity.Cluster$Expenditure)
datanew

# determine new data segment
Identity.Cluster$Segment.Dataset[which.min(sapply( 1:5, function(x) sum((datanew[Identity.Cluster$selected_field] - Identity.Cluster$segmentation$centers[x,])^2 ))), ]

