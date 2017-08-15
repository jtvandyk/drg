#############
# Read data #
data <- read.csv("census.csv", header=TRUE, stringsAsFactors=FALSE, sep=",", 
                 colClasses=c("id.cedar"="character", "fam.rate"="numeric", "mhi"="integer",
                              "attainment.rate"="numeric", "mgmt.rate"="numeric", "enroll"="integer",
                              "frl.rate"="numeric","non.english.rate"="numeric"),
                 fill=TRUE)

# Required packages #
require(dplyr)
require(magrittr)
require(gtools)
require(cluster)

###################################
# Construct Z-Scores for all data #

# mean 	  // var.m
fam.rate.m          <- mean(data$fam.rate)
mhi.m               <- mean(data$mhi)
attainment.rate.m   <- mean(data$attainment.rate)
mgmt.rate.m         <- mean(data$mgmt.rate)
enroll.m            <- mean(data$enroll)
frl.rate.m          <- mean(data$frl.rate)
non.english.rate.m  <- mean(data$non.english.rate)

# stdev 	// var.sd
fam.rate.sd          <- sd(data$fam.rate)
mhi.sd               <- sd(data$mhi)
attainment.rate.sd   <- sd(data$attainment.rate)
mgmt.rate.sd         <- sd(data$mgmt.rate)
enroll.sd            <- sd(data$enroll)
frl.rate.sd          <- sd(data$frl.rate)
non.english.rate.sd  <- sd(data$non.english.rate)

# construct zscores // var.z
data$fam.rate.z         <- (data$fam.rate - fam.rate.m)/fam.rate.sd
data$mhi.z              <- (data$mhi - mhi.m)/mhi.sd
data$attainment.rate.z  <- (data$attainment.rate - attainment.rate.m)/attainment.rate.sd
data$mgmt.rate.z        <- (data$mgmt.rate - mgmt.rate.m)/mgmt.rate.sd
data$enroll.z           <- (data$enroll - enroll.m)/enroll.sd
data$frl.rate.z         <- (data$frl.rate - frl.rate.m)/frl.rate.sd
data$non.english.rate.z <- (data$non.english.rate - non.english.rate.m)/non.english.rate.sd

# weight enrollment z score by .5
data$w.enroll.z <- .5 * data$enroll.z

###################################
# Set seeding and select Z scores #

# E - D - F - C - G - B - H - A - I #
# Use seed.order and set ascending for kmeans #
kdata <- data %>%
  arrange(seed.order) %>%
  select(fam.rate.z, mhi.z, attainment.rate.z, mgmt.rate.z, w.enroll.z, frl.rate.z, non.english.rate.z)

# Using seed.order to set ascending for joining fit$cluster after kmeans completed #
findings <- data %>%
  arrange(seed.order)

# Scale data for kmeans #
sdata <- scale(kdata)

# Determine number of clusters to see if initial selection of 9 makes sense #
mydata <- sdata
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# Somewhere between 10-12

##########
# Kmeans #
fit <- kmeans(sdata, 9)

# Get cluster means #
aggregate(mydata,by=list(fit$cluster),FUN=mean)

# Plot cluster #
clusplot(kdata, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# Join clusters with existing dataframe #
findings <- data.frame(findings, fit$cluster)

# Plot matrix with prior DRG and new DRG classifications #
findings_matrix <- table(findings$drg, fit$cluster)
findings_old    <- table(findings$drg.old, fit$cluster)

# Subset for slopegraph - old and new DRG classifications #
drg_new <- findings             %>%   # select dataframe
  arrange(seed.order)           %>%   # ascend according seeding order
  select(name, seed.order)      %>%   # select district name and seed order as substitute for drg class
  mutate(type = "old")                # create new type for data

drg_old <- findings             %>%   # select dataframe
  arrange(seed.order)           %>%   # ascend according seeding order
  select(name, seed.order.old)  %>%   # select district name and seed order as substitute for drg class
  mutate(type = "old")                # create new type for data

kdrg <- findings                %>%   # select dataframe
  select(name, fit.cluster)     %>%   # select district name and fit.cluster 
  mutate(type = "new")                # create new type for data

# Rename columsn to match drg subsets
names(kdrg) <- c("name", "seed.order", "type")

# Create dataframes for slopegraph
slope_new <- smartbind(drg_new, kdrg)
slope_old <- smartbind(drg_old, kdrg)

##########################
# Export Data and Charts #
write.csv(findings, "findings.csv", row.names=FALSE, sep=",")
write.csv(findings_matrix, "findings_matrix.csv", sep=",")
write.csv(findings_old, "findings_old.csv", sep=",")