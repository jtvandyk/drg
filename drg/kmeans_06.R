# Read data #
# Read data #
data_06 <- read.csv("census_06.csv", header=TRUE, stringsAsFactors=FALSE, sep=",", 
                 colClasses=c("id.cedar"="character","name"="character"),
                 fill=TRUE)

# Required packages #
require(dplyr)
require(magrittr)
require(gtools)
require(cluster)

###################################
# Construct Z-Scores for all data #

# mean 	  // var.m
fam.rate.m.06          <- mean(data_06$family.rate.06, na.rm=TRUE)
mhi.m.06               <- mean(data_06$mhi.06, na.rm=TRUE)
attainment.rate.m.06   <- mean(data_06$attainment.rate.06, na.rm=TRUE)
mgmt.rate.m.06         <- mean(data_06$mgmt.rate.06, na.rm=TRUE)
enroll.m.06            <- mean(data_06$enroll.06, na.rm=TRUE)
frl.rate.m.06          <- mean(data_06$frl.rate.06, na.rm=TRUE)
non.english.rate.m.06  <- mean(data_06$non.english.rate.06, na.rm=TRUE)

# stdev 	// var.sd
fam.rate.sd.06          <- sd(data_06$family.rate.06, na.rm=TRUE)
mhi.sd.06               <- sd(data_06$mhi.06, na.rm=TRUE)
attainment.rate.sd.06   <- sd(data_06$attainment.rate.06, na.rm=TRUE)
mgmt.rate.sd.06         <- sd(data_06$mgmt.rate.06, na.rm=TRUE)
enroll.sd.06            <- sd(data_06$enroll.06, na.rm=TRUE)
frl.rate.sd.06          <- sd(data_06$frl.rate.06, na.rm=TRUE)
non.english.rate.sd.06  <- sd(data_06$non.english.rate.06, na.rm=TRUE)

# construct zscores // var.z
data_06$fam.rate.z.06         <- (data_06$family.rate.06 - fam.rate.m.06)/fam.rate.sd.06
data_06$mhi.z.06              <- (data_06$mhi.06 - mhi.m.06)/mhi.sd.06
data_06$attainment.rate.z.06  <- (data_06$attainment.rate.06 - attainment.rate.m.06)/attainment.rate.sd.06
data_06$mgmt.rate.z.06        <- (data_06$mgmt.rate.06 - mgmt.rate.m.06)/mgmt.rate.sd.06
data_06$enroll.z.06           <- (data_06$enroll.06 - enroll.m.06)/enroll.sd.06
data_06$frl.rate.z.06         <- (data_06$frl.rate.06 - frl.rate.m.06)/frl.rate.sd.06
data_06$non.english.rate.z.06 <- (data_06$non.english.rate.06 - non.english.rate.m.06)/non.english.rate.sd.06

# weight enrollment z score by .5
data_06$w.enroll.z.06 <- .5 * data_06$enroll.z.06

###################################
# Set seeding and select Z scores #

# E - D - F - C - G - B - H - A - I #
# Use seed.order and set ascending for kmeans #
kdata_06 <- data_06 %>%
  arrange(seed.order) %>%
  select(fam.rate.z.06, mhi.z.06, attainment.rate.z.06, mgmt.rate.z.06, w.enroll.z.06, frl.rate.z.06, non.english.rate.z.06)

join_06 <- data_06 %>%
  arrange(seed.order) %>%
  select(id.cedar, name, mhi.06, attainment.rate.06, mgmt.rate.06, family.rate.06, enroll.06, frl.rate.06, non.english.rate.06, drg.old, seed.order.old, drg, seed.order)


# Using seed.order to set ascending for joining fit$cluster after kmeans completed #
findings_06 <- data_06 %>%
  arrange(seed.order)

# Scale data for kmeans #
sdata_06 <- scale(kdata_06)

# Determine number of clusters to see if initial selection of 9 makes sense #
mydata_06 <- sdata_06
wss <- (nrow(mydata_06)-1)*sum(apply(mydata_06,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata_06,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# Somewhere between 10-12

##########
# Kmeans #
sdata_06 <- sdata_06[complete.cases(sdata_06),] # Remove NAs for K Means Analysis in following line
join_06 <- join_06[complete.cases(join_06),] # Remove NAs for joining with fit cluster outputs
fit_06 <- kmeans(sdata_06, 9)                   # Kmeans analysis with historical data 

# Get cluster means #
aggregate(mydata_06,by=list(fit$cluster),FUN=mean)

# Plot cluster #
clusplot(join_06, fit_06$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# Join clusters with existing dataframe #
findings_06 <- data.frame(join_06, fit_06$cluster)

# Plot matrix with prior DRG and new DRG classifications #
findings_matrix_06 <- table(findings_06$drg, fit_06$cluster)
findings_old_06    <- table(findings_06$drg.old, fit_06$cluster)

##########################
# Export Data and Charts #
write.csv(findings_06, "findings_06.csv", row.names=FALSE)
write.csv(findings_matrix_06, "findings_matrix_06.csv")
write.csv(findings_old_06, "findings_old_06.csv")