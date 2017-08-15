jvd_classification<- summary %>%
  group_by(fit.cluster) %>%
  select(mhi, attainment.rate, mgmt.rate, enroll, frl.rate, non.english.rate) %>%
  summarise(
            avg.mhi = mean(mhi, na.rm=TRUE),
            avg.attainment.rate = mean(attainment.rate, na.rm=TRUE),
            avg.mgmt.rate = mean(mgmt.rate, na.rm=TRUE),
            avg.enroll = mean(enroll, na.rm=TRUE),
            avg.frl.rate = mean(frl.rate, na.rm=TRUE),
            avg.non.english.rate = mean(non.english.rate, na.rm=TRUE)
            ) %>%
  arrange(desc(avg.mhi))

classification <- c("a","b","c","d","e","f","g","h","i")

jvd_classification <- data.frame(jvd_classification, classification)

write.csv(jvd_classification, "jvd_classification.csv", row.names=FALSE)