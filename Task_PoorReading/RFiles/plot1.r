# Only one row per school, remove duplicated rows. The data set has one row for each school, there are 284 schools in der data set.

pisa18_sc <- pisa18[!duplicated(pisa18$school.id), ]
dim(pisa18)
rm(pisa18)
#View(pisa18_sc)

dim(pisa18_sc)

# Plot the outcome distribution


pdf("outcomeLDFRAC.pdf")
ggplot(pisa18_sc, aes(LDFRAC)) +
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity') + ggtitle("Distribution of Outcome Variable") + theme(text = element_text(size = 19), axis.text = element_text(size = 8)) + annotate(geom="text", x=0.8, y=3.5, label="n = 284 schools",
                                                                                                                                                                                                                color="red",size=8)
dev.off()

# Desciptive summary


DS <- interaction(pisa18_sc$DISHOME, pisa18_sc$STAFFLACK)
ggplot(pisa18_sc, aes(LDFRAC, fill = factor(DS))) +
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')


table(pisa18_sc$DISHOME,pisa18_sc$STAFFLACK)


#DS <- interaction(pisa18_sc$DISHOME, pisa18_sc$STAFFLACK)
#levels(DS)

p1 <- ggplot(pisa18_sc[DS==levels(DS)[1],], aes(LDFRAC)) +
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity') + ggtitle("DISHOME = 0, STAFFLACK = 0") + theme(text = element_text(size = 11), axis.text = element_text(size = 8))+ annotate(geom="text", x=0.8, y=3.5, label="n = 106 schools",
                                                                                                                                                                                                         color="red",size=4)
p3 <- ggplot(pisa18_sc[DS==levels(DS)[2],], aes(LDFRAC)) +
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity') + ggtitle("DISHOME = 1, STAFFLACK = 0") + theme(text = element_text(size = 11), axis.text = element_text(size = 8)) + annotate(geom="text", x=0.8, y=3.5, label="n = 92 schools",
                                                                                                                                                                                                          color="red",size=4)
p2 <- ggplot(pisa18_sc[DS==levels(DS)[3],], aes(LDFRAC)) +
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity') + ggtitle("DISHOME = 0, STAFFLACK = 1") + theme(text = element_text(size = 11), axis.text = element_text(size = 8))+ annotate(geom="text", x=0.8, y=3.5, label="n = 28 schools",
                                                                                                                                                                                                         color="red",size=4)
p4 <- ggplot(pisa18_sc[DS==levels(DS)[4],], aes(LDFRAC)) +
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity') + ggtitle("DISHOME = 1, STAFFLACK = 1") + theme(text = element_text(size = 11), axis.text = element_text(size = 8))+ annotate(geom="text", x=0.8, y=3.5, label="n = 58 schools",
                                                                                                                                                                                                         color="red",size=4)
require(gridExtra)
pdf("outcome4.pdf")
grid.arrange(p1, p2, p3,p4, nrow=2)
dev.off()


