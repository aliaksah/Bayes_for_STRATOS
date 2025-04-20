# Pole so that high values indicate a higher sense of belonging
IMPDAT <- subset(IMPDAT,IMPDAT$CNT == "AUT")
school.id <- IMPDAT$CNTSCHID
pisa18 <- data.frame(school.id)
pisa18$CNT  <- IMPDAT$CNT

# Competencies
pisa18$nld <- 1- as.numeric(IMPDAT$PV1READ < 407)
pisa18$NLD <- ave(pisa18$nld, school.id, FUN = function(x) mean(x, na.rm = TRUE))
pisa18$NLDFRAC <- ave(pisa18$NLD, school.id, FUN = function(x) mean(x, na.rm = TRUE))
pisa18$scie <- IMPDAT$PV1SCIE
pisa18$scie_std <- scale(pisa18$scie)[,1]
# Gender PISA
gender_pisa <- IMPDAT$ST004D01T
pisa18$female <- rep(0, length(gender_pisa))
pisa18$female[gender_pisa == 1] <- 1
#pisa18$FEMFRAC_ST <- ave(pisa18$female, school.id, FUN = function(x) mean(x, na.rm = TRUE))
# NATIVE
lang_home <- IMPDAT$ST022Q01TA
pisa18$native <- rep(0,length(lang_home))
pisa18$native[lang_home == 1] <- 1
pisa18$NATIVE <- ave(pisa18$native, school.id, FUN = function(x) mean(x, na.rm = TRUE))
# IMMIG
pisa18$immig <- 1 - IMPDAT$IMMIG
pisa18$immig[IMPDAT$IMMIG == 1] <- 0
pisa18$immig[IMPDAT$IMMIG > 1] <- 1
pisa18$nimmig <- 1 - pisa18$immig
pisa18$IMMIG <- ave(pisa18$immig, school.id, FUN = function(x) mean(x, na.rm = TRUE))
pisa18$NIMMIG <- ave(pisa18$nimmig, school.id, FUN = function(x) mean(x, na.rm = TRUE))

pisa18$escs <- scale(IMPDAT$ESCS)[,1]
pisa18$ESCS <- ave(pisa18$escs, school.id, FUN = function(x) mean(x, na.rm = TRUE))

hisced <- IMPDAT$HISCED
pisa18$aca <- as.numeric(hisced >= 6)
pisa18$ACA<- ave(pisa18$aca, pisa18$school.id, FUN = function(x) mean(x, na.rm = TRUE))

books <- IMPDAT$ST013Q01TA
# 25 books maximum -> rename few_books
pisa18$fewbooks <- rep(0,length(books))
pisa18$fewbooks[books < 3] <- 1
pisa18$FEWBOOKS <- ave(pisa18$fewbooks, school.id, FUN = function(x) mean(x, na.rm = TRUE))

pisa18$nfewbooks <- 1 - pisa18$fewbooks
pisa18$NFEWBOOKS <- ave(pisa18$nfewbooks, school.id, FUN = function(x) mean(x, na.rm = TRUE))

pisa18$uni  <- IMPDAT$ST225Q06HA # Which of the following do you expect to complete?, ISCED level 5A or 6
pisa18$UNI <- ave(pisa18$uni, pisa18$school.id, FUN = function(x) mean(x, na.rm = TRUE))
#cutoff <- quantile(pisa18$UNI, probs= 3/4)
#pisa18$UNI01 <- as.numeric(pisa18$UNI > cutoff)



# Exposure to bullying # 1 = Never or almost never, 2 = A few times a
# year, 3 = A few times a month ..., 4 = Once a week or more
bul1  <- IMPDAT$ST038Q03NA  # Other students left me out of things on purpose.
bul2  <- IMPDAT$ST038Q04NA  # Other students made fun of me.
bul3  <- IMPDAT$ST038Q05NA  # I was threatened by other students.
bul4  <- IMPDAT$ST038Q06NA  # Other students took away or destroyed things that belonged to me.
bul5  <- IMPDAT$ST038Q07NA  # I got hit or pushed around by other students.
bul6  <- IMPDAT$ST038Q08NA  # Other students spread nasty rumours about me.

bul1_bin  <- ifelse(bul1 > 2,  1, 0)   
bul2_bin  <- ifelse(bul2 > 2,  1, 0)   
bul3_bin  <- ifelse(bul3 > 2,  1, 0)  
bul4_bin  <- ifelse(bul4 > 2,  1, 0)   
bul5_bin  <- ifelse(bul5 > 2,  1, 0)   
bul6_bin  <- ifelse(bul6 > 2,  1, 0) 

bully.f <- data.frame(bul1_bin,bul2_bin,bul3_bin,bul4_bin,bul5_bin)
sum.bully <- apply(bully.f,FUN = sum, MARGIN = 1, na.rm = TRUE)

bul.yes <- rep(0,length(sum.bully))

bul.yes[is.na(sum.bully)] <- NA
bul.yes[sum.bully >= 2] <- 1
pisa18$bull <- bul.yes

pisa18$BULL <- ave(pisa18$bull, school.id, FUN = function(x) mean(x, na.rm = TRUE))


# SENSE OF BELONGING # 1 = Strongly, 2= agree, 3 = disagree, agree, 4 = Strongly disagree
belong1 <- IMPDAT$ST034Q01TA     # I feel like an outsider (or left out of things) at school.
belong2 <- 5 - IMPDAT$ST034Q02TA # I make friends easily at school.
belong3 <- 5 - IMPDAT$ST034Q03TA # I feel like I belong at school
belong4 <- IMPDAT$ST034Q04TA     # I feel awkward and out of place in my school.
belong5 <- 5 - IMPDAT$ST034Q05TA # Other students seem to like me.
belong6 <- IMPDAT$ST034Q06TA     # I feel lonely at school.

belong.f <- data.frame(belong1, belong2, belong3, belong4, belong5 , belong6)
mean.belong <- apply(belong.f,FUN = mean, MARGIN = 1, na.rm = TRUE)
pisa18$belong <- mean.belong #scale(mean.belong)[,1]
#pisa18$BELONG <- ave(pisa18$belong, school.id, FUN = function(x) mean(x, na.rm = TRUE))

# GFOFAIl
# 1 = Strongly disagree...4 = Strongly agree
gfofail1 <- IMPDAT$ST183Q01HA # When I am failing, I worry about what others think of me.
gfofail2 <- IMPDAT$ST183Q02HA # When I am failing, I am afraid that I might not have enough talent.
gfofail3 <- IMPDAT$ST183Q03HA # When I am failing, this makes me doubt my plans for the future.

gfofail.f <- data.frame(gfofail1, gfofail2, gfofail3)
gfofail.mean <- apply(gfofail.f,FUN = mean, MARGIN = 1, na.rm = TRUE)
pisa18$fof <- scale(gfofail.mean)[,1]
pisa18$FOF <- ave(pisa18$fof, school.id, FUN = function(x) mean(x, na.rm = TRUE))
pisa18$nfof <- -scale(gfofail.mean)[,1]

# Meaning in life, 1 = Strongly disagree,... 4 = Strongly agree
mean1  <- IMPDAT$ST185Q01HA  # My life has clear meaning or purpose. 
mean2  <- IMPDAT$ST185Q02HA  # I have discovered a satisfactory meaning in life.
mean3  <- IMPDAT$ST185Q03HA  # I have a clear sense of what gives meaning to my life.

mean.f <- data.frame(mean1, mean2, mean3)
mean.mean <- apply(mean.f ,FUN = mean, MARGIN = 1, na.rm = TRUE)
pisa18$mean <- scale(mean.mean)[,1]
pisa18$MEAN <- ave(pisa18$mean, school.id, FUN = function(x) mean(x, na.rm = TRUE))

pisa18$swbp <- IMPDAT$SWBP
# motivational and metacognitive variables
# SCREADCOMP # Self-concept of reading: Perception of competence
# # 1 = Strongly disagree...4 = Strongly agree
screadcomp1 <- IMPDAT$ST161Q01HA # I am a good reader.
screadcomp2 <- IMPDAT$ST161Q02HA # I am able to understand difficult texts.
screadcomp3 <- IMPDAT$ST161Q03HA # I read fluently.


screadcomp.f <- data.frame(screadcomp1, screadcomp2,  screadcomp3)
screadcomp.mean <- apply(screadcomp.f,FUN = mean, MARGIN = 1, na.rm = TRUE)
pisa18$comp <- scale(screadcomp.mean)[,1]


# SCREADDIFF # Self-concept of reading: Perception of difficulty
# # 1 = Strongly disagree...4 = Strongly agree
screaddiff1 <- IMPDAT$ST161Q06HA # I have always had difficulty with reading.
screaddiff2 <- IMPDAT$ST161Q07HA # I have to read a text several times before completely understanding it.
screaddiff3 <- IMPDAT$ST161Q08HA # I find it difficult to answer questions about a text.

screaddiff.f <- data.frame(screaddiff1, screaddiff2,  screaddiff3)
screaddiff.mean <- apply(screaddiff.f,FUN = mean, MARGIN = 1, na.rm = TRUE)
pisa18$diff <- scale(screaddiff.mean)[,1]
pisa18$ndiff <- - pisa18$diff

# p. 79, participation
part1  <- IMPDAT$ST062Q01TA # I <skipped> a whole school day.
part2  <- IMPDAT$ST062Q02TA # I <skipped> some classes.
pisa18$late  <- IMPDAT$ST062Q03TA # I arrived late for school. (not included)

st_attendance <- data.frame(part1,part2)
mean.st_attendance <- apply(st_attendance,FUN = mean, MARGIN = 1, na.rm = TRUE)
att <- scale(mean.st_attendance)[,1]
ATT <- ave(att, school.id, FUN = function(x) mean(x, na.rm = TRUE))
att01 <- as.numeric(att > 0.0)
pisa18$attp <- rep(0, length(att01))
pisa18$attp[part1 > 1] <- 1
pisa18$attp[part2 > 2] <- 1
pisa18$ATT01 <- ave(pisa18$attp, school.id, FUN = function(x) mean(x, na.rm = TRUE))

# enjoyment of reading, 1 = Strongly disagree, 4 = Strongly agree

# enjoyment of reading, high values indicate a positive attitude towards reading
joy_read1 <- 5 - IMPDAT$ST160Q01IA # I read only if I have to. 
joy_read2 <- IMPDAT$ST160Q02IA     # Reading is one of my favorite hobbies.
joy_read3 <- IMPDAT$ST160Q03IA     # I like talking about books with other people.
joy_read4 <- 5 - IMPDAT$ST160Q04IA # For me, reading is a waste of time.
joy_read5 <- 5 - IMPDAT$ST160Q05IA # I read only to get information that I need.

joy_read <- data.frame(joy_read1,joy_read2,joy_read3,joy_read4,joy_read5)
mean.joy_read <- apply(joy_read,FUN = mean, MARGIN = 1, na.rm = TRUE)
pisa18$joyread <- scale(mean.joy_read)[,1]
pisa18$JOYREAD <- ave(pisa18$joyread, school.id, FUN = function(x) mean(x, na.rm = TRUE))

# learning goals, 1 = Not at all true of me ...5 = Extremely true of me

goal1 <- IMPDAT$ST208Q01HA # My goal is to learn as much as possible.
goal2 <- IMPDAT$ST208Q02HA # My goal is to completely master the material.
goal3 <- IMPDAT$ST208Q04HA # My goal is to understand the content of my classes as thoroughly as possible. 

goal.f <- data.frame(goal1, goal2, goal3)
mean.goal <- apply(goal.f,FUN = mean, MARGIN = 1, na.rm = TRUE)
pisa18$goal <- scale(mean.goal)[,1]
pisa18$GOAL <- ave(pisa18$goal, school.id, FUN = function(x) mean(x, na.rm = TRUE))

# motivation to master tasks, 1  Strongly disagree, 4  Strongly agree

mot1  <- IMPDAT$ST182Q03HA  # I find satisfaction in working as hard as I can. 
mot2  <- IMPDAT$ST182Q04HA  # Once I start a task, I persist until it is finished.
mot3  <- IMPDAT$ST182Q05HA  # Part of the enjoyment I get from doing things is when I improve on my past performance.
mot4  <- IMPDAT$ST182Q06HA  # If I am not good at something, I would rather keep struggling to 
# master it than move on to something I may be good at.
mot.f <- data.frame(mot1, mot2, mot3)
mean.mot <- apply(mot.f ,FUN = mean, MARGIN = 1, na.rm = TRUE)
pisa18$mot <- scale(mean.mot)[,1]
pisa18$MOT <- ave(pisa18$mot, school.id, FUN = function(x) mean(x, na.rm = TRUE))




#Parent support, 1 = Strongly disagree, to 4 = Strongly agree
parent_sup1 <- IMPDAT$ST123Q02NA # My parents support my educational efforts and achievements.
parent_sup2 <- IMPDAT$ST123Q03NA # My parents support me when I am facing difficulties at school.
parent_sup3 <- IMPDAT$ST123Q04NA # My parents encourage me to be confident.

parent_sup <- data.frame(parent_sup1,parent_sup2,parent_sup3)
mean.parent_sup <- apply(parent_sup,FUN = mean, MARGIN = 1, na.rm = TRUE)
pisa18$parent_sup <- scale(mean.parent_sup)[,1]
pisa18$PARENT_SUP <- ave(pisa18$parent_sup, school.id, FUN = function(x) 
  mean(x, na.rm = TRUE))

# Disciplinary climate # 1 = Every lesson to 4 = Never or hardly ever
dis1  <- IMPDAT$ST097Q01TA  # Students don't listen to what the teacher says.
dis2  <- IMPDAT$ST097Q02TA  # There is noise and disorder.
dis3  <- IMPDAT$ST097Q03TA  # The teacher has to wait a long time for students to quiet down.
dis4  <- IMPDAT$ST097Q04TA  # Students cannot work well.
dis5  <- IMPDAT$ST097Q05TA  # Students don't start working for a long time after the lesson begins.

dis_clim <- data.frame(dis1,dis2,dis3,dis4,dis5)
mean.dis_clim <- apply(dis_clim,FUN = mean, MARGIN = 1, na.rm = TRUE)
pisa18$dis_clim <- scale(mean.dis_clim)[,1]
pisa18$DISCLIMAT <- ave(pisa18$dis_clim, school.id, FUN = function(x) 
  mean(x, na.rm = TRUE))
pisa18$ndis_clim <- -scale(mean.dis_clim)[,1]

# Value of school, 1 = Strongly agree,... 4 = Strongly disagree
value1  <- 5 - IMPDAT$ST036Q05TA  # Trying hard at school will help me get a good job. 
value2  <- 5 - IMPDAT$ST036Q06TA  # Trying hard at school will help me get into a good <college>. 
value3  <- 5 - IMPDAT$ST036Q08TA  # Trying hard at school is important.

value.f <- data.frame(value1, value2, value3)
mean.value <- apply(value.f ,FUN = mean, MARGIN = 1, na.rm = TRUE)
pisa18$val <- scale(mean.value)[,1]
pisa18$VAL <- ave(pisa18$val, school.id, FUN = function(x) mean(x, na.rm = TRUE))

# self-efficacy, 1  Strongly disagree, 4  Strongly agree
self_eff1  <- IMPDAT$ST188Q01HA  # I usually manage one way or another.
self_eff2  <- IMPDAT$ST188Q02HA  # I feel proud that I have accomplished things.
self_eff3  <- IMPDAT$ST188Q03HA  # I feel that I can handle many things at a time.
self_eff4  <- IMPDAT$ST188Q06HA  # My belief in myself gets me through hard times.
self_eff5  <- IMPDAT$ST188Q07HA  # When I'm in a difficult situation, I can usually find my way out of it.

self_eff <- data.frame(self_eff1,self_eff2,self_eff3,self_eff4,self_eff5)
mean.self_eff <- apply(self_eff,FUN = mean, MARGIN = 1, na.rm = TRUE)
pisa18$res <- scale(mean.self_eff)[,1]
pisa18$RES <- ave(pisa18$res, school.id, FUN = function(x) mean(x, na.rm = TRUE))

# S. 16, 1.1.2 Besuchte Schulart
school.type <- unclass(IMPDAT$PROGN)  # Schulform, Study programme indices
# Gymnasium
pisa18$GYM <- rep(0,length(school.type))
pisa18$GYM[school.type == "00400002" ] <- 1
pisa18$GYM[school.type == "00400005" ] <- 1
