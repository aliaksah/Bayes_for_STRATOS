# Follow example in Yao 2018, do a repeated regression for rural and urban
# schools to explore if the association between variables that reflect identification,
# integration, participation or motivation relate differs in the rural and urban context.
# First, a descriptive analysis will be carried out to determine whether different 
# patterns can be identified. Variables that affect the composition of the school (e. g.
# fraction of non-natives) will not be taken into account. This task refers only to the first step.

# Not exactly like Yao, but seven key predictors, each key predictor has
# a own model in the stack and contains the other predictors in the same form.
# Logistic regression is non-collapsible and therefore the combination matters for
# the power to predict certain regions of the outcome.

# Todo: decide if a certain predictor based on student or-school-level,
# use similar predictors and use one on each level, maybe also escs
# create data frame
IMPDAT <- subset(IMPDAT,IMPDAT$CNT == "AUT")
school.id <- IMPDAT$CNTSCHID
pisa18 <- data.frame(school.id)
pisa18$CNT  <- IMPDAT$CNT
pisa18$ld <- as.numeric(IMPDAT$PV1READ < 407)
# Outcome, Fraction of literary deprived students
pisa18$LD <- ave(pisa18$ld, school.id, FUN = function(x) mean(x, na.rm = TRUE))

# Gender PISA
gender_pisa <- IMPDAT$ST004D01T
pisa18$female <- rep(0, length(gender_pisa))
pisa18$female[gender_pisa == 1] <- 1
pisa18$FEMFRAC_ST <- ave(pisa18$female, school.id, FUN = function(x) mean(x, na.rm = TRUE))
# NATIVE
lang_home <- IMPDAT$ST022Q01TA
pisa18$native <- rep(0,length(lang_home))
pisa18$native[lang_home == 1] <- 1
pisa18$NATIVE <- ave(pisa18$native, school.id, FUN = function(x) mean(x, na.rm = TRUE))
# IMMIG
pisa18$immig <- IMPDAT$IMMIG
pisa18$immig[IMPDAT$IMMIG == 1] <- 0
pisa18$immig[IMPDAT$IMMIG > 1] <- 1
pisa18$IMMIG <- ave(pisa18$immig, school.id, FUN = function(x) mean(x, na.rm = TRUE))

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

pisa18$uni  <- IMPDAT$ST225Q06HA # Which of the following do you expect to complete?, ISCED level 5A or 6
pisa18$UNI <- ave(pisa18$uni, pisa18$school.id, FUN = function(x) mean(x, na.rm = TRUE))
#cutoff <- quantile(pisa18$UNI, probs= 3/4)
#pisa18$UNI01 <- as.numeric(pisa18$UNI > cutoff)

# Hindered learning 1
# Is your school’s capacity to provide instruction hindered
# by any of the following issues?
# 1 = not at all, 2 = very little, 3 = to some extent 4 = a lot
# STAFFSHORT: staff_lack, poor_staff, ass_staff_lack, poor_ass_staff
# 
staff_lack     <- IMPDAT$SC017Q01NA # A lack of teaching staff.
poor_staff     <- IMPDAT$SC017Q02NA # Inadequate or poorly qualified teaching staff.
ass_staff_lack <- IMPDAT$SC017Q03NA # A lack of teaching staff.
poor_ass_staff <- IMPDAT$SC017Q04NA # Inadequate or poorly qualified teaching staff.


staffshort.f <- data.frame(staff_lack, poor_staff, ass_staff_lack, poor_ass_staff) #
mean.staffshort <- apply(staffshort.f,FUN = mean, MARGIN = 1, na.rm = TRUE)
staffshort <- scale(mean.staffshort)[,1]
cutoff <- quantile(staffshort, probs= 5/10)
pisa18$STAFFLACK <- as.numeric(staffshort > cutoff)
pisa18$STAFFSHORT <- staffshort

# EDUSHORT: pisa18$mat_lack,pisa18$poor_mat, pisa18$infra_lack, pisa18$poor_infra

#  1 = not at all, 2 = very little, 3 = to some extent 4 = a lot
mat_lack   <- IMPDAT$SC017Q05NA # A lack of educational material (e.g. textbooks, IT equipment, library or laboratory material).
poor_mat   <- IMPDAT$SC017Q06NA # Inadequate or poor quality educational material (e.g. textbooks, IT equipment, library or laboratory material).
infra_lack <- IMPDAT$SC017Q07NA # A lack of physical infrastructure (e.g. building, grounds, heating/cooling, lighting and acoustic systems).
poor_infra <- IMPDAT$SC017Q08NA # Inadequate or poor quality physical infrastructure (e.g. building, grounds, heating/cooling, lighting an.

edushort.f <- data.frame(mat_lack, poor_mat, infra_lack, poor_infra)# ass_staff_lack, poor_ass_staff)
mean.edushort <- apply(edushort.f,FUN = mean, MARGIN = 1, na.rm = TRUE)
pisa18$EDUSHORT <- scale(mean.edushort)[,1]
pisa18$EDULACK <- as.numeric(pisa18$EDUSHORT  > 0.0)

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
pisa18$belong <- scale(mean.belong)[,1]
pisa18$BELONG <- ave(pisa18$belong, school.id, FUN = function(x) mean(x, na.rm = TRUE))

# p. 79, participation
pisa18$part1  <- IMPDAT$ST062Q01TA # I <skipped> a whole school day.
pisa18$part2  <- IMPDAT$ST062Q02TA # I <skipped> some classes.
pisa18$part3  <- IMPDAT$ST062Q03TA # I arrived late for school. (not included)

st_attendance <- data.frame(pisa18$part1,pisa18$part2)
mean.st_attendance <- apply(st_attendance,FUN = mean, MARGIN = 1, na.rm = TRUE)
pisa18$att <- scale(mean.st_attendance)[,1]
pisa18$ATT <- ave(pisa18$att, school.id, FUN = function(x) mean(x, na.rm = TRUE))
pisa18$att01 <- as.numeric(pisa18$att > 0.0)
pisa18$ATT01 <- ave(pisa18$att01, school.id, FUN = function(x) mean(x, na.rm = TRUE))

pisa18$attp <- rep(0, length(pisa18$att01))
pisa18$attp[pisa18$part1 > 1] <- 1
pisa18$attp[pisa18$part2 > 2] <- 1
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

pisa18$DISHOME  <- IMPDAT$SC048Q03NA/100 # Students from socioeconomically disadvantaged homes
pisa18$DISH <- as.numeric(cut_number(pisa18$DISHOME,2)) - 1


# Hindered learning 2
# In your school, to what extent is the learning of students
# hindered by the following phenomena?
# STUBEHA, 1 = not at all, 2 = very little, 3 = to some extent 4 = a lot
truancy    <- IMPDAT$SC061Q01TA #  Student truancy
skip       <- IMPDAT$SC061Q02TA #  Students skipping classes
no_respect <- IMPDAT$SC061Q03TA #  Students lacking respect for teachers
alc        <- IMPDAT$SC061Q04TA #  Student use of alcohol or illegal drugs
bull       <- IMPDAT$SC061Q05TA #  Students intimidating or bullying other students
no_att     <- IMPDAT$SC061Q11HA #  Students not being attentive

discipline.f <- data.frame(truancy,skip,no_respect, alc, bull, no_att)
mean.discipline <- apply(discipline.f,FUN = mean, MARGIN = 1, na.rm = TRUE)
discipline_sc <- scale(mean.discipline)[,1]
pisa18$DISCIPLINE  <- ave(discipline_sc , school.id, FUN = function(x) mean(x, na.rm = TRUE))

bad_teach  <- IMPDAT$SC061Q06TA #  Teachers not meeting individual students’ needs
miss_teach <- IMPDAT$SC061Q07TA #  Teacher absenteeism
resi_staff <- IMPDAT$SC061Q08TA #  Staff resisting change
strict     <- IMPDAT$SC061Q09TA #  Teachers being too strict with students
unprepared <- IMPDAT$SC061Q10TA #  Teachers not being well prepared for classes

teachbad.f <- data.frame(bad_teach,miss_teach,resi_staff, strict, unprepared)
mean.teachbad <- apply(teachbad.f,FUN = mean, MARGIN = 1, na.rm = TRUE)
teachbad_sc <- scale(mean.teachbad)[,1]
pisa18$TEACHBAD <- ave(teachbad_sc, school.id, FUN = function(x) mean(x, na.rm = TRUE))


# Value of school, 1 = Strongly agree,... 4 = Strongly disagree
value1  <- IMPDAT$ST036Q05TA  # Trying hard at school will help me get a good job. 
value2  <- IMPDAT$ST036Q06TA  # Trying hard at school will help me get into a good <college>. 
value3  <- IMPDAT$ST036Q08TA  # Trying hard at school is important.

value.f <- data.frame(value1, value2, value3)
mean.value <- apply(value.f ,FUN = mean, MARGIN = 1, na.rm = TRUE)
pisa18$val <- scale(mean.value)[,1]
pisa18$VAL <- ave(pisa18$val, school.id, FUN = function(x) mean(x, na.rm = TRUE))

# S. 16, 1.1.2 Besuchte Schulart
pisa18$school.type <- unclass(IMPDAT$PROGN)  # Schulform, Study programme indices
# Gymnasium
pisa18$GYM <- rep(0,length(pisa18$school.type))
pisa18$GYM[pisa18$school.type == "00400002" ] <- 1
pisa18$GYM[pisa18$school.type == "00400005" ] <- 1
# by(IMPDAT$PV1READ, IMPDAT$PROGN, summary)
# big city
# Which of the following definitions best describes the community in which your school is located?
pisa18$RUR <- IMPDAT$SC001Q01TA # 1 = < 3000, 2 = 3000 to 15 000, 3 = 15 000 to 100000, 4 = 100000 to 1 000 000, 5 = more than 1 000 000

pisa18$RUR[pisa18$RUR == 1] <- 1
pisa18$RUR[pisa18$RUR == 2] <- 1
pisa18$RUR[pisa18$RUR == 3] <- 0
pisa18$RUR[pisa18$RUR == 4] <- 0
pisa18$RUR[pisa18$RUR == 5] <- 0
 

pisa18$SN_SC <- IMPDAT$SC048Q02NA/100 # Students with special needs
pisa18$FAILED <- IMPDAT$SC164Q01HA/100 # in the last full academic year, what proportion of 
# students in your school’s final grade left school without 
pisa18$WITHOUT  <- as.numeric(pisa18$FAILED > 0.02) # above median

#As of <February 1, 2018>, what was the total school enrolment (number of students)?

MALES <- IMPDAT$SC002Q01TA
FEMS <- IMPDAT$SC002Q02TA
# Fraction of female students
pisa18$FEMFRAC <- FEMS/(FEMS + MALES)

pisa18$NN_SC <- IMPDAT$SC048Q01NA/100 # Students whose <heritage language> is different from <test language>