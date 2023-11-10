# create data frame
IMPDAT <- subset(IMPDAT,IMPDAT$CNT == "AUT")
school.id <- IMPDAT$CNTSCHID
pisa18 <- data.frame(school.id)
pisa18$CNT  <- IMPDAT$CNT
LD <- as.numeric(IMPDAT$PV1READ < 407)
# Outcome, Fraction of literary deprived students
pisa18$LDFRAC <- ave(LD, school.id, FUN = function(x) mean(x, na.rm = TRUE))/100

#Parent support, 1 = Strongly disagree, to 4 = Strongly agree
parent_sup1 <- IMPDAT$ST123Q02NA # My parents support my educational efforts and achievements.
parent_sup2 <- IMPDAT$ST123Q03NA # My parents support me when I am facing difficulties at school.
parent_sup3 <- IMPDAT$ST123Q04NA # My parents encourage me to be confident.

parent_sup <- data.frame(parent_sup1,parent_sup2,parent_sup3)
mean.parent_sup <- apply(parent_sup,FUN = mean, MARGIN = 1, na.rm = TRUE)
parent_sup_sc <- scale(mean.parent_sup)[,1]
pisa18$PARENT_SUP <- ave(parent_sup_sc, school.id, FUN = function(x) mean(x, na.rm = TRUE))

# Gender PISA
gender_pisa <- IMPDAT$ST004D01T
FEMALE <- rep(0, length(gender_pisa))
FEMALE[gender_pisa == 1] <- 1
pisa18$FEMFRAC_ST <- ave(FEMALE, school.id, FUN = function(x) mean(x, na.rm = TRUE))
# NATIVE
lang_home <- IMPDAT$ST022Q01TA
NATIVE <- rep(0,length(lang_home))
NATIVE[lang_home == 1] <- 1
pisa18$NATIVEFRAC <- ave(NATIVE, school.id, FUN = function(x) mean(x, na.rm = TRUE))
# IMMIG
immig <- IMPDAT$IMMIG
immig[IMPDAT$IMMIG == 1] <- 0
immig[IMPDAT$IMMIG > 1] <- 1
pisa18$IMMIGFRAC <- ave(immig, school.id, FUN = function(x) mean(x, na.rm = TRUE))

ESCS <- scale(IMPDAT$ESCS)[,1]
pisa18$ESCS <- ave(ESCS, school.id, FUN = function(x) mean(x, na.rm = TRUE))

books <- IMPDAT$ST013Q01TA
# 25 books maximum -> rename few_books
few_book <- rep(0,length(books))
few_book[books < 3] <- 1
pisa18$FEWBOOKSFRAC <- ave(few_book, school.id, FUN = function(x) mean(x, na.rm = TRUE))

repeatclass <- IMPDAT$REPEAT
pisa18$REPEATFRAC <- ave(repeatclass, school.id, FUN = function(x) mean(x, na.rm = TRUE))

# Disciplinary climate # 1 = Every lesson to 4 = Never or hardly ever
dis1  <- IMPDAT$ST097Q01TA  # Students don't listen to what the teacher says.
dis2  <- IMPDAT$ST097Q02TA  # There is noise and disorder.
dis3  <- IMPDAT$ST097Q03TA  # The teacher has to wait a long time for students to quiet down.
dis4  <- IMPDAT$ST097Q04TA  # Students cannot work well.
dis5  <- IMPDAT$ST097Q05TA  # Students don't start working for a long time after the lesson begins.

dis_clim <- data.frame(dis1,dis2,dis3,dis4,dis5)
mean.dis_clim <- apply(dis_clim,FUN = mean, MARGIN = 1, na.rm = TRUE)
dis_clim_sc <- scale(mean.dis_clim)[,1]
pisa18$DISCLIMAT <- ave(dis_clim_sc, school.id, FUN = function(x) mean(x, na.rm = TRUE))

# Teacher support # 1 = every lesson,..4 = never or hardly ever
teachsupp1  <- 5 - IMPDAT$ST100Q01TA  # The teacher shows an interest in every student's learning.
teachsupp2  <- 5 - IMPDAT$ST100Q02TA  # The teacher gives extra help when students need it.
teachsupp3  <- 5 - IMPDAT$ST100Q03TA  # The teacher helps students with their learning.
teachsupp4  <- 5 - IMPDAT$ST100Q04TA  # The teacher continues teaching until the students understand.

teachsupp <- data.frame(teachsupp1,teachsupp2,teachsupp3,teachsupp4)
mean.teachsupp <- apply(teachsupp,FUN = mean, MARGIN = 1, na.rm = TRUE)
teachsupp_sc <- scale(mean.teachsupp)[,1]
pisa18$TEACHSUPP <- ave(teachsupp_sc, school.id, FUN = function(x) mean(x, na.rm = TRUE))

# Teacher feedback # 1 = never or hardly ever,..4 = every lesson
teachfeed1  <- IMPDAT$ST104Q02NA  # The teacher gives me feedback on my strengths in this subject.
teachfeed2  <- IMPDAT$ST104Q03NA  # The teacher tells me in which areas I can still improve.
teachfeed3  <- IMPDAT$ST104Q04NA  # The teacher tells me how I can improve my performance.

teachfeed <- data.frame(teachfeed1,teachfeed2,teachfeed3)
mean.teachfeed <- apply(teachfeed,FUN = mean, MARGIN = 1, na.rm = TRUE)
teachfeed_sc <- scale(mean.teachfeed)[,1]
pisa18$TEACHFEED <- ave(teachfeed_sc, school.id, FUN = function(x) mean(x, na.rm = TRUE))

# Teacher-directed instruction  # 1 = every lesson,..4 = never or hardly ever
dirins1 <- 5 - IMPDAT$ST102Q01TA # The teacher sets clear goals for our learning.
dirins2 <- 5 - IMPDAT$ST102Q02TA # The teacher asks questions to check whether we have understood what was taught.
dirins3 <- 5 - IMPDAT$ST102Q03TA # At the beginning of a lesson, the teacher presents a short summary of the previous lesson.
dirins4 <- 5 - IMPDAT$ST102Q04TA # The teacher tells us what we have to learn.

dirins <- data.frame(dirins1,dirins2,dirins3,dirins4)
mean.dirins <- apply(dirins,FUN = mean, MARGIN = 1, na.rm = TRUE)
dirins_sc <- scale(mean.dirins)[,1]
pisa18$DIRINS <- ave(dirins_sc, school.id, FUN = function(x) mean(x, na.rm = TRUE))

# Adaptive instruction,  # 1 = never or hardly ever,..4 = every lesson
adaptive1  <- IMPDAT$ST212Q01HA  # The teacher adapts the lesson to my class’s needs and knowledge.
adaptive2  <- IMPDAT$ST212Q02HA  # The teacher provides individual help when a student has difficulties understanding a topic or task.
adaptive3  <- IMPDAT$ST212Q03HA  # The teacher changes the structure of the lesson on a topic that most students find difficult to understand.

adaptive <- data.frame(adaptive1,adaptive2,adaptive3)
mean.adaptive <- apply(adaptive,FUN = mean, MARGIN = 1, na.rm = TRUE)
adaptive_sc <- scale(mean.adaptive)[,1]
pisa18$ADAPTIVE <- ave(adaptive_sc, school.id, FUN = function(x) mean(x, na.rm = TRUE))

# Teacher enthusiasm, strongly disagree = 1, strongly agree = 4
enthus1 <- IMPDAT$ST213Q01HA # It was clear to me that the teacher liked teaching us.
enthus2 <- IMPDAT$ST213Q02HA # The enthusiasm of the teacher inspired me. 
enthus3 <- IMPDAT$ST213Q03HA # It was clear that the teacher likes to deal with the topic of the lesson.
enthus4 <- IMPDAT$ST213Q04HA # The teacher showed enjoyment in teaching.

enthus <- data.frame(enthus1,enthus2,enthus3,enthus4)
mean.enthus <- apply(enthus,FUN = mean, MARGIN = 1, na.rm = TRUE)
enthus_sc <- scale(mean.enthus)[,1]
pisa18$ENTHUS <- ave(enthus_sc, school.id, FUN = function(x) mean(x, na.rm = TRUE))

# Hindered learning 1
# Is your school’s capacity to provide instruction hindered
# by any of the following issues?
# 1 = not at all, 2 = very little, 3 = to some extent 4 = a lot
# STAFFSHORT: staff_lack, poor_staff, ass_staff_lack, poor_ass_staff
# 
staff_lack     <- IMPDAT$SC017Q01NA # A lack of teaching staff.
poor_staff     <- IMPDAT$SC017Q02NA # Inadequate or poorly qualified teaching staff.
ass_staff_lack <- IMPDAT$SC017Q01NA # A lack of teaching staff.
poor_ass_staff <- IMPDAT$SC017Q02NA # Inadequate or poorly qualified teaching staff.

# EDUSHORT: pisa18$mat_lack,pisa18$poor_mat, pisa18$infra_lack, pisa18$poor_infra

#  1 = not at all, 2 = very little, 3 = to some extent 4 = a lot
mat_lack   <- IMPDAT$SC017Q05NA # A lack of educational material (e.g. textbooks, IT equipment, library or laboratory material).
poor_mat   <- IMPDAT$SC017Q06NA # Inadequate or poor quality educational material (e.g. textbooks, IT equipment, library or laboratory material).
infra_lack <- IMPDAT$SC017Q07NA # A lack of physical infrastructure (e.g. building, grounds, heating/cooling, lighting and acoustic systems).
poor_infra <- IMPDAT$SC017Q08NA # Inadequate or poor quality physical infrastructure (e.g. building, grounds, heating/cooling, lighting an.

staffshort.f <- data.frame(staff_lack, poor_staff)# ass_staff_lack, poor_ass_staff)
mean.staffshort <- apply(staffshort.f,FUN = mean, MARGIN = 1, na.rm = TRUE)
staffshort_sc <- scale(mean.staffshort)[,1]
pisa18$STAFFLACK <- as.numeric(staffshort_sc > 0.0)

edushort.f <- data.frame(mat_lack, poor_mat, infra_lack, poor_infra)# ass_staff_lack, poor_ass_staff)
mean.edushort <- apply(edushort.f,FUN = mean, MARGIN = 1, na.rm = TRUE)
edushort_sc <- scale(mean.edushort)[,1]
pisa18$EDULACK <- as.numeric(edushort_sc > 0.0)

# Competing school, 1 = There are two or more other schools in this area that compete for our students. 
# 2 = There is one other school in this area that competes for our students., 
# 3 = There are no other schools in this area that compete for our students.
pisa18$COMP_SC <- IMPDAT$SC011Q01TA

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

pisa18$BULL <- ave(bul.yes, school.id, FUN = function(x) mean(x, na.rm = TRUE))

# SENSE OF BELONGING # 1 = Strongly, 2= agree, 3 = disagree, agree, 4 = Strongly disagree
belong1 <- IMPDAT$ST034Q01TA     # I feel like an outsider (or left out of things) at school.
belong2 <- 5 - IMPDAT$ST034Q02TA # I make friends easily at school.
belong3 <- 5 - IMPDAT$ST034Q03TA # I feel like I belong at school
belong4 <- IMPDAT$ST034Q04TA     # I feel awkward and out of place in my school.
belong5 <- 5 - IMPDAT$ST034Q05TA # Other students seem to like me.
belong6 <- IMPDAT$ST034Q06TA     # I feel lonely at school.

belong.f <- data.frame(belong1, belong2, belong3, belong4, belong5 , belong6)
mean.belong <- apply(belong.f,FUN = mean, MARGIN = 1, na.rm = TRUE)
belong_sc <- scale(mean.belong)[,1]
pisa18$BELONG <- ave(belong_sc, school.id, FUN = function(x) mean(x, na.rm = TRUE))

# p. 79, participation
part1  <- IMPDAT$ST062Q01TA # I <skipped> a whole school day.
part2  <- IMPDAT$ST062Q02TA # I <skipped> some classes.
part3  <- IMPDAT$ST062Q03TA # I arrived late for school. (not included)

st_attendance <- data.frame(part1,part2)
mean.st_attendance <- apply(st_attendance,FUN = mean, MARGIN = 1, na.rm = TRUE)
st_attendance_sc <- scale(mean.st_attendance)[,1]
pisa18$ATT <- ave(st_attendance_sc, school.id, FUN = function(x) mean(x, na.rm = TRUE))

# self-efficacy, 1  Strongly disagree, 4  Strongly agree
self_eff1  <- IMPDAT$ST188Q01HA  # I usually manage one way or another.
self_eff2  <- IMPDAT$ST188Q02HA  # I feel proud that I have accomplished things.
self_eff3  <- IMPDAT$ST188Q03HA  # I feel that I can handle many things at a time.
self_eff4  <- IMPDAT$ST188Q06HA  # My belief in myself gets me through hard times.
self_eff5  <- IMPDAT$ST188Q07HA  # When I'm in a difficult situation, I can usually find my way out of it.

self_eff <- data.frame(self_eff1,self_eff2,self_eff3,self_eff4,self_eff5)
mean.self_eff <- apply(self_eff,FUN = mean, MARGIN = 1, na.rm = TRUE)
self_eff_sc <- scale(mean.self_eff)[,1]
pisa18$RES <- ave(self_eff_sc, school.id, FUN = function(x) mean(x, na.rm = TRUE))

# enjoyment of reading, 1 = Strongly disagree, 4 = Strongly agree

# enjoyment of reading, high values indicate a positive attitude towards reading
joy_read1 <- 5 - IMPDAT$ST160Q01IA # I read only if I have to. 
joy_read2 <- IMPDAT$ST160Q02IA     # Reading is one of my favorite hobbies.
joy_read3 <- IMPDAT$ST160Q03IA     # I like talking about books with other people.
joy_read4 <- 5 - IMPDAT$ST160Q04IA # For me, reading is a waste of time.
joy_read5 <- 5 - IMPDAT$ST160Q05IA # I read only to get information that I need.

joy_read <- data.frame(joy_read1,joy_read2,joy_read3,joy_read4,joy_read5)
mean.joy_read <- apply(joy_read,FUN = mean, MARGIN = 1, na.rm = TRUE)
joy_read_sc <- scale(mean.joy_read)[,1]
pisa18$JOYREAD <- ave(joy_read_sc, school.id, FUN = function(x) mean(x, na.rm = TRUE))

# learning goals, 1 = Not at all true of me ...5 = Extremely true of me

goal1 <- IMPDAT$ST208Q01HA # My goal is to learn as much as possible.
goal2 <- IMPDAT$ST208Q02HA # My goal is to completely master the material.
goal3 <- IMPDAT$ST208Q04HA # My goal is to understand the content of my classes as thoroughly as possible. 

goal.f <- data.frame(goal1, goal2, goal3)
mean.goal <- apply(goal.f,FUN = mean, MARGIN = 1, na.rm = TRUE)
goal_sc <- scale(mean.goal)[,1]
pisa18$GOAL <- ave(goal_sc, school.id, FUN = function(x) mean(x, na.rm = TRUE))

# Value of school, 1 = Strongly agree,... 4 = Strongly disagree
value1  <- IMPDAT$ST036Q05TA  # Trying hard at school will help me get a good job. 
value2  <- IMPDAT$ST036Q06TA  # Trying hard at school will help me get into a good <college>. 
value3  <- IMPDAT$ST036Q08TA  # Trying hard at school is important.

value.f <- data.frame(value1, value2, value3)
mean.value <- apply(value.f ,FUN = mean, MARGIN = 1, na.rm = TRUE)
value_sc <- scale(mean.value)[,1]
pisa18$VAL <- ave(value_sc, school.id, FUN = function(x) mean(x, na.rm = TRUE))

# motivation to master tasks, 1  Strongly disagree, 4  Strongly agree

mot1  <- IMPDAT$ST182Q03HA  # I find satisfaction in working as hard as I can. 
mot2  <- IMPDAT$ST182Q04HA  # Once I start a task, I persist until it is finished.
mot3  <- IMPDAT$ST182Q05HA  # Part of the enjoyment I get from doing things is when I improve on my past performance.
mot4  <- IMPDAT$ST182Q06HA  # If I am not good at something, I would rather keep struggling to 
                                   # master it than move on to something I may be good at.
mot.f <- data.frame(mot1, mot2, mot3)
mean.mot <- apply(mot.f ,FUN = mean, MARGIN = 1, na.rm = TRUE)
mot_sc <- scale(mean.mot)[,1]
pisa18$MOT <- ave(mot_sc, school.id, FUN = function(x) mean(x, na.rm = TRUE))

# Meaning in life, 1 = Strongly disagree,... 4 = Strongly agree
mean1  <- IMPDAT$ST185Q01HA  # My life has clear meaning or purpose. 
mean2  <- IMPDAT$ST185Q02HA  # I have discovered a satisfactory meaning in life.
mean3  <- IMPDAT$ST185Q03HA  # I have a clear sense of what gives meaning to my life.

mean.f <- data.frame(mean1, mean2, mean3)
mean.mean <- apply(mean.f ,FUN = mean, MARGIN = 1, na.rm = TRUE)
mean_sc <- scale(mean.mean)[,1]
pisa18$MEAN <- ave(mean_sc, school.id, FUN = function(x) mean(x, na.rm = TRUE))


# motivational and metacognitive variables
# SCREADCOMP # Self-concept of reading: Perception of competence
# # 1 = Strongly disagree...4 = Strongly agree
screadcomp1 <- IMPDAT$ST161Q01HA # I am a good reader.
screadcomp2 <- IMPDAT$ST161Q02HA # I am able to understand difficult texts.
screadcomp3 <- IMPDAT$ST161Q03HA # I read fluently.


screadcomp.f <- data.frame(screadcomp1, screadcomp2,  screadcomp3)
screadcomp.mean <- apply(screadcomp.f,FUN = mean, MARGIN = 1, na.rm = TRUE)
screadcomp_sc <- scale(screadcomp.mean)[,1]
pisa18$COMP <- ave(screadcomp_sc, school.id, FUN = function(x) mean(x, na.rm = TRUE))

# SCREADDIFF # Self-concept of reading: Perception of difficulty
# # 1 = Strongly disagree...4 = Strongly agree
screaddiff1 <- IMPDAT$ST161Q06HA # I have always had difficulty with reading.
screaddiff2 <- IMPDAT$ST161Q07HA # I have to read a text several times before completely understanding it.
screaddiff3 <- IMPDAT$ST161Q08HA # I find it difficult to answer questions about a text.

screaddiff.f <- data.frame(screaddiff1, screaddiff2,  screaddiff3)
screaddiff.mean <- apply(screaddiff.f,FUN = mean, MARGIN = 1, na.rm = TRUE)
screaddiff_sc <- scale(screaddiff.mean)[,1]
pisa18$DIFF <- ave(screaddiff_sc, school.id, FUN = function(x) mean(x, na.rm = TRUE))

uni  <- IMPDAT$ST225Q06HA # Which of the following do you expect to complete?, ISCED level 5A or 6
pisa18$UNI <- ave(uni, pisa18$school.id, FUN = function(x) mean(x, na.rm = TRUE))

# Is your school a public or a private school?
PUBLIC12 <- IMPDAT$SC013Q01TA # 1 = public, 2 = private
pisa18$PUBLIC <- rep(1, length(PUBLIC12))
pisa18$PUBLIC[PUBLIC12 == 2] <- 0

# big city
# Which of the following definitions best describes the community in which your school is located?
pisa18$RUR <- IMPDAT$SC001Q01TA # 1 = < 3000, 2 = 3000 to 15 000, 3 = 15 000 to 100000, 4 = 100000 to 1 000 000, 5 = more than 1 000 000

#As of <February 1, 2018>, what was the total school enrolment (number of students)?

MALES <- IMPDAT$SC002Q01TA
FEMS <- IMPDAT$SC002Q02TA
# Fraction of female students
pisa18$FEMFRAC <- FEMS/(FEMS + MALES)

pisa18$NN_SC <- IMPDAT$SC048Q01NA/100 # Students whose <heritage language> is different from <test language>
pisa18$DISH  <- IMPDAT$SC048Q03NA/100 # Students from socioeconomically disadvantaged homes
pisa18$SN_SC <- IMPDAT$SC048Q02NA/100 # Students with special needs

pisa18$DISHOME <- as.numeric(cut_number(pisa18$DISH,2)) - 1

pisa18$FAILED <- IMPDAT$SC164Q01HA/100 # n the last full academic year, what proportion of 
# students in your school’s final grade left school without 


# GFOFAIl
# 1 = Strongly disagree...4 = Strongly agree
gfofail1 <- IMPDAT$ST183Q01HA # When I am failing, I worry about what others think of me.
gfofail2 <- IMPDAT$ST183Q02HA # When I am failing, I am afraid that I might not have enough talent.
gfofail3 <- IMPDAT$ST183Q03HA # When I am failing, this makes me doubt my plans for the future.

gfofail.f <- data.frame(gfofail1, gfofail2, gfofail3)
gfofail.mean <- apply(gfofail.f,FUN = mean, MARGIN = 1, na.rm = TRUE)
gfofail_sc <- scale(gfofail.mean)[,1]
pisa18$FOF <- ave(gfofail_sc, school.id, FUN = function(x) mean(x, na.rm = TRUE))

# parents, enjoyment of reading, 1  Strongly disagree, 4  Strongly agree
# enjoyment of reading, high values indicate a positive attitude towards reading
joyp_read1 <- 5 - IMPDAT$PA158Q01HA # I read only if I have to. 
joyp_read2 <- IMPDAT$PA158Q02IA     # Reading is one of my favorite hobbies.
joyp_read3 <- IMPDAT$PA158Q03HA     # I like talking about books with other people.
joyp_read4 <- 5 - IMPDAT$PA158Q04IA # For me, reading is a waste of time.
joyp_read5 <- 5 - IMPDAT$PA158Q05HA # I read only to get information that I need.

joyp_read <- data.frame(joy_read1,joy_read2,joy_read3,joy_read4,joy_read5)
mean.joyp_read <- apply(joyp_read,FUN = mean, MARGIN = 1, na.rm = TRUE)
joyp_read_sc <- scale(mean.joyp_read)[,1]
pisa18$JOYPREAD <- ave(joyp_read_sc, school.id, FUN = function(x) mean(x, na.rm = TRUE))