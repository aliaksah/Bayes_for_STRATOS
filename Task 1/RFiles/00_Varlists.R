
## Definition of variable lists

## imputation variables
impvar <- c("PV1READ", "PV1MATH", "PV1SCIE", "AGE", "PARED", "PAREDINT",
            "ST123Q02NA", "ST123Q03NA", "ST123Q04NA", "ST004D01T", "ST022Q01TA",
            "IMMIG", "ESCS", "ST013Q01TA", 
            "REPEAT", "ST001D01T", "ST097Q01TA", "ST097Q02TA", "ST097Q03TA",
            "ST097Q04TA", "ST097Q05TA", "ST100Q01TA", "ST100Q02TA", "ST100Q03TA",
            "ST100Q04TA",  "ST104Q02NA", "ST104Q03NA",  "ST104Q04NA",
            "PERFEED", "GFOFAIL", "ST212Q01HA", "ST212Q02HA", "ST212Q03HA",
            "ST213Q01HA", "ST213Q02HA", "ST213Q03HA", "ST213Q04HA", "TEACHINT", 
            "DIRINS", "SC017Q01NA", "SC017Q02NA", "SC017Q03NA", "SC017Q04NA", "STAFFSHORT",
            "SC017Q05NA", "SC017Q06NA", "SC017Q07NA", "SC017Q08NA", "EDUSHORT",
            "STUBEHA", "SC011Q01TA", "SC061Q01TA",
            "SC061Q02TA", "SC061Q03TA", "SC061Q04TA", "SC061Q05TA", "SC061Q06TA", 
            "SC061Q07TA", "SC061Q08TA", "SC061Q09TA", "SC061Q10TA", "SC061Q11HA",
            "ST038Q03NA", "ST038Q04NA", "ST038Q05NA", "ST038Q06NA", "ST038Q07NA",
            "ST038Q08NA", "ST034Q01TA", "ST034Q02TA", "ST034Q03TA", "ST034Q04TA",
            "ST034Q05TA", "ST034Q06TA", "ST062Q01TA", "ST062Q02TA", "ST062Q03TA", 
            "ST188Q01HA", "ST188Q02HA", "ST188Q03HA", "ST188Q06HA", "ST188Q07HA", 
            "ST160Q01IA", "ST160Q02IA", "ST160Q03IA", "ST160Q04IA", "ST160Q05IA", 
            "JOYREAD", "ST208Q01HA", "ST208Q02HA", "ST208Q04HA", "MASTGOAL", 
            "ST036Q05TA", "ST036Q06TA", "ST036Q08TA", "ST182Q03HA", "ST182Q04HA", 
            "ST182Q05HA", "ST182Q06HA", "WORKMAST", "ST185Q01HA", "ST185Q02HA", 
            "ST185Q03HA", "SWBP", "ST161Q01HA", "ST161Q02HA", "ST161Q03HA", 
            "SCREADCOMP", "ST161Q06HA", "ST161Q07HA", "ST161Q08HA", "SCREADDIFF",
            "ST165Q01IA", "ST165Q02IA", "ST165Q03IA", "ST165Q04IA", "ST165Q05IA", # not included
            "METASUM", "ST164Q01IA", "ST164Q02IA", "ST164Q03IA", "ST164Q04IA", # not included
            "ST164Q05IA", "ST164Q06IA", "UNDREM", "ST225Q03HA", "ST225Q04HA", 
            "ST225Q06HA", "SC013Q01TA", "SC001Q01TA", "SC002Q01TA", "SC002Q02TA",
            "SC048Q01NA", "SC048Q02NA", "SC048Q03NA", "SC164Q01HA",
            "ST102Q01TA","ST102Q02TA","ST102Q03TA","ST102Q04TA",
            "ST183Q01HA","ST183Q02HA","ST183Q03HA", 
            "MISCED", "FISCED", "HISCED", "JOYREADP", "PA158Q01HA", "PA158Q02IA",
            "PA158Q03HA", "PA158Q04IA", "PA158Q05HA")


## model variables
modvar0 <- c("school.id","CNT")
modvar1 <- c("read_sdg","read", "female",  "escs","native")
modvar2 <- c("escs_SC", "located",  "STAFFSHORT_cnt", "EDUSHORT_cnt",
             "STUBEHA_cnt")
modvar3 <- c("z_st_attendance","JOYREAD_cnt","SCREADCOMP_cnt","SCREADDIFF_cnt","METASUM_cnt","UNDREM_cnt")
modvar4 <- c("PERFEED_cnt","TEACHINT_cnt", "DIRINS_cnt","WORKMAST_cnt","MASTGOAL_cnt","GFOFAIL_cnt")