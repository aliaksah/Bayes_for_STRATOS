# https://search.r-project.org/CRAN/refmans/HuraultMisc/html/post_pred_pval.html
# 
# posterior checks, two-sided


pppv_Stack <- two.sided.pppv(y.obs = pisa18$ld, yrepM = ypredStack, alternative = 
                               "two.sided")
num<-2
sink(file = file.path("Results/pppv_Stack.txt"))
for(i in 1:num){
  print(paste("Table   ",i))
  print(kable(pppv_Stack[[i]], format = "latex", digits = 2))
  print("                 ")
}
sink(file=NULL)




