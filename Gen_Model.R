library('haven')
library('msm')
setwd('C://Users/kevin/Desktop/Code/Markov_Debilitating/')
data<-read_dta('finaldata.dta') #读取基础数据，数据是由'ID','time','state'组成的。

Q_init <- matrix(c(    #初始速率矩阵，填写估计值。
  -0.1, 0.09, 0.0, 0.01,   # state 1
  0.04, -0.1, 0.05, 0.01,   # state 2
  0.00, 0.05, -0.1, 0.05,   # state 3
  0.0, 0.0, 0.0, 0.0      # state 4，all zero(终止状态)
), byrow = TRUE, nrow = 4)

data[c("age", "gender","education","marryandlive","rural","sleep","fall","depsymptom","num_co","hyper","diabe","cancer","digeste","cardio","lunge","asthmae","memrye","stroke","satlife","bmigroup")] <- lapply(data[c("age", "gender","education","marryandlive","rural","sleep","fall","depsymptom","num_co","hyper","diabe","cancer","digeste","cardio","lunge","asthmae","memrye","stroke","satlife","bmigroup")], factor)#将data中的列转化为因子
data$age <- relevel(data$age, ref = "0")
data$gender <- relevel(data$gender, ref = "0")
data$education <- relevel(data$education, ref = "0")
data$marryandlive <- relevel(data$marryandlive, ref = "0")
data$rural <- relevel(data$rural, ref = "0")
data$sleep <- relevel(data$sleep, ref = "0")
data$fall <- relevel(data$fall, ref = "0")
data$depsymptom <- relevel(data$depsymptom, ref = "0")
data$num_co <- relevel(data$num_co, ref = "0")
data$hyper <- relevel(data$hyper, ref = "0")       #设定各个协变量的参考值
data$diabe <- relevel(data$diabe, ref = "0")
data$cancer <- relevel(data$cancer, ref = "0")
data$digeste <- relevel(data$digeste, ref = "0")
data$cardio <- relevel(data$cardio, ref = "0")
data$lunge <- relevel(data$lunge, ref = "0")
data$asthmae <- relevel(data$asthmae, ref = "0")
data$memrye <- relevel(data$memrye, ref = "0")
data$stroke <- relevel(data$stroke, ref = "0")
data$satlife <- relevel(data$satlife, ref = "0")
data$bmigroup <- relevel(data$bmigroup, ref = "0")
covariates <- c("age", "gender", "education", "marryandlive", "rural", "sleep", "fall", "depsymptom", 
                "num_co", "hyper", "diabe", "cancer", "digeste", "cardio", "lunge", "asthmae", 
                "memrye", "stroke", "satlife", "bmigroup")
# 初始化空数据框
result_df <- data.frame()

for (cov in covariates) {
cov_formula <- as.formula(paste("~", cov)) 
model <- msm(state ~ time, subject = ID, data = data,covariates = cov_formula
,qmatrix = Q_init, method = "CG", death = 4,control = list(fnscale = 500, maxit = 100000))

msummary<-summary(model) #生成模型总结

  for (varname in names(msummary$hazard)) {
    var_matrix <- msummary$hazard[[varname]]
    transitions <- rownames(var_matrix)
    HR <- var_matrix[, "HR"]
    CI_lower <- var_matrix[, "L"]
    CI_upper <- var_matrix[, "U"]
    
    # 构建数据框并格式化数值为普通十进制格式
    temp_df <- data.frame(
      Variable = varname,
      Transition = transitions,
      HR = sprintf("%.4f", HR),
      CI_Lower = sprintf("%.4f", CI_lower),
      CI_Upper = sprintf("%.4f", CI_upper)
    )
    
    # 合并进总表
    result_df <- rbind(result_df, temp_df)
  }
}
print(result_df)
write.table(result_df, file = "Results.txt", sep = "\t", row.names = TRUE, col.names = NA, quote = FALSE)