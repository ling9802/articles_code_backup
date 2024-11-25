# t.test的函数
perform_t_test <- function(x) {
  ambient <- x$`SUE_Van`[x$Warm == "Ambient"]
  warming <- x$`SUE_Van`[x$Warm == "Warming"]
  test_result <- t.test(warming, ambient)
  return(c(test_result$statistic, test_result$p.value, test_result$conf.int))
}

# 计算tillage Hedges' g 及其置信区间的函数
calculate_hedges_g_tillage <- function(data) {
  TN_values <- data$Value[data$Treat == "TN"]
  TW_values <- data$Value[data$Treat == "TW"]
  
  result <- esc_mean_sd(
    grp2m = mean(TN_values), grp1sd = sd(TN_values), grp1n = length(TN_values),
    grp1m = mean(TW_values), grp2sd = sd(TW_values), grp2n = length(TW_values),
    es.type = "g"
  )
  
  return(c(Hedges_g = result$es, CI_Lower = result$ci.lo, CI_Upper = result$ci.hi))
}

# 计算no_tillage Hedges' g 及其置信区间的函数
calculate_hedges_g_notillage <- function(data) {
  TN_values <- data$Value[data$Treat == "NN"]
  TW_values <- data$Value[data$Treat == "NW"]
  
  result <- esc_mean_sd(
    grp2m = mean(TN_values), grp1sd = sd(TN_values), grp1n = length(TN_values),
    grp1m = mean(TW_values), grp2sd = sd(TW_values), grp2n = length(TW_values),
    es.type = "g"
  )
  
  return(c(Hedges_g = result$es, CI_Lower = result$ci.lo, CI_Upper = result$ci.hi))
}


#' rrnDBcorrectOTU
rco = function(otu,classifer,rrnDB){
  
  hang1 = length(rownames(otu))
  #rrnDB只到class;
  #a为genus,family,order,class
  #b比a低一级
  #从genus到class,即从低到高遍历levels
  a= c(6,5,4,3)
  b= c(7,6,5,4)
  levels= c("genus","family","order","class")
  whole.res = c()
  
  for (q in 1:4){  #q=1
    spe = classifer[(classifer[,a[q]] != "Unclassified") &  (classifer[,b[q]] == "Unclassified"),]
    rspe = rrnDB[ rrnDB[,2]==levels[q],c(3,9)]
    
    name.spe = rownames(spe)
    name.otu = rownames(otu)
    name.match = match(name.spe,name.otu)
    match.otu = otu[name.match,]
    #注释信息加到otu中
    otu.spe = cbind(spe[,a[q]],match.otu)
    
    ##############################
    #用for 循环遍历
    #规定OTU中有而数据库中没有的为-1
    whole = data.frame(matrix(data=NA,
                              nrow=length(rownames(otu.spe)),
                              ncol=length(colnames(otu.spe))),stringsAsFactors=FALSE)
    
    for (i in 1:length(rownames(otu.spe))){  #i=1
      mat = match(as.character(otu.spe[i,1]),as.character(rspe[,1]))
      if(!is.na(mat)){
        each = cbind(rspe[mat,2],otu.spe[i,-1])
      } else{
        each = cbind(-1,otu.spe[i,-1])
      }
      whole[i,] = each
    }
    #加上物种信息
    spe.res = cbind(taxa=otu.spe[,1],level=rep(levels[q],length(rownames(otu.spe))),whole)
    rownames(spe.res) = rownames(otu.spe)
    
    #输出到whole.res
    whole.res = rbind(whole.res,spe.res)
    ##############每一个level做完之后都需要将已做过的OTU去掉。加快后面的速度。
    ##############最重要的是避免“有物种信息；unclassifiered；有物种信息”这种情况出现。保证有信息的是最小的level，结果最准确。
    diffotu = setdiff(rownames(otu),rownames(otu.spe))
    otu = otu[diffotu,]
  }
  whole.res = whole.res[complete.cases(whole.res),]
  colnames(whole.res) = c("taxa","level","CopyNumber",colnames(otu))
  
  #检验一下whole.res是否和OTU表的行数一样
  #结果的行数+属水平数据库空缺的行数+class做完之后还剩下来的OTU
  hang2 = length(rownames(whole.res))+length(rownames(otu))
  
  if (hang1==hang2){
    print("Well done!")}
  
  #######################OTU丰度除以拷贝数，得到校正的表格
  #先去掉第三列为-1的值
  simply.res = whole.res[whole.res[,3]!= -1,]
  correct = simply.res[,-c(1:3)] / simply.res[,3]
  correct.table = as.data.frame(cbind(simply.res[,1:2],correct))
  
  list(whole.res=whole.res,correct.table=correct.table)
}


partialCor <- function(data = test_data, x = NULL,
                      y, method= "spearman"){
  require(ppcor)
  require(tidyverse)
  
  dat<- as.data.frame(data[, -which(colnames(data) %in% c(x , y))])
  if(ncol(dat) <= 1) {
    warning("Small number of remained variable may causes an invalid var1 name")
    colnames(dat) = colnames(data)[-match(c(x,y),colnames(data))]
  }
  dat1<- data.frame()
  
  if(is.null(x) == T){
    for(i in 1:ncol(dat)){
      for(j in 1:ncol(dat)){
        if(i == j) next
        dat2 <- pcor.test(dat[[j]], data[,y], dat[[i]], method = method) %>% 
          as_tibble() %>% 
          mutate(var2 = y,
                 var1 = names(dat[j]),
                 control_var = names(dat[i]),
                 signif = ifelse(p.value< 0.001, "***",
                                 ifelse(p.value< 0.01, "**",
                                        ifelse(p.value< 0.05, "*", "")))) %>%
          dplyr::select(var1,var2,
                        control_var,estimate,
                        p.value,signif,everything())
        
        dat1<- rbind(dat1,dat2)
      }
    }
  }
  else{
    for(i in 1:ncol(dat)){
      dat2 <- pcor.test(dat[[i]], data[,y], data[,x], method = method) %>% 
        as_tibble() %>% 
        mutate(var2 = y,
               var1 = names(dat[i]),
               control_var = str_c(x, collapse = "-"),
               signif = ifelse(p.value< 0.001, "***",
                               ifelse(p.value< 0.01, "**",
                                      ifelse(p.value< 0.05, "*", "")))) %>%
        dplyr::select(var1,var2,
                      control_var,estimate,
                      p.value,signif,everything())
      
      dat1<- rbind(dat1,dat2)
    }
  }
  dat1
}

alpha_diversity <- function(x, tree = NULL) {
  # Richness指数，又称observed species指数
  observed_species <- estimateR(x)[1, ]
  
  # Chao 1指数
  Chao1 <- estimateR(x)[2, ]
  
  # ACE指数
  ACE <- estimateR(x)[4, ]
  
  # Shannon指数
  Shannon <- diversity(x, index = 'shannon', base = 2)
  
  # Gini-Simpson指数
  Simpson <- diversity(x, index = 'simpson')
  
  # goods_coverage指数
  goods_Coverage <- 1 - rowSums(x == 1) / rowSums(x)
  
  # 保留四位小数
  Shannon <- sprintf("%0.4f", Shannon)
  Simpson <- sprintf("%0.4f", Simpson)
  goods_Coverage <- sprintf("%0.4f", goods_Coverage)
  
  # 结果整合到数据框中
  result <- data.frame(observed_species, ACE, Chao1, Shannon, Simpson, goods_Coverage)
  
  # 如果提供了树，则计算PD_whole_tree
  if (!is.null(tree)) {
    PD_whole_tree <- pd(x, tree, include.root = FALSE)[, 1]
    result <- cbind(result, PD_whole_tree = PD_whole_tree)
  }
  
  return(result)
}


