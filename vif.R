

# df=read.csv("neData.csv")
# head(df)
# col=c()
# 
# for(var in colnames(df)){
#   if(!grepl("_",var)){
#     col=c(col,var)
#   }
# }





# df=df[,col]
# df=df[,!colnames(df) %in% c("X","Zip","X.1")]
# 
# df_noise=df
# df_noise$noise=df_noise$glucose*df_noise$pressure
# df_noise$noise2=df_noise$insulin*df_noise$age
# 
# colnames(df)
# target=c("diabetes")
# num_var_list=colnames(df)[!colnames(df) %in% target]
# num_var_list1=colnames(df_noise)[!colnames(df_noise) %in% target]


# impute_base=function(var_list,df){
#   return 0
# }

recursion_vif_func=function(var_list,df,threshold){
  var_list=var_list
  df=df[,var_list]
  df=df[complete.cases(df),]
  vifs=c()
  
  for(var in var_list){
    eval(parse(text=paste0("vif_mod=lm(",var,"~.,data=df)")))
    r2=summary(vif_mod)$r.squared
    vif=1/(1-r2)
 
    vifs=c(vifs,vif)
    
  }
  vif_df=data.frame("Variable"=var_list[order(vifs)],"VIF"=vifs[order(vifs)])

  # vif_df=cbind.data.frame("Variable"=var_list[order(vifs)],"VIF"=vifs[order(vifs)])
  if(vif_df[dim(vif_df)[1],2]>=threshold){
    new_var_list=var_list[!var_list %in% vif_df[dim(vif_df)[1],1]]
    recursion_vif_func(new_var_list,df,threshold)
  }else{
  

  return(vif_df)
  }
  
  
}

vif_func=function(var_list_orig,df,threshold){
  for(var in var_list_orig){
    df[,var]=as.numeric(df[,var])
  }
  vif_df=recursion_vif_func(var_list_orig,df,threshold)
  accepted_cols=as.vector(vif_df[,1])
  removed_cols=setdiff(var_list_orig,accepted_cols)
  return(list("vif_df"=vif_df,"accpeted_cols"=accepted_cols,"removed_cols"=removed_cols))
}


# vif_func(num_var_list1,df_noise,5)





