# copyright from airbnb winner's scripts
Folds <- 10
glm_cv <- createFolds(1:nrow(df_train), k = Folds)
glm_stack <- data.frame()
glm_test_stack <- data.frame()
for(i in 1:Folds){ 
  # i <- 1
  set.seed(123 * i)
#   glm_id_ <- df_train[-glm_cv[[i]], ]
  # X19_fold_id_ <- X19_id[glm_cv[[i]], ]
  glm_sp_ <- df_train[-glm_cv[[i]], !colnames(df_train)%in%c("TARGET")]
  glm_fold_sp_ <- df_train[glm_cv[[i]], !colnames(df_train)%in%c("TARGET")]
  glm_y_ <- df_train[-glm_cv[[i]],c("TARGET")]
  glm_fold_y_ <- df_train[glm_cv[[i]],c("TARGET")]
  
  cvfit = glmnet::cv.glmnet(x = as.matrix(glm_sp_), y = as.matrix(glm_y_), type.measure = "auc", nfolds = 5, family="binomial")
  saveRDS(cvfit, paste0("cache/",folder,"/glm_",i,"_cvfit.RData"))
  print(min(sqrt(cvfit$cvm)))
  
  y19_fold_ <- predict(cvfit, newx = X19_fold_sp_, s = "lambda.min")
  
  y19_fold_df <- data.frame(glm_dfb_tfa_lag_pred = y19_fold_[, 1])
  y19_fold_df <- mutate(y19_fold_df,
                        id = unique(X19_fold_id_$id))
  y19_fold_df <- y19_fold_df[c("id", "glm_dfb_tfa_lag_pred")]
  glm_stack <- bind_rows(glm_stack,
                         y19_fold_df)
  
  y19_test_ <- predict(cvfit, newx = X19_test_sp, s = "lambda.min")
  y19_test_df <- data.frame(glm_dfb_tfa_lag_pred = y19_test_[, 1])
  y19_test_df <- mutate(y19_test_df,
                        id = unique(X19_test_id$id))
  y19_test_df <- y19_test_df[c("id", "glm_dfb_tfa_lag_pred")]
  glm_test_stack <- bind_rows(glm_test_stack,
                              y19_test_df)
}

glm_test_stack <- glm_test_stack %>%
  group_by(id) %>%
  summarise_each(funs(mean))

glm_stack <- melt.data.table(as.data.table(glm_stack))
glm_stack <- data.frame(glm_stack)
names(glm_stack) <- c("id", "feature", "value")

glm_test_stack <- melt.data.table(as.data.table(glm_test_stack))
glm_test_stack <- data.frame(glm_test_stack)
names(glm_test_stack) <- c("id", "feature", "value")

saveRDS(glm_stack, paste0("cache/",folder,"/glm_stack.RData"))
saveRDS(glm_test_stack, paste0("cache/",folder,"/glm_test_stack.RData"))
gc()