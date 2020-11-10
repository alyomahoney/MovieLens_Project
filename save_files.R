# all plots saved using ggsave with width of 8 and height of 5

# needed for rmarkdown
save(edx_str,
     file = "rmd_files/rda/edx_str.rda")
save(lambda_m, lambda_u, opt_p,
     file = "rmd_files/rda/opt_lambdas.rda")
save(mu, mu_train, predictions_2, predictions_3, predictions_4, predictions_5, predictions_6, predictions_7, predictions_8, predictions_final,
     file = "rmd_files/rda/predictions.rda") 
save(top_five, bottom_five, top_five_reg, bottom_five_reg,
     file = "rmd_files/rda/reg_motivation.rda")
save(rmse_results, final_results,
     file = "rmd_files/rda/rmse_results.rda") 
save(rmse_1, rmse_2, rmse_3, rmse_4, rmse_5, rmse_6, rmse_7, rmse_8, rmse_final,
     file = "rmd_files/rda/rmse_values.rda")



# not needed for rmarkdown
save(edx,
     file = "rda/edx.rda")
save(validation,
     file = "rda/validation.rda")
save(preds_ibcf, preds_ubcf,
     file = "rda/cf_preds.rda")
save(edx_train_r, edx_train_rm,
     file = "rda/cf.rda")
save(colMean_m, colMean_u, rmses_lambda_m, rmses_lambda_u, lambdas_m, lambdas_u,
     file = "rda/cv.rda")
save(train_mat,
     file = "rda/train_mat.rda")
save(col_map_edx, edx_by_genre, movie_bias, movie_bias_reg, movies, row_map_edx, user_bias, user_bias_reg, test_subset, RMSE,
     file = "rda/extra.rda")
save(edx_train, edx_test,
     file = "rda/edx_tt.rda")
save(proportions, ens, prop_df,
     file = "rda/ensemble.rda")
save(movie_bias_reg_final, user_bias_reg_final, edx_mat, edx_rm, row_map_edx_final, col_map_edx_final, validation_subset,
     file = "rda/final.rda")
save(preds_ubcf_final, preds_ibcf_final, predictions_u, predictions_i, predictions_final,
     file = "rda/final_preds.rda")
