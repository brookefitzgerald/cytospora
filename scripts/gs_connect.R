library(googlesheets4)

add_data_to_data_store <- function(data_to_add, settings_or_results_db='settings'){
  sheet_id <- case_when(
    # Note that this accesses the local and production data from the .Rprofile file 
    # (not in file management for security reasons)
    getOption("env")=="production" ~ getOption("prod_google_sheet_id")[[settings_or_results_db]],
    getOption("env")=="local"      ~ getOption("local_google_sheet_id")[[settings_or_results_db]]
  )
  
  if (gs4_has_token()==TRUE){
    sheet_append(sheet_id, data_to_add)
  }
  else {
    tryCatch(
      {
        gs4_auth(
          email = gargle::gargle_oauth_email(),
          path = NULL,
          scopes = "https://www.googleapis.com/auth/spreadsheets",
          cache = gargle::gargle_oauth_cache(), # cache points to a file path in .Rprofile with the manually generated Oauth token
          use_oob = gargle::gargle_oob_default(),
          token = NULL
        )
        sheet_append(sheet_id, data_to_add)
      },
      error=errorCondition("Authentication failed, or other error"),
      finally =  print("Authentication succeeded.")
    )
  }
}
