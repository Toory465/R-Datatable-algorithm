## This function that help to merge two datatables without being worried about the duplicate columns

#' Title
#'
#' @param dt1 the first data table 
#' @param dt2 the second data table 
#' @param by_cols 
#' @param all_x 
#' @param all_y 
#' @param get_shared_columns_from 
#'
#' @return
#' @export
#'
#' @examples
func_merge <- function(dt1, 
                       dt2,
                       by_cols,
                       all_x = FALSE,
                       all_y = FALSE,
                       get_shared_columns_from = "left"){
  
  
  if (tolower(get_shared_columns_from) == "right"){
    selected_cols <- c(by_cols, setdiff(names(dt1), names(dt2)))
    dt1 <- dt1[, ..selected_cols]
  } else {
      selected_cols <- c(by_cols, setdiff(names(dt2), names(dt1)))
      dt2 <- dt2[, ..selected_cols]
  }
  
  result <- merge(dt1, 
                  dt2, 
                  by = by_cols,
                  all.x = all_x,
                  all.y = all_y)
  return(result)
  
}


########### EOF ###############################


# test_that("the merge function works fine", {
#   
#   
#   data_table_1 <- data.table(a = c(1,2,3), b = c(1, 5, 2))
#   data_table_2 <- data.table(b = c(1,2), c = c(6, 7))
#   
#   result <- func_merge(dt1 = data_table_1,
#              dt2 = data_table_2,
#              by_cols = "b")
# 
#   expect_that(nrow(result), equals(2) ) 
# })