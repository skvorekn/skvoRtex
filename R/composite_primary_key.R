#' Find what columns a data set is unique by
#'
#' @param data Dataframe to find primary keys of
#' @param expected_primary_keys List form of columns that should identify the dataset uniquely
#'
#' @export
#' @examples 
#' data <- data.frame(a = c(1, 2, 3, 4, 5, 6),
#'                         b = c(1, 2, 2, 3, 4, 5),
#'                         c = c('a', 'b', 'b', 'c', 'd','e'),
#'                         id = c(1111, 2222, 2222, 3333, 4444, 5555))
#' nrow(data)
#' n_distinct(data$id)
#' primary_keys <- composite_primary_keys(data, c("id"))
#' print(primary_keys)
#' 
#' composite_primary_keys()

composite_primary_keys <- function(data, expected_primary_keys){
  
  ## find duplicates
  dupes <- data %>% 
    dplyr::group_by_(expected_primary_keys) %>% 
    dplyr::summarise(n=n()) %>% 
    filter(n>1)
  dupes_data <- merge(dupes, data, by = expected_primary_keys)

  # how many duplicates are there?
  distinct_ids <- dplyr::n_distinct(dupes_data[,expected_primary_keys])
  message(paste('There is/are', distinct_ids, 'duplicate observation(s) in your data, according to expected primary keys:', paste(expected_primary_keys, collapse = ", ")))
  
  is_key <- function(col){
    if(!col %in% c("n",expected_primary_keys)){
      distinct_vals <- dupes_data %>% 
        dplyr::group_by_(.dots = expected_primary_keys) %>% 
        dplyr::summarise(dist_vals = n_distinct(!!rlang::sym(col)))
      if(any(distinct_vals$dist_vals > 1)){
        dupe_cols <<- c(dupe_cols, col)
      }
    }
  }
  
  dupe_cols <<- c()
  for(col in names(dupes_data)){
    is_key(col)
  }
  
  # check that data is distinct by dupe_cols + expected_primary_keys
  if(nrow(data) == n_distinct(data[,c(expected_primary_keys, dupe_cols)])){
    message('Data is distinct by the following composite primary keys:', paste(c(expected_primary_keys, dupe_cols), collapse = ", "))
  }
  
  # return names of columns that data is distinct by  
  return(c(expected_primary_keys, dupe_cols))
  
}