check_trans <- function(x) {
  # Find a way to solve object 'no_trans' not found
  print(x)
  trans_msg <- "`transform` should have two functions: 'func' and 'inv'"
  if(length(x) != 2) {
    stop(trans_msg, call. = FALSE)
  } else {
    if(!all(sort(names(x)) == c("func", "inv")))
      stop(trans_msg, call. = FALSE)
    if(!all(is.function(transform)))
      stop(trans_msg, call. = FALSE)
  }
  invisible(x)
}

avg_es <- function(object, ...){
  as.data.frame(object$stan , pars = "b_")[,1]
}

extract.model <- function(object, ...){
  object$stan
}


extract_par <- function(object, logHR = T, ...){

  if(logHR){
    object$SE <- as.double(gsub(".*\\(|)","", object$logHR) )

    object$logHR <- as.double(gsub(" \\(.*","", object$logHR) )
  }else{
    object$LL <- as.double(gsub(".*\\[ |, .*","", object$HR) )

    object$UL <- as.double(gsub(".*, | \\]$","",object$HR) )

    object$HR <- as.double(gsub(" \\[.*","", object$HR) )
  }



  object

}
