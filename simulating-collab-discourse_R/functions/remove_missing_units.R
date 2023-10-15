remove_missing_units = function(res,missing,type){
  #browser()
  if(type == "sim"){
    if(is.null(missing)){
      res = res
    }else{
      if (length(missing) >= 1){
        miss.id = which(names(res) %in% missing)
        if(length(miss.id) == 0){
          res = res
        }else{
          res = res[-miss.id]
        }
      }
      else{
        res = res
      }
    }
  }else{
    if(is.null(missing)){
      res = res
    }else{
      if (length(missing) >= 1){
        miss.id= which(rownames(res) %in% missing)
        if(length(miss.id) == 0){
          res = res
        }else{
          res = res[-miss.id,]
        }
      }
      else{
        res = res
      }
    }
  }
  return(res)
}
