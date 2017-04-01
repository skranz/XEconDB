data.action.proposer = function(dat) {
  prop = new.env()
  prop$dat = dat
  prop$ask = function(...) {
    ask.data.action.proposer(..., prop=prop)
  }
  prop$new.match = function(game,...) {}
  prop
}

loop.data.action.proposer = function(dat) {
  prop = new.env()
  prop$dat = dat
  prop$row = 0
  prop$ask = function(action,stage, player,game,...) {
    return(list(value=dat[[action]][prop$row], type="data"))
  }
  prop$new.match = function(player,game,...) {
    if (player==1) {
      prop$row = prop$row+1
      if (prop$row>NROW(dat)) prop$row=1
      cat("\nplay row ", prop$row)
    }
  }
  prop
}


#' Propose an action by randomly drawing an action from observed data
ask.data.action.proposer = function(action,stage, player, prop,game) {
  restore.point("ask.data.action.proposer")
  
  sfg = game$sfg
  
  know = game$know[[player]]
  dat = prop$dat

  d = filter_by_list(dat, game$values[know]) 
  if (NROW(d)>0) {
    res = sample(d[[action]],1)
    return(list(value=res, type="data"))
  }

  # If nothing found, remove freeActions from filter
  freeActions = names(sfg.variables(sfg,"freeAction"))
  know = setdiff(know, freeActions)
  d = filter_by_list(dat, game$values[know]) 
  if (NROW(d)>0) {
    res = sample(d[[action]],1)
    return(list(value=res, type="data.excluding.freeAction"))
  }
  
  
  # Finally just draw a random action
  aobj = game$act.action.obj[[player]][[action]]
  if (!is.null(aobj$set)) {
    res = sample(aobj$set,1)
    return(list(value=res, type="random"))
  } else {
    res = NULL
    return(list(value=res, type="empty"))
  }
}