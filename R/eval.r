example.my.eval.set.on.df = function() {
  call = quote(1:cake)
  T = 100000
  df = data.frame(a=1:T, cake=sample.int(6,T, replace=TRUE))
  res = eval.set.to.df(call, df,"x") %>% arrange(a)
  
}

eval.set.to.df = function(call, df, var, expand=TRUE) {
  restore.point("my.eval.set.on.df")

  if (!is(call,"call")) {
    set = as.vector(call)
    df[[var]] = replicate(NROW(df),set,simplify = FALSE)
    if (!expand) return(df)
    return(unnest_(df,var))
  }  
  #df$.ORG.ROW = seq.int(NROW(df)) 
  
  # reduce df to unique combination of used variables
  vars = find.variables(call)
  if (length(vars)==0) {
    set = eval(call)
    df[[var]] = replicate(NROW(df),set,simplify = FALSE)
    if (expand)
      df = unnest_(df,var)
    return(df)
  }
  
  sdf = as_data_frame(unique(df[,vars,drop=FALSE]))
  
  # compute set for each row of df
  sets = lapply(seq.int(NROW(sdf)), function(i) {
    eval(call,sdf[i,,drop=FALSE])
  })
  
  sdf[[var]] = sets
  
  if (expand) {
    sdf = unnest_(sdf,var)
  }
  res = right_join(df,sdf,by=vars)
  res
}


eval.on.df = function(call, df) {
  eval(call, df)
}

old.eval.set.on.df = function(set, df) {
  restore.point("eval.set.on.df")
  
  if (length(set)==0) {
    stop("Set has length 0")
    return(NULL)
  }

  # set is numeric: no need to evaluate
  if (!is.character(set)) {
    set.df = data_frame(
      .value=rep(set, times=NROW(df)),
      .row.num = rep(1:NROW(df), each=NROW(set)),
      .set.ind = rep(seq_along(set), times=NROW(df))
    )
    return(set.df)
  }
  
  call.li = lapply(set, function(el) parse(text=el,srcfile=NULL)[[1]])
  
  # Consider special cases
  if (length(set)==1) {
    # multi_size_cases
    if (identical(call.li[[1]][[1]],quote(multi_size_cases))) {
      restore.point("multi_size_cases_set")    
      res = eval(call.li[[1]],df )
      case = res$case
      vals = res$vals
      
      i = 2
      set.li = lapply(seq_along(vals), function(i) {
        rows = which(case==i)
        if (length(rows)==0) return(NULL)
        d = vals[[i]]
        set.size = NCOL(d)
        set.df = data_frame(
          .value   = as.vector(as.matrix(d[rows,])),
          .row.num = rep(rows, times=set.size),
          .set.ind = rep(1:set.size, each=length(rows))
        )
      })
      set.df = bind_rows(set.li)
      return(set.df)
    }
  }

  fixed.size = TRUE
  # The set always has the same number of elements 
  if (fixed.size) {
    li = lapply(call.li, function(call) {
      res = eval(call, df)
      if (length(res)==1) res = rep(res, NROW(df))
      res
    })
    
    
    val.mat = do.call(cbind,li) 
    val = as.vector(t(val.mat))
    
    set.df = data_frame(
      .value=val,
      .row.num = rep(1:NROW(df), each=NROW(set)),
      .set.ind = rep(seq_along(set), times=NROW(df))
    )
    return(set.df)
  }
  
  # Sets can have variable length
  if (length(set)>1) stop("Sets with more than one element must be fixed size!")
#   call = call.li[[1]] 
#   
#   as.list.call = substitute(as.list(call), list(call=call))
#   
#   df1 = mutate_(group_by(df,.row.num), .set.li = as.list.call)
#   test = df[2,,drop=FALSE]
#   eval(call, test)
#   
# 
#   length.call = substitute(length(call), list(call=call))
#   set.length = eval(length.call,df)
#   
#   lapply(1:NROW(df), function(i) {
#     
#   })
#   for (i in 1:NROW(df)) {
#     
#   }
  
  
  
}
