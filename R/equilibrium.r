# equilirbria

#' Get the equilibrium outcomes for given pure 
#' strategy equilibria. eq.df contains the move
#' for every information set.
get.eq.out = function(eq.df,efg, oco.df = efg$oco.df, reduce=TRUE) {
  restore.point("gambit.eq.out")

  iso.df=efg$iso.df

  #sdf = filter(sol, eq==1)
  df = left_join(eq.df, iso.df, by=c("player","level",".info.set",".move.ind"))

  outcomes = oco.df$.outcome
  
  get.feasible.outcomes = function(.infeasible, outcomes) {
    restore.point("get.feasible")
    infeasible = do.call("c",.infeasible)
    feasible = setdiff(outcomes,infeasible)
    list(feasible)
  }
  #.inf = list(do.call("c",.infeasible))
  
  odf = summarise(group_by(df,eq),
    .outcome = get.feasible.outcomes(.infeasible,oco.df$.outcome))
  
  odf = tidyr::unnest(odf,.outcome)  
  odf = left_join(odf, oco.df, by=".outcome")
  
  if (reduce) odf  =reduce.eq.out(odf)
  
  odf
}

reduce.eq.out = function(out.df) {
  restore.point("reduce.eq.out")
  inner.reduce.out = function(df) {
    restore.point("dhkghkdgk")
    cols = colnames(df)
    res = df[1,,drop=FALSE]
    
    wm.cols = cols[str.starts.with(cols,"u_") | str.starts.with(cols,"payoff_")] 
    res[wm.cols] = lapply(df[wm.cols], function(val) weighted.mean(val,df$.prob))
    res[[".prob"]] = sum(df$.prob)
    res
    
  }
  red.out = do(group_by(out.df,eq), inner.reduce.out(.))
  cols = setdiff(colnames(red.out),c("eq",".outcome",".prob",".node",".level.row"))
  df = red.out[,cols]
  dupl = duplicated(df)
  red.out[!dupl,]
  
}

examples.filtered.eq.out = function() {
  value.filter = list(offer=7)
  get.filtered.eq.out(eq.df, value.filter, efg)
}

#' Get the outcomes of the continuation equilibria
#' in which certain variables (actions or moves of nature)
#' satisfy the values in the filter.
#' @param eq.df a data.frame for pure Nash equilibria that specifies the moves for each information set
#' @param value.filter a named list that can contain values for given variables. 
#' @param efg
get.filtered.eq.out = function(eq.df,value.filter, efg) {
  restore.point("gambit.filtered.eq.out")
  
  if (length(value.filter)==0)
    return(get.eq.out(eq.df,efg=efg))
  
  iso.df=efg$iso.df
  oco.df=efg$oco.df
  header.df = efg$header.df
  lev.li = efg$lev.li
  
  filter.var = names(value.filter)
  # Filter those outcomes that satisfy the value filter
  
  
  is.char = sapply(filter.var, function(var) {
    is.character(oco.df[1,var])
  })
  quot = ifelse(is.char,'"','')
  filter.str = paste0(filter.var,"==",quot, value.filter,quot)
  foco.df = s_filter(oco.df, .dots=filter.str)
  
  # Filter those information sets that satisfy the value filter
  # 
  # ignore the following info sets:
  #   - in which a filter variable is the action
  #   - in which not all filter variables precede
  #   - that has not at least one node that matches
  #     the value of all filter variables

  levs = which( header.df$type == "action" &
                (!header.df$var %in% filter.var) &
                efg$active.lev)
  
  filter.str = paste0(filter.str,",.active==TRUE")

  lev = levs[1]
  isets = lapply(levs, function(lev) {
    lev.df = lev.li[[lev]]
    if (!all(filter.var %in% colnames(lev.df)))
      return(NULL)
    
    flev.df = s_filter(lev.df, filter.str)
    unique(flev.df$.info.set)
  })
  isets = unlist(isets)
  
  # only consider the remaining info sets
  feq.df = filter(eq.df, .info.set %in% isets)
  
  
  # code similar to get.eq.out
  # open question: need to deal with probabilities
  
  df = left_join(feq.df, iso.df, by=c("player","level",".info.set",".move.ind"))

  outcomes = foco.df$.outcome
  
  get.feasible.outcomes = function(.infeasible, outcomes) {
    restore.point("get.feasible")
    infeasible = do.call("c",.infeasible)
    feasible = setdiff(outcomes,infeasible)
    list(feasible)
  }
  #.inf = list(do.call("c",.infeasible))
  

  odf = summarise(group_by(df,eq),
  .outcome = get.feasible.outcomes(.infeasible,foco.df$.outcome))
  
  odf = tidyr::unnest(odf,.outcome)  
  odf = left_join(odf, foco.df, by=".outcome")
  odf
}
