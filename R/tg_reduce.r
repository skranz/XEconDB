# Try to reduce a tg game
# Moving backwards over levels
# Evaluate moves of nature
# Check for dominant actions

examples.reduce.tg = function() {
  setwd("D:/libraries/XEconDB/projects/UltimatumGame/")
	gameId = "Cournot"
	
  txt = readLines("GiftExchange.json")
  txt = readLines("Cournot.json")
  jg = fromJSON(txt,simplifyDataFrame = FALSE,simplifyMatrix = FALSE)$game
  rg = jg.to.rg(jg)
  vg = rg.to.vg(rg,variant=1)
  #vg = rg.to.vg(rg,variant=2)
  tg = vg.to.tg(vg)
  alpha = 0.75; beta=0.5
  util.funs = list(ineqAvUtil(1, alpha,beta),ineqAvUtil(2,alpha,beta))
  set.tg.util(tg, util.funs)
  oco.df= tg$oco.df
  
  rtg = reduce.tg(tg)
  oco.df = rtg$oco.df
  
  efg = tg.to.efg(rtg, path=getwd())
  eq.li = gambit.solve.eq(rtg, just.spe=TRUE)
  eqo.df = eq.outcomes(eq.li, tg=rtg)
  eqo.df

  efg = tg.to.efg(tg, path=getwd())
  eq.li = gambit.solve.eq(tg, just.spe=TRUE)
  eqo.df = eq.outcomes(eq.li, tg=tg)
  eqo.df

}

reduce.tg = function(tg) {
  # TO DO:
  # Compute expected payoffs for 
  # each action profile (unique of paste.cols of et.mat)
  # and use those for dominance
  # allows better elimination even if there
  # are random moves of nature
  restore.point("reduce.tg")
  rtg = as.environment(as.list(tg))
  iteration = 0
  rtg$was.reduced = TRUE
  
  # iteratievly eliminate strictly
  # dominated moves at action levels
  # stops when no elimination took place
  # in any action level
  while(rtg$was.reduced & iteration <2000) {
    iteration = iteration +1
    rtg$was.reduced = FALSE
    lev.num = length(tg$lev.li)+1
    while (lev.num >1) {
      lev.num = lev.num-1
      if (lev.num==1 & !rtg$was.reduced & iteration>1) {
        break
      }
      lev = rtg$lev.li[[lev.num]]
      if (lev$type == "action") {
        reduce.action.level(lev,rtg, tg, iteration=iteration)
      }
    }
  }
  oco.df = rtg$oco.df
  cols = colnames(rtg$oco.df)
  rcols = setdiff(cols[str.starts.with(cols,".")],".outcome")
  rtg$oco.df = remove.cols(rtg$oco.df, rcols)
  
  # remove rows from et.mat
  rtg$et.mat = tg$et.mat[rtg$oco.df$.outcome,,drop=FALSE]
  
  # renumber .info.set.moves in all action lev.df and in et.mat
  et.mat = rtg$et.mat
  org.move.inds = sort(unique(-et.mat[et.mat<0]))
  org.info.set.inds = sort(unique(unlist(lapply(rtg$lev.li, function(lev) {
    if (lev$type != "action") return(NULL)
    unique(lev$lev.df$.info.set.ind)
  }))))
  
    
  rtg$lev.li = lapply(rtg$lev.li, function(lev) {
    restore.point("inner.rtg.reduce")
    
    lev.df = lev$lev.df
    lev$know.li = NULL
    if (lev$type == "action") {
      # adapt info sets and move indices for actions
      lev.df$.info.set.move.ind = match(lev.df$.info.set.move.ind, org.move.inds)
      lev.df$.info.set.ind = match(lev.df$.info.set.ind, org.info.set.inds)
      lev.df = lev.df %>% 
        arrange(.info.set.move.ind) %>% 
        group_by(.info.set.ind) %>%
        mutate(.move.ind = 1:n()) %>%
        ungroup()
    } else if (lev$type == "nature") {
      # remove rows from nature
      join.cols = unique(sapply(tg$lev.li[1:lev$lev.num], function(ilev) ilev$var))
      lev.df = semi_join(lev.df,rtg$oco.df, by=join.cols)
    }
    lev$lev.df = lev.df
    lev
  })  
  # adapt et.mat
  move.inds = as.vector(-et.mat[et.mat<0])
  et.mat[et.mat<0] = -match(move.inds, org.move.inds)
  rtg$et.mat = et.mat
  
  rtg$oco.df$.org.outcome = rtg$oco.df$.outcome
  rtg$oco.df$.outcome = seq.int(NROW(rtg$oco.df))
  
  rtg
}

reduce.action.level = function(lev,rtg,tg, iteration=1) {
  
  lev.df = remove.cols(lev$lev.df,".dominated")
  restore.point("reduce.action.level")
  # 1. join with oco.df (or with last lev.df)
  join.vars = unique(sapply(tg$lev.li[1:lev$lev.num], function(lev) lev$var))
  
  # oco.df may have been reduced in other levels
  # so we first reduce lev.df
  if (iteration > 1) {
    lev.df = semi_join(lev.df,rtg$oco.df, by=join.vars)
  }
  
  cols = c(join.vars, ".info.set.ind", ".info.set.move.ind",".player")
  
  oco.df = remove.cols(rtg$oco.df, c(".info.set.ind", ".info.set.move.ind",".player"))
  # compute oco.df
  odf = left_join(oco.df,lev.df[,cols,drop=FALSE], by=join.vars)
  

  # compute utility of relevant player
  player = odf$.player[[1]]
  odf[[".util"]] = odf[[paste0("util_",player)]]
  for (i in setdiff(na.omit(unique(odf$.player)),player)) {
    rows = isTRUE(odf.$player == i)
    odf[[".util"]][rows] = odf[[paste0("util_",odf$.player)]][rows] 
  }

  # compute SOME dominated moves in odf
  # TO DO: Find all dominated moves.
  # Needs Rcpp implementation of pairwise comparision
  odf = odf %>% 
    group_by(.info.set.move.ind) %>%
    mutate(.move.u.max = max(.util),.move.u.min = min(.util)) %>%
    group_by(.info.set.ind) %>%
    mutate(.move.u.max.min = max(.move.u.min), .dominated = .move.u.max < .move.u.max.min)
    
  # compute corresponding dominated moves in lev.df
  ldf = odf %>% group_by(.info.set.move.ind) %>% summarize(.dominated = first(.dominated))
  ldf = left_join(lev.df,ldf, by = ".info.set.move.ind" )

  # flag if any reduction took place for outer while loop
  if (!rtg$was.reduced) {
    rtg$was.reduced = any(ldf$.dominated)
  }
  
  rtg$oco.df = filter(odf, !.dominated)
  rtg$lev.li[[lev$lev.num]]$lev.df = filter(ldf, !.dominated)
  
}