# Transform efg to normal form games


examples.subgames =  function() {
  setwd("D:/libraries/XEconDB/")
  library(XEconDB)
  init.ee()
   


  sfg = load.sfg("TrustGame")
  sfg = load.sfg("UGRandOffer")
  #sfg = extract.variant.sfg("randomOrPlayer", asfg)
  
  
  header.df = sfg.to.efg.header.df(sfg)
  par.df = sfg.to.par.df(sfg, variant = 1)
  lev.li = make.all.level.df(header.df,par.df,sfg)
  observe.df = sfg.to.observe.df(sfg)
  lev.li = add.all.information.sets(header.df=header.df, lev.li=lev.li, observe.df=observe.df)
  oco.df = make.oco.df(header.df,lev.li)
  
  efg = as.environment(list(sfg = sfg, header.df = header.df, par.df=par.df, lev.li=lev.li, oco.df = oco.df))
  
  iso.df = make.iso.table(efg)
  
  sg.df = make.sg.df(lev.li = lev.li,header.df = header.df)
  make.sg.stats(sg.df, iso.df)
  sgo.df = make.sgo.df(lev.li = lev.li,oco.df = oco.df,sg.df = sg.df)
  
  ret = solve.subgames(iso.df = iso.df,sg.df = sg.df,sgo.df = sgo.df,oco.df = oco.df,util.fun = NULL)
  
  game = efg
  game$sg.df = sg.df
  game$sgo.df = sgo.df
  
  sg = sg.df$.sg[1]
  
  sol = solve.sg(sg=sg,iso.df = iso.df,sg.df = sg.df,sgo.df = sgo.df,util.fun = NULL)
  
  
  siso.df = make.sg.iso.df(sg=sg, iso.df=iso.df, sg.df = sg.df)
  outcomes = get.sg.outcomes(sg, sgo.df)
  
  
  # Next step: create nfg for subgame
  
  
  # strategy profile infos
  spi = make.spi(siso.df)
  
  # strategy profile to strategies table
  sps = make.sps.df(spi, siso.df)
  
  
  #library(compiler)
  #make.spo = cmpfun(make.spo)
  Rprof(tmp <- tempfile())
  spo = make.spo(iso.df = siso.df, spi=spi,oco.df = oco.df, outcomes=outcomes)
  Rprof()
  summaryRprof(tmp)
  #spo = left_join(spo, sps, by="sp")
  
  table(spo$sp)
  
  #spo = make.spo(iso.df = iso.df, spi=spi,oco.df = oco.df)
  #sfg = load.sfg("LureOfAuthorityAlternative")
  util.fun = NULL
  util.fun =  envyUtil(alpha=0.5,n=2)
  utab = spo.to.utab(spo,util.fun = util.fun)
  utab = add.strats.to.utab(utab, spi, siso.df)
  
  rtab = add.br.and.nash(utab,spi)
  rtab = add.weakly.dominated(rtab,spi)

  rtab[rtab$is.nash,]
  
  rtab  
  rtab[rtab$is.admis,]
  
  
  is.dominated(player=1, utab=rtab)
  is.dominated(player=2, utab=rtab)
  
}
#' Solve all subgames via backward induction
solve.subgames = function(iso.df, sg.df, sgo.df,oco.df, util.fun=NULL, verbose=TRUE) {
  restore.point("solve.subgames")
  sg.df = add.parent.sg(sg.df)
  sg.df = arrange(sg.df, -root.level, level)

  sgs = unique(sg.df$.sg)
  
  N = length(sgs)
  sol.li = vector("list",N)
  names(sol.li) = sgs
  moves.li = sol.li
  
  i = 1
  for (i in seq_along(sgs)) {
    sg = sgs[i]
    if (verbose)
      display("Solve subgame ", sg, "...")
    
    child.sg = sg.df$.sg[sg.df$parent.sg == sg]
    if (length(child.sg)>0)
      cat("Has child")
    
    sol = solve.sg(sg = sg,iso.df = iso.df,sg.df = sg.df,sgo.df = sgo.df,oco.df=oco.df,util.fun = util.fun)
    sol.li[[i]] = sol$sol.df
    moves.li[[i]] = sol$moves.df
  }
  

}

# Add index of parent subgames for those subgames that have a proper subgame as parent
add.parent.sg = function(sg.df) {
  find.parent.sg = function(sg, level, root.level) {
    restore.point("find.parent.sg")
    ind = which(root.level<level)
    if (length(ind)==0) return("")
    ind.ind = which.max(root.level[ind])
    sg[ind[ind.ind]]
  }
  
  sg.df = mutate(group_by(sg.df,.info.set),
            parent.sg = find.parent.sg(.sg, level, root.level)
          ) %>% 
          ungroup %>%
          mutate_if(root.level != level, parent.sg = "")

  sg.df
}

solve.sg = function(sg, iso.df, sg.df, sgo.df, oco.df, util.fun = NULL, only.admissible=TRUE) {
  restore.point("solve.sg")
  

  if (sg == "0") { # the complete game
    siso.df = iso.df
    outcomes = oco.df$.outcome
  } else { # proper subgame
    siso.df = make.sg.iso.df(sg=sg, iso.df=iso.df, sg.df = sg.df)
    outcomes = get.sg.outcomes(sg, sgo.df)
  }
  
  # strategy profile infos
  spi = make.spi(siso.df)
  # strategy profile to strategies table
  sps = make.sps.df(spi, siso.df)
  
  spo = make.spo(iso.df = siso.df, spi=spi,oco.df = oco.df, outcomes=outcomes)
  #spo = left_join(spo, sps, by="sp")
  
  utab = spo.to.utab(spo,util.fun = util.fun)
  utab = add.strats.to.utab(utab, spi, siso.df)
  rtab = add.br.and.nash(utab,spi)
  rtab = add.weakly.dominated(rtab,spi)

  if (only.admissible) {
    sol.df = rtab[rtab$is.admis,,drop=FALSE]
  } else {
    sol.df = rtab[rtab$is.nash,,drop=FALSE]
  }
  
  
  sol.moves = sp.to.moves(sp=sol.df$sp, spi=spi, wide=FALSE)
  sol.moves = left_join(sol.moves, select(iso.df, .info.set, .move.ind, .move.name),
            by=c(".info.set",".move.ind"))

  sol.moves$.sg = sg
  
  sol.df$.sg = sg
  sol.df = select(sol.df, .sg,sp, starts_with("u_"))
  
  list(sol.df=sol.df, moves.df = sol.moves)

}

get.sg.outcomes = function(sg, sco.df) {
  filter(sgo.df,.sg==sg)$outcomes[[1]]  
}

# extract from iso.df to a subset iso.df for a subgame
make.sg.iso.df = function(sg=sg.df$.sg[1], iso.df, sg.df) {
  restore.point("make.sg.iso.df")
  
  #sg = "2.5"
  sug = filter(sg.df, .sg==sg)
  
  left_join(select(sug,.info.set), iso.df, by=".info.set")
}

make.sg.stats = function(sg.df, iso.df) {
  
  restore.point("make.sg.stats")
  iso.moves = summarize(group_by(iso.df, .info.set), moves=n())
  
  isg = left_join(sg.df, iso.moves,by = c(".info.set"))
  all.sg = unique(isg$.sg) 
  isg = mutate(isg,is.subgame = .info.set %in% all.sg & .info.set != .sg)
  isg = mutate_if(isg, is.subgame==TRUE, moves=1)
  
  sgs = summarize(group_by(isg,.sg), num.sp = prod(moves), num.info.set=n())
  
  big = data_frame(.sg="big",num.sp=prod(iso.moves$moves), num.info.set=NROW(iso.moves))
  rmoves = iso.moves$moves
  rmoves[iso.moves$.info.set %in% all.sg]=1
  zero = data_frame(.sg="0",num.sp=prod(rmoves), num.info.set=NROW(iso.moves))
  
  reduced = data_frame(.sg="reduced",num.sp=sum(sgs$num.sp)+zero$num.sp, num.info.set=NROW(iso.moves))
  
  sgs = bind_rows(big,reduced,zero,sgs)
  sgs
}

#' Find for each subgame the outcomes that can happen
make.sgo.df = function(lev.li, sg.df, oco.df) {
  restore.point("make.sgo.df")
  
  # keep only roots
  rdf = filter(sg.df, .sg==.info.set)
  # we have no proper subgame starting in the first level
  
  lev = 2
  sgo.li = lapply(unique(rdf$level), function(lev) {
    lev.df = lev.li[[lev]]
    sg = rdf$.sg[rdf$level == lev]
    rows = which(lev.df$.info.set %in% sg & lev.df$.active & lev.df$.move.ind==1)
    
    # loop through all obs.groups
    obs.group = lev.df$.obs.group[rows]
    obs.groups = unique(obs.group)
    obs.df = attr(lev.df,"obs.df")
  
    og = obs.groups[1]
    og.li = lapply(obs.groups, function(og) {
      og.rows = rows[obs.group==og]
      key.cols = colnames(obs.df)[ unlist(obs.df[og.rows[1],]) ]
      key.df = lev.df[og.rows,key.cols]
      # subgame index is simply the info set name of its root
      key.df$.sg = lev.df$.info.set[og.rows]       
      
      small.oco.df = s_select(oco.df, c(".outcome",key.cols))  
      # merge with oco.df
      res.df = left_join(key.df, small.oco.df, by=key.cols)
      sum.df = summarize(group_by(res.df, .sg), outcomes=list(.outcome))
      return(sum.df)
    })
    rbindlist(og.li)
  })
  
  sgo.df = as_data_frame(rbindlist(sgo.li)) 
  rownames(sgo.df) = sgo.df$.sg
  sgo.df
}

#' Extract all subgames
make.sg.df = function(lev.li, header.df) {
  # action levels
  restore.point("extract.subgames")
  
  # we have no proper subgame starting in the first level
  levs = which(header.df$type=="action" & header.df$level > 1)
  

  # non-terminal levels: a subgame starts at a singleton and information set 
  # of successor nodes must consist only of successor nodes
  
  i = 1
  sg.li = lapply(seq_along(levs[-length(levs)]), function(i) {
    lev = levs[i]
    lev.df = lev.li[[lev]]
    if (is.null(lev.df)) return(NULL)
    rows = which(lev.df$.singleton & lev.df$.move.ind==1 &  lev.df$.active==TRUE)
    
    if (length(rows)==0) return(NULL)
      
    # loop through all obs.groups
    obs.group = lev.df$.obs.group[rows]
    obs.groups = unique(obs.group)
    obs.df = attr(lev.df,"obs.df")
  
    og = obs.groups[1]
    og.li = lapply(obs.groups, function(og) {
      restore.point("og.li.element")
      og.rows = rows[obs.group==og]
      key.cols = colnames(obs.df)[unlist(obs.df[og.rows[1],]) ]
      
      # if nothing is observed, no subgame can start in a level >= 2
      if (length(key.cols)==0) return(NULL)
      
      key.df = lev.df[og.rows,key.cols]
      # subgame index is simply the info set name of its root
      key.df$.sg = lev.df$.info.set[og.rows]       
        
      sg.li = vector("list",length(levs)-(i)+1)
      sg.li[[1]] = data_frame(.sg = key.df$.sg,level = lev,.info.set = key.df$.sg)

      j = i+1
      # loop through all later levels and merge
      for (j in (i+1):length(levs)) {
        slev.df = lev.li[[ levs[j] ]]
        res = get.sub.info.sets(key.df = key.df,slev.df = slev.df,key.cols = key.cols, level=levs[j])
        key.df = res$key.df
        counter = j-i+1
        sg.li[[counter]] = res$sg.df
      }
      sg.df = rbindlist(sg.li)
      
      # remove subgames that are no longer in key.df
      # because not all their information sets were contained
      sg.df = sg.df[sg.df$.sg %in%  key.df$.sg,]

      sg.df
    })

    sg.df = rbindlist(og.li)    
    

    
  })
  
  sg.df = as_data_frame(rbindlist(sg.li))
  
  # get subgames of last level
  i = length(levs)
  lev = levs[i]
  lev.df = lev.li[[lev]]
  lev.df = filter(lev.df, .move.ind==1, .active==TRUE)
  rows = which(lev.df$.singleton)
  last.sg.df = data_frame(.sg = lev.df$.info.set[rows],.info.set = lev.df$.info.set[rows], level = lev)
  
  sg.df = bind_rows(sg.df, last.sg.df)
  
  sg.df$root.level = as.numeric(str.left.of(sg.df$.sg,"."))
  sg.df
  
}

get.sub.info.sets = function(key.df, slev.df, key.cols = setdiff(colnames(key.df),".sg"), level) {
  
  restore.point("get.sub.info.sets")
  #stop("get.sub.info.sets")
  
  slev.df = filter(slev.df, .move.ind==1, .active==TRUE)
  # compute info set sizes
  slev.df = mutate(group_by(slev.df, .info.set), .info.set.size=n()) %>% ungroup

  sub.df = left_join(key.df, slev.df,by = key.cols)
  
  # compute whether info set is fully contained in the subgame
  sub.df = mutate(group_by(sub.df,.sg, .info.set), .contained = (.info.set.size[1] == n()) ) %>% ungroup
  # index of subgame candidates which have non-contained info sets
  no.sg = sub.df$.sg[! sub.df$.contained] 
  
  # remove subgame candidates which have non-contained info sets
  new.key.df = filter(key.df, ! .sg %in% no.sg )
  new.sub.df = filter(sub.df, ! .sg %in% no.sg )
  
  if (NROW(new.sub.df)>0) {
    sg.df = data_frame(.sg=new.sub.df$.sg, level=level, .info.set = new.sub.df$.info.set)
  } else {
    sg.df = NULL
  }
  list(key.df=new.key.df, sg.df=sg.df)
}