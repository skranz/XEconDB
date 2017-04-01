# Transform efg to normal form games


examples.sfg.to.nfg =  function() {
  setwd("D:/libraries/XEconDB/")
  library(XEconDB)
  init.ee()
   
  sfg = load.sfg("ug_stages")
  #sfg = extract.variant.sfg("randomOrPlayer", asfg)
  
  
  header.df = sfg.to.efg.header.df(sfg)
  par.df = sfg.to.par.df(sfg, variant = 1)
  lev.li = make.all.level.df(header.df,par.df,sfg)
  observe.df = sfg.to.observe.df(sfg)
  lev.li = add.all.information.sets(header.df=header.df, lev.li=lev.li, observe.df=observe.df)
  oco.df = make.oco.df(header.df,lev.li)
  
  efg = as.environment(list(sfg = sfg, header.df = header.df, par.df=par.df, lev.li=lev.li, oco.df = oco.df))
  
  iso.df = make.iso.table(efg)
  spi = make.spi(iso.df)
  sps = make.sps.df(spi, iso.df)
  
  
  #library(compiler)
  #make.spo = cmpfun(make.spo)
  Rprof(tmp <- tempfile())
  spo = make.spo(iso.df = iso.df, spi=spi,oco.df = oco.df)
  Rprof()
  summaryRprof(tmp)
  #spo = left_join(spo, sps, by="sp")
  
  table(spo$sp)
  
  #spo = make.spo(iso.df = iso.df, spi=spi,oco.df = oco.df)
  #sfg = load.sfg("LureOfAuthorityAlternative")
  util.fun = NULL
  util.fun =  envyUtil(alpha=0.5,n=2)
  utab = spo.to.utab(spo,util.fun = util.fun)
  utab = add.strats.to.utab(utab, spi, iso.df)
  
  rtab = add.br.and.nash(utab,spi)
  rtab = add.weakly.dominated(rtab,spi)

  rtab[rtab$is.nash,]
  
  rtab  
  rtab[rtab$is.admis,]
  
  
  is.dominated(player=1, utab=rtab)
  is.dominated(player=2, utab=rtab)
  
}

nfg.solve.pure = function(nfg,util.fun=NULL, only.admissible=TRUE) {
  restore.point("nfg.solve.pure")

  spo = nfg$spo; iso.df = nfg$iso.df; spi = nfg$spi
  utab = spo.to.utab(spo,util.fun = util.fun)
  utab = add.strats.to.utab(utab, spi, iso.df)

  rtab = add.br.and.nash(utab,spi)
  
  if (only.admissible) {
    rtab = add.weakly.dominated(rtab,spi)
    rtab = rtab[rtab$is.admis,]
  }
  eq.df = rtab[rtab$is.nash,]
  eq.df = select(eq.df, sp,sp.names,starts_with("strat_"),
    starts_with("u_"))
  nfg
}

efg.to.nfg = function(efg) {
  restore.point("efg.to.nfg")
  nfg = efg

  if (is.null(nfg$iso.df))
    nfg$iso.df = make.iso.table(nfg)
  
  nfg$spi = make.spi(nfg$iso.df)
  nfg$sps = make.sps.df(nfg$spi, nfg$iso.df)
  
  nfg$spo = make.spo(iso.df = nfg$iso.df, spi=nfg$spi,oco.df = nfg$oco.df)

  nfg
}

add.weakly.dominated = function(utab,spi) {
  restore.point("add.weakly.dominated")
  n = max(spi$player)
  
  strat.cols = colnames(utab)[str.starts.with(colnames(utab),"strat_")]
  players = unique(as.numeric(str.right.of(strat.cols,"strat_")))

  dom.li = lapply(players, is.dominated, utab=utab)
  attr(utab,"dom.li") = dom.li
  
  for (i in 1:n) {
    if (i %in% players) {
      dom.df = dom.li[[i]]
      utab[[paste0("wdom_",i)]] = dom.df[utab[[paste0("strat_",i)]],"weakly"]
    } else {
#      utab[[paste0("wdom_",i)]] = TRUE
    }
  }
  
  cols = paste0("wdom_",players)
  utab$is.admis = utab$is.nash & rowSums(utab[,cols,drop=FALSE])==0
  
  utab
}

is.dominated = function(player,strat=NULL, by.strat=NULL, utab) {
  restore.point("is.dominated")
  strat.col = paste0("strat_",player)
  u.col = paste0("u_",player)

  tab = arrange_(ungroup(utab), strat.col)
  
  ncol = length(unique(tab[[strat.col]]))
  pmat = matrix(tab[[u.col]],ncol=ncol)
  
  if (FALSE) {
    colnames(pmat) = make.strat.names(iso.df)[[player]]
  }
  # strats are in columns

  # reduce pmat if by.strat is given
  if (!is.null(by.strat))
    pmat = pmat[,by.strat] 
  
  if (is.null(strat))
    strat = 1:NCOL(pmat)
  
  li = lapply(strat, function(strat) {
    strat.payoff = pmat[,strat]
    
    strict = pmat > strat.payoff
    weak = pmat >= strat.payoff
    
    all.weak = colSums(weak) == NROW(pmat)
    one.strict = colSums(strict) >= 1
    all.strict = colSums(strict) == NROW(pmat)
    
    weakly = any(all.weak & one.strict)
    strictly = any(all.strict)
    
    c(weakly=weakly, strictly=strictly)      
  })
  do.call("rbind",li)
  

}

add.br.and.nash = function(utab, spi) {
  restore.point("add.br.and.nash")
  n = max(spi$player)
  
  strat.cols = colnames(utab)[str.starts.with(colnames(utab),"strat_")]
  players = unique(as.numeric(str.right.of(strat.cols,"strat_")))

  
  rtab = utab
  i = 2
  i = 1
  for (i in players) {

    other.strats = sc("strat_",setdiff(players,i))
    
    if (length(other.strats)>0) {
      subst.str = c("u_")
      subst.li = lapply(paste0(subst.str,i), as.name)
      names(subst.li) = paste0(subst.str,"i")
      
      ca = substitute(
        mutate(group_by_(rtab, other.strats),
          u.br_i = max(u_i),
          is.br_i = u_i == u.br_i
        ), subst.li      
      )
      rtab = eval(ca)
      rtab = ungroup(rtab)
    } else {
      rtab$u.br_i = max(rtab[[paste0("u_",i)]])
      rtab$is.br_i = (rtab[[paste0("u_",i)]] == rtab$u.br_i)
    }    
    
    rtab = change.name(rtab, old = c("is.br_i","u.br_i"),
                       new = paste0(c("is.br_","u.br_"),i))         
  }
  
  cols = paste0("is.br_",players)
  rtab$is.nash = rowSums(rtab[,cols, drop=FALSE])==length(players)
  rtab
}

payoffs.to.u = function(payoff.mat, util.fun=NULL,...) {
  if (is.null(util.fun)) {
    df = payoff.mat
  } else if (is.function(util.fun)) {
    df = util.fun(payoff.mat)
  } else if (is.character(util.fun)) {
    util.fun = lapply(util.fun, function(str) parse(text=str))
    u.li = lapply(util.fun, eval, envir = payoff.mat)
    df = do.call("cbind", u.li)
  }
  colnames(df) = paste0("u_", 1:NCOL(payoff.mat))
  df
}

# compute payoff / utility table
spo.to.utab = function(spo,util.fun = NULL,...) {
  n = sum(str.starts.with(colnames(spo),"payoff_"))
  cols = paste0("payoff_",1:n)
  u.mat = payoffs.to.u(spo[,cols], util.fun,...)
  
  restore.point("spo.to.utab")
  

  
  spo = cbind(spo, u.mat)
  eu.fun = function(u,.prob) {
    sum(u * .prob)
  }
  cols = paste0("u_",1:n)

  utab = summarise_each_(group_by(spo,sp),funs(eu.fun(.,.prob)),cols)
  #utab = change.name(utab, cols, paste0("u_",1:n))
  return(ungroup(utab))
  
}

# strategy profiles and strats
make.sps.df = function(spi, iso.df) {
  restore.point("make.sps.df")
  si = summarize(group_by(spi, player), moves = prod(moves))
  n = NROW(si)
  dim = si$moves
  strat.mat = make.grid.matrix(x.dim=dim)
  colnames(strat.mat) = paste0("strat_",1:n)
  
  strat.names = make.strat.names(iso.df)
  names.li = lapply(1:n, function(i) {
    strat.names[[i]][strat.mat[,i]] 
  })
  sp.name = do.call(paste, c(names.li, list(sep=" ")))

  as_data_frame(data.frame(sp=seq_along(sp.name),sp.name,as.data.frame(strat.mat)))
}

add.strats.to.utab = function(utab, spi, iso.df) {
  # Add strategy profiles
  si = summarize(group_by(spi, player), moves = prod(moves))
  n = NROW(si)
  dim = si$moves
  strat.mat = make.grid.matrix(x.dim=dim)
  colnames(strat.mat) = paste0("strat_",1:n)
  
  strat.names = make.strat.names(iso.df)
  names.li = lapply(1:n, function(i) {
    strat.names[[i]][strat.mat[,i]] 
  })
  sp.names = do.call(paste, c(names.li, list(sep=" ")))
  
  utab = cbind(sp.names,strat.mat, utab)
  utab

}

make.strat.names = function(iso.df) {
  restore.point("make.strat.names")
  i = 2
  players = unique(iso.df$player)
  
  li = lapply(players, function(i) {
    piso = filter(iso.df,player==i)
    
    moves.df = summarise(group_by(piso,.info.set), moves = list(.move.name))
    mat = make.grid.matrix(moves.df$moves)
    paste.matrix.cols(mat,sep=",")    
  })
  li
}

make.oco.df = function(header.df,lev.li) {
  
  oco.df = lev.li[[length(lev.li)]]
  
  var = unique(header.df$var[header.df$type=="nature"])
  prob.df = attr(oco.df,"prob")[,var,drop=FALSE]
  .prob = exp(rowSums(log(prob.df)))
  
  oco.df = cbind(data.frame(.outcome=1:NROW(oco.df), .prob=.prob), oco.df)
  oco.df
}


#' Create strategy profile outcome table.
#' 
#' This table can be quite large.
make.spo = function(iso.df, spi, oco.df, outcomes=NULL) {
  restore.point("make.spo")
  
  return(make.spo.with.matrix(iso.df,spi,oco.df,outcomes))
  
  n.sp = prod(spi$moves)
  n.iso = NROW(spi)
  moves = spi$moves
  
  sp = 1:n.sp
  moves = sp.to.moves(sp, spi)
  
  if (is.null(outcomes))
    outcomes = oco.df$.outcome
  
  # compute spo recursevly for each iso.ind
  internal.make.spo = function(sp,iso.ind, outcomes, iso.row.add, move.mult, .infeasible, verbose=FALSE) {
    # last info set
    spaces = paste0(rep(" ", iso.ind*2), collapse="")    
    #restore.point("internal.make.strout")
    #if (iso.ind==3)
    #  stop()

    li = lapply(1:spi$moves[iso.ind], function(move.ind) {
      #restore.point("loop.inner.level")
 
      iso.row = move.ind + iso.row.add[iso.ind]      
      offset = move.mult[iso.ind]*(move.ind-1)
      new.outcomes = setdiff(outcomes, .infeasible[[iso.row]]) 
      
      if (verbose) {
        cat("\n",spaces,"sp = ", sp+offset, "iso.row = ", iso.row,iso.df$.info.set[iso.row], " iso.ind =", iso.ind, " move.ind = ", move.ind)
        #cat(" out=",paste0(new.outcomes, collapse=","))
        #cat(" infeas=",paste0(iso.df$.infeasible[[iso.row]], collapse=","))      
      }
      if (iso.ind<n.iso) {
        df = internal.make.spo(sp=sp+offset, iso.ind=iso.ind+1,
                 outcomes=new.outcomes,
                 iso.row.add=iso.row.add, move.mult=move.mult,
                 .infeasible = .infeasible, verbose=verbose)
      } else  {   
        df = quick.df(sp=rep(sp+offset,length(new.outcomes)), .outcome = new.outcomes)
      }
      return(df)

    })
    df = bind_rows(li)
    return(df)
  }
  
  
  spo = internal.make.spo(sp=1, iso.ind=1, outcomes=outcomes,
                         iso.row.add=spi$iso.row.add, move.mult=spi$move.mult,
                         .infeasible = iso.df$.infeasible, verbose=verbose)
  

  spo = inner_join(spo, select(oco.df,.outcome, .prob, starts_with("payoff_")), by=".outcome")
  
  spo
}


#' Create strategy profile outcome table.
#' 
#' This table can be quite large.
make.spo.with.matrix = function(iso.df, spi, oco.df, outcomes=NULL) {
  restore.point("make.spo")

  if (is.null(outcomes))
    outcomes = oco.df$.outcome
  
  n.sp = prod(spi$moves)
  n.iso = NROW(spi)
  n.out = length(outcomes)

  moves.df = sp.to.moves(sp = 1:n.sp, spi)
  
  # This matrix can be quite big
  # If memory is a concern, we may split the matrix
  # and the move.df in different chunks
  # and apply the stuff for each chunk seperately
  feas.mat = matrix(TRUE,n.sp,n.out )
  
  iso.infeasible = iso.df$.infeasible

  iso.ind = 1
  move.ind = 1
  iso.row = 0
  for (iso.ind in seq.int(1,n.iso)) {
    for (move.ind in seq.int(1,spi$moves[iso.ind])) {
      iso.row = iso.row + 1
      infeas = iso.infeasible[[iso.row]]
      rows = which(moves.df[,iso.ind]==move.ind)
      feas.mat[rows,infeas] = FALSE
    }
  }
  
  spo = which(feas.mat,arr.ind = TRUE)
  colnames(spo) = c("sp",".outcome")
  spo = as.data.frame(spo)
  spo$.outcome = outcomes[spo$.outcome]
  spo = arrange(spo,sp,.outcome)
  
  spo = inner_join(spo, select(oco.df,.outcome, .prob, starts_with("payoff_")), by=".outcome")
  
  spo
}

# strategy profiles info
make.spi = function(iso.df) {
  restore.point("make.spi")
  
  spi = summarise(group_by(iso.df, player, level, .info.set),
                             moves = length(.move.ind)
                            )
  spi$iso.row.add = c(0, cumsum(spi$moves[-NROW(spi)]))
  spi$move.mult = rev(c(1,cumprod(rev(spi$moves[-1]))))
  spi
}

examples.moves.to.sp = function() {
  moves = rbind(c(2,1,2,1),c(1,2,2,1),c(3,1,1,1))

  spi = data.frame(moves=c(4,2,3), move.mult=c(6,3,1))
  moves = rbind(c(2,1,1),c(1,1,2),c(1,1,3),c(1,2,1))
  
  sp = moves.to.sp(moves,spi)
  sp
  sp.to.moves(sp,spi)
}

# matrix of moves at each information set to strategy profile index
moves.to.sp = function(moves,spi) {
  if (is.null(dim(moves)))
    return(sum(moves*spi$move.mult))
  
  sp = rep(1, NROW(moves))
  col = 2
  for (col in 1:NCOL(moves)) {
    sp = sp + (moves[,col]-1)*spi$move.mult[col] 
  }
  sp
}

# strategy profile index to matrix of moves at each information set
sp.to.moves = function(sp, spi, wide=TRUE) {
  

  moves = matrix(0, NROW(sp), NROW(spi))
  
  col = NROW(spi)
  for (col in 1:NROW(spi)) {
    # integer division
    ind = ( (sp-1) %/% spi$move.mult[col])+1
    moves[,col] = ind
    # remainder
    sp = sp - (ind-1)*spi$move.mult[col]
  }

  if (wide) {
    colnames(moves) = spi$.info.set
    return(moves)
  }
  
  moves.df = data.frame(
    sp = rep(sp,times=NCOL(moves)),
    .info.set = rep(spi$.info.set, each = length(sp)),
    .move.ind = as.vector(moves)
  )
  as_data_frame(moves.df)

}

make.iso.table = function(efg) {
  restore.point("make.iso.table")  
  header.df = efg$header.df;lev.li = efg$lev.li;
  oco.df = efg$oco.df
  
  levels = which(header.df$type=="action" & efg$active.lev)
  li = lapply(levels, function(le) {
    old.lev.df.to.iso(header=header.df[le,], lev.df=lev.li[[le]], oco.df = oco.df)
  })
  iso.df = bind_rows(li)
  iso.df = select(iso.df, player, level, .info.set,.move.ind,.move.name, .infeasible)
  iso.df = arrange(iso.df, player, level, .info.set, .move.ind)
  iso.df
}

# computes the iso.df from a level.df
old.lev.df.to.iso = function(header, lev.df, oco.df) {
  restore.point("lev.df.to.iso")
  
  # determine all observation groups and loop through them
  obs.df = attr(lev.df,"obs.df")

  
  #obs.group = identical.rows.groups(obs.df)
  obs.group = lev.df$.obs.group
  
  og = 1
  li = lapply(unique(obs.group), function(og) {
    rows = which(obs.group==og & lev.df$.active)
    if (length(rows)==0) return(NULL)
    row = rows[1]
    obs.vars = colnames(obs.df)[unlist(obs.df[row,])]
    
    by = c(obs.vars, header$var )
    idf = lev.df[rows,c(by,".move.ind",".info.set"), drop=FALSE]
    idf$.move.name = as.character(lev.df[[header$var]][rows])
    idf = unique(idf)
    
    jdf = left_join(idf, oco.df[,c(".outcome",by)], by=by)
    
    fun = function(idf) {
      outcomes = unique(idf$.outcome)
      summarise(group_by(idf,.move.ind, .move.name), .infeasible = list(setdiff(outcomes, .outcome)))
    }
    inf.df = do(group_by(jdf,.info.set), fun(.))
    inf.df
  })
  res = bind_rows(li)
  res$player = header$player
  res$level = header$level
  
  res
}