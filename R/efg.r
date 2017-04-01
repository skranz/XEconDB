# Functions to generate a level form extensive form based game


examples.sfg.to.efg =  function() {
  setwd("D:/libraries/XEconDB/")
  init.ee()
   
  sfg = load.sfg("ug_stages")
  sfg = load.sfg("ReducedLureOfAuthority")
  #sfg = extract.variant.sfg("randomOrPlayer", asfg)
  
  
  header.df = sfg.to.efg.header.df(sfg)
  par.df = sfg.to.par.df(sfg, variant = 1)
  lev.li = make.all.level.df(header.df,par.df,sfg)
  observe.df = sfg.to.observe.df(sfg)
  lev.li = add.all.information.sets(header.df=header.df, lev.li=lev.li, observe.df=observe.df)
  oco.df = lev.li[[length(lev.li)]]
  
  #efg = new.env(sfg = sfg, header.df = header.df, efg.li = efg.li, par.df=par.df)
  #sfg = load.sfg("LureOfAuthorityAlternative")
  
}


add.all.information.sets = function(header.df, lev.li, observe.df) {
  restore.point("add.all.information.sets")
  levels = which(header.df$type=="action")
  le = levels[1]
  for (le in levels) {
    lev.li[[le]] = add.level.information.sets(le, header.df, lev.li,observe.df)
  }
  lev.li
}

add.level.information.sets=function(le, header.df, lev.li, observe.df) {
  restore.point("add.level.information.sets")
  
  header = header.df[le,]
  lev.df = lev.li[[le]]
  
  if (is.null(lev.df)) return(NULL)
  
  # To fill up .info.set labels with zeros for correct order
  stage10 = ceiling(log(NROW(header.df),10))
  lev10 = ceiling(log(NROW(lev.df),10))
  stage.zeros = paste0(rep("0",stage10-1),collapse="")
  lev.zeros = paste0(rep("0",lev10-1),collapse="")

  
  # Generate obs.df filled with FALSE
  prows = setdiff(1:le,le)
  pvars = unique(header.df$var[prows]) 
  obs.li = replicate(length(pvars),rep(FALSE,NROW(lev.df)),simplify = FALSE)
  names(obs.li) = pvars
  obs.df = as.data.frame(obs.li) 
  
  if (length(obs.df)>0) {
  
    # Update obs.df using previous observations
    o.df = filter(observe.df, sfg.row < header$sfg.row, player==header$player)
    for (row in int.seq(1,NROW(o.df))) {
      var = o.df$var[row]
      obs.val = eval(o.df$cond[[row]], lev.df)
      obs.df[[var]] = obs.df[[var]] | obs.val
        
    }
    obs.df
  
    
    
    # Find groups of observational identical rows 
    search.df = lev.df[,colnames(obs.df)]
    search.df[!obs.df]=NA
    group = identical.rows.groups(search.df[lev.df$.active,])
    
    #search.df$group = group
    
    # Add information set
    le.char = paste0(substring(stage.zeros,1,stage10-nchar(le)), le)
    group.char = paste0(substring(lev.zeros,1,lev10-nchar(group)), group)
    .info.set = paste0(le.char,".",group.char)
    lev.df = set.col.active(lev.df, col=".info.set", val=.info.set, lev.df$.active)
  
    # mark singleton information sets
    lev.df = mutate(group_by(lev.df, .info.set), .singleton=length(unique(.node))==1) %>% ungroup()
  
    # Add observation group
    obs.group = identical.rows.groups(obs.df)
    lev.df$.obs.group = obs.group
  
  # no observations made
  } else {
    # Add information set
    lev.df = mutate(lev.df,
      .info.set=ifelse(lev.df$.active,paste0(le,".1"),NA),
      .singleton=ifelse(lev.df$.active,TRUE,NA),
      .obs.group=ifelse(lev.df$.active,1,NA)
    )
  }  
  attr(lev.df,"obs.df") = obs.df
  
  lev.df
}

eval.on.df = function(call, df) {
  eval(call, df)
}

# Generate a table with all parameter values from a stages form game
sfg.to.par.df = function(sfg, variant=1) {
  # Variants
  restore.point("sfg.to.par.df")
  
  if (is.numeric(variant)) {
    obj = tt.object(sfg)$variants
    #obj = tt.find.object(sfg, name=="variants")
    variant = names(obj)[variant]
  }
  df = data_frame(variant = variant)
  
  
  
  rows = which(is.subtype(sfg$type,"parameter"))
  objs = tt.objects(sfg,rows = rows)
  par = names(objs)
  
  for (i in seq_along(par)) {
    call = objs[[i]]$formula.calls[[1]]
    df[[par[i]]] <- eval.on.df(call,df)
  }
  df
}

# To do: Add observations for past actions
sfg.to.observe.df = function(sfg) {
  restore.point("sfg.to.observe.df")
  
  rows.stages = which(is.subtype(sfg$type,"stage"))
  rows.obs = which(is.subtype(sfg$type,"observe"))
  
  obs.stages = findInterval(rows.obs, rows.stages)
  rows.obs.stages = rows.stages[obs.stages]
  stage = sfg$name[rows.obs.stages]

  
  # store conditions for each level
  stages.objs = tt.objects(sfg,rows.stages)
  stages.cond = lapply(stages.objs, function(obj) as.character(obj$condition))
  
  obs.objs = tt.objects(sfg,rows.obs)
  obs.cond = lapply(obs.objs, function(obj) as.character(obj$condition))
  obs.var = lapply(obs.objs, function(obj) as.character(obj$variables))
  
  i = 1
  cond.str = sapply(seq_along(rows.obs), function(i) {
    str = combine.conditions.str(stages.cond[[stage[i] ]],obs.cond[[i]])
    if (is.null(str)) str="TRUE"
    str
  })
  cond = lapply(cond.str, function(str) parse(text=str)[[1]])
  
  stages.player = lapply(stages.objs, function(obj) as.numeric(obj$player))
  obs.player = lapply(obs.objs, function(obj) as.numeric(obj$player))
  player = lapply(seq_along(obs.player), function(i) {
    if (length(obs.player[[i]])==0)
      return(stages.player[[ obs.stages[i] ]])
    obs.player[[i]]
  })

  df = data_frame(var=obs.var, player=player, stage=stage, cond.str = cond.str, cond=cond, sfg.row = rows.obs)
  df
  d = unnest(df,"var")
  d = unnest(d,"player")
  d
}

# Create a data frame with header data for each level of an efg
sfg.to.efg.header.df = function(sfg) {
  restore.point("sfg.to.header.df")
  rows.stages = which(is.subtype(sfg$type,"stage"))
  rows.var = which(is.subtype(sfg$type,"variable"))
  # ignore parameters specified before stages
  rows.var = rows.var[rows.var >= min(rows.stages)]
  
  var = sfg$name[rows.var]
  var.stages = findInterval(rows.var, rows.stages)
  rows.var.stages = rows.stages[var.stages]
  stage = sfg$name[rows.var.stages]
  inst = paste0(var,".",stage)

  type = rep("action",length(var))
  type[is.subtype(sfg$type[rows.var],"randomVariable")] = "nature"
  type[is.subtype(sfg$type[rows.var],"formulaVariable")] = "trans"
  type[var=="payoff"] = "payoff"
  
  
  # store conditions for each level
  stages.objs = tt.objects(sfg,rows.stages)
  var.objs = tt.objects(sfg,rows.var)


  stages.cond = lapply(stages.objs, function(obj) as.character(obj$condition))  
  var.cond = lapply(var.objs, function(obj) as.character(obj$condition))
 
  cond.str = sapply(seq_along(var), function(i) {
    str = combine.conditions.str(stages.cond[[stage[i] ]],var.cond[[i]])
    if (is.null(str)) str="TRUE"
    str
  })
  cond = lapply(cond.str, function(str) parse(text=str)[[1]])

  stages.player = lapply(stages.objs, function(obj) as.numeric(obj$player))
  var.player = lapply(var.objs, function(obj) as.numeric(obj$player))
  player = lapply(seq_along(var.player), function(i) {
    if (length(var.player[[i]])==0)
      return(stages.player[[ var.stages[i] ]])
    var.player[[i]]
  })
  
  lp = sapply(player, length)
  
  if (any(lp==0 & type=="action")) 
    stop("There are actions without a player being specified.")

  if (any(lp>1 & type=="action")) 
    stop("There are actions with more than 1 player specified. We cannot yet deal with this case!")

  player = sapply(player, function(p) {
    c(p,NA)[1]
  })
  player[type!="action"] = NA
  
  
  data_frame(level = seq_along(var),var=var, inst=inst, stage=stage, type=type,player=player, sfg.row=rows.var, cond.str = cond.str, cond=cond)
}

make.all.level.df = function(header.df,par.df, sfg) {
  restore.point("make.all.level.df")
  
  parent.df = bind_cols(data_frame(.node=""),par.df)
  levels = probs = par.df
  levels[] = 0
  probs[] = 1
  attr(parent.df,"levels") = levels
  attr(parent.df,"probs") = probs
  
  
    
  df.li = vector("list",NROW(header.df))
  level = 1
  while (level <= NROW(header.df)) {
    header = header.df[level,]
    df = make.level.df(header, parent.df,sfg = sfg)
    
    # Don't store transform level df to save memory
    if (header$type != "transform")
      df.li[[level]] = df
    if (!is.null(df)) {
      parent.df = remove.cols(df, c(".node.ind", ".move.ind", ".prob"))
      attr(parent.df,"level") = attr(df,"level"); attr(parent.df,"prob") = attr(df,"prob")
    }
    level = level+1
    #df
  }
  df.li  
}

make.level.df = function(header, parent.df, sfg) {
  restore.point("make.level.df")
  df = parent.df


  df$.node.ind = 1:NROW(df)  
  df$.node = paste0(header$inst,".", 1:NROW(df))
  df$.node = paste0("n",header$level,".", 1:NROW(df))
  
  df$.active = eval(header$cond[[1]], df)
  
  # No active nodes of this level for the current variant
  if (sum(df$.active)==0) return(NULL)
   
  if (header$type == "action") {
    ndf = make.action.level.df(header,df,sfg)
  } else if (header$type == "nature") {
    ndf = make.nature.level.df(header,df,sfg)
  } else if (header$type == "trans") {
    ndf = make.trans.level.df(header,df,sfg)
  } else if (header$type == "payoff") {
    ndf = make.payoff.level.df(header,df,sfg)
  }  
  
  # Update level and prob attributes
  prob = attr(df,"prob")
  level = attr(df,"level")
  prob$.node.ind = level$.node.ind = 1:NROW(df)  

  prob = left_join(prob, select(ndf,.node.ind), by=".node.ind")
  level = left_join(level, select(ndf,.node.ind), by=".node.ind")
  
  var = header$var
  if (header$type=="nature") {
    prob = set.col.active(df=prob,col=var,val = ndf$.prob[ndf$.active], active=ndf$.active)
  } else {
    prob = set.col.active(df=prob,col=var,val = 1, active=ndf$.active)    
  }
  level = set.col.active(df=level,col=var,val = header$level, active=ndf$.active)    
  attr(ndf,"prob") = prob
  attr(ndf,"level") = level
  
  ndf$.level.row = 1:NROW(ndf)
  ndf
  #stop("Unknown type")  
}

set.col.active = function(df, col, val, active) {
  if (! col %in% colnames(df)) df[[col]] = NA
    
  if (sum(active)>0)
    df[active,col] = val
  df
}

make.action.level.df = function(header, df, sfg) {
  restore.point("make.action.level.df")
  obj = tt.object(sfg, header$sfg.row)

  adf = df[df$.active,]
  adf$.row.num = 1:NROW(adf)
  
  set.df = eval.set.on.df(obj$set,adf)
  
  ndf = inner_join(adf, set.df, by=".row.num")
  ndf = change.name(ndf, c(".value",".set.ind"), c(header$var,".move.ind"))
  
  mdf = arrange(bind_rows(ndf,df[!df$.active,]), .node.ind)
  mdf = select(mdf, -.row.num)
  mdf

}


make.nature.level.df = function(header, df, sfg) {
  restore.point("make.nature.level.df")
  
  obj = tt.object(sfg, header$sfg.row)


  adf = df[df$.active,]
  adf$.row.num = int.seq(1,NROW(adf))
  
  
  set.df = eval.set.on.df(obj$set,adf)
  prob.df = eval.set.on.df(obj$prob,adf)
  prob.df = change.name(prob.df,".value",".prob")
  
  ndf = inner_join(adf, set.df, by=".row.num")
  ndf = bind_cols(ndf, select(prob.df,.prob))  
  ndf = change.name(ndf, c(".value",".set.ind"), c(header$var,".move.ind"))
  
  mdf = arrange(bind_rows(ndf,df[!df$.active,]), .node.ind)
  mdf = select(mdf, -.row.num)
  mdf
  
  mdf
}

make.trans.level.df = function(header, df, sfg) {
  restore.point("make.trans.level.df")
  obj = tt.object(sfg, header$sfg.row)
  if (! header$var %in% colnames(df)) df[[header$var]] = NA
  
  
  if (sum(df$.active)>0)
    df[df$.active,header$var] = eval.on.df(obj$formula.calls[[1]],df[df$.active,])
  df
}


make.payoff.level.df = function(header, df, sfg) {
  restore.point("make.payoff.level.df")
  obj = tt.object(sfg, header$sfg.row)
  n=sfg.num.players(sfg)
  
  i = 1
  active.rows = which(df$.active)
  for (i in 1:n) {
    var = paste0("payoff_",i)
    df[active.rows,var] = eval.on.df(obj$formula.calls[[i]],df[active.rows,])
  }
  # Set utility by deafault equal to payoff 
  for (i in 1:n) {
    pvar = paste0("payoff_",i)
    uvar = paste0("u_",i)
    df[active.rows,uvar] = df[active.rows,pvar]
  }
  
  # Possibly adapt utility for preference types
  df = add.prefTypes.u.to.payoff.level.df(header=header, df=df,sfg=sfg,n=n)
  df
}

eval.set.on.df = function(set, df) {
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


make.ise.df = function(header.df,lev.li) {
  levs = header.df$level[header.df$type %in% c("action")]
  lev = levs[2]
  li = lapply(levs, function(lev) {
    lev.df = lev.li[[lev]]
    if (is.null(lev.df)) return(NULL)
    df = summarise(group_by(lev.df, .info.set),
      num.moves = max(.move.ind),
      moves.li = list(sort(unique(.move.ind)))
    )
    df$player = header.df$player[[lev]]
    df$level = lev
    df
  })
  df = as_data_frame(rbindlist(li))
  df
}

#' Create an extensive form game that contains all required information
#' to export and solve via gambit and analyse the solution
make.efg = function(sfg, variant = 1, efg.name=NULL, efg.name.add=NULL, add.iso.df = TRUE, add.sg.df=TRUE) {
  restore.point("make.efg")

  
    
  header.df = sfg.to.efg.header.df(sfg)
  par.df = sfg.to.par.df(sfg, variant = 1)
  lev.li = make.all.level.df(header.df,par.df,sfg)
  active.lev = !sapply(lev.li, is.null)
  
  observe.df = sfg.to.observe.df(sfg)
  lev.li = add.all.information.sets(header.df=header.df, lev.li=lev.li, observe.df=observe.df)
  oco.df = make.oco.df(header.df,lev.li)
  ise.df = make.ise.df(header.df,lev.li)

  variant = unique(par.df$variant)
  n = unique(par.df$numPlayers)
  prefTypes = attr(sfg,"prefTypes")
  gameId = as.character(tt.object(sfg)$gameId)
  
  # Create name for efg
  if (is.null(efg.name)) {
    efg.name = paste0(gameId,"_",variant)
    if (is.null(efg.name.add) & !is.null(prefTypes)) {
      str = lapply(seq_along(prefTypes$types), function(i) {
        types = names(prefTypes$types[[i]])
        if (length(types)==0) return(NULL)
        prob = round(prefTypes$probs[[i]]*100)
        sc(i,types,"_",prob,collapse="_")
      })
      str = str[!sapply(str, is.null)]
      efg.name.add = paste0(str,collapse="_")
    }
    if (!is.null(efg.name.add)) {
      efg.name = paste0(efg.name,"_",efg.name.add)
    } 
  }
  
  sfg.md5 = digest::digest(sfg)
  
  efg = as.environment(list(efg.name=efg.name,gameId=gameId, variant=variant, n=n,sfg = sfg, header.df = header.df, par.df=par.df, lev.li=lev.li,active.lev=active.lev, oco.df = oco.df, ise.df=ise.df, prefTypes=prefTypes, sfg.md5=sfg.md5))
  
  if (add.iso.df) {
    efg$iso.df = make.iso.table(efg)
  } 
  if (add.sg.df) {
    efg$sg.df = make.sg.df(lev.li=efg$lev.li, header.df = efg$header.df)
  } 

  efg
}

#' Set the utility function (transformation of payoffs) in an efg
set.efg.util = function(util, efg, adjust.name=TRUE) {
  restore.point("set.efg.util")
  add.name = NULL

  i = 1
  for (i in 1:efg$n) {
    util.fun = util[[i]]
    if (length(util.fun)==0) next
    
    if (is.character(util.fun)) {
      if (nchar(util.fun)==0) next
      util.call = parse.as.call(text=util[[i]])
    } else if (is.call(util.fun)) {
      util.call = util.fun
    }
    uvar = paste0("u_",i)
    efg$oco.df[[uvar]] = eval.on.df(util.call,efg$oco.df)
    add.name = c(add.name, paste0(i,names(util)[i]))
  }
  if (adjust.name) {
    efg$efg.name = paste0(gameId,"_",variant)
    if (length(add.name)>0) {
      add.name = sc(add.name, collapse="_")
      efg$efg.name = paste0(efg$efg.name,"_",add.name)
    } 
  }
  efg
}

# A table to show user the actions of a game
efg.action.table = function(efg) {
  restore.point("efg.action.table")
  #efg = gx$efg
  header.df = efg$header.df
  lev.li = efg$lev.li
  
  levs = which(header.df$type %in% c("action","nature"))
  lev = levs[1]
  li = lapply(levs, function(lev) {
    lev.df = lev.li[[lev]]
    header = header.df[lev,,drop=FALSE]
    
    moves = unique(lev.df[[header$var]][lev.df$.active])
    num.moves = length(moves)
    moves.str = paste0(moves,collapse=",")
    if (nchar(moves.str)>83) {
      moves.str = paste0(substring(moves.str,1,80),"...")
    }
    data_frame(level=lev,player=header.df$player[lev],var = header.df$var[lev], num=num.moves, moves=moves.str)
  })
  df = bind_rows(li)
  df
}

#' Create roco.df: reduced outcome data frame
#' 
#' reduce move of nature levels that are not followed by any action
reduce.efg = function(efg) {
  restore.point("reduce.efg")
  #efg = gx$efg
  
  header.df = efg$header.df
  
  last.action = max(which(header.df$type=="action"))
  red.levels = which(header.df$type=="nature")
  red.levels = red.levels[red.levels>last.action]
  
  efg$red.levels = red.levels
  
  if (length(red.levels)==0) {
    efg$roco.df = efg$oco.df
    return(efg)
  }
  
  roco.df = efg$oco.df
  
  for (lev in rev(red.levels)) {

    lev.df = efg$lev.li[[lev]]
    header = header.df[lev,]
    
    key.cols = c("variant",unique(header.df$var[seq.int(1,lev-1)]))
    key.df = unique(lev.df[lev.df$.active,key.cols])
    
    # Filter relevant roco rows that will be reduced and those that will be kept
    red.rows = filter_by_list(roco.df, key.df, return.rows=TRUE)
    keep.rows = !red.rows
    
    # Group the to be reduced rows by all key columns
    gr.df = group_by_(roco.df[red.rows,,drop=FALSE],.dots=key.cols)
    # Create the summarise command: weighted sum of utils and payoffs; sum of prob
    com = paste0(c(
      paste0("u_",1:efg$n," = weighted.mean(u_",1:efg$n,",.prob)", collapse=","),
      paste0("payoff_",1:efg$n," = weighted.mean(payoff_",1:efg$n,",.prob)", collapse=","),
      paste0(".prob = sum(.prob)")
      ), collapse=","
    )
    red.df = s_summarise(gr.df,com) %>% ungroup()
    keep.df = roco.df[keep.rows,,drop=FALSE]
      
    roco.df = rbind(keep.df,red.df)
    roco.df$.outcome = 1:NROW(roco.df)
  }  

  efg$iso.df = efg$spo = efg$spi = efg$sps =  NULL
  efg$is.reduced = TRUE
  efg$long.oco.df = efg$oco.df
  efg$active.lev[red.levels] = FALSE
  efg$oco.df = roco.df
  efg
} 