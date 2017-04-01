
load.behavior.struct = function(gameId, path=paste0(ee$struc.path,"/Behavior/",gameId), files = NULL) {
  restore.point("load.behavior.struct")
  
  # By default load all behaviors associated with the game
  if (is.null(files))
    files = list.files(path)

  
  name = paste0("behavior_",gameId)
  
  # Load behaviors from all files
  objs = lapply(files, function(f) {
    file = paste0(path,"/",f)
    load.struct(name=name,file=file,typeName="behaviors", just.obj=TRUE)
  })
  
  # combine behaviors from all files
  obj = do.call(c,objs)
  # generate a structure
  struc = obj.to.struct(obj,name=name)
  
  struc  
}

eval.behaviors = function(bs, game.dt) {
  restore.point("eval.behaviors")
  
  N = NROW(game.dt)
  
  brows = which(is.subtype(bs$df$type,"behavior"))
  bnames = bs$df$name[brows]
  nb = length(brows)
  

  bok = rep(FALSE,nb)
  benv.li = replicate(nb,new.env(parent=.GlobalEnv), simplify=FALSE)
  names(bok) = names(benv.li) = bnames
  
  while(TRUE) {
    had.import = FALSE
    some.checked  = FALSE
    i = 1
    for (i in seq_along(benv.li)) {
      if (bok[i])
        next
      obj = bs$obj.li[[ brows[i] ]]
      some.checked = TRUE
      if (is.subtype(bs$df$type[brows[i]],"intersectBehavior")) {
        
        ret = import.intersectBehavior(i=i,obj=obj,bs=bs,benv.li=benv.li,bok=bok)
        message("intersectBehavior ",bnames[i], ": ", ret )
        bok[i] = ret
        had.import = had.import | ret

      # A simple behavior consisting of several actions  
      } else {
        bsi = obj.to.struct(obj,bnames[i])
        ret = compute.variables(gs=bsi,venv=game.dt,denv=benv.li[[i]],N=N)
        had.import = bok[i] = TRUE        
      }
    }
    if ((!had.import) | (!some.checked))
      break    
  }
  
  bdt.li = lapply(benv.li,data.env.to.dt)
  bdt.li
}

#' Gets a list containing action compartors for a given variable
#' for different conditions
get.actionComparator = function(var.name, gs, comparator.type = "nicer") {
  restore.point("get.actionComparator")

  vrows = which(gs$df$name == var.name & is.subtype(gs$df$type,"variable"))

  comps = NULL
    
  vrow = vrows[1]
  for (vrow in vrows) {
    obj = gs$obj.li[[vrow]]
        
    cond <- get.conditions(vrow,gs)
    comp = obj[[comparator.type]]

    if (!has._if.children(comp)) {
      attr(comp,"cond") <- cond
      comps = c(comps,list(comp))      
    
    # The actionComparator is splitted by different _if conditions
    # add a separate actionComparator for each condition
    } else {
      upcomp = move._if.upwards(comp)
      i = 1
      for (i in seq_along(upcomp)) {
        actcond = combine.conditions(cond,upcomp[[i]][[1]])
        attributes(actcond) = NULL
        actcomp = upcomp[[i]][[2]]
        attr(actcomp,"cond") <- actcond
        comps = c(comps,list(actcomp))
      }
    }
  }
  return(comps)
}

examples.get.actionComparator = function() {
  setwd("C:/libraries/ExpEconDB")
  init.ee("C:/libraries/ExpEconDB")
  gs = load.game.struct("LureOfAuthority")
  
  get.actionComparator("recommendation",gs,comparator.type="nicer")
}

get.actionComparator.points = function(val, comp, game.dt, N= NROW(game.dt)) {
  restore.point("get.actionComparator.points")
  
  if (!is.null(comp$points)) {
    comp.points = names(comp$points)
    comp.points = lapply(comp.points,function(p) rep(eval(parse(text=p),game.dt),length.out=N))
    
    other.ind = which(sapply(comp$points, function(p) p == "_other"))
    if (length(other.ind)>0) {
      other.points = comp.points[[other.ind]]
      comp.points = comp.points[-other.ind]
      comp$points = comp$points[-other.ind]
    } else {
      other.points = NA
    }
    comp.val = lapply(comp$points,function(p) {
      return(eval(parse(text=p),game.dt))
      })
    
    points = rep(other.points, length.out = N)
    for (i in seq_along(comp.points)) {
      rows = which(is.true(val == comp.val[[i]]))
      points[rows] = comp.points[[i]][rows]
    }
    points[is.na(val)] = NA
    return(points)
  } else if (!is.null(comp$order)) {
    if (comp$order == "decreasing")
      return(-val)
    return(val)
  }
  
}

var.compare.to.behavior = function(var,bdt,game.dt,gs,comparator.type="nicer", N= NROW(game.dt)) {
  restore.point("var.compare.to.behavior")
  comps = get.actionComparator(var,gs,comparator.type=comparator.type)
  
  
  val = rep(NA,N)
  
  i = 1
  for (i in seq_along(comps)) {
    comp = comps[[i]]
    gpoints = get.actionComparator.points(val = game.dt[[var]], comp=comp,game.dt = game.dt)
    bpoints = get.actionComparator.points(val = bdt[[var]], comp=comp,game.dt = game.dt)
  
    cond = attr(comp,"cond")    
    if (!is.null(cond)) {
      rows = which(eval(parse(text=cond),game.dt) & !is.na(bpoints))
    } else {
      rows = which(!is.na(bpoints))
    }      
    arows  = rows[gpoints[rows]>bpoints[rows]]
    val[arows] = 1
    arows  = rows[gpoints[rows]<bpoints[rows]]
    val[arows] = -1
    arows  = rows[gpoints[rows]==bpoints[rows]]
    val[arows] = 0
  }
  return(val)
  
}



examples.var.compare.to.behavior = function() {
  setwd("C:/libraries/ExpEconDB")
  init.ee("C:/libraries/ExpEconDB")
  
  gameId = "LureOfAuthority"
  bs = load.behavior.struct(gameId)
  game.dt = load.game.data(gameId)
  gs = load.game.struct(gameId)
  
  bdt.li = eval.behaviors(bs,game.dt=game.dt)
  
  var.compare.to.behavior("recommendation",bdt = bdt.li$informedRecommendPreferred,game.dt=game.dt,gs=gs)
  
  compare.to.behavior(bdt = bdt.li$informedRecommendPreferred,game.dt=game.dt,gs=gs)
  bc = compare.to.behaviors(bdt.li,game.dt=game.dt,gs=gs)
  
  tabulate.behavior.comparisons(bc)
}

#' Compare actual data with a specified behavior, e.g. w.r.t. niceness 
compare.to.behavior = function(bdt,game.dt,gs, comparator.type = "nicer", T=NROW(game.dt)) {
  restore.point("is.nicer.than.behavior")

  val = rep(NA,T)
  col = names(bdt)[1]
  for (col in names(bdt)) {
    col.val = var.compare.to.behavior(col,bdt = bdt,game.dt=game.dt,gs=gs, comparator.type=comparator.type)
    NaN.rows = is.true(val * col.val == -1 | is.nan(val))
    One.rows = is.true(!NaN.rows & 
                 ((val + col.val >= 1) | ( is.na(val) & col.val==1)))
    Neg.rows = is.true(!NaN.rows & 
                         ((val + col.val <= -1) | ( is.na(val) & col.val==-1)))
    Zero.rows = is.true((val == 0 | is.na(val))  & col.val == 0)
    val[NaN.rows] = NaN
    val[One.rows] = 1
    val[Neg.rows] = -1
    val[Zero.rows] = 0
  }
  val  
}


#' Compare actual data with a specified behavior, e.g. w.r.t. niceness 
compare.to.behaviors = function(bdt.li,game.dt,gs, comparator.type = "nicer") {
  restore.point("is.nicer.than.behavior")
  lapply(bdt.li,compare.to.behavior, game.dt=game.dt,gs=gs,comparator.type=comparator.type)
}



# Which rows in data are consistent with a specified behavior?
# returns NA for rows for which the conditions of the behavior are not satisfied
matches.behavior = function(bdt,game.dt) {
  restore.point("matches.behavior")
  same = rep(TRUE,N)
  for (col in names(bdt)) {
    same = same & game.dt[[col]] == bdt[[col]]
  }
  same
}

# Which rows in data are consistent with a specified behavior?
# returns NA for rows for which the conditions of the behavior are not satisfied
matches.behaviors = function(bdt.li,game.dt) {
  li = lapply(bdt.li,matches.behavior, game.dt=game.dt)
  do.call(data.table,li)
}



import.intersectBehavior = function(i,obj,bs,benv.li,bok) {
  restore.point("import.intersectBehavior")
  subs = obj$behaviors
  if (!all(bok[subs]))
    return(FALSE)

  benv = benv.li[[i]]
  for (sub in subs) {
    sub.env = benv.li[[sub]]
    vars = objects(sub.env)
    for (v in vars) {
      if (!exists(v,benv)) {
        benv[[v]] = sub.env[[v]]
      } else {
        rows = !is.na(sub.env[[v]])
        benv[[v]][rows] = sub.env[[v]][rows]
      }
    }           
  }
  return(TRUE)
}


tabulate.behavior.comparisons = function(bc, comparator.type = "nicer") {
  li = lapply(bc,function(val) {
    list("bigger" = sum(is.true(val==1)), "equal" = sum(is.true(val==0)),
         "smaller" = sum(is.true(val==-1)),"not.comparable" = sum(is.nan(val)),
         "NA" = sum(is.na(val)) )
  })
  df = as.data.frame(rbindlist(li))
  rownames(df) = names(bm)
  df$bigger.share = round(df$bigger / (df$bigger+df$equal+df$smaller+df$not.comparable),4)
  df$equal.share = round(df$equal / (df$bigger+df$equal+df$smaller+df$not.comparable),4)
  df$smaller.share = round(df$smaller / (df$bigger+df$equal+df$smaller+df$not.comparable),4)
  
  
  if (comparator.type == "nicer") {
    colnames(df) = c("nicer","as.nice","less.nice","not.comparable", "NA","nicer.share","as.nice.share","less.nice.share")
  }
  df
}

examples.tabulate.behavior.comparisons = function() {
  setwd("C:/libraries/ExpEconDB")
  init.ee("C:/libraries/ExpEconDB")
  
  gameId = "LureOfAuthority"
  bs = load.behavior.struct(gameId)
  game.dt = load.game.data(gameId)
  gs = load.game.struct(gameId)
  
  bdt.li = eval.behaviors(bs,game.dt=game.dt)
  bc = compare.to.behaviors(bdt.li,game.dt=game.dt,gs=gs)  
  tabulate.behavior.comparisons(bc)
}


tabulate.behavior.matches = function(bm) {
  li = lapply(bm,function(val) {
         list(true = sum(is.true(val)), false = sum(is.false(val)),
              "NA" = sum(is.na(val)) )
        })
  df = as.data.frame(rbindlist(li))
  rownames(df) = names(bm)
  df$true.share = round(df$true / (df$true+df$false),4)
  df$na.share = round(df[["NA"]] / (df$true+df$false+df[["NA"]]),4)
  df
}

examples.load.behavior.struct = function() {
  setwd("C:/libraries/ExpEconDB")
  init.ee("C:/libraries/ExpEconDB")

  gameId = "LureOfAuthority"
  bs = load.behavior.struct(gameId)
  bs
  bs$obj.li[[97]]
  dt.li = tableBehavior.comp.stat.dt(bs=bs,vars=c("searchP","searchA","util_1","util_2"),gameId=gameId)
  dt = dt.li$util_1
  d = dt[key == "delegate",]
  
  d$better.del = d$val1 <= d$val2
  setkeyv(d,c("key","var","behavior", "variant", "keyv1","keyv2"))
  d
  
  game.dt = load.game.data(gameId)
  
  bdt.li = eval.behaviors(bs,game.dt=game.dt)
  bm = matches.behaviors(bdt.li,game.dt)
  tabulate.behavior.matches(bm)
  
  dt = cbind(game.dt,be)
}

tableBehavior.comp.stat.dt = function(bs,vars, be = NULL, keyCols=NULL,gameId=NULL) {
  restore.point("tableBehavior.comp.stat")
  ret = combine.tableBehavior(bs=bs,vars=vars,be=be,keyCols=keyCols,gameId=gameId)
  be = ret$be; vars=ret$vars; keyCols=ret$keyCols; be.dt=ret$dt
  be.dt

  bk.cols = c("behavior",keyCols)
  # Simply remove multiple equilibria!!!!!
  # Need to correct!!!
  be.dt = be.dt[!duplicated(be.dt[,bk.cols,with=FALSE]),]
  
  bk.list = lapply(bk.cols,function(col) unique(be.dt[[col]]))
  names(bk.list) = bk.cols
  
  var = "util_1"
  key = "delegate"
  csgrid.li = lapply(vars, function(var) {
    grid.li = lapply(bk.cols, function(key) {
      myvar = var
      restore.point("jhfjsdhfj")
      var = myvar
      vals = bk.list[[key]]
      if (length(vals)<=1)
        return(NULL)
      # All possible value combinations
      vg = expand.grid(list(seq_along(vals),seq_along(vals)))
      vg = vg[vg[,1]<vg[,2],]
      vg[] = vals[as.matrix(vg)]
      names(vg) = c("keyv1","keyv2")
      vg
      
      other.cols = setdiff(bk.cols,key)
      grid.list = c(list(vg.ind = 1:NROW(vg)),bk.list[other.cols])
      gr = as.data.table(expand.grid(grid.list))
      
      i = 1
      gr[[key]] = vg[gr$vg.ind,i]
      mgr = merge(gr,be.dt, all.y=FALSE, by = bk.cols)
      
      dt = mgr[,c(bk.cols,"vg.ind"),with=FALSE]    
      
      dt$keyv1 = vg[mgr$vg.ind,i]
      dt$val1 = mgr[[var]]
  
      i = 2
      gr[[key]] = vg[gr$vg.ind,i]
      mgr = merge(gr,be.dt, all.y=FALSE, by = bk.cols)
      
      dt$keyv2 = vg[mgr$vg.ind,i]
      dt$val2 = mgr[[var]]
  
      dt$key = key
      dt$var = var
      dt[,vg.ind:=NULL]
      dt[[key]] = vals[1]
      setcolorder(dt,c(bk.cols,c("var", "key", "keyv1","keyv2","val1","val2")))
      dt
    })
    rbindlist(grid.li)
  })
  
  names(csgrid.li)= vars
  csgrid.li
  

}

# Do not deal with strategyMethod yet
make.obsTableBehavior = function(gameId,vars, keyCols, meanMedian="mean",obsData=NULL, util=NULL,n=NULL) {
  restore.point("make.obsTableBehavior")
  
  # Load data
  if (is.null(obsData)) {
    obsData = load.game.data(gameId=gameId)
  }
  # Get number of players
  if (is.null(n)) {
    str = colnames(obsData)
    str = str[str.starts.with(str,"player_")]
    str = str.split(str,"_")
    iv = sapply(str,function(v) as.numeric(v[2]))
    n = max(iv)
  }
  
  vars = union(vars,paste0("util_",1:n))
  
  # Get utility formula
  if (is.null(util)) {
    util = list("payoffUtil",n=n)
  }
  util.formula = do.call(util[[1]],util[-1])
  
  # Add utility to observed data
  for (i in 1:n) {
    run.dt(obsData,paste0("util_",i," := ", util.formula[i]))
  }

  # Make obsData smaller
  obsData = obsData[,c(keyCols,vars),with=FALSE]
  
  # Aggregate data by keyCol
  fun = meanMedian[1]
  code = paste0("list(", paste0(vars,"=",fun,"(",vars,")", collapse=","), ")")
  data = run.dt(obsData,code,by=keyCols)

  
  # Generate name and shortName
  name = paste0("obs_",gameId,"_",paste0(util[[1]]),"_",paste0(sample(c(1:9,LETTERS),5),collapse=""))
  
  arg.names = setdiff(names(util[-1]),"n")
  if (length(arg.names)>0) {
    args = substring(as.character(unlist(util[arg.names])),1,4)
    shortName = paste0("obs_",meanMedian[1],"_",str.replace(util[[1]],"Util","")[[1]],"_",paste0(args,collapse="_"))
  } else {
    shortName = paste0("obs_",meanMedian[1],"_",str.replace(util[[1]],"Util","")[[1]])      
  }
  
  # Generate object
  values = list(sourceGameId=gameId,destGameId=gameId,shortName=shortName,numPlayers=n,numRows=NROW(data),keyCols=keyCols,meanMedian=meanMedian, actions=setdiff(vars,paste0("util_",1:n)), multiple=FALSE,obsData=obsData,data=data)
  
  obj = new.obj(typeName="obsTableBehaviors",name=name,values = values)
  obj
}

examples.make.obsTableBehavior = function() {
  gameId = "LureOfAUthority"
  vars = c("searchP","searchA","util_1","util_2")
  keyCols = c("variant","delegate")
  make.obsTableBehavior(gameId=gameId,vars=vars,keyCols=keyCols)
}


combine.tableBehavior = function(bs,vars,be=NULL,keyCols=NULL, add.obs=TRUE, gameId, meanMedian="mean") {
  restore.point("tableBehavior.comp.stat")
  
  # Find relavant behaviors manually
  if (is.null(be)) {
    rows = is.subtype(bs$df$type,"tableBehaviors")
    objs = bs$obj.li[rows]
    names(objs) = bs$df$name[rows]
    has.action = sapply(objs, function(obj) all(vars %in% names(obj$data)))
    objs = objs[has.action]
    be = names(objs)
  } else {
    objs = bs$obj.li[[1]][be]
  }
  
  
  # Find common keyCols
  if (is.null(keyCols)) {
    li = lapply(objs, function(obj) obj$keyCols)
    keyCols = intersect.vector.list(li)
  }

  if (add.obs) {
    obs.obj = make.obsTableBehavior(gameId=gameId,vars=vars,keyCols=keyCols)
    obs.objs = list(obs.obj)
    names(obs.objs) = get.name(obs.obj)
    objs = c(objs,obs.objs)
  }
  
  
  dt = rbindlist(lapply(objs, function(obj) {
    restore.point("hshfghsf")
    dt = obj$data[,c(keyCols,vars),with=FALSE]
    dt[,"behavior":=obj$shortName,with=FALSE]
    setcolorder(dt,c("behavior",keyCols,vars))
    dt
  })) 
  return(list(be=be,keyCols=keyCols,vars=vars,dt=dt))
}
