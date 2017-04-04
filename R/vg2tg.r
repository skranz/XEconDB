# Convert variant stages form game (vg) to a table form game (tg)

# A table form game consists of different level:
# one level for each move of nature and action definition
# we also have computation levels, which may later be skipped, however.

# We have the following data frames in each level:
#
#   columns all defined variables 
#   rows = outcomes: each feasible value combination of the vars
#   oco.df: variable values for each outcome
#     special columns:
#       .player_i (for each player a boolean, 
#                  whether she is active in the stage)
#       .node.ind 
#       .move.ind (action + randomVar)
#       .info.set (action)
#       .prob     (randomVar)
#       
#   know.mat:  defined for each player. 
#           TRUE=Players knows that variable in the outcome

examples.vg.to.tg = function() {
  setwd("D:/libraries/XEconDB/projects/UltimatumGame")
 
	vg = get.vg(gameId="MaxUltimatum")
  tg = vg.to.tg(vg)
  et.mat = tg$et.mat
  oco.df = tg$oco.df
  lev.li = tg$lev.li
}

tg.msg.fun = function(...) {
	cat(paste0("\n",...))
}

vg.to.tg = function(vg, max.rows = Inf, add.sg=TRUE, add.spi=TRUE, add.spo=FALSE, msg.fun = tg.msg.fun) {
  restore.point("vg.to.tg")
	
	if (is.null(msg.fun)) msg.fun = function(...) {}
	msg.fun("Compute game tree for ", vg$gameId," variant ", vg$variant,"...")

  tg = new.env()
  tg$kel = vg$kel
  
  tg$max.rows = max.rows
  
  tg$gameId = vg$gameId
  tg$variant = vg$variant
  tg$jg.hash = vg$jg.hash
  
  tg$params = c(list(variant=tg$variant),vg$params)
  tg$lev.li = list()
  tg$n = tg$numPlayers =  tg$params$numPlayers
  tg$players = 1:tg$numPlayers
  tg$stages = vector("list",length(vg$stages))
  tg$info.set.counter = 0
  tg$info.set.move.counter = 0
  
  
  tg$stage.df = as_data_frame(as.data.frame(tg$params,stringsAsFactors = FALSE))
  tg$stage.df$.prob = 1
  tg$know.li = lapply(1:tg$n,function(i) {
    mat = matrix(TRUE, 1, length(tg$params))
    colnames(mat) = setdiff(colnames(tg$stage.df),".prob")
    mat
  })

  stage.num = 0
  while (stage.num < length(vg$stages)) {
 	
    stage.num = stage.num+1
 		msg.fun("Gametree for ", vg$gameId," variant ", vg$variant,": Add stage ", vg$stages[[stage.num]]$name, " (", NROW(tg$stage.df)," outcomes so far) ...")
    tg$kel$setKey("stages", stage.num)
    stage <- try(compute.tg.stage(stage.num, tg, vg, tg$kel))
    if (tg$kel$count>0) return(tg)


    tg$lev.li
    tg$stage.df
    tg$know.li
    #stage <- tg$kel$kelTry(compute.tg.stage(stage.num, tg, vg, tg$kel),msg = "{{error}}",default=NULL)
    if (is.null(stage)) {
      tg$failed = TRUE
      return(tg)
    }
    tg$stages[[stage.num]] = stage
  }
 	msg.fun("Game tree for ", vg$gameId," variant ", vg$variant,": All stages parsed (",NROW(tg$stage.df)," outcomes), compute info sets...")
  
  df = tg$stage.df

  # sort oco.df
  order.cols = paste0(".row.", seq_along(tg$lev.li))
  if (all(order.cols %in% colnames(df)))
  	df = arrange_(df,.dots = order.cols)

  tg$lev.vars = unique(sapply(tg$lev.li, function(lev) lev$var))
  tg$vars = unique(c(names(tg$params), tg$lev.vars))
  
  # Equilibrium template matrix
  et.mat = matrix(1,NROW(df),length(tg$lev.vars))
  colnames(et.mat) = tg$lev.vars
  for (lev.num in seq_along(tg$lev.li)) {
    lev = tg$lev.li[[lev.num]]
    lev.df = lev$lev.df
    row.col = paste0(".row.",lev.num)
    rows = match(df[[row.col]],lev.df[[row.col]])
    
    set.rows = !is.na(rows)
    if (lev$type == "action") {
      et.mat[set.rows,lev$var] = - lev.df$.info.set.move.ind[rows[set.rows]]
    } else {
      et.mat[set.rows,lev$var] = lev.df$.move.prob[rows[set.rows]]
    }
  }
  tg$et.mat = et.mat
  
  
  # reorder helper cols that are only interesting 
  # in lev.li
  cols = colnames(df)
  cols = c(unique(c(cols[!str.starts.with(cols,".")])),".prob")
  df = df[,cols, drop=FALSE]
  df$.outcome = seq.int(NROW(df))
  tg$oco.df = df

  # know.var groups help to compute iso.df
  # later on
  make.tg.know.var.groups(tg)
  make.tg.ise.df(tg)
  make.tg.iso.df(tg)
  
  # set payoff utility as standard
  set.tg.util(tg=tg)
  
  if (add.sg) {
  	msg.fun("Game tree for ", vg$gameId," variant ", vg$variant,": All stages parsed (",NROW(tg$stage.df)," outcomes), compute subgames...")
  	compute.tg.subgames(tg)
  }
	if (add.spi) {
	 	msg.fun("Game tree for ", vg$gameId," variant ", vg$variant,": All stages parsed (",NROW(tg$stage.df)," outcomes), compute spi...")
		make.tg.spi.li(tg)
	}
	if (add.spo) {
	 	msg.fun("Game tree for ", vg$gameId," variant ", vg$variant,": All stages parsed (",NROW(tg$stage.df)," outcomes), compute spo table...")
  	make.tg.spo.li(tg)
	}
	 msg.fun("Game tree for ", vg$gameId," variant ", vg$variant,": completely generated.")

  return(tg)
}

compute.tg.stage = function(stage.num, tg, vg, kel) {
  prev.stage.df = tg$stage.df
  prev.know.li = tg$know.li
  
  restore.point("compute.tg.stage")
  vg.stage = vg$stages[[stage.num]]
  stage = as.environment(list(
    stage.num = stage.num
  ))

  base.key = kel$key
  
  # compute condition
  kel$setKey(base.key,"condition")
  tg.compute.stage.condition(tg, stage, vg.stage, prev.stage.df, prev.know.li, kel)
  stage$stage.df
  stage$know.li
  
  # compute player set for each node
  kel$setKey(base.key,"player")
  tg.compute.stage.players(tg, stage, vg.stage, kel)
  
  stage$stage.df
  
  # update knowledge
  kel$setKey(base.key,"observe")
  tg.update.stage.knowledge(tg, stage, vg.stage, kel)
  
  lev = list(lev.df = stage$stage.df, know.li=stage$know.li)

  # compute moves of nature
  for (i in seq_along(vg.stage$nature)) {
    randomVar = vg.stage$nature[[i]]
    kel$setKey(base.key,"nature",i)
    lev = compute.nature.level(tg,stage, randomVar, lev$lev.df, lev$know.li, kel)
  }
  
  # compute transformations
  for (i in seq_along(vg.stage$compute)) {
    trans = vg.stage$compute[[i]]
    kel$setKey(base.key,"compute",i)
    lev = compute.transformation.level(tg,stage, trans, lev$lev.df, lev$know.li, kel)
  }
  
  # compute actions
  for (i in seq_along(vg.stage$actions)) {
    action = vg.stage$actions[[i]]
    kel$setKey(base.key,"actions",i)
    lev = compute.action.level(tg,stage, action, lev$lev.df, lev$know.li, kel)
 
  }

  
  stage$lev = lev
  
    
  # add missing rows to stage.df
  stage.df = lev$lev.df
  know.li = lev$know.li
  if (length(stage$ignore.rows)>0) {
    restore.point("add ignored rows")
    stage.df = bind_rows(list(
      stage.df,
      tg$stage.df[stage$ignore.rows,,drop=FALSE]
    ))
    know.li = lapply(seq_along(know.li), function(i) {
      mat = as.matrix(bind_rows(list(
        as_data_frame(know.li[[i]]),
        as_data_frame(tg$know.li[[i]][stage$ignore.rows,,drop=FALSE])
      )))
      mat[is.na(mat)] = FALSE
      mat
    })
  }
  tg$stage.df = stage.df
  tg$know.li = know.li
  stage
}


tg.compute.stage.condition = function(tg, stage, vg.stage, prev.stage.df, prev.know.li,  kel) {
  restore.point("tg.compute.stage.condition")
  cond = vg.stage$cond
  if (!is.call(cond) &!is.name(cond)) {
    # no condition
    if (identical(str.trim(vg.stage$cond), "")) {
      stage$ignore.rows = NULL
      stage$stage.df = prev.stage.df
      stage$know.li = prev.know.li
      return()
    }
    kel$error("Either you specify no stage condition, or you write an R formula starting with '=', which evaluates as TRUE or FALSE.")
  }
  
  
  # rows that satisfy the condition  
  rows = is.true(eval.on.df(cond,prev.stage.df))
  stage$ignore.rows = which(!rows)
  # reduce level.df and know mats to those rows
  stage$stage.df = prev.stage.df[rows,,drop=FALSE]
  for (i in tg$players) {
    stage$know.li[[i]] = prev.know.li[[i]][rows,,drop=FALSE]
  }
  return()
}


tg.compute.stage.players = function(tg, stage, vg.stage, kel) {
  restore.point("tg.compute.stage.players")
  # compute player set for each node
  df = stage$stage.df
  call = vg.stage$player
  
  # fixed player sets
  if (!is(call, "call")) {
    stage$fixed.players = TRUE
    stage$players = call
    stage$multi.player = length(call)>1
    for (i in tg$players) {
      df[[paste0(".player_",i)]] = i %in% stage$players
    }
    if (identical(stage$players,"")) stage$players = NA
    df[[".player"]] = stage$players[1]
    stage$stage.df = df
    return()
  }
  
  # players is a call
  df$.ROW = seq.int(NROW(df))
  # reduce df to unique combination of used variables
  vars = find.variables(call)
  
  if (length(vars)==0) {
    kel$error("Please only use a formula in players if it depends on some earlier defined parameter or variable.")
  }
  
  if (length(unknown <- setdiff(vars, colnames(df)))>0) {
    kel$error("Your observe formula depends on the variables {{unknown}}, which have not been defined earlier.", unknown=unknown)
  }


    
  sdf = as_data_frame(unique(df[,vars,drop=FALSE]))
  
  for (i in tg$players) {
    sdf[[paste0(".player_",i)]] = FALSE
    df[[paste0(".player_",i)]] = FALSE
  }
  
  for (row in seq.int(NROW(sdf))) {
    rdf = sdf[i,,drop=FALSE]
    players = eval(call,rdf)
    if (length(players)==0) next
    if (length(unknown <- setdiff(players, tg$players))>0) {
        kel$error("Your evaluated formula states to observe the variable(s) {{unknown}}, which have not been defined earlier.", unknown=unknown)
      }
    cols = paste0(".player_",players)

    # get rows in original df
    mdf = left_join(rdf,df, by="vars")
    rows = mdf$.ROW
    
    for (i in players) {
      df[rows,cols[i]] = TRUE
    }
    df[rows,".player"] = players[1]
  }
  
  stage$stage.df = df
  return()
}

tg.update.stage.knowledge = function(tg, stage, vg.stage, kel) {
  observe = vg.stage$observe
  know.li = stage$know.li
  df = stage$stage.df
  restore.point("tg.update.stage.knowledge")
  
  # observe is fixed, no formula
  if (!is(observe, "call")) {
    if (length(observe)==0 | identical(observe,"")) return(know.li)
    if (length(unknown <- setdiff(observe, colnames(df)))>0) {
      kel$error("You cannot observe the variable(s) {{unknown}}, because they have not been defined earlier.", unknown=unknown)
    }
    
    # the relevant player now knows the observed variables
    for (i in tg$players) {
      know.li[[i]][,observe] = know.li[[i]][,observe] | df[[paste0(".player_",i)]]
    }
    stage$know.li = know.li
    return()
  }
  
  # observe is a formula
  call = observe

  
  df$.ROW = seq.int(NROW(df))
  # reduce df to unique combination of used variables
  vars = find.variables(call)
  
  if (length(vars)==0) {
    kel$error("Please only use a formula in observe if the observed variables depend on some earlier defined parameter or variable.")
  }
  
  if (length(unknown <- setdiff(vars, colnames(df)))>0) {
    kel$error("Your observe formula depends on the variables {{unknown}}, which have not been defined earlier.", unknown=unknown)
  }
  
  sdf = as_data_frame(unique(df[,vars,drop=FALSE]))
  
  for (row in seq.int(NROW(sdf))) {
    # compute set of observed vars
    rdf = sdf[i,,drop=FALSE]
    obs.vars = eval(call,rdf)
    if (length(obs.vars)==0) next
    
    # get rows in original df
    mdf = left_join(rdf,df, by="vars")
    rows = mdf$.ROW

    if (length(unknown <- setdiff(obs.vars, colnames(df)))>0) {
      kel$error("Your evaluated observe formula states to observe the variable(s) {{unknown}}, which have not been defined earlier.", unknown=unknown)
    }
    for (i in players) {
      know.li[[i]][rows,obs.vars] = know.li[[i]][rows,obs.vars] | df[rows,paste0(".player_",i)]
    }
  }
  stage$know.li = know.li
  return()
}





compute.action.level = function(tg,stage, action,lev.df, know.li, kel) {
  lev.num = length(tg$lev.li)+1
  restore.point("parse.tg.action")

  if (isTRUE(stage$multi.player)) {
    kel$error("Currently actions can only be defined for stages with a single player!")  
  }
  
  
  # make info set
  var = action$name
  check.var.name(var, kel)

  # check if all var in set are defined
  kel$withKey(sub.key = "set",
    kel.check.call.vars(action$set,names(lev.df),kel=kel)
  )

  
  # remove var column
  # neccessary if for other conditions
  # it has been defined before

  # don't remove .player etc!
  lev.df = remove.cols(lev.df,c(var,".move.prob", ".info.set",".move.ind"))

  lev.df$.node.ind = seq.int(NROW(lev.df))

    
  .info.set = compute.info.sets(lev.df,know.li,var)
  
  unique.id = unique(.info.set)
  .info.set.ind = match( .info.set,unique.id) +tg$info.set.counter
  
  lev.df$.info.set = .info.set
  lev.df$.info.set.ind = .info.set.ind

  tg$info.set.counter = max(.info.set.ind)
  
  # eval set
  lev.df = eval.set.to.df(action$set, lev.df, var)
  tg.check.max.rows(tg=tg, lev.df = lev.df, kel=kel)
  
  
  lev.df = lev.df %>% 
    group_by(.node.ind) %>%
    mutate(.move.ind=1:n()) %>% 
    ungroup()

  # compute global .info.set.move.ind 
  # this is needed to map to gambit equilibria
  lev.df = lev.df %>% 
    arrange(.info.set.ind, .move.ind, .node.ind) %>%
    group_by(.info.set.ind) %>%
    mutate(.num.moves = max(.move.ind), .is.first = (1:n() == 1)) %>%
    ungroup() %>%
    mutate(.offset = cumsum(.is.first * (.num.moves)) - .num.moves) %>%
    mutate(.info.set.move.ind = .move.ind + .offset + tg$info.set.move.counter) %>%
    remove.cols(c(".offset",".num.moves",".is.first"))
  
  tg$info.set.move.counter = max(lev.df$.info.set.move.ind)

  

  # save this levels node ind and move ind to 
  # reference back later to this level
  #lev.df[[paste0(".node.ind.", lev.num)]] = lev.df$.node.ind
  #lev.df[[paste0(".move.ind.", lev.num)]] = lev.df$.move.ind

  # save this level's rows to
  # reference back later to this level
  lev.df[[paste0(".row.", lev.num)]] = seq.int(NROW(lev.df))

   
  # row vector for expanding know.mat
  erows = lev.df$.node.ind
  
  # update knowledge matrices
  know.li = lapply(seq_along(know.li), function(i) {
    mat = add.var.to.know.mat(know.li[[i]][erows,,drop=FALSE],var, lev.df$.player == i)
    mat
  })

  lev = nlist(
    type="action",
    var = var,
    lev.num,
    lev.df,
    know.li
  )
  tg$lev.li[[lev.num]] = lev
  lev
}

# transform a knowledge matrix
# to a vector of unique information sets
compute.info.sets = function(lev.df, know.li,var) {
  restore.point("compute.info.set")

  oco.mat = as.matrix(lev.df)

  ise.id = rep("",NROW(lev.df))
  players = unique(lev.df$.player)
  
  for (i in players) {
    rows = lev.df$.player == i
    know.mat = know.li[[i]][rows,,drop=FALSE]
    # different knowledge and inf 
    cols = intersect(colnames(oco.mat), colnames(know.mat))
    val.mat = oco.mat[rows,cols, drop=FALSE]
    val.mat[!know.mat[,cols]] = "."
    temp.id = paste.matrix.cols(val.mat)
  
    # transform to integer
    unique.id = unique(temp.id)
    ise.ind = match(temp.id, unique.id)
    ise.id[rows] = paste0(i,"_",var,"_",ise.ind)
  }
  ise.id
}


compute.nature.level = function(tg,stage, randomVar, lev.df, know.li, kel) {
  lev.num = length(tg$lev.li)+1
  restore.point("compute.nature.level")
  var = randomVar$name
  check.var.name(var, kel)
  
  # check if all var in set are defined
  kel$withKey(sub.key = "set",
    kel.check.call.vars(randomVar$set,names(lev.df),kel=kel)
  )
  # check if all var in probs are defined
  kel$withKey(sub.key = "probs",
    kel.check.call.vars(randomVar$probs,names(lev.df),kel=kel)
  )
 
  # don't remove .player etc!
  lev.df = remove.cols(lev.df,c(var, ".info.set",".move.ind"))
  lev.df$.node.ind = seq.int(NROW(lev.df))
  lev.df = eval.randomVar.to.df(randomVar$set,randomVar$prob,df = lev.df, var=var,kel = kel,prob.col = ".move.prob")
  tg.check.max.rows(tg=tg, lev.df = lev.df, kel=kel)
  
  # adapt outcome probs
	lev.df$.prob = lev.df$.prob * lev.df$.move.prob 
  
  
  
  if (!has.col(lev.df,".move.ind")) {
    lev.df = lev.df %>% 
      group_by(.node.ind) %>%
      mutate(.move.ind=1:n()) %>% 
      ungroup()
  }
 
  # save this level's rows to
  # reference back later to this level
  lev.df[[paste0(".row.", lev.num)]] = seq.int(NROW(lev.df))
  
  # rows for expanding
  erows = lev.df$.node.ind
  # update knowledge matrices
  know.li = lapply(seq_along(know.li), function(i) {
    mat = add.var.to.know.mat(know.li[[i]],var)
    mat[erows,]
  })
  
  lev = nlist(
    type="nature",
    var = var,
    lev.num,
    player=0,
    lev.df,
    know.li
  )
  tg$lev.li[[lev.num]] = lev
  lev  
}

add.var.to.know.mat = function(know.mat, var, value=FALSE) {
  restore.point("add.var.to.know.mat")
  if (var %in% colnames(know.mat)) {
    know.mat[,var] = value
  } else {
    know.mat = cbind(know.mat,value)
    colnames(know.mat)[NCOL(know.mat)] = var
  }   
  know.mat
}

eval.or.return = function(call,...) {
  if (!is(call,"call") & !is(call,"expression")) return(call)
  eval(call,...)
}

adapt.prob.to.set = function(prob,set) {
  restore.point("adapt.prob.to.set")
  
  if (length(prob)==0 | is.null(prob) | identical(prob,"")) {
    prob = rep(1 / length(set), length(set))
  } else {
    prob = rep(prob, length.out=length(set))
    # normalize to 1
    prob = prob / sum(prob)
  }
  prob
}

eval.randomVar.to.df = function(set.call, prob.call, df, var, kel, prob.col = ".move.prob") {
  restore.point("eval.randomVar.to.df")

  set.vars = NULL
  prob.vars = NULL
  
  set.is.call = is(set.call,"call") | is(set.call,"name")
  prob.is.call = is(prob.call,"call") | is(prob.call,"name")
  
  if (set.is.call) set.vars = find.variables(set.call)
  if (prob.is.call) prob.vars = find.variables(prob.call)

  vars = c(set.vars, prob.vars)

  # set and prob are both defined independently of the data frame
  if (length(vars)==0) {
    set = eval.or.return(set.call)
    prob = eval.or.return(prob.call)
    prob = adapt.prob.to.set(prob,set)
    df$.move.ind = replicate(NROW(df),seq_along(set),simplify = FALSE)     
    df = unnest(df,.move.ind)
    df[[var]] = set[df$.move.ind]
    df[[prob.col]] = prob[df$.move.ind]
    return(df)
  }

  sdf = as_data_frame(unique(df[,vars,drop=FALSE]))
  # just a single variable combination
  if (NROW(sdf)==1) {
    set = eval.or.return(set.call,sdf)
    prob = eval.or.return(prob.call,sdf)
    prob = adapt.prob.to.set(prob,set)
    df$.move.ind = replicate(NROW(df),seq_along(set),simplify = FALSE)     
    df = unnest(df,.move.ind)
    df[[var]] = set[df$.move.ind]
    df[[prob.col]] = prob[df$.move.ind]
    return(df)
  }

  set.class = "character"
  # compute set probability string for each row of sdf
  sdf$.sepro = lapply(seq.int(NROW(sdf)), function(i) {
    values = sdf[i,,drop=FALSE]
    set = eval.or.return(set.call,values)
    if (i == 1) set.class <<- class(set)[1]
    prob = eval.or.return(prob.call,values)
    prob = adapt.prob.to.set(prob,set)
    sepro = paste0(prob,";", set)
    sepro
  })

  sdf = unnest(sdf,.sepro)
  sdf[[var]] = as(str.right.of(sdf$.sepro,";"), set.class)
  sdf[[prob.col]] = as.numeric(str.left.of(sdf$.sepro,";"))
  sdf = remove.cols(sdf, ".sepro")
  res = right_join(df,sdf,by=vars)
  res
}



compute.transformation.level = function(tg,stage, trans, lev.df, know.li,kel) {
  restore.point("compute.transformation.level")
  lev.num = length(tg$lev.li)+1
  var = trans$name
  check.var.name(var, kel)

  # check if all var in formula are defined
  kel$withKey(sub.key = "formula",
    kel.check.call.vars(trans$formula,names(lev.df),kel=kel)
  )
  
  
  # don't remove .player etc!
  lev.df = remove.cols(lev.df,c(var,".move.prob", ".info.set",".move.ind"))

  lev.df$.node.ind = seq.int(NROW(lev.df))
  
  # eval formula on df
  if (!is.call(trans$formula) &!is.name(trans$formula)) {
    val = trans$formula
  } else {
    val = eval.on.df(trans$formula, lev.df)
  }
  
  
  lev.df[[var]] = val
  
  # update knowledge matrices
  know.li = lapply(seq_along(know.li), function(i) {
    add.var.to.know.mat(know.li[[i]],var)
  })
  
  lev = nlist(
    type="transformation",
    lev.num,
    var,
    player=0,
    lev.df,
    know.li
  )
  # We don't save transformations
  # in order to save memory
  #tg$lev.li[[lev.num]] = lev
  lev
}

check.var.name = function(var, kel) {
  if (is.null(var) | var == "") {
    kel$error("You must specify a valid variable name.")
  }
}

kel.check.call.vars = function(call, known.vars, kel) {
  if (!is.call(call) & !is.name(call)) return(TRUE)
  vars = find.variables(call)
  unknown = setdiff(vars, known.vars)
  if (length(unknown)) {
    kel$error(paste0("The referenced variable(s) ", paste0(unknown, collapse=", ")," have not yet been defined."))
  }
  
}

tg.check.max.rows = function(tg, lev.df, kel=tg$kel) {
	if (isTRUE(NROW(lev.df) > tg$max.rows)) {
    kel$error(paste0("Maximum size of outcomes .", tg$max.rows, " has been exceeded. Stop checking..."))
	}
	
}
