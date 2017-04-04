examples.make.tg.iso.df = function() {
  setwd("D:/libraries/XEconDB/projects/UltimatumGame")
	tg = get.tg(gameId="BunchedUltimatum", never.load=TRUE)
	tg = get.tg(gameId="TwoChoices",never.load = TRUE)
	set.tg.util(tg)
	make.tg.iso.df(tg)
	make.tg.ise.df(tg)
	ise.df = tg$ise.df
	iso.df = tg$iso.df
	lev1 = tg$lev.li[[1]]$lev.df
	#lev3 = tg$lev.li[[3]]$lev.df
	
	compute.tg.subgames(tg)
	sg.df = tg$sg.df
	sgi.df = tg$sgi.df
	
	make.tg.spi.li(tg)
	spi = tg$spi.li[[2]]
	spi = tg$spi.li[[1]]

	make.tg.spo.li(tg)
	
	make.sg.spo.df(.sg.ind = 1,tg=tg)
	tg$spo.li
	
	solve.all.tg.spe(tg=tg)
	tg$spe.li
	eqo.df = tg$spe.li[[1]]$eqo.df
	
	solve.sg.spe(.sg.ind = 1, tg=tg)
}




make.tg.iso.df = function(tg) {
	tg$iso.df = tg.to.iso.df(tg)
}

# information sets moves and outcomes
tg.to.iso.df = function(tg) {
  restore.point("tg.to.iso.df")  
  lev.li = tg$lev.li

  levels = which(sapply(lev.li, function(lev) lev$type=="action"))
  li = lapply(levels, function(le) {
    lev.df.to.iso(lev=lev.li[[le]])
  })
  iso.df = bind_rows(li)
  #iso.df = select(iso.df, .player, .lev.num,.info.set.ind, .info.set,.move.ind,.move.name, .infeasible)
	iso.df = select(iso.df, .player, .lev.num,.info.set.ind, .info.set,.move.ind,.var,.char.move.val)  
  
  iso.df = arrange(iso.df, .player, .lev.num, .info.set.ind, .move.ind)
  iso.df
}


# computes the iso.df from a level.df
lev.df.to.iso = function(lev) {
  restore.point("lev.df.to.iso")
  
	lev.df = lev$lev.df
	know.mat = lev$know.mat
  #know.var.group = identical.rows.groups(know.mat)
  know.var.group = lev.df$.know.var.group
  
  kv = 1
  li = lapply(unique(know.var.group), function(kv) {
    rows = which(know.var.group==kv)
    if (length(rows)==0) return(NULL)
    know.vars = lev$know.var.li[[kv]]

    by = unique(c(know.vars, lev$var))
    
    idf = lev.df[rows,c(by,".move.ind",".info.set",".info.set.ind", ".player"), drop=FALSE]

    idf$.char.move.val = as.character(lev.df[[lev$var]][rows])
    idf = unique(idf)
    
    df = select(idf,.player,.info.set,.info.set.ind,.move.ind, .char.move.val)
    return(df)
    
    # ommited compute infeasible
    jdf = left_join(idf, oco.df[,c(".outcome",by)], by=by)
    
    fun = function(idf) {
      outcomes = unique(idf$.outcome)
      summarise(group_by(idf,.move.ind, .move.name, .player), .infeasible = list(setdiff(outcomes, .outcome)))
    }
    inf.df = do(group_by(jdf,.info.set.ind,.info.set), fun(.)) %>% ungroup()
    inf.df
  })
  res = bind_rows(li)
  res$.lev.num = lev$lev.num
  res$.var = lev$var
  
  res
}

# computes the ise.df contains some summary statistics for information sets
# these will be helpful when defining subgames later on
make.tg.ise.df = function(tg) {
	restore.point("tg.to.ise.df")
	tg$action.levels = which(sapply(tg$lev.li, function(lev) lev$type=="action"))
	
	li = lapply(tg$lev.li[tg$action.levels], function(lev) {
		#restore.point("sfhdhfhdu")
		lev$lev.df %>%
			group_by(.info.set.ind, .info.set, .player) %>%
			summarize(.num.moves = length(unique(.move.ind)), .num.nodes = sum(.move.ind==1), .lev.num = as.integer(lev$lev.num))
	})
	tg$ise.df = bind_rows(li)
	invisible(tg)
}

# known.var groups:
# group multiple information sets on levels to know.var sets
# each information set in which the same variables are observed
# will belong to the same know.var group.
# Note that information sets from different players may belong
# to the same know.var group.
# know.var groups are only used to allow fast
# vectorized computation of the iso.df
make.tg.know.var.groups = function(tg) {
	tg$lev.li = lapply(tg$lev.li, make.tg.lev.know.var.groups)
}
make.tg.lev.know.var.groups = function(lev) {
	restore.point("make.tg.lev.know.var.groups")
	if (lev$type != "action") return(lev)
	
	lev.df = lev$lev.df
	players = unique(lev.df$.player)
	know.mat = lev$know.li[[players[1]]]
	know.mat[,] = NA
	
	for (player in players) {
		rows = lev.df$.player == player
		know.mat[rows,] = lev$know.li[[player]][rows,]
	}
	lev$know.mat = know.mat
		
	.know.var.group = identical.rows.groups(know.mat)
	lev$lev.df$.know.var.group = .know.var.group
	lev$know.var.li = lapply(unique(.know.var.group), function(kv) {
		row = which(.know.var.group == kv)[1]
		vars = colnames(know.mat)[know.mat[row]]
	})
	lev

}


compute.tg.subgames = function(tg) {
	restore.point("compute.tg.subgames")
	
	# no action level
	if (length(tg$action.levels)==0) {
		stop("Computation of subgames for games without actions not yet specified.")
	}
	
	
	# first take the last action level
	lev.num = rev(tg$action.levels)[1] 
	
	ise.df = tg$ise.df
	# On the last level all singleton information sets are subgames
	
	rows = which(ise.df$.lev.num == lev.num & ise.df$.num.nodes == 1)
	
	if (length(rows)>0) {
		sgi.df1 = data_frame(.lev.num = lev.num, .root.info.set.ind = ise.df$.info.set.ind[rows],.info.set.ind = .root.info.set.ind)
	} else {
		sgi.df1 = NULL
	}
	
	#sgi.li = vector("list",length(tg$action.levels))
	#sgi.li[[1]] = sgi.df
	
	
	# to do for earlier levels
	# good vectorized method to find all descendant information sets
	
	lev.num = 1
	li = lapply(rev(tg$action.levels)[-1], function(lev.num) {
		restore.point("compute.subgame.inner")
		lev = tg$lev.li[[lev.num]]
		
		dis.df = find.lev.descendant.info.sets(lev=lev, tg=tg,add.roots = TRUE)
		sgi.df = dis.df %>%
			rename(.root.info.set.ind = pinfo.set.ind) %>%
			mutate(.lev.num = lev.num) %>%
			select(.lev.num, .root.info.set.ind, .info.set.ind)
	})
	
	sgi.df = bind_rows(c(list(sgi.df1),li)) %>%
		arrange(.root.info.set.ind, .info.set.ind) %>%
		ungroup()
	
	# generate a subgame index
	# subgames starting at earlier information nodes
	# will have lower subgame indices
	roots = sort(unique(sgi.df$.root.info.set.ind))
	sgi.df$.sg.ind = match(sgi.df$.root.info.set.ind,roots)
	sgi.df$is.root = sgi.df$.root.info.set.ind == sgi.df$.info.set.ind
	
	# mark information sets that are member of a descendant
	# subgame
	sgi.df = sgi.df %>%
		group_by(.info.set.ind) %>%
		mutate(.in.descendant = c(rep(TRUE,n()-1),FALSE)) %>%
		ungroup()
	
	# create some summary information about subgames
	df = sgi.df %>%
		left_join(select(ise.df,.info.set,.info.set.ind,.player,.num.moves), by=".info.set.ind")
	
	sg.df = df %>%
		group_by(.sg.ind,.lev.num, .root.info.set.ind) %>%
		summarize(.num.strats.without.desc = prod(ifelse(.in.descendant,1,.num.moves)), .num.strats = prod(.num.moves), .num.players=length(unique(.player)))

	max.or.na = function(x) {
		if (length(x)==0) return(NA)
		max(x)
	}
	
	# find parent indices
	#sgi.df$.sg.ind = as.numeric(sgi.df$.sg.ind)
	sg.df = suppressWarnings(sg.df %>%
		group_by(.sg.ind, .root.info.set.ind) %>%
		mutate(parent.sg.ind = max.or.na(sgi.df$.sg.ind[sgi.df$.info.set.ind == .root.info.set.ind & !sgi.df$is.root])))

	# find possible outcomes for each subgame
	sg.df = compute.sg.outcomes(sg.df = sg.df, tg=tg)
	
	tg$sgi.df = sgi.df
	tg$sg.df = sg.df
	invisible(tg)
	
}

# find for each info set in the level 
# all descendant info sets (all nodes must be descendants)
# uses know.var.groups for vectorization
find.lev.descendant.info.sets = function(lev, tg, singleton.only=FALSE, add.roots = FALSE) {
	restore.point("find.lev.descendant.info.sets")
	
	lev.df = lev$lev.df
	kvg = 1
	li = lapply(seq_along(lev$know.var.li), function(kvg) {
		restore.point("find.lev.descendant.info.sets.inner1")
		# find info sets of the current know.var group
		lev.rows = which(lev.df$.know.var.group == kvg & lev.df$.move.ind==1)
		
		.info.set.inds = lev.df$.info.set.ind[lev.rows]
		
		# may only consider information sets that are singleton
		# since only those can start subgames
		if (singleton.only) {
			singleton = tg$ise.df$.num.nodes[info.set.inds] == 1
			.info.set.inds = .info.set.inds[singleton]
			lev.rows = lev.rows[singleton]
		}
		
		cols = setdiff(lev$know.var.li[[kvg]],c(lev$var,"variant","numPlayers"))
		if (length(cols)>0) {
			cols.id = paste.matrix.cols(lev.df[lev.rows,cols,drop=FALSE])
		} else {
			cols.id = rep("0",length(lev.rows))
		}
		
		# now we check all subsequent action levels to find descendant
		# information sets
		desc.levels = tg$action.levels[tg$action.levels > lev$lev.num]
		
		#dlev = tg$lev.li[[2]]
		li = lapply(tg$lev.li[desc.levels], function(dlev) {
			restore.point("find.lev.descendant.info.sets.inner2")
			dlev.df = dlev$lev.df
			if (length(cols)>0) {
				dcols.id = paste.matrix.cols(dlev.df[,cols,drop=FALSE])
			} else {
				dcols.id = rep("0",NROW(dlev.df))
			}

			# we try to match each row in dlev.df to our
			# lev.rows in lev.df
			dlev.df$.match = match(dcols.id, cols.id)
			
			# try to find those information sets that are fully contained
			# this means dind must be the same for all moves and
			# cannot contain any NA
			dlev.df = dlev.df %>%
				group_by(.info.set.ind) %>%
				mutate(.contained = sum(is.na(.match)) == 0 & length(unique(.match)) == 1) %>%
				ungroup(pinfo.set.ind=)
			
			# return a data_frame with the 
			# indices of the parent info set and the
			# descendant info sets
			match.df = dlev.df %>%
				filter(.contained, .move.ind==1) %>%
				mutate(pinfo.set.ind = .info.set.inds[.match]) %>%
				select(pinfo.set.ind, .info.set.ind) %>%
				unique()
			
			if (add.roots) {
				match.df = rbind(data_frame(pinfo.set.ind=.info.set.inds, .info.set.ind = .info.set.inds),match.df) %>%
					arrange(pinfo.set.ind, .info.set.ind)
			}
			
			match.df
		})
		return(bind_rows(li))
	})
	
	bind_rows(li)
}

# compute for each subgame the possible outcomes of the subgame
# use known.var matrices for vectorization
compute.sg.outcomes = function(sg.df, tg, add.to.sg.df = TRUE) {
	restore.point("compute.sg.outcomes")
	
	#lev = tg$lev.li[[3]]	
	# loop through action levels for known.var vectorization
	li = lapply(tg$lev.li[rev(tg$action.levels)], function(lev) {
		#
		lev.df = lev$lev.df
		kvg = 1
		li = lapply(seq_along(lev$know.var.li), function(kvg) {
			restore.point("compute.sg.outcomes.inner2")
			# find info sets at which subggame starts of the current know.var group
			lev.rows = which(lev.df$.know.var.group == kvg & lev.df$.info.set.ind %in% sg.df$.root.info.set.ind & lev.df$.move.ind == 1)
			if (length(lev.rows)==0) return(NULL)
			
			.info.set.inds = lev.df$.info.set.ind[lev.rows]
			cols = setdiff(lev$know.var.li[[kvg]],c(lev$var,"variant","numPlayers"))
			# if no cols are observed the subgame is the whole game
			if (length(cols)==0) {
				all.df = data_frame(.root.info.set.ind = .info.set.inds, .outcomes=list(seq_len(NROW(tg$oco.df))) )
				return(all.df)
			}
			
			cols.id = paste.matrix.cols(lev.df[lev.rows,cols,drop=FALSE])
			ocols.id = paste.matrix.cols(tg$oco.df[,cols,drop=FALSE])
			
			# match each outcome rows to subgame root nodes
			# at the current level
			# non-contained outcomes will have an NA
			oco.match = match(ocols.id, cols.id)
			
			df = data_frame(.root.info.set.ind=.info.set.inds[oco.match],.outcome=tg$oco.df$.outcome)
			df %>%
				group_by(.root.info.set.ind) %>%
				summarize(.outcomes = list(.outcome))
		})
		bind_rows(li)
	})
	df = bind_rows(li)
	
	# TO DO: .info.set.ind is so far not globally
	# defined by counter starts at each level
	# need global counter!!!
	if (add.to.sg.df) {
		if (".outcomes" %in% colnames(sg.df))
			sg.df = select(sg.df, -.outcomes)
		res = left_join(sg.df,df, by=".root.info.set.ind")
	
	}
	
}

make.tg.spi.li = function(tg) {
	restore.point("make.tg.spi.li")
	tg$spi.li = lapply(tg$sg.df$.sg.ind, make.sg.spi,tg=tg)
	invisible(tg)
}

make.tg.spo.li = function(tg) {
	restore.point("make.tg.spi.li")
	tg$spo.li = lapply(tg$sg.df$.sg.ind, make.sg.spo.df,tg=tg)
	invisible(tg)
}


# strategy profiles info for a specified subgame
# we need this for fast computation of the spo.df
make.sg.spi = function(.sg.ind=1,tg, include.descendants=FALSE) {
  restore.point("make.sg.spi")
  
	# shall info sets from descendant subgames be included
	# if FALSE we solve for SPE by backward induction
	# if TRUE we will find all NE of the subgame
  if (include.descendants) {
  	.info.set.inds = tg$sgi.df$.info.set.ind[tg$sgi.df$.sg.ind==.sg.ind]
  } else {
  	.info.set.inds = tg$sgi.df$.info.set.ind[tg$sgi.df$.sg.ind==.sg.ind & tg$sgi.df$.in.descendant==FALSE]
  }
  
	spi = tg$iso.df %>%
  	filter(.info.set.ind %in% .info.set.inds) %>%
  	group_by(.player,.lev.num, .info.set.ind) %>%
  	summarise(moves = length(.move.ind)) %>%
		ungroup()
  
	# these indexes will be used for fast computation
	# of the spo table                          
  spi$iso.row.add = c(0, cumsum(spi$moves[-NROW(spi)]))
  spi$move.mult = rev(c(1,cumprod(rev(spi$moves[-1]))))
  
  spi

}


find.info.set.outcomes = function(.info.set.ind, tg, oco.df = tg$oco.df, return.logical=FALSE) {
	restore.point("find.info.set.outcomes")
	
	.lev.num = tg$ise.df$.lev.num[.info.set.ind]
	lev = tg$lev.li[[.lev.num]]
	row = which(lev$lev.df$.info.set.ind == .info.set.ind)[1]
	keys = setdiff(colnames(lev$know.mat)[lev$know.mat[row,]],lev$var)
	vals = as.list(lev$lev.df[row, keys])
	
	code = paste0('oco.df[["', keys,'"]] == vals[["',keys,'"]]', collapse=" & ")
	if (!return.logical)
		code = paste0("oco.df$.outcome[ ",code," ]")
	call = parse(text=code)
	
	eval(call)
}

make.sg.spo.df = function(.sg.ind = 1, sg.df = tg$sg.df, sgi.df = tg$sgi.df, spi.df  = tg$spi.li[[.sg.ind]], tg) {
	restore.point("make.sg.spo.df")	
	
	# we need to specify outcomes for each strategy profile
	# of each subgame
	sg.df = sg.df[sg.df$.sg.ind == .sg.ind,]
	sgi.df = sgi.df[sg.df$.sg.ind == .sg.ind,]
	spi = tg$spi.li[[.sg.ind]]
	
	.info.set.inds = spi$.info.set.ind
	
	ise.df = filter(tg$ise.df, .info.set.ind %in% .info.set.inds)
	iso.df = filter(tg$iso.df, .info.set.ind %in% .info.set.inds)
	

	# relevant outcomes for this subgame	
	outcomes = sg.df$.outcomes[[1]]

	oco.df = tg$oco.df[outcomes,,drop=FALSE]
	
  n.sp = prod(spi$moves)
  n.ise = NROW(spi)
  n.out = length(outcomes)

  moves.df = sp.to.moves(sp = 1:n.sp, spi)
  
  # This matrix can be quite big
  # If memory is a concern, we may split the matrix
  # and the move.df in different chunks
  # and apply the stuff for each chunk seperately
  feas.mat = matrix(TRUE,n.sp,n.out )
  
  #iso.infeasible = iso.df$.infeasible

  ise.ind = 1
  move.ind = 1
  iso.row = 0
  for (ise.ind in seq_len(n.ise)) {
  	# outcome values of the variable
  	# that is decided at this info set
  	.char.oco.val = as.character(oco.df[[iso.df$.var[iso.row+1] ]])
  	is.ise.oco = find.info.set.outcomes(.info.set.ind = ise.df$.info.set.ind[ise.ind],tg = tg, oco.df=oco.df,return.logical = TRUE)
  	
  	#move.ind = 1
    for (move.ind in seq_len(spi$moves[ise.ind])) {
      iso.row = iso.row + 1
      
      .char.move.val = iso.df$.char.move.val[iso.row]
      
      #infeas = match(iso.infeasible[[iso.row]], outcomes)
      # infeasible outcomes have a different
      # value of the info set variable than
      # the value of the current move
      infeas = which(is.ise.oco & .char.oco.val != .char.move.val)
      
      rows = which(moves.df[,ise.ind]==move.ind)
      feas.mat[rows,infeas] = FALSE
    }
  }
  
  spo = which(feas.mat,arr.ind = TRUE)
  colnames(spo) = c("sp",".outcome")
  spo = as.data.frame(spo)
  spo$.outcome = outcomes[spo$.outcome]
  spo = arrange(spo,sp,.outcome)
  
  # TO DO: Need to add .prob to oco.df!
  #spo = inner_join(spo, select(tg$oco.df,.outcome, .prob, starts_with("payoff_")), by=".outcome")
  spo = inner_join(spo, select(tg$oco.df,.outcome,.prob), by=".outcome")
  
  # add indices of other player strategy profiles
  #for (player in 1:tg$params$numPlayers) {
  #	sp_i = sp.to.sp_i(player=player, sp=spo$sp,spi=spi)
  #	spo[[paste0("sp_",player)]] = sp_i
  #}
  
  spo

	
}

old.make.sg.spo.df = function(.sg.ind = 1, sg.df = tg$sg.df, sgi.df = tg$sgi.df, spi.df  = tg$spi.li[[.sg.ind]], tg) {
	restore.point("make.sg.spo.df")	
	
	# we need to specify outcomes for each strategy profile
	# of each subgame
	sg.df = sg.df[sg.df$.sg.ind == .sg.ind,]
	sgi.df = sgi.df[sg.df$.sg.ind == .sg.ind,]
	spi = tg$spi.li[[.sg.ind]]
	
	.info.set.inds = spi$.info.set.ind
	
	ise.df = filter(tg$ise.df, .info.set.ind %in% .info.set.inds)
	iso.df = filter(tg$iso.df, .info.set.ind %in% .info.set.inds)
	

	# relevant outcomes for this subgame	
	outcomes = sg.df$.outcomes[[1]]

  n.sp = prod(spi$moves)
  n.ise = NROW(spi)
  n.out = length(outcomes)

  moves.df = sp.to.moves(sp = 1:n.sp, spi)
  
  # This matrix can be quite big
  # If memory is a concern, we may split the matrix
  # and the move.df in different chunks
  # and apply the stuff for each chunk seperately
  feas.mat = matrix(TRUE,n.sp,n.out )
  
  iso.infeasible = iso.df$.infeasible

  ise.ind = 1
  move.ind = 1
  iso.row = 0
  for (ise.ind in seq.int(1,n.ise)) {
    for (move.ind in seq.int(1,spi$moves[ise.ind])) {
      iso.row = iso.row + 1
      infeas = match(iso.infeasible[[iso.row]], outcomes)
      rows = which(moves.df[,ise.ind]==move.ind)
      feas.mat[rows,infeas] = FALSE
    }
  }
  
  spo = which(feas.mat,arr.ind = TRUE)
  colnames(spo) = c("sp",".outcome")
  spo = as.data.frame(spo)
  spo$.outcome = outcomes[spo$.outcome]
  spo = arrange(spo,sp,.outcome)
  
  # TO DO: Need to add .prob to oco.df!
  #spo = inner_join(spo, select(tg$oco.df,.outcome, .prob, starts_with("payoff_")), by=".outcome")
  spo = inner_join(spo, select(tg$oco.df,.outcome,.prob), by=".outcome")
  
  # add indices of other player strategy profiles
  #for (player in 1:tg$params$numPlayers) {
  #	sp_i = sp.to.sp_i(player=player, sp=spo$sp,spi=spi)
  #	spo[[paste0("sp_",player)]] = sp_i
  #}
  
  spo

	
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
    colnames(moves) = as.character(spi$.info.set.ind)
    return(moves)
  }
  
  moves.df = data.frame(
    sp = rep(sp,times=NCOL(moves)),
    .info.set = rep(spi$.info.set, each = length(sp)),
    .move.ind = as.vector(moves)
  )
  as_data_frame(moves.df)

}

# strategy profile index to matrix of moves at each information set
sp.to.sp_i = function(player = 1,sp, spi) {
  restore.point("sp.to.sp_i")

	
	#sp.to.moves(sp,spi)
	cols = which(spi$.player != player)
	
	# player makes no moves
	if (length(cols)==0)
		return(rep(0L, NROW(sp)))
	
	
	sp_i = integer(NROW(sp))
  col = cols[1]
	for (col in 1:max(cols)) {
    # integer division
    move_1 = ( (sp-1) %/% spi$move.mult[col])
    if (col %in% cols)
    	sp_i = sp_i + move_1 * spi$move.mult[col]
    
    # remainder
    sp = sp - (move_1)*spi$move.mult[col]
  }
	
	sp_i = id.to.index(sp_i, sort(unique(sp_i)))
	sp_i
}

get.child.subgames = function(.sg.ind, tg) {
	rows = tg$sg.df$parent.sg.ind %in% .sg.ind
	tg$sg.df$.sg.ind[rows]
}

solve.all.tg.spe = function(tg) {
	restore.point("solve.all.tg.spe")
	
	# solve via backward induction
	.sg.inds = rev(unique(tg$sg.df$.sg.ind))
	
	tg$spe.li = vector("list", length(.sg.inds)) 
	
	for (.sg.ind in .sg.inds) {
		tg$spe.li[[.sg.ind]] = solve.sg.spe(.sg.ind = .sg.ind, tg=tg)	
	}
}

#
spo.to.speu = function(spo.df, tg=NULL, add.outcomes = FALSE) {
	restore.point("spo.to.eu.df")
	all.players = seq_len(tg$params$numPlayers)
	oco = tg$oco.df[,c(".outcome",paste0("util_",all.players))]
	spo.df = left_join(spo.df, oco, by=".outcome")
	
	code = paste0("Eutil_", all.players," =sum(util_",all.players," * .prob) / sum(.prob)", collapse=",")
	if (add.outcomes) {
		code = paste0(code, ", .outcomes = list(.outcome)")
	}

	# group by strategy profiles
	speu = group_by(spo.df, sp) %>%
		s_summarise(code)

	speu
}

#' solve all spe of subgame .sg.ind
#' assumes that descendent subgames have already been solved
#' and uses backward induction
solve.sg.spe = function(.sg.ind=1, tg) {
	
	# 1. We first generate a grid of all children
	#    subgame equilibrium outcome combinations
	# 2. We solve the subgame for each row of that grid

	
	# all children subgames
	child.sg = get.child.subgames(.sg.ind, tg)
	
	# no children subgames => solve directly
	if (length(child.sg)==0) {
		eq = solve.sg.spe.given.remove(.sg.ind=.sg.ind, tg=tg)
		return(eq)
	}
	
	# list of equilibrium outcome indices
	# for all child-subgames
	eqo.li = lapply(child.sg, function(cind) {
		tg$spe.li[[cind]]$eqo.df$.eqo.ind
	})
	
	# grid of all child subgame eq. outcomes
	eqo.grid = expand.grid(eqo.li)
	names(eqo.grid) = child.sg
	
	tg$sg.df
	
	# vector of all possible outcomes (oco rows) of
	# child subgames
	child.sg.outcomes = unique(unlist(lapply(child.sg, function(cind) {
		tg$sg.df$.outcomes
	})))
	
	
	# loop through each child subgame eq. outcome combination
	# and compute corresponding spe of this subgame
	eq.li = lapply(NROW(eqo.grid), function(grid.row) {
		eqo.outcomes =  unique(unlist(lapply(seq_along(child.sg), function(i) {
			cind = child.sg[i]
			.eqo.ind = eqo.grid[grid.row,i]
			tg$spe.li[[cind]]$eqo.df$.outcomes[[.eqo.ind]]
		})))
		
		remove.outcomes = setdiff(child.sg.outcomes,eqo.outcomes)
		eq = solve.sg.spe.given.remove(.sg.ind = .sg.ind,tg = tg, remove.outcomes = remove.outcomes, child.eqo.inds = as.integer(eqo.grid[grid.row,]))
		
		# to do: need to add subgame info to eq
		return(eq)
	})

	speq.df = bind_rows(lapply(eq.li, function(eq) eq$speq.df))
	eqo.df = bind_rows(lapply(eq.li, function(eq) eq$eqo.df))
	
	nlist(speq.df, eqo.df)
}

# internal function to solve sg.spe
# remove.outcomes depend on the equilibria
# of the child subgames 
# backward induction simple works
# by removing from spo.df all outcomes from remove.outcomes
solve.sg.spe.given.remove = function(.sg.ind=1, tg, remove.outcomes=NULL, child.eqo.inds = NULL) {
	restore.point("solve.sg.spe.givem.remove")
	
	spo.df = tg$spo.li[[.sg.ind]]
	
	if (!is.null(remove.outcomes)) {
		rows = !spo.df$.outcome %in% remove.outcomes
		spo.df = spo.df[rows,,drop=FALSE]
	}
	
	spi = tg$spi.li[[.sg.ind]]
	
	players = unique(spi$.player) # players who pick action
	all.players = seq_len(tg$params$numPlayers)
	
	# compute expected utility for each player
	# and each strategy profile sp
	speu = spo.to.speu(spo.df = spo.df, tg=tg)
	
	# for each player who moves
	# mark best reply strategy profiles 
	for (player in players) {
		speu[[paste0("sp_",player)]] = sp.to.sp_i(player, speu$sp, spi)
		speu = s_group_by(speu, paste0("sp_",player)) %>%
			s_mutate(paste0("is_br_",player," = Eutil_",player," == max(Eutil_",player,")"))
	}
	
	# a strategy profile is an equilibrium if it is
	# a best reply for each player
	cond = parse(text=paste0("speu$is_br_",players, collapse=" & "))
	speu$is_eq = eval(cond)
	
	# only keep equilibria
	speu = speu[speu$is_eq,,drop=FALSE]
	

	# add outcomes and store them in a list .outcomes
	# we will need them for backward induction
	speq = speu %>%
		left_join(select(spo.df,sp,.outcome), by="sp") %>%
		s_group_by(c("sp",paste0("Eutil_",all.players))) %>%
		summarize(.outcomes = list(.outcome), .outcomes.id = paste0(.outcome, collapse=","))%>%
		ungroup()		

	eqo.df = 	speq[!duplicated(speq$.outcomes.id),]
	speq$.eqo.ind = match(speq$.outcomes.id,eqo.df$.outcomes.id)
	eqo.df$.eqo.ind = seq_len(NROW(eqo.df))
	cols = setdiff(unique(c(".eqo.ind",colnames(eqo.df))),c("sp",".outcomes.id"))
	eqo.df = eqo.df[,cols]

	cols = setdiff(unique(colnames(speq)),c(".outcomes.id"))
	speq = speq[,cols]
	
	speq$child.eqo.inds = replicate(n = NROW(speq),child.eqo.inds,simplify = FALSE)
	eqo.df$child.eqo.inds = replicate(n = NROW(eqo.df),child.eqo.inds,simplify = FALSE)

	nlist(speq.df = speq, eqo.df)
}

