# Own implementations to solve pure SPE
# Probably it is better to use Gambit, however.
# The spi can nevertheless be computed to get
# some information about the number of strategy profiles

examples.make.tg.spe = function() {
  setwd("D:/libraries/XEconDB/projects/UltimatumGame")
	
	gameId = "LureOfAuthorityReduced"
	gameId = "CournotSmall"
	gameId = "BunchedUltimatum"
	gameId = "GiftEff"
	
	tg = get.tg(gameId=gameId, never.load=TRUE)
	sort(unlist(lapply(as.list(tg), object.size)),decreasing = TRUE)
	
	make.tg.spo.li(tg)
	
	spo.df = tg$spo.li[[1]]
	
	sg.df = tg$sg.df
	make.sg.spo.df(.sg.ind=3, tg=tg)
	tg$spo.li
	
	solve.all.tg.spe(tg=tg)
	tg$spe.li
	eqo.df = tg$spe.li[[1]]$eqo.df
	eq.li = tg$eq.li
	eq.li
	
	eqo.df = eq.outcomes(eq.li, tg=tg)
	
	gambit.eq.li = gambit.solve.eq(tg)
	eq.li
	gambit.eq.li
	identical(eq.li, gambit.eq.li)
	
	eq.li = tg.spe.li.to.eq.li(tg)
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


	spi = tg$ise.df %>%
		filter(.info.set.ind %in% .info.set.inds) %>%
		select(.player, .lev.num, .info.set.ind, .var, .num.moves) %>%
		rename(moves = .num.moves)
	
# 	spi = tg$iso.df %>%
#   	filter(.info.set.ind %in% .info.set.inds) %>%
#   	group_by(.player,.lev.num, .info.set.ind) %>%
#   	summarise(moves = max(.move.ind)) %>%
# 		ungroup()
  
	# these indexes will be used for fast computation
	# of the spo table
	if (NROW(spi)>0) {
  	spi$iso.row.add = c(0, cumsum(spi$moves[-NROW(spi)]))
  	spi$move.mult = rev(c(1,cumprod(rev(spi$moves[-1]))))
	} else {
		spi$iso.row.add = spi$move.mult = integer(0)
		
	}                    
  
  spi
}


make.sg.spo.df = function(.sg.ind = 1, sg.df = tg$sg.df, sgi.df = tg$sgi.df, spi.df  = tg$spi.li[[.sg.ind]], tg) {
	restore.point("make.sg.spo.df")	
	
	# we need to specify outcomes for each strategy profile
	# of each subgame
	sg.df = sg.df[sg.df$.sg.ind == .sg.ind,]
	sgi.df = sgi.df[sgi.df$.sg.ind == .sg.ind,]
	spi = tg$spi.li[[.sg.ind]]
	
	# a super subgame without any own
	# information sets
	if (NROW(spi)==0) return(NULL)
	
	.info.set.inds = spi$.info.set.ind
	
	ise.df = filter(tg$ise.df, .info.set.ind %in% .info.set.inds)


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
  
  ise.ind = 1
  move.ind = 1
  iso.row = 0
  for (ise.ind in seq_len(n.ise)) {
  	# outcome values of the variable
  	# that is decided at this info set
  	char.move.vals = as.character(ise.df$.move.vals[[ise.ind]])
  	var = ise.df$.var[ise.ind]
  	.char.oco.val = as.character(oco.df[[var]])
  	is.ise.oco = find.info.set.outcomes(.info.set.ind = ise.df$.info.set.ind[ise.ind],tg = tg, oco.df=oco.df,return.logical = TRUE)
  	
  	#move.ind = 1
    for (move.ind in seq_len(spi$moves[ise.ind])) {
      iso.row = iso.row + 1
      
      .char.move.val = char.move.vals[move.ind]
      
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
sp.to.moves = function(sp, spi=tg$spi, ise.df=NULL, wide=TRUE) {
  restore.point("sp.to.moves")

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
  
  org.sp = sp
  moves.df = data_frame(
    sp = rep(sp,times=NCOL(moves)),
  	.info.set.ind = rep(spi$.info.set.ind, each = length(org.sp)),
    .move.ind = as.vector(moves),
  	.info.set.move.ind = .move.ind - 1 + ise.df$.info.set.move.ind.start[.info.set.ind]
  )
  moves.df

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

get.tg.spo.li = function(tg) {
	make.tg.spo.li(tg)
}

solve.all.tg.spe = function(tg, eq.dir = get.eq.dir(tg$gameId), save.eq=TRUE) {
	restore.point("solve.all.tg.spe")
	
	start.time = Sys.time()

	if (is.null(tg[["spo.li"]])) {
		get.tg.spo.li(tg)
	}
	
	# solve via backward induction
	.sg.inds = rev(unique(tg$sg.df$.sg.ind))
	
	tg$spe.li = vector("list", length(.sg.inds)) 
	
	for (.sg.ind in .sg.inds) {
		tg$spe.li[[.sg.ind]] = solve.sg.spe(.sg.ind = .sg.ind, tg=tg)	
	}
	tg$eq.li = tg.spe.li.to.eq.li(spe.li=tg$spe.li, tg=tg)
	
	solve.time = Sys.time()-start.time
  attr(tg$eq.li,"solve.time") = solve.time

	
	if (save.eq) {
		eq.id = get.eq.id(tg=tg,solvemode="spe_xs")
		save.eq.li(eq.li=tg$eq.li, tg=tg, eq.id=eq.id, eq.dir = eq.dir)
	}
	
	invisible(tg$eq.li)
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

	
	restore.point("solve.sg.spe")
	
	if (.sg.ind == 1 & tg$sg.df$num.direct.info.sets[[.sg.ind]]==0) {
		return(solve.nature.super.sg(tg))
	}
	
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
		tg$sg.df$.outcomes[[cind]]
	})))
	
	
	# loop through each child subgame eq. outcome combination
	# and compute corresponding spe of this subgame
	eq.li = lapply(seq_len(NROW(eqo.grid)), function(grid.row) {
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
	restore.point("solve.sg.spe.given.remove")
	
	spo.df = tg$spo.li[[.sg.ind]]
	
	#df = left_join(spo.df, tg$oco.df, by=".outcome")
	
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


# the largest subgame for a game that
# starts with a move of nature
solve.nature.super.sg = function(tg) {
	restore.point("solve.nature.super.sg")

	.sg.ind = 1
	# all children subgames
	child.sg = get.child.subgames(.sg.ind, tg)
	
	# list of equilibrium outcome indices
	# for all child-subgames
	eqo.li = lapply(child.sg, function(cind) {
		tg$spe.li[[cind]]$eqo.df$.eqo.ind
	})
	
	# grid of all child subgame eq. outcomes
	eqo.grid = expand.grid(eqo.li)
	names(eqo.grid) = child.sg

	grid.row = 1
	li = lapply(seq_len(NROW(eqo.grid)), function(grid.row) {
		eqo.outcomes =  unique(unlist(lapply(seq_along(child.sg), function(i) {
			cind = child.sg[i]
			.eqo.ind = eqo.grid[grid.row,i]
			tg$spe.li[[cind]]$eqo.df$.outcomes[[.eqo.ind]]
		})))
		
		cei = as.integer(eqo.grid[grid.row,])
		names(cei) = NULL
		row.eqo.df = data_frame(.eqo.ind=1,.outcomes=eqo.outcomes, child.eqo.inds=list(cei))
	})
	eqo.df = bind_rows(li)
	eqo.df$.eqo.ind = seq_len(NROW(eqo.df))
	speq.df = eqo.df %>%
		mutate(sp=NA) %>%
		select(sp,.outcomes,.eqo.ind, child.eqo.inds)
	
	list(speq.df=speq.df, eqo.df=eqo.df)	
}


# An equilibrium when we solve a game by gambit an transform it, is specified by an eq.mat.
# It has one row for each outcome and
# one column for each variable that is an action or move of nature. The value is the probability that the variable takes in equilibrium the value it has in that row of oco.df.
tg.spe.li.to.eq.li = function(spe.li,tg, .sg.ind=1) {
	restore.point("tg.spe.li.to.eq.li")
	speq.df = spe.li[[.sg.ind]]$speq.df
	
	num.moves = sum(tg$ise.df$.num.moves * tg$ise.df$.num.nodes)
	ceq.start = matrix(0, nrow=1, ncol=num.moves)

	row = 1
	ceq.li = lapply(1:NROW(speq.df), function(row) {
		speq = speq.df[row,]
		recursive.speq.to.ceq(speq = speq,ceq = ceq.start,.sg.ind = .sg.ind,tg = tg, spe.li=spe.li)
	})
	ceq.mat = do.call(rbind,ceq.li)

	et.ind=which(tg$et.mat<0)

	# now use the same conversion as in 
	# xs_gambit
	eq.li = lapply(seq_len(NROW(ceq.mat)), function(row) {
		eq.mat = ceq.to.eq.mat(ceq=ceq.mat[row,], eq.ind=row, tg=tg, et.ind=et.ind)
	})
		
	eq.li
}


recursive.speq.to.ceq = function(ceq, speq, .sg.ind, tg, spe.li=tg$spe.li) {
	#restore.point("recursive.speq.to.ceq")
	cat("\n.sg.ind = ", .sg.ind, " NROW(ceq) = ",NROW(ceq))
	
	spi = tg$spi.li[[.sg.ind]]
	
	# satisfied unless for nature super subgame
	if (NROW(spi)>0) {
		 
		moves.df = sp.to.moves(speq$sp, spi=spi, ise.df = tg$ise.df, wide=FALSE)
	
		# insert a 1 for the info set moves taken
		# in equilibrium
		ceq[,moves.df$.info.set.move.ind] = 1
		
	}
	
	# all children subgames	
	child.sg = get.child.subgames(.sg.ind, tg)
	
	# no more subgames => we are done
	if (length(child.sg)==0) {
		return(ceq)
	}
	
	# child.eqo
	# for each subgame we have a
	# single equilibrium outcome
	child.eqo = speq$child.eqo.inds[[1]]

	# yet there may be multiple child
	# equilibria associated with a
	# child outcome

	#restore.point("recursive.speq.to.ceq.2")
	# loop through all child.sg
	i = 1
	for (i in seq_along(child.sg)) {
		# get all speq (subgame equilibria)
		# of the current child sg
		cdf = spe.li[[ child.sg[i] ]]$speq.df
		cdf = cdf[cdf$.eqo.ind == child.eqo[i],]
		
		# loop through all subgame equilibria
		# of the current child subgame
		ceq.li = lapply(seq_len(NROW(cdf)), function(row) {
			#restore.point("recursive.speq.to.ceq.3")
			speq = cdf[row,]
			recursive.speq.to.ceq(speq=speq,ceq = ceq,.sg.ind = child.sg[i],tg = tg, spe.li=spe.li)
			
		})
		# for each subgame equilibrium
		# we get new ceq rows
		ceq = do.call(rbind,ceq.li)
	}
	ceq
}