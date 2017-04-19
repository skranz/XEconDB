# Convert a table form game into a gambit extensive form game

get.xs = function(xs = app[["xs"]],app=getApp()) {
	xs
}

get.project.dir = function(xs=get.xs()) {
	if (!is.null(xs$project.dir)) return(xs$project.dir)
	getwd()
}

get.games.dir = function(project.dir = get.project.dir()) {
	file.path(project.dir,"games")
}

get.game.dir = function(gameId, project.dir=get.project.dir()) {
	file.path(project.dir,"games",gameId)
}

get.eq.dir = function(gameId, project.dir = get.project.dir()) {
	file.path(project.dir,"games",gameId,"eq")
}

get.efg.dir = function(gameId, project.dir = get.project.dir()) {
	file.path(project.dir,"games",gameId,"gambit")
}

get.pages.dir = function(gameId, project.dir = get.project.dir()) {
	file.path(project.dir,"games",gameId,"pages")
}


make.game.dir = function(gameId, games.dir = file.path(project.dir,"games"), project.dir=get.project.dir()) {
	if (length(gameId)!=1) return()
	if (nchar(gameId)==0) return()
	game.dir = file.path(games.dir, gameId)
	
	if (dir.exists(game.dir)) return()
	
	dir.create(game.dir)
	dir.create(file.path(game.dir,"eq"))
	dir.create(file.path(game.dir,"gambit"))
	dir.create(file.path(game.dir,"pages"))
}

get.jg.hash = function(jg.hash=NULL, jg=NULL, rg=NULL,vg=NULL, tg=NULL) {
	hash = first.non.null(jg.hash, rg$jg.hash, vg$jg.hash, tg$jg.hash)
	if (is.null(hash) & !is.null(jg)) hash = digest(jg)
	hash
}

get.jg = function(gameId,json.file = paste0(game.dir,"/",gameId,".json"), game.dir=file.path(games.dir,gameId), games.dir = get.games.dir(project.dir), project.dir = get.project.dir(), jg=NULL) {
	restore.point("get.jg")
	if (!is.null(jg)) return(jg)
	
	json = merge.lines(readLines(json.file,warn = FALSE))
  content = fromJSON(json,simplifyDataFrame = FALSE,simplifyMatrix = FALSE,simplifyVector = FALSE)
	content$game
}

get.rg = function(gameId = jg$gameId, jg.hash = get.jg.hash(jg=jg),jg=NULL,rg=NULL, games.dir = get.games.dir(project.dir), project.dir = get.project.dir(), save.new = TRUE) {
	if (!is.null(rg)) return(rg)
	restore.point("get.rg")
	if (is.null(jg.hash)) {
		jg = get.jg(gameId = gameId, games.dir = games.dir)
		jg.hash = get.jg.hash(jg=jg)
	}
	
	
	file = file.path(games.dir,gameId, paste0(gameId,".rg"))
	if (file.exists(file)) {
		# return old rg if jg.hash has not changed
		rg = readRDS(file)
		if (identical(rg$jg.hash, jg.hash) | is.null(jg.hash))
			return(rg)
	}
	
	# need to create new rg
	if (is.null(jg)) jg = get.jg(gameId=gameId, games.dir = games.dir)
	
	rg = jg.to.rg(jg)
	if (save.new) {
		saveRDS(rg, file)
	}
	rg
}

get.vg = function(variant=1, gameId = first.non.null(jg$gameId,rg$gameId), jg.hash = get.jg.hash(jg=jg, rg=rg),jg=NULL,rg=NULL, vg=NULL, games.dir = get.games.dir(project.dir), project.dir = get.project.dir(), save.new = FALSE, always.new=FALSE) {
	if (!is.null(vg)) return(vg)
	restore.point("get.vg")
	if (is.null(jg.hash)) {
		jg = get.jg(gameId = gameId, games.dir = games.dir)
		jg.hash = get.jg.hash(jg=jg)
	}
	
	if (is.numeric(variant)) {
		rg = get.rg(gameId=gameId, jg=jg, rg=rg, jg.hash=jg.hash, games.dir=games.dir)
		variant = rg$variants[[variant]]
	}
	
	
	file = file.path(games.dir,gameId, paste0(gameId,"_",variant, ".vg"))
	if (file.exists(file) & !always.new) {
		# return old vg if jg.hash has not changed
		vg = readRDS(file)
		if (identical(vg$jg.hash, jg.hash) | is.null(jg.hash))
			return(vg)
	}

	rg = get.rg(gameId=gameId, jg=jg, rg=rg, jg.hash=jg.hash, games.dir=games.dir)

	vg = rg.to.vg(rg,variant = variant)
	if (save.new) {
		saveRDS(vg, file)
	}
	vg
}

get.tg = function(variant=first.non.null(vg$variant,1), gameId = first.non.null(vg$gameId,jg$gameId,rg$gameId), jg.hash = get.jg.hash(jg=jg, rg=rg,vg=vg),jg=NULL,rg=NULL, vg=NULL, tg=NULL, games.dir = get.games.dir(project.dir), project.dir = get.project.dir(), save.new = TRUE,branching.limit = 10000,msg.fun=NULL, never.load = FALSE) {
	if (!is.null(tg)) return(tg)
	restore.point("get.tg")
	
	if (is.null(jg.hash)) {
		jg = get.jg(gameId = gameId, games.dir = games.dir)
		jg.hash = get.jg.hash(jg=jg)
	}
	
	if (is.numeric(variant)) {
		rg = get.rg(gameId=gameId, jg=jg, rg=rg, jg.hash=jg.hash, games.dir=games.dir)
		variant = rg$variants[[variant]]
	}
	
	
	file = file.path(games.dir,gameId, paste0(gameId,"_",variant, ".tg"))
	if (file.exists(file) & !never.load) {
		# return old vg if jg.hash has not changed
		tg = readRDS(file)
		if (identical(tg$jg.hash, jg.hash) | is.null(jg.hash))
			return(tg)
	}

	vg = get.vg(variant=variant, gameId=gameId, jg=jg, rg=rg, vg=vg, jg.hash=jg.hash, games.dir=games.dir)

	tg = vg.to.tg(vg,branching.limit = branching.limit, msg.fun=msg.fun)
	if (tg$kel$count>0) {
		return(tg)
	}
	if (save.new) {
		if (!is.null(msg.fun)) msg.fun("Save game tree in ",file,"...")
		saveRDS(tg, file)
		if (!is.null(msg.fun)) {
			msg.fun("Game tree saved as ",file, ".\n Size = ", file.size(file))
		}
	}
	tg
}

save.rg = function(rg, games.dir = get.games.dir(project.dir), project.dir = get.project.dir()) {
	gameId = rg$gameId
	make.game.dir(gameId,games.dir = games.dir)
	
	file = file.path(games.dir,rg$gameId, paste0(rg$gameId, ".rg"))
	saveRDS(rg, file)
}

save.vg = function(vg, games.dir = get.games.dir(project.dir), project.dir = get.project.dir()) {
	gameId = vg$gameId
	make.game.dir(gameId,games.dir = games.dir)
	
	file = file.path(games.dir,gameId, paste0(gameId,"_",vg$variant, ".vg"))
	saveRDS(vg, file)
}


save.tg = function(tg, games.dir = get.games.dir(project.dir), project.dir = get.project.dir()) {
	gameId = tg$gameId
	make.game.dir(gameId,games.dir = games.dir)
	
	file = file.path(games.dir,gameId, paste0(gameId,"_",tg$variant, ".tg"))
	saveRDS(tg, file)
}


get.eq = function(tg, util.funs=NULL, just.spe=TRUE, mixed=FALSE, eq.dir = get.eq.dir(tg$gameId,project.dir), project.dir = get.project.dir(), save.new = TRUE, solvemode=NULL, solver=NULL,...) {
	restore.point("get.eq")
	if (!is.null(util.funs))
		set.tg.util(tg=tg,util.funs)
	
	
	eq.id = get.eq.id(tg=tg, just.spe=just.spe, mixed=mixed, solvemode=solvemode)
	file = file.path(eq.dir, paste0(eq.id,".eq"))
	if (file.exists(file)) {
		# return old vg if jg.hash has not changed
		eq = readRDS(file)
		if (identical(tg$jg.hash, eq$jg.hash))
			return(eq$eq.li)
	}
	
	
	# create efg file
	tg.to.efg(tg=tg, path=eq.dir)	


	# solve equilibrium
	eq.li = gambit.solve.eq(tg, just.spe=just.spe, mixed=mixed,eq.dir=eq.dir,save.eq = save.new,solver=solver,solvemode=solvemode,...)
	eq.li
}
