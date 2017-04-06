examples.xs.pref = function() {
	setwd("D:/libraries/XEconDB/projects/UltimatumGame")
	precl = load.pref.classes()
	prefs = pref.classes.default.prefs(precl)
	
	tg = get.tg(gameId ="BunchedUltimatum")
	pref = prefs[[3]]
	
	set.tg.pref(pref,tg)
	tg$tg.id
	oco.df = tg$oco.df
	

	
}

load.pref.classes = function() {
	file = system.file("spec","pref_classes.yaml",package = "XEconDB")
	precl = read.yaml(file)
}

pref.classes.default.prefs = function(precl=load.pref.classes()) {
	prefs = lapply(seq_along(precl), function(i) {
		pc = precl[[i]]
		pc = list(
			class = names(precl)[i],
			params = lapply(pc$params,function(par) {
				par$default
			}),
			descr = pc$descr,
			name = pc$defaultName,
			label = pc$defaultName,
			fun = pc$fun
		)
		#class(pc) = c("pref","list")
		pc
	})
		
	names(prefs) = sapply(precl, function(pc) {
		pc$defaultName
	})
	prefs
	
}

set.tg.pref = function(pref,tg) {
	restore.point("set.tg.pref")
	n = tg$params$numPlayers
	args = c(pref$params,list(n=n,player=1))
	
	util.funs = sapply(seq_len(n), function(i) {
		args$player = i
		do.call(pref$fun,args)
	})
	
	if (!is.null(pref$label)) {
		names(util.funs) = rep(pref$label,n)
	}
	
	set.tg.util(tg = tg, util.funs=util.funs)
	#oco.df = tg$oco.df
}