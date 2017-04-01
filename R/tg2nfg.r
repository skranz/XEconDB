examples.tg2nfg = function() {
	setwd("D:/libraries/XEconDB/projects/UltimatumGame")
	tg = get.tg(gameId = "MaxUltimatum")
	
	make.tg.know.var.groups(tg)
	ls(tg)
	lev = tg$lev.li[[3]]
	lev.df = lev$lev.df
}


tg.to.iso.df = function(tg) {
	
}