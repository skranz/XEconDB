examples.make.pages = function() {
	setwd("D:/libraries/XEconDB/projects/UltimatumGame")
	gameId = "UltimatumGame"
	
	rg = get.rg(gameId=gameId, always.new=TRUE)
	make.rg.pages(rg)
	make.stage.page(stage = 2,rg=rg)
}

load.rg.stage.page = function(stage, rg,  pages.dir = file.path(get.project.dir(),"pages", rg$gameId), file=NULL, make.if.missing = TRUE, remake.auto = TRUE) {
	restore.point("load.rg.stage.page")
	if (is.numeric(stage) | is.character(stage)) {
		stage = rg$stages[[stage]]
	}
	if (is.null(file)) {
		file = paste0(stage$name,".Rmd")
		if (!file.exists(file.path(pages.dir,file)) & !remake.auto)
			file = paste0(stage$name,".auto.Rmd")
	}
	
	
	if (!file.exists(file.path(pages.dir,file))) {
		if (make.if.missing) {
			page = make.stage.page(stage=stage, rg=rg)
		} else {
			stop(paste0("Page for stage ", stage$name, " for game ", rg$gameId, " does not exist in folder ", pages.dir))
		}
	} else {
		page = readLines(file.path(pages.dir,file))
	}
	return(merge.lines(page))
}

make.rg.pages = function(rg) {
	for (stage in rg$stages) {
		make.stage.page(stage=stage, rg=rg)
	}
}

make.stage.page = function(stage=rg$stages[[1]], rg, pages.dir = file.path(get.project.dir(),"pages", rg$gameId), file = NULL, lang="en") {
	restore.point("make.stage.page")
	
	if (is.numeric(stage)) stage = rg$stages[[stage]]
	
	if (is.null(file)) {
		file = paste0(stage$name,".auto.Rmd")
	}
	
	head.txt = paste0(
'<h3>', stage$name,'</h3>		
<h4>Player: {{.player}}</h4>'		
	)
	
	if (!identical(stage$observe,"")) {
		if (is.call(stage$observe)) {
			obs.txt = paste0("Cannot automatically generate observations for R formula <br>\n",deparse1(stage$observe))
		} else {
			obs.txt = paste0(stage$observe, ": {{", stage$observe,"}}", collapse = "<br>\n" )
		}
		obs.txt = paste0("\n\n<h3>Observations</h3>\n<p>\n", obs.txt,"\n</p>")
		
	} else {
		obs.txt = ""
	}
	
	if (lang != "en") lang = "native"
	action.txt = ""
	if (length(stage$actions)>0) {
		action.txt = lapply(stage$actions, function(action){
			form = 	action[[paste0("form_",lang)]]
			label = first.non.null(form$inputText,paste0(action$name,":"))
			choiceLabels = form$labels
			if (identical(choiceLabels,"")) choiceLabels=NULL
			if (is.null(choiceLabels)) {
				clc = "NULL"
			} else {
				clc = paste0("c(", paste0('"', choiceLabels,'"', collapse=", "),")")
			}
			paste0(
'{{actionField(name="',action$name,'", label="',label,'", choiceLabels = ', clc,")}}") 
		})
		
		action.txt = paste0("\n",paste0(action.txt, collapse="\n<br>\n"))
	} else {
		action.txt = ""
	}
	
	btn.txt = paste0('\n<br>\n{{submitPageBtn("Press to proceed")}}')
	txt = c(head.txt, obs.txt, action.txt, btn.txt)
	
	if (!dir.exists(pages.dir))
		try(dir.create(pages.dir, recursive = TRUE))
	
	writeLines(txt, file.path(pages.dir,file))
	
	invisible(sep.lines(txt))
	
}

save.stage.page = function(txt,gameId, stage.name, pages.dir = file.path(get.project.dir(),"pages", gameId), file = NULL, auto=FALSE) {
	restore.point("save.stage.page")
	if (is.null(file)) {
		file = paste0(stage.name, ifelse(auto,".auto",""), ".Rmd")
	}
	if (!dir.exists(pages.dir))
		try(dir.create(pages.dir, recursive = TRUE))
	
	writeLines(txt, file.path(pages.dir,file))
	
	
}