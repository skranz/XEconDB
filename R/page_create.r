examples.make.pages = function() {
	setwd("D:/libraries/XEconDB/projects/UltimatumGame")
	gameId = "UltStratMeth"
	
	rg = get.rg(gameId=gameId)
	make.rg.pages(rg)
	make.stage.page(stage = 2,rg=rg)
}

load.rg.wait.page = function(rg,  pages.dir = get.pages.dir(gameId=rg$gameId), file=NULL, make.if.missing = TRUE, remake.auto = !TRUE) {
	restore.point("load.rg.wait.page")
	if (is.null(file)) {
		file = paste0("wait-page.Rmd")
		if (!file.exists(file.path(pages.dir,file)) & !remake.auto)
			file = paste0("wait-page.auto.Rmd")
	}
	
	
	if (!file.exists(file.path(pages.dir,file))) {
		if (make.if.missing) {
			page = make.wait.page(rg=rg)
		} else {
			stop(paste0("Wait page for for game ", rg$gameId, " does not exist in folder ", pages.dir))
		}
	} else {
		page = readLines(file.path(pages.dir,file))
	}
	return(merge.lines(page))
}


load.rg.stage.page = function(stage, rg,  pages.dir = get.pages.dir(gameId=rg$gameId), file=NULL, make.if.missing = TRUE, remake.auto = TRUE) {
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

make.wait.page = function(rg, pages.dir = get.pages.dir(gameId=rg$gameId), file = NULL, lang="en") {
	if (is.null(file)) {
		file = paste0("wait-page.auto.Rmd")
	}
	txt = paste0("<h3>Please wait...</h3>")
	writeLines(txt, file.path(pages.dir, file))
	merge.lines(txt)
}

make.stage.page = function(stage=rg$stages[[1]], rg, pages.dir = get.pages.dir(gameId=rg$gameId), file = NULL, lang="en") {
	restore.point("make.stage.page")
	
	if (is.numeric(stage) | is.character(stage)) stage = rg$stages[[stage]]
	
	if (is.null(file)) {
		file = paste0(stage$name,".auto.Rmd")
	}
	
	head.txt = paste0(
'<h3>', stage$name,'</h3>		
<h4>Player: {{.player}}</h4>'		
	)
	if (is.call(stage$observe)) {
		obs.txt = paste0("Cannot automatically generate observations for R formula <br>\n",deparse1(stage$observe))
	} else {
		obs.vars = setdiff(stage$observe, c("",stage$domain.vars))
		if (length(obs.vars)>0) {
			obs.txt = paste0(stage$observe, ": {{", stage$observe,"}}", collapse = "<br>\n" )
			obs.txt = paste0("\n\n<h3>Observations</h3>\n<p>\n", obs.txt,"\n</p>")
		} else {
			obs.txt = ""
		}
	}

	action.txt = ""
	if (length(stage$actions)>0) {
		action.txt = lapply(stage$actions, make.page.action.txt, rg=rg, stage=stage)
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



make.page.action.txt = function(action,rg, stage) {
	restore.point("make.page.action.txt")
	label = paste0(action$name,":")
	choiceLabels = action$labels
	if (identical(choiceLabels,"")) choiceLabels=NULL
	
	if (is.null(choiceLabels)) {
		clc = "NULL"
	} else {
		clc = paste0("c(", paste0('"', choiceLabels,'"', collapse=", "),")")
	}
	
	if (!is.null(action$domain.var)) {
		restore.point("make.page.stratmeth.txt")
		domain.var = action$domain.var
		domain.var.td = paste0('<td>', domain.var,'</td>', collapse=" ")
		domain.val.td = paste0('<td>{{domain.val[["', domain.var,'"]]}}</td>', collapse=" ")
		
		
		table.class = paste0("table-",stage$name,"-",action$name) 
		res = paste0('
Choose your action "',action$name,'" conditional on the value of "',paste0(domain.var, collapse=", "),'"
<!--
You can adapt the style of the strategy method table cells here. -->
<style>
	table.',table.class,' > tbody > tr > td {
		border-bottom: solid;
		border-bottom-width: 1px;
		padding-left: 5px;
	}
	table.',table.class,' table.rowRadioTable td {
		padding-left: 5px;
		padding-right: 3px;
		padding-top: 3px;
		padding-bottom: 3px;
	}
</style>
<table class="',table.class,'">
<tr>', domain.var.td,'<td>Your choice</td></tr>

#< stratMethRows action= "',action$name,'"
<tr>
',domain.val.td,'
<!-- possible input types: "rowRadio", "select", "radio" --> 
<td>{{stratMethInput(inputType="rowRadio", choiceLabels= ', clc,')}}</td>
</tr>
#> end stratMethRows

</table>
')
		return(res)
	}
	paste0(
'{{actionField(name="',action$name,'", label="',label,'", choiceLabels = ', clc,")}}")
}

save.stage.page = function(txt,gameId, stage.name, pages.dir = get.pages.dir(gameId=gameId), file = NULL, auto=FALSE) {
	restore.point("save.stage.page")
	if (is.null(file)) {
		file = paste0(stage.name, ifelse(auto,".auto",""), ".Rmd")
	}
	if (!dir.exists(pages.dir))
		try(dir.create(pages.dir, recursive = TRUE))
	
	writeLines(txt, file.path(pages.dir,file))
}

