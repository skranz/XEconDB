
# new experiment match
new.em = function(vg=vg, subIds=NULL, app.li = NULL, container.ids = "mainUI") {
	restore.point("new.em")
	
	if (is.null(subIds)) {
		subIds = paste0("SubTest_", seq_len(vg$params$numPlayers))
	}
	
	em = as.environment(nlist(gameId=vg$gameId, variant=vg$variant, vg=vg, subIds=subIds, app.li=app.li, container.ids = container.ids))
	em$em = em
	
	vg = get.vg(gameId=em$gameId, variant=em$variant)
	em$vg = vg
	
	n = vg$params$numPlayers
	em$n = n
	em$players = seq_len(n)
	em$values = vg$params
	
	em$wait.page.ui = HTML(load.rg.wait.page(rg=vg))
  em
}

em.start.match = function(em) {
	restore.point("em.start.match")
	n = em$n
	em$update.page.info.fun = NULL
	em$values = c(list(variant=em$variant),em$vg$params)
	em$delayed.strat.meth = list()
	em$player.stage = rep(0,n)
  em$is.waiting  = rep(TRUE,n)
  em$stage.computed = rep(FALSE, length(em$vg$stages))
  em.proceed.all.player.stage(em=em)
}


em.make.stage.ui = function(stage, player, em) {
	restore.point("em.make.stage.ui")
	vg = em$vg
	page = load.rg.stage.page(stage, rg=vg)
	
	em$page.values = c(em$values, list(.player = player, .em=em, .vg=vg))
	
	# will only be temporary assigned
	em$player = player
	em$ns = NS(paste0("em-page-",stage$stage.name,"-",player))
	
	# set global em for rendered fields
	app = getApp()
	app$glob$em = em
	
	cr = compile.rmd(text=page, out.type = "shiny",envir = em$page.values,blocks = "render")
  ui = render.compiled.rmd(cr,envir=em$page.values,use.commonmark=FALSE)
	ui
}

get.em.container.id = function(em, player=1) {
	if (is.null(em$container.ids)) return("mainUI")
	pos = min(player,length(em$container.ids))
	em$container.ids[[pos]]
}

get.em.player.app = function(em, player=1) {
	restore.point("get.em.player.ap")
	if (is.null(em$app.li)) return(getApp())
	pos = min(player,length(em$app.li))
	em$app.li[[pos]]
} 

em.show.current.page = function(em, player=seq_len(em$vg$params$numPlayers)) {
  restore.point("em.show.current.page.multi")
 	
	if (length(player)>1) {
		for (p in player) {
			em.show.current.page(em, player=p)
		}
		return()
	}
  restore.point("em.show.current.page")
	
	# may differ depending whether we are in
	# test mode in the xs or not
	container.id = get.em.container.id(em, player=player)
	
	# each subject will have a separate app
	app = get.em.player.app(em=em, player=player)
	
	stage.num = em$player.stage[player]
	if (stage.num < 1 | stage.num > length(em$vg$stages)) {
		ui = wait.ui(em=em, player)
		setUI(container.id,ui, app=app)
		dsetUI(container.id,ui, app=app)
	}
	
	stage = em$vg$stages[[stage.num]]

  if (!em$is.waiting[player]) {
  	stage.ui = try(em.make.stage.ui(stage=stage,player=player,em=em))
  } else {
  	stage.ui = try(wait.ui(em))
  }
  
  
  if (is(stage.ui, "try-error")) {
  	stage.ui = HTML(paste0("An error occured when parsing the page for stage ", stage$name,":<br><br>", as.character(stage.ui)))
  }

  setUI(container.id,stage.ui, app=app)  
 	dsetUI(container.id,stage.ui, app=app)  
}


get.em.stage = function(em, player=1) {
	stage.num = em$player.stage[player]
	if (stage.num < 1 | stage.num > length(em$vg$stages)) return(NULL)
	em$vg$stages[[stage.num]]
}

em.is.stage.for.player = function(em,player=1, stage.num) {
	restore.point("em.is.stage.for.player")

	stage = em$vg$stages[[stage.num]]
	has.vars = names(em$values)
	
	cond.var = stage$condition.need.var
	if (!all(cond.var %in% has.vars)) return("wait")
	if (is.call(stage$condition) | is.name(stage$condition)) {
		cond.val = eval(stage$condition, em$values)
		if (!cond.val) return("skip")
	}
	
	# wait until all needed earlier stages are solved
	# and the corresponding variables are computed
	if (!all(stage$need.vars %in% has.vars)) return("wait")

	
	if (is.call(stage$player) | is.name(stage$player)) {
		stage.player = eval(stage$player, em$values)
	} else {
		stage.player = stage$player
	}
	
	if (!player %in% stage.player) {
		# stage was already computed
		if (em$stage.computed[stage.num]) return("skip")
		
		# a player exists for this stage
		# only compute when that player enters
		if (any(1:em$n %in% stage.player)) return("skip")
		
		# this is a stage without players
		# perform computations
		return("compute")
	}

	return("show")
}

em.proceed.all.player.stage = function(em) {
	for (player in seq_len(em$n)) {
		# only proceeds if player is waiting
		em.proceed.player.stage(em, player=player)
	}
	# if we want to see page info
	# for debugging purposes
	if (!is.null(em$update.page.info.fun))
		em$update.page.info.fun(em)
}

# only proceed if a player has finished her current stage
em.proceed.player.stage = function(em, player=1) {
  restore.point("em.proceed.player.stage")
	
	#if (player == 2 & em$player.stage[1] >= 1 & em$is.waiting[1]) stop()
	if (!em$is.waiting[player])
		return()

  vg = em$vg
  n = em$n
  
  stage.num = em$player.stage[player]
  
  restore.point("em.proceed.player.stage.2")
  
  next.stage = stage.num
  while(TRUE) {
  	next.stage = next.stage + 1
	  if (next.stage > length(vg$stages)) {
	  	# TO DO: special handling of final stage
	  	em$player.stage[player] = length(vg$stages)
	  	em$is.waiting[player] = TRUE
	  	em.show.current.page(em, player)
	    return()
	  }
  	res <- em.is.stage.for.player(em=em,player=player, stage.num = next.stage)
  	if (res == "compute") {
  		em.run.stage.computations(stage.num=next.stage, em=em)
  		next
  	}
  	if (res == "skip") next
  	break
  }
  

  # res is "wait" or "show"
  if (res == "wait") {
  	# set previous stage for player
  	em$player.stage[player] = next.stage-1
  	em$is.waiting[player] = TRUE
  	em.show.current.page(em, player)
    return()
  }

   # res is "show"
  
  # set current stage for player
  em$player.stage[player] = next.stage

  em$is.waiting[player] = FALSE
  em.run.stage.computations(next.stage, em)
  em.show.current.page(em=em, player=player)
}


em.finish.match = function(em) {
	ui = tags$p("The game is finished")
	for (player in seq_along(em$players)) {
		app = get.em.player.app(em=em, player=player)
		id = get.em.container.id(em=em, player=player)
		setUI(id,ui, app=app)
		dsetUI(id,ui, app=app)
	}
	
	
	cat("\n\nThe game is finished...")
}

# simply perform all computations:
# draw random variables and compute transformations
# for a stage.
# Should be called when a stage is reached in
# a running experiment
em.run.stage.computations = function(stage.num, em, skip.if.computed=TRUE) {
  restore.point("em.run.stage.computations")
	
	if (em$stage.computed[stage.num] & skip.if.computed) 
		return() 
	
	em$stage.computed[stage.num] = TRUE
	stage = em$vg$stages[[stage.num]]
	
	# draw from random variables
	for (rv in stage$nature) {
		var = rv$name
		set = eval.or.return(rv$set, em$values)
		prob = eval.or.return(rv$prob, em$values)
		if (nchar(prob)==0) prob=NULL
		val = sample(set,1,prob=prob)
		
		em$values[[var]] = val
	}
  
	# compute transformations
	for (tr in stage$compute) {
		var= tr$name
		val = eval.or.return(tr$formula, em$values)
		em$values[[var]] = val
	}
	
	# check if delayed strategy method values
	# can be assigned and if yes do so
	em.assign.delayed.strat.meth.realizations(em=em)
}


get.page.ns = function(stage.name, player) {
	NS(paste0("page-",stage.name,"-",player))
}

wait.ui = function(..., em=get.em()) {
	if (!is.null(em$wait.page.ui))
		return(em$wait.page.ui)
	
	html = load.rg.wait.page(rg=em$vg)
	HTML(html)
  #ui = h3("Please wait...")
  #ui
}



em.submit.btn.click = function(formValues, player, stage.name,action.ids,sm.ids, ..., em=get.em()) {
	restore.point("em.submit.btn.click")
	cat("\nsubmit.btn.clicked!\n")
	
	stage = get.em.stage(em=em, player=player)
	
	ids = c(action.ids, sm.ids)
	for (id in ids) {
		if (isTRUE(length(formValues[[id]])==0) |  isTRUE(formValues[[id]]=="")) {
			errorMessage(get.page.ns(stage.name = stage.name,player=player)("msg"),"Please make all required choices, before you continue.")
			return()
		}
	}
	
	if (length(formValues)>0 & length(ids)>0) {
		avals = lapply(formValues[ids], convert.atom)
		em$values[names(ids)] = avals

		
		em.assign.delayed.strat.meth.realizations(em=em)		
		em.assign.strat.meth.realizations(em=em, actions=stage$actions)
	}
	
	
	em$is.waiting[player] = TRUE
	em.proceed.all.player.stage(em)
	
}


get.sm.value = function(action.name, values, domain.var) {
	restore.point("get.sm.value")
	postfix = paste0(values[domain.var], collapse="_")
	var = paste0(action.name,"_",postfix)
	values[[var]]
	
}

# try to assign the actual action value 
# from the values of a strategy method
# E.g. in an ultimatum game if offer=4
# and accept_4 = TRUE, we set accept= TRUE
# If offer is not yet computed
# (stages can be shown in parallel to players)
# store the action accept in 
# em$delayed.strat.meth and try to assign
# the value of accept later with 
# em.assign.delayed.strat.meth.realizations
em.assign.strat.meth.realizations = function(em,actions) {
	restore.point("em.assign.strat.meth.realizations")
	# which actions use strategy method
	use.sm = sapply(actions, function(action) !is.null(action$domain.var))
	actions = actions[use.sm]
	
	for (action.name in names(actions)) {
		action = actions[[action.name]]
		
		has.domain = unlist(lapply(action$domain.var, function (dv) dv %in% names(em$values)))
		
		if (!all(has.domain)) {
			
			em$delayed.strat.meth[[action.name]] = action	
		} else {
			em$values[[action.name]] = get.sm.value(action.name = action.name,values = em$values,domain.var = actions[[action.name]]$domain.var)
		}
	}
}

em.assign.delayed.strat.meth.realizations = function(em) {
	restore.point("em.assign.delayed.strat.meth.realizations")
	# which actions use strategy method
	actions = em$delayed.strat.meth
	
	for (action.name in names(actions)) {
		action = actions[[action.name]]
		
		has.domain = unlist(lapply(action$domain.var, function (dv) dv %in% names(em$values)))
		
		if (all(has.domain)) {
			em$values[[action.name]] = get.sm.value(action.name = action.name,values = em$values,domain.var = actions[[action.name]]$domain.var)
			
			em$delayed.strat.meth = em$delayed.strat.meth[setdiff(names(em$delayed.strat.meth), action.name)]	
		}
	}
}


submitPageBtn = function(label="Press to continue",em=get.em(),player=em$player,...) {
	restore.point("submitPageBtn")
	
	stage = get.em.stage(em=em, player=player)
	
	ns = get.page.ns(stage$name,em$player)

	id = paste0(ns("submitPageBtn"))
	
	actions = stage$actions
	
	# which actions use strategy method
	use.sm = unlist(sapply(actions, function(action) !is.null(action$domain.var)))
	if (is.null(use.sm)) use.sm = logical(0)
	
	action.ids = unlist(sapply(names(actions[!use.sm]),get.action.input.id, em=em,USE.NAMES = FALSE))
	
	# get ids of all strategy method fields
	li = lapply(actions[use.sm], function(action) {
		postfix = paste.matrix.cols(action$domain.vals,sep="_")
		get.action.input.id(name=paste0(action$name,"_",postfix),em=em)
	})
	names(li) = NULL
	sm.ids = unlist(li)

	app = get.em.player.app(em=em, player=player)
	
	buttonHandler(id, em.submit.btn.click, player=em$player, stage.name = stage$name, action.ids=action.ids,sm.ids=sm.ids, app = app)
	
	dsetUI(ns("msg"),"", app=app)

	as.character(
		tagList(
			uiOutput(ns("msg")),
			smallButton(id,label, form.ids = c(action.ids,sm.ids))
		)
	)
}

actionField = function(name,label=NULL,choiceLabels=NULL, inputType="auto",em=get.em(),player=em$player,action.name = name, ...) {
	vg = em$vg
	stage = get.em.stage(em, player)
	action = stage$actions[[action.name]]
	if (identical(choiceLabels,""))
		choiceLabels = NULL
	restore.point("actionField")
	
	if (!is.null(label)) {
		label = replace.whiskers(label, em$page.values,whisker.start = "<<", whisker.end = ">>")
	}
	
	id = get.action.input.id(name=name,em=em, player=player) 
  choices = eval.or.return(action$set, em$page.values)
	
	if (inputType=="auto") {
    if (length(choices)<=12){
      inputType="radio"      
    } else {
      inputType="selectize"
    }
	}
  #inputType = "selectize"
  
  if (!is.null(choiceLabels)) {
    choices = as.list(choices)
    names(choices) = choiceLabels
  }
  if (inputType=="radio") {
    ui = radioButtons(inputId = id,label = label,choices = choices, selected=NA)
  } else if (inputType=="rowRadio") {
    ui = rowRadioButtons(inputId = id,label = "",choices = choices, selected=NA)
  } else {
  	choices = c(list(""),as.list(choices))
    ui = selectizeInput(inputId = id,label = label,choices = choices, selected=NA)    
  }
  
  html = as.character(ui)
	html	
}

rowRadioButtons = function(inputId,label=NULL, choices, selected = NA) {
	restore.point("rowRadioButtons")
	choices =  shiny:::choicesWithNames(choices)

	checked = rep("", length(choices))
	if (!is.na(selected)) {
		names(checked) = as.character(choices)
		checked[selected] = ' checked="checked"'
	}

	
	inner = paste0('
<td><label>
		<input type="radio" name="', inputId,'" value="',choices,'"',checked,'/>
		<span>',names(choices),'</span>
</label></td>', collapse="\n")
	
	html = paste0('<div id="',inputId,'" class="shiny-input-radiogroup shiny-input-container"><table class="rowRadioTable"><tr>',inner,'</tr></table></div>')
	
	HTML(html)
		
}


eval.stratMethRows.block = function(txt,envir=parent.frame(), out.type=first.none.null(cr$out.type,"html"),info=NULL, cr=NULL,...) {
	args = list(...)
	restore.point("eval.stratMethRows.block")
	
	html = merge.lines(info$inner.txt)
	# need to reverse placeholders to original whiskers
	html = reverse.whisker.placeholders(html, cr=cr)
	
	
	args = parse.block.args(info$header)
	action.name = args$action
	em = envir$.em
	
	stage = get.em.stage(em=em, player=em$player)
	action = stage$actions[[action.name]]
	
	out = stratMethRows(action=action.name, domain.vals =action$domain.vals, html=html, em=em)
	out
}


stratMethRows = function(action.name,domain.vals, html,em=get.em(),player=em$player,as.tr = FALSE, ...) {
	restore.point("stratMethTable")
	vg = em$vg
	stage = get.em.stage(em, player)
	domain.var = names(domain.vals)
	
	domain.vals = as_data_frame(domain.vals)
	
	stratMethInput = function(inputType="select",choiceLabels=NULL,...) {
		actionField(name = paste0(action.name,"_",domain.val),label = "",inputType = inputType,choiceLabels = choiceLabels, em=em, action.name=action.name)
	}

	values = c(nlist(action=action.name, domain.var, stratMethInput), em$page.values)
	
	domain.val = 0
	res.html = unlist(lapply(seq_len(NROW(domain.vals)), function(row) {
		# assign to global
		# to make domain.val
		# accessible in stratMethodInput
		domain.val <<- as.list(domain.vals[row,])
		values$domain.val = domain.val
		replace.whiskers(merge.lines(html), values, eval=TRUE)
	}))

	if (as.tr) {
		res.html = paste0("<tr>", res.html,"</tr>", collapse="\n")
	} else {
		res.html = paste0(res.html, collapse="\n")
	}
	res.html
}



get.action.input.id = function(name, stage=get.em.stage(em, player), player=em$player, vg=em$vg, em=NULL) {
	id = paste0(em$vg$vg.id,"-action-",name, "-",em$player)
	names(id) = name
	id
}

set.app.em = function(em, app=getApp()) {
	app$experiment.match = em
}

get.em = function(...,app=getApp()) {
	em = 	app$experiment.match
	if (!is.null(em)) return(em)
	app$glob$em
}

