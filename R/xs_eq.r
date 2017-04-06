xs.show.eq.tab = function(gameId, xs=app$xs, app=getApp()) {
  restore.point("xs.show.eq.tab")
  cat("\nxs.show.eq.tab")
  tabId = paste0("tab_eq_",gameId)
  if (tabId %in% xs$tabs) {
    w2tabs.select("xsTabs", tabId)
    return()
  }
  xs$tabs = c(xs$tabs, tabId)
  
  divId = paste0("div_eq_",gameId)
  tab=list(id=tabId,caption=paste0("Eq. ", gameId), closable=TRUE,div_id = divId)
  w2tabs.add(id="xsTabs", tabs=list(tab))
  ui = xs.eq.ui(gameId)
  appendToHTML(selector="#mainDiv", as.character(hidden_div(id=divId, ui)))
  w2tabs.select("xsTabs", tabId)
}


init.xeq = function(gameId, branching.limit = 10000, sp.limit=1e6) {
	xeq = as.environment(nlist(gameId, branching.limit,sp.limit))
	xeq$rg = 	get.rg(gameId=gameId)
	xeq$variants = xeq$rg$variants
	xeq$tg.li = list()
	xeq$eq.li = list()
	xeq$eqo.li = list()
	xeq
}


xs.eq.ui = function(gameId, xs = app$xs, app=getApp()) {
  restore.point("xs.eq.ui")
	
	ns = NS(paste0("eq-",gameId))
	
	xeq = init.xeq(gameId)
	xeq$ns = ns
	xs$xeq = xeq
	
	rg = xeq$rg
	
	xeq$prefs = pref.classes.default.prefs()
		
	form.sel = ids2sel(c(ns("variants"),ns("prefs"),ns("reduce"), ns("branchingLimit"), ns("spLimit"))) 
	
	ui = tagList(
		h5(paste0("Equilibrium analysis of ", gameId)),
		HTML("<table><tr><td valign='top'>"),
		selectizeInput(ns("variants"),label="Variants",choices = xeq$variants,selected = xeq$variants, multiple=TRUE),
		selectizeInput(ns("prefs"),label="Preferences",choices = names(xeq$prefs),selected = "payoff", multiple=TRUE),
		HTML("</td><td valign='top'>"),
		numericInput(ns("branchingLimit"),label="Branching limit",value = xeq$branching.limit),
		numericInput(ns("spLimit"),label="Strategy Profiles Limit",value = xeq$sp.limit),
		selectInput(ns("reduce"),label="Reduce game by Eliminating some dominated moves",choices = list("No reduction"="noreduce", "Reduce"="reduce","Both"="both")),
		HTML("</td></tr></table>"),
		smallButton(ns("makeTgBtn"),"Generate Game Trees", "data-form-selector"=form.sel),
		smallButton(ns("solveSPEBtn"),"Solve Pure SPE", "data-form-selector"=form.sel),
    uiOutput(ns("tgmsg")),
		br(),
		uiOutput(ns("tginfo")),
		uiOutput(ns("eqsUI"))
		
	)
		
	buttonHandler(ns("makeTgBtn"),function(formValues,...) {
	})
	
	buttonHandler(ns("solveSPEBtn"),function(formValues,...) {
		xeq.solve.spe(xeq=xeq, formValues=formValues, clear=TRUE, never.load = xs$never.load.tg)
		xeq.show.tg.info(xeq)
		xeq.show.eqo(xeq)
	})
	

	
	ui
}

xeq.solve.spe = function(xeq, formValues,clear=TRUE, just.make.tg=FALSE, never.load=TRUE) {
	restore.point("xeq.solve.spe")
	ns = xeq$ns

	if (clear) {
		xeq$tg.li = xeq$eq.li = xeq$eqo.li = list()
	}
	
	variants = unlist(formValues[[ns("variants")]])
	pref_names = unlist(formValues[[ns("prefs")]])
	reduce.method = unlist(formValues[[ns("reduce")]])
	branching.limit = unlist(formValues[[ns("branchingLimit")]])
	sp.limit = unlist(formValues[[ns("spLimit")]])
		
	if (reduce.method=="reduce") {
		reduce.vec = TRUE
	} else if (reduce.method=="noreduce") {
		reduce.vec = FALSE 
	} else {
		reduce.vec = c(FALSE,TRUE)
	}
	
	xeq$sel.variants = variants
	xeq$sel.prefs = xeq$prefs[pref_names]
	xeq$eqo.li = xeq$eq.li = list()

	timedMessage(ns("tgmsg"),msg=paste0("Solve SPE for variants ",paste0(variants,collapse=", ")))
	
	msg.fun = function(...) {
		msg = paste0(...)
		timedMessage(ns("tgmsg"),msg=msg,millis = Inf)
	}
	for (variant in xeq$sel.variants) {
		msg = paste0("Create or load game tree for variant ",variant,"... ")
		timedMessage(ns("tgmsg"),msg=msg)
		tg = get.tg(gameId=gameId, variant=variant, rg=xeq$rg, msg.fun=msg.fun, never.load=never.load)
		
		for (pref in xeq$sel.prefs) {
			tg = as.environment(as.list(tg))
			set.tg.pref(pref,tg)
			for (reduce in reduce.vec) {
				if (reduce) {
					msg = paste0("Solve all pure SPE for reduced variant ",variant," for pref ", pref$name,"... ")
					timedMessage(ns("tgmsg"),msg=msg)
					tg = reduce.tg(tg)
				} else {
					msg = paste0("Solve all pure SPE for variant ",variant," for pref ", pref$name,"... ")
					timedMessage(ns("tgmsg"),msg=msg)
				}
				id = tg$tg.id
				id = str.right.of(id,paste0(tg$gameId,"_"))
				xeq$tg.li[[id]] = tg
				
				if (!just.make.tg) {
					eq.li = get.eq(tg = tg)
					xeq$eq.li[[id]] = eq.li
					eqo = eq.outcomes(eq.li, tg=tg)
					eqo$id = id
					eqo = select(eqo, id, everything())
					xeq$eqo.li[[id]] = eqo
					
				}
			}
		}
	}
	timedMessage(ns("tgmsg"),msg=paste0("All SPE have been generated..."))

}

xeq.show.tg.info = function(xeq) {
	ns = xeq$ns
	restore.point("xeq.show.tg.info")
	
	info.df = xeq.tg.info.df(xeq=xeq)
	html = html.table(info.df)
	setUI(ns("tginfo"),HTML(html))
	dsetUI(ns("tginfo"),HTML(html))
}

xeq.show.eqo = function(xeq) {
	ns = xeq$ns
	restore.point("xeq.show.eqo")
	
	eqo.df = bind_rows(xeq$eqo.li)
	html = html.table(eqo.df)
	ui = tagList(h5("Pure SPE outcomes:"), HTML(html))
	setUI(ns("eqsUI"),ui)
	dsetUI(ns("eqsUI"),ui)
}

xeq.tg.info.df = function(xeq,ids = names(xeq$tg.li),...) {
	restore.point("xeq.tg.info.df")
	
	tg = xeq$tg.li[[1]]
	
	
	no.oco = lapply(ids, function(id) {
		tg = xeq$tg.li[[id]]
		if (is.null(tg)) return("-")
		as.character(NROW(tg$oco.df))
	})
	no.ise = lapply(ids, function(id) {
		tg = xeq$tg.li[[id]]
		if (is.null(tg)) return("?")
		as.character(NROW(tg$ise.df))
	})
	no.sg = lapply(ids, function(id) {
		tg = xeq$tg.li[[id]]
		if (is.null(tg$sg.df)) return("?")
		as.character(NROW(tg$sg.df))
	})
	no.all.sp = lapply(ids, function(id) {
		tg = xeq$tg.li[[id]]
		if (is.null(tg$sg.df)) return("?")
		as.character(tg$sg.df$.num.strats[1])
	})
	no.sp = lapply(ids, function(id) {
		tg = xeq$tg.li[[id]]
		if (is.null(tg$sg.df)) return("?")
		as.character(sum(tg$sg.df$.num.strats.without.desc))
	})

	no.eq = lapply(ids, function(id) {
		eq.li = xeq$eq.li[[id]]
		if (is.null(eq.li)) return("?")
		as.character(length(eq.li))
	})
	
	no.eqo = lapply(ids, function(id) {
		eqo.df = xeq$eqo.li[[id]]
		if (is.null(eqo.df)) return("?")
		as.character(NROW(eqo.df))
	})

	mat = matrix(nrow=8, byrow = TRUE,c(
		"Outcomes",no.oco,
		"Info sets", no.ise,
		"Subgames", no.sg,
		"Strat-profiles...",rep("",length(ids)),
		"...normal-form",no.all.sp,
		"...backward-induction", no.sp,
		"Pure SPE", no.eq,
		"Pure SPE outcomes", no.eqo
	))
	colnames(mat) = c("",ids)
	as.data.frame(mat)
}
