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


init.xeq = function(gameId, branching.limit = 10000) {
	xeq = as.environment(nlist(gameId, branching.limit))
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
	xs$xeq = xeq
	
	rg = xeq$rg
		
	form.sel = ids2sel(c(ns("variants"),ns("branchingLimit"))) 
	
	ui = tagList(
		h5(paste0("Equilibrium analysis of ", gameId)),
		selectizeInput(ns("variants"),label="Variants",choices = xeq$variants,selected = xeq$variants, multiple=TRUE),
		numericInput(ns("branchingLimit"),label="Branching limit",value = xeq$branching.limit),
		helpText("Some game trees may be too large to handle. We stop generating the game tree if the number of branches in the current level has exceeded the branching limit."),
		smallButton(ns("makeTgBtn"),"Generate Game Trees", "data-form-selector"=form.sel),
		smallButton(ns("solveSPEBtn"),"Solve Pure SPE", "data-form-selector"=form.sel),
    uiOutput(ns("tgmsg")),
		br(),
		uiOutput(ns("tginfo")),
		uiOutput(ns("eqsUI"))
		
	)
	buttonHandler(ns("makeTgBtn"),function(formValues,...) {
		variants = unlist(formValues[[ns("variants")]])
		branching.limit = formValues[[ns("branchingLimit")]]
		restore.point("makeTgBtnClick")
		xeq$sel.variants = variants
		
		timedMessage(ns("tgmsg"),msg=paste0("Generate game trees for variants ",paste0(variants,collapse=", ")))
		
		msg.fun = function(...) {
			msg = paste0(...)
			timedMessage(ns("tgmsg"),msg=msg,millis = Inf)
		}
		for (variant in xeq$sel.variants) {
			msg = paste0("Generate or load game tree for variant ",variant,"... ")
			timedMessage(ns("tgmsg"),msg=msg)
			tg = get.tg(gameId=gameId, variant=variant, rg=xeq$rg, msg.fun=msg.fun, never.load=xs$never.load.tg)
			xeq$tg.li[[variant]] = tg
			
			rtg = reduce.tg(tg)
			xeq$tg.li[[paste0(variant,".reduced")]] = rtg
			
		}
		rvariants = c(variants,paste0(variants,".reduced"))

		timedMessage(ns("tgmsg"),msg=paste0("Game trees generated..."))
		info.df = xeq.tg.info.df(xeq=xeq, variants=rvariants)
		html = html.table(info.df)
		setUI(ns("tginfo"),HTML(html))
		dsetUI(ns("tginfo"),HTML(html))
	})
	
	buttonHandler(ns("solveSPEBtn"),function(formValues,...) {
		variants = unlist(formValues[[ns("variants")]])
		restore.point("solveSPEClick")
		xeq$sel.variants = variants
		xeq$eqo.li = xeq$eq.li = list()

		timedMessage(ns("tgmsg"),msg=paste0("Solve SPE for variants ",paste0(variants,collapse=", ")))
		
		msg.fun = function(...) {
			msg = paste0(...)
			timedMessage(ns("tgmsg"),msg=msg,millis = Inf)
		}
		for (variant in xeq$sel.variants) {
			msg = paste0("Create or load game tree for variant ",variant,"... ")
			timedMessage(ns("tgmsg"),msg=msg)
			tg = get.tg(gameId=gameId, variant=variant, rg=xeq$rg, msg.fun=msg.fun, never.load=TRUE)
			xeq$tg.li[[variant]] = tg
			

			#msg = paste0("Create Gambit .efg file for ",variant,"... ")
			#tg.to.efg(tg=tg)
			msg = paste0("Solve all pure SPE for variant ",variant," with Gambit... ")
			msg.fun(msg)
			eq.li = get.eq(tg = tg)
			xeq$eq.li[[variant]] = eq.li
			
			eqo = eq.outcomes(eq.li, tg=tg)
			eqo$variant = variant
			eqo = select(eqo, variant, everything())
			
			xeq$eqo.li[[variant]] = eqo

			msg = paste0("Reduce game and compute SPE for variant ",variant," with Gambit... ")
			msg.fun(msg)
			
			rtg = reduce.tg(tg)
			rvariant = rtg$variant
			xeq$tg.li[[rvariant]] = rtg
			
			eq.li = get.eq(tg = rtg)
			xeq$eq.li[[rvariant]] = eq.li
			
			eqo = eq.outcomes(eq.li, tg=rtg)
			eqo$variant = rvariant
			eqo = select(eqo, variant, everything())
			
			xeq$eqo.li[[rvariant]] = eqo
			
		}
		timedMessage(ns("tgmsg"),msg=paste0("SPE have been generated..."))
		
		rvariants = names(xeq$eqo.li)
		
		info.df = xeq.tg.info.df(xeq=xeq, variants = rvariants)
		html = html.table(info.df)
		setUI(ns("tginfo"),HTML(html))
		dsetUI(ns("tginfo"),HTML(html))
		
		eqo.df = bind_rows(xeq$eqo.li)
		html = html.table(eqo.df)
		ui = tagList(h5("Pure SPE outcomes:"), HTML(html))
		setUI(ns("eqsUI"),ui)
		dsetUI(ns("eqsUI"),ui)
		
	})
	

	
	ui
}

xeq.tg.info.df = function(xeq, variants = xeq$sel.variants) {
	restore.point("xeq.tg.info.df")
	
	tg = xeq$tg.li[[1]]
	no.oco = lapply(variants, function(variant) {
		tg = xeq$tg.li[[variant]]
		if (is.null(tg)) return("-")
		as.character(NROW(tg$oco.df))
	})
	no.ise = lapply(variants, function(variant) {
		tg = xeq$tg.li[[variant]]
		if (is.null(tg)) return("?")
		as.character(NROW(tg$ise.df))
	})
	no.sg = lapply(variants, function(variant) {
		tg = xeq$tg.li[[variant]]
		if (is.null(tg$sg.df)) return("?")
		as.character(NROW(tg$sg.df))
	})
	no.all.sp = lapply(variants, function(variant) {
		tg = xeq$tg.li[[variant]]
		if (is.null(tg$sg.df)) return("?")
		as.character(tg$sg.df$.num.strats[1])
	})
	no.sp = lapply(variants, function(variant) {
		tg = xeq$tg.li[[variant]]
		if (is.null(tg$sg.df)) return("?")
		as.character(sum(tg$sg.df$.num.strats.without.desc))
	})

	no.eq = lapply(variants, function(variant) {
		eq.li = xeq$eq.li[[variant]]
		if (is.null(eq.li)) return("?")
		as.character(length(eq.li))
	})
	
	no.eqo = lapply(variants, function(variant) {
		eqo.df = xeq$eqo.li[[variant]]
		if (is.null(eqo.df)) return("?")
		as.character(NROW(eqo.df))
	})

	mat = matrix(nrow=8, byrow = TRUE,c(
		"Outcomes",no.oco,
		"Info sets", no.ise,
		"Subgames", no.sg,
		"Strat-profiles...",rep("",length(variants)),
		"...normal-form",no.all.sp,
		"...backward-induction", no.sp,
		"Pure SPE", no.eq,
		"Pure SPE outcomes", no.eqo
	))
	colnames(mat) = c("Variant",variants)
	as.data.frame(mat)
}
