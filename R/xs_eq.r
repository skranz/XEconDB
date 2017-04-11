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
	
	xeq$solve.modes = list(
		"All pure SPE (Gambit)"="spe",
		"All pure SPE (internal)"="spe_xs",
		"Just Gametree"="gametree",
		"All pure NE"="ne",
		"All NE (including mixed)"="ne_am",
		"Some SPE (logit)"="spe_sm_logit",
		"Some SPE (lcp)"="spe_sm_lcp",
		"Some NE (ipa)"="ne_sm_ipa",
#		"Some NE (liap)"="ne_sm_liap",
		"Some NE (gnm)"="ne_sm_gnm",
#		"Some NE (simpdiv)"="ne_sm_simpdiv",
		"Some NE (lcp)"="ne_sm_lcp"
#		"QRE (Quantal Response Eq.)"="qre"
	)

	
	xeq$prefs = pref.classes.default.prefs()
		
	form.sel = ids2sel(c(ns("variants"),ns("prefs"),ns("reduce"), ns("branchingLimit"), ns("spLimit"),ns("solvemode"))) 
	
	ui = tagList(
		h5(paste0("Equilibrium analysis of ", gameId)),
		HTML("<table><tr><td valign='top'>"),
		selectizeInput(ns("variants"),label="Variants",choices = xeq$variants,selected = xeq$variants, multiple=TRUE),
		selectizeInput(ns("prefs"),label="Preferences",choices = names(xeq$prefs),selected = "payoff", multiple=TRUE),
		HTML("</td><td valign='top'>"),
		numericInput(ns("branchingLimit"),label="Branching limit",value = xeq$branching.limit),
		numericInput(ns("spLimit"),label="Strategy Profiles Limit",value = xeq$sp.limit),
		selectInput(ns("reduce"),label="Reduce game by Eliminating some dominated moves",choices = list("No reduction"="noreduce", "Reduce"="reduce","Both"="both")),
		selectInput(ns("solvemode"),label="Solve for",choices = xeq$solve.modes),
		HTML("</td></tr></table>"),
		smallButton(ns("solveBtn"),"Solve", "data-form-selector"=form.sel),
    uiOutput(ns("tgmsg")),
		br(),
		uiOutput(ns("tginfo")),
		uiOutput(ns("eqsUI"))
		
	)
		
	buttonHandler(ns("solveBtn"),function(formValues,...) {
		restore.point("xeqSolveClick")
		ok = xeq.solve(xeq=xeq, formValues=formValues, clear=TRUE, never.load = xs$never.load.tg)
		
		if (ok) {
			xeq.show.tg.info(xeq)
			xeq.show.eqo(xeq)
		}
	})
	

	
	ui
}

xeq.solve = function(xeq, formValues,clear=TRUE,  never.load=TRUE) {
	restore.point("xeq.solve")
	ns = xeq$ns

	if (clear) {
		xeq$tg.li = xeq$eq.li = xeq$eqo.li = list()
	}
	
	variants = unlist(formValues[[ns("variants")]])
	pref_names = unlist(formValues[[ns("prefs")]])
	reduce.method = unlist(formValues[[ns("reduce")]])
	branching.limit = unlist(formValues[[ns("branchingLimit")]])
	sp.limit = unlist(formValues[[ns("spLimit")]])
	solvemode = unlist(formValues[[ns("solvemode")]])
	xeq$solvemode = solvemode
		
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

	timedMessage(ns("tgmsg"),msg=paste0("Solve equilibria for variants ",paste0(variants,collapse=", ")))
	
	msg.fun = function(...) {
		msg = paste0(...)
		timedMessage(ns("tgmsg"),msg=msg,millis = Inf)
	}
	variant = xeq$sel.variants[[1]]
	for (variant in xeq$sel.variants) {
		msg = paste0("Create or load game tree for variant ",variant,"... ")
		timedMessage(ns("tgmsg"),msg=msg)
		org.tg = get.tg(gameId=xeq$gameId, variant=variant, rg=xeq$rg, msg.fun=msg.fun, never.load=never.load, branching.limit = branching.limit)
		
		if (org.tg$kel$count>0) {
    	timedMessage(ns("tgmsg"),paste0("There are problems:<br>",paste0(org.tg$kel$log, collapse="<br>\n")),millis = Inf)
    	return(FALSE)
  	}

		
		solver = xeq.solvemode.to.solver(solvemode, n=org.tg$params$numPlayers)
		just.make.tg= (solvemode == "gametree")
		
		for (pref in xeq$sel.prefs) {
			tg = as.environment(as.list(org.tg))
			set.tg.pref(pref,tg)
			for (reduce in reduce.vec) {
				if (reduce) {
					msg = paste0("Solve equilibria for reduced variant ",variant," for pref ", pref$name,"... ")
					timedMessage(ns("tgmsg"),msg=msg)
					tg = reduce.tg(tg)
				} else {
					msg = paste0("Solve equilibria for variant ",variant," for pref ", pref$name,"... ")
					timedMessage(ns("tgmsg"),msg=msg)
				}
				id = tg$tg.id
				id = str.right.of(id,paste0(tg$gameId,"_"))
				xeq$tg.li[[id]] = tg
				
				if (!just.make.tg) {
					eq.li = get.eq(tg = tg,solver=solver, solvemode = solvemode)
					xeq$eq.li[[id]] = eq.li
					eqo = eq.outcomes(eq.li, tg=tg)
					eqo$.id = rep(id,NROW(eqo))
					eqo = select(eqo, .id, everything())
					xeq$eqo.li[[id]] = eqo
				}
			}
		}
	}
	timedMessage(ns("tgmsg"),msg=paste0("Equilibria have been generated..."))
	return(TRUE)

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
	
	if (isTRUE(xeq$solvemode=="gametree")) {
		ui = p("")
		setUI(ns("eqsUI"),ui)
		dsetUI(ns("eqsUI"),ui)
		return()
	}
	
	eqo.df = bind_rows(xeq$eqo.li)
	html = html.table(eqo.df)
	
	compute.expected = TRUE
	if (compute.expected) {
		eeqo.df = expected.eq.outcomes(eqo.df,group.vars = c(".id","eqo.ind"))
		html2 = html.table(eeqo.df)
	}
	ui = tagList(
		if (compute.expected) {
			tagList(
				h5("Expected equilibrium outcomes:"), HTML(html2)
			)
		},
		h5("Equilibrium outcomes:"), HTML(html)
	)
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
	colnames(mat) = c(".",ids)
	as.data.frame(mat)
}

xeq.solvemode.to.solver = function(solvemode, n=Inf) {
	if (solvemode == "spe") {
		solver = "gambit-enumpure -q -P"
	} else if (solvemode == "ne") {
		solver = "gambit-enumpure -q"
	} else if (solvemode == "ne_am") {
		if (n == 2) {
			solver = "gambit-enummixed -q -d 4"
		} else {
			solver = "gambit-enumpoly -q"
		}
	} else if (solvemode == "qre") {
		solver = "gambit-logit -q"
	} else if (solvemode == "spe_sm_lcp") {
		solver = "gambit-lcp -q -d 4 -P"
	} else if (solvemode == "spe_sm_logit") {
		solver = "gambit-logit -q -e"
	} else if (solvemode == "ne_sm_simpdiv") {
		solver = "gambit-simpdiv -q"
	} else if (solvemode == "ne_sm_liap") {
		solver = "gambit-liap -q -d 4"
	} else if (solvemode == "ne_sm_lcp") {
		solver = "gambit-lcp -q -d 4"
	} else if (solvemode == "ne_sm_ipa") {
		solver = "gambit-ipa -q -d 4"
	} else if (solvemode == "ne_sm_gnm") {
		solver = "gambit-gnm -q -d 4"
	} else {
		solver = ""
	}
	return(solver)
	
	
}
