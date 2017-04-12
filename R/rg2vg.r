# Reduce an R game rg to a single variant game vg

# Steps
# 1. Set variant and params
# 2. TO DO: Remove stages whose condition always fails in this variant


rg.to.vg = function(rg, variant=1) {
  restore.point("rg.to.vg")
  vg = new.env()
  vg$kel = keyErrorLog()
  vg$gameId = rg$gameId
  if (is.numeric(variant)) variant = rg$variants[variant]
  vg$variant = variant
  vg$vg.id = paste0(vg$gameId,"_",vg$variant)
  vg$params = as.list(rg$varpar[variant,,drop=FALSE])
  vg$stages = rg$stages
  vg = extract.vg.vars.info(vg=vg)
  vg$jg.hash = rg$jg.hash
  return(vg)
}

# a vector of the classes of all vg variables
# useful for otree export without need to 
# compute the whole table game tg representation
extract.vg.vars.info = function(vg, kel=vg$kel) {
	restore.point("extract.vg.vars.info")
	
  # variables defined in stages
  svars = unique(unlist(lapply(vg$stages, function(stage) {
    get.names(c(stage$actions, stage$nature, stage$compute))
  })))
  
  # all variables and paramaters
  vars = unique(c(names(vg$params), svars))
  n = length(vars)
  
  # a list with values
  vals = lapply(1:n, function(i) NA)
  names(vals) = vars
  vals[names(vg$params)] = vg$params
  
  classes = sapply(1:n, function(i) "NA")
  names(classes) = vars
  classes[names(vg$params)] = sapply(vg$params, function(x) class(x)[1])
  
  
  
  # go through stages and compute values and class of variables
  for (stage.num in seq_along(vg$stages)) {
  	stage = vg$stages[[stage.num]]
    stage.key = kel$setKey("stages", stage.num)

    # check condition
    kel$setKey(stage.key, "condition")
  	cond = stage$cond
	  if (!is.call(cond) &!is.name(cond)) {
	    # no condition
	    if (!identical(str.trim(cond), "")) {
	    	kel$write("Either you specify no stage condition, or you write an R formula starting with '=', which evaluates as TRUE or FALSE.")
	    }
	  }
    kel$kelTry(eval(cond, vals))
  	
    for (a.num in seq_along(stage$nature)) {
    	a = stage$nature[[a.num]]
      var = a$name
    	move.key = kel$setKey(stage.key, "nature", a.num)
      
    	
    	kel$setKey(move.key, "set")
      set = kel$kelTry(eval(a$set, vals), msg=paste0("Evaluating set for ", var))
      
    	kel$setKey(move.key, "probs")
      kel$kelTry(eval(a$probs, vals), msg=paste0("Evaluating probs for ", var))
      
      if (length(set)>0) {
        val = set[ceiling(length(set)*0.3)]
        vals[[var]] = val
        classes[[var]] = class(val)[1]
      }
    }
    for (a.num in seq_along(stage$compute)) {
    	a = stage$compute[[a.num]]
      var = a$name
    	move.key = kel$setKey(stage.key, "compute", a.num)
      var = a$name
    	kel$setKey(move.key, "formula")
    	if (is.call(a$formula) | is.name(a$formula)) {
    		variables = find.variables(a$formula)
    		undefined = setdiff(variables, vars)
    		if (length(undefined) >0 ) {
    			kel$write(paste0("The variable(s) ",paste0(undefined, collapse=", ")," have not been defined earlier."))
    		}
    	}
      val = kel$kelTry(eval(a$formula, vals), msg=paste0("Evaluating formula for ", var))
      vals[[var]] = val
      classes[[var]] = class(val)[1]
    }
    
    for (a.num in seq_along(stage$actions)) {
    	a = stage$actions[[a.num]]
    	move.key = kel$setKey(stage.key, "actions", a.num)
      
    	var = a$name
      set = kel$kelTry(eval(a$set, vals),msg=paste0("Evaluating set for ", var))
      if (length(set)>0) {
        val = set[ceiling(length(set)*0.3)]
        vals[[var]] = val
        classes[[var]] = class(val)[1]
      }
    }
 
    # check observe
  	# observe is fixed, no formula
  	kel$setKey(stage.key, "observe")
 		observe = stage$observe
  	if (is.character(observe)) {
			unknown = setdiff(observe, c(names(vals),""))
    	if (length(unknown)>0) {
      	kel$write("You cannot observe the variable(s) {{unknown}}, because they have not been defined earlier.", unknown=unknown)
    	}
    } else if (is.call(observe) | is.name(observe)) {
    		kel$warning("Warning: Better don't use a formula for observe: Forms and export to oTree may not work correctly. If you have fixed variables that are observed, just write a list, like [var1, var2]. If the observed variables depend on earlier variables, better create multiple stages with different conditions, that then each have fixed observed variables.")
    		kel$kelTry(eval(observe, vals))
    }
  }

       
  
  vg$vars = vars
  vg$vars.class = classes
  vg$vars.sample = vals
  vg
}