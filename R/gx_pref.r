example.pref.types.panel = function() {
  pref.types.panel()
}

pref.type.to.yaml = function(pref) {
  restore.point("pref.type.to.yaml")
  
  txt = paste0(pref$name,":\n",
               "\tname: _auto # ", pref$name,"_params...\n",
               "\tplayer: _all\n",
               "\tprob: 1  # probabiliy of type")
  # par = "alpha"
  param.txt = sapply(names(pref$params), function(par) {
    p = pref$params[[par]]
    paste0("\t",par,": ", quote.char(p$default,"'"), " # ", p$descr)
  }) 
  yaml = merge.lines(c(txt,param.txt))
  yaml
}

examples.parse.pref.yaml = function() {

yaml = '
name: _auto

envyUtil:
	name: _auto # envyUtil_params...
	player: _all
	prob: 1  # probabiliy of type
	alpha: 1 # the degree of envy
'

}

parse.pref.yaml = function(yaml, li=NULL) {
  restore.point("parse.pref.yaml")
  if (is.null(li))
    li = read.yaml(text=yaml)  

  li
  
}


gx.pref.types.panel = function(id.prefix="prefTypes_", types=get.types(), pref.types =  types$prefType$subTypes, position="below") {
  restore.point("pref.types.panel")
  #utypes = 
  li = lapply(pref.types, function(pt) {
    type = types[[pt]]
    ex = make.prefs.example.yaml(pt)
    pre.html = HTML(paste0('<pre style="tab-size: 4"><span class="inner-pre" style="font-size: small">',ex,'</span></pre>'))

    tabPanel(title=str.left.of(pt,"Util"),value=pt,
      p(paste0(type$descr," Example:"), pre.html)       
    )
  })
  ui = do.call(tabsetPanel, c(list(id=paste0(id.prefix,"Tabset")),position=position,li))
  ui  
}

gx.pref.panel.ui = function(app=getApp(), gx = app$gx,...) {
  restore.point("gx.pref.panel.ui")
  ui = fluidRow(column(offset=1, width=11,
    gx.pref.types.panel(position="below"),
    bsButton("gxPrefCopyExampleBtn","add example",size="small"),
    aceEditor("gxPrefEditor",height = "100px",showLineNumbers = TRUE, wordWrap = FALSE,mode = "yaml", value=  gx$pref.yaml),
    bsAlert("prefAlert"),
    actionButton("gxPrefSaveBtn","save")
  ))
  buttonHandler("gxPrefCopyExampleBtn", mode="add", gx.pref.copy.example)
  buttonHandler("gxPrefSaveBtn", gx.yaml.pref.save)
  ui  
}

gx.pref.copy.example = function(app=getApp(), gx = app$gx,mode="copy",...) {
  ptype = getInputValue("prefTypes_Tabset")
  restore.point("gx.pref.type.copy.example")
  yaml = make.prefs.example.yaml(ptype)
  
  if (mode=="add") {
    txt = paste0(getInputValue("gxPrefEditor"),"\n",yaml)
    updateAceEditor(app$session,"gxPrefEditor", value=txt)

  } else {
    try(writeClipboard(yaml))
  }
}  

gx.import.yaml.pref = function(app=getApp(), gx=app$gx,yaml,...) {
  pref = try(read.yaml(text = yaml))
  if (is(pref,"try-error")) {
    return(pref)
  }

  pref.md5 = digest(pref)

  gx$pref.names = names(pref)
  gx$pref.yaml = yaml
  gx$prefs.li = pref
  
  return(TRUE)
  
  writeLines(yaml,paste0(gx$project.dir,"/",gx$pref.yaml.file))
  set.gx.sol.pref.set.ui()
  
  createAlert(app$session, "prefAlert", title = "preferences saved",
        content = "", style = "info", append = FALSE)
  
}

gx.yaml.pref.load = function(app=getApp(), gx=app$gx, file = gx$pref.yaml.file,dir = gx$project.dir,...) {
  
  restore.point("gx.yaml.pref.load")
  
  long.file = paste0(dir,"/",file)
  if (!file.exists(long.file))
    return(NULL)
  
  yaml = paste0(readLines(long.file), collapse="\n")
  
  ret = gx.import.yaml.pref(yaml = yaml)
  if (is(ret,"try-error")) {
    return(ret)
  }
  updateAceEditor(app$session,"gxPrefYaml", value=yaml)
  set.gx.sol.pref.set.ui()
  return(TRUE)
}


gx.yaml.pref.save = function(app=getApp(), gx=app$gx,...) {
  yaml = getInputValue("gxPrefEditor")
  restore.point("gx.save.preferences")
  ret = gx.import.yaml.pref(yaml = yaml)
  if (is(ret,"try-error")) {
    title="Error when parsing preferences:"
    msg = paste0("", as.character(ret))
    createAlert(app$session, "prefAlert", title = title,
        content = msg, style = "error", append = FALSE)
    return(FALSE)
  }
  writeLines(yaml,paste0(gx$project.dir,"/",gx$pref.yaml.file))
  set.gx.sol.pref.set.ui()
  createAlert(app$session, "prefAlert", title = "preferences saved",
        content = "", style = "info", append = FALSE)
  
}

make.prefs.example.yaml = function(pref.types =  types$prefType$subTypes, types=get.types() ) {
  restore.point("make.default.prefs.yaml")
  
  #pname = pref.types[[2]]
  li = lapply(pref.types, function(pname) {
    pt = get.type(typeName=pname)
    if (!is.null(pt$example)) return(as.character(pt$example))
    class = str.left.of(pname,"Util")
    name = pt$defaultName
    if (is.null(name)) name = class
    params = lapply(pt$params, function(par) {
      paste0(quote.char(par$default,"'"), " # ", par$descr)
    })
    names(params) = names(pt$params)
    pref = c(list(class=class, player = "_all", prob = 1),params)
    pref
    
    paste0(name,":\n","\t",name,":\n",
           paste0("\t\t",names(pref),": ", pref,collapse="\n"))
    
  }) 
  yaml = paste0(li, collapse="\n\n")
  #cat(yaml)
  yaml
}


prefs.to.util.fun = function(prefs, efg) {
  restore.point("pref.to.util.funs")
  oco.df = efg$oco.df
  n = efg$n

  pname = names(prefs)[1]
  li = lapply(names(prefs), function(pname) {
    p = prefs[[pname]]
    fun.name = paste0(p$class,"Util")
    ptype = get.type(typeName=fun.name)
    
    par.names = names(ptype$params)
    params = p[par.names]
     
    
    # need to adapt
    is.formula = sapply(par.names, function(par) {
      !is.character(ptype$params[[par]]$default) & is.character(params[[par]])
    })
  
    if (length(is.formula)>0) {
      # parse formulas  
      params[is.formula] = lapply(params[is.formula], function(par){
        parse.as.call(text=par)
      })
    }
    
    # loop through all players and generate parameters
    i = 1
    li = lapply(1:n, function(i) {
      if (!(p$player == "_all" | i %in% p$player)) return(NULL)
      par.vals = params
      if (length(is.formula)>0) {
        par.vals[is.formula] = lapply(par.vals[is.formula], function(par){
          new.par = substitute.call(par,
            list(payoff_i = as.name(paste0("payoff_",i)))
          )
          eval(new.par, oco.df)
        })
      }      
      if (length(params)>0) {
        # need to adapt to vector sized parameters
        args.str = paste0(",",paste0(par.names,"=",quote.char(par.vals),collapse=","))
      } else {
        args.str = ""
      }
      util.call = paste0(fun.name,"(",i,args.str,")")
      names(util.call) = pname
      dt = data_frame(i=i, pref=pname, class=p$class, prob=p$prob, util.call = util.call)
      dt
    })
    dt = rbindlist(li)
    dt
  })
  uf.df = as_data_frame(rbindlist(li))
  uf.df
}


get.pref.auto.name = function(pref) {
  type = get.type(typeName=paste0(pref$class,"Util"))
  name = as.character(pref$name)
  if (! (length(name)==0 | identical(name,"_auto")) )
    return(name)
  
  pars = names(type$params)
  if (length(pars)>0) {
    par.vals = sapply(pars, function(par) {
      val = pref[[par]]
      if (is.character(val)) 
        val = str.trim(str.right.of(val,"|"))
      if (length(val)>1)
        val = paste0(val,collapse="_")
      val
    })
    par.txt = paste0("_", paste0(pars,par.vals,collapse="_"))  
  }
  name = paste0(pref$class,par.txt)
  name
}
