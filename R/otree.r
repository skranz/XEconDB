# Transform a variant R game (vg) to 
# otree python code

example.vg2otree = function() {
  setwd("D:/libraries/XEconDB/projects/UltimatumGame/games")
  txt = readLines("GiftExchange.json")
  jg = fromJSON(txt,simplifyDataFrame = FALSE,simplifyMatrix = FALSE)$game
  rg = jg.to.rg(jg)
  vg = rg.to.vg(rg,variant=1)

  setwd("D:/libraries/XEconDB/otree")
  vg.to.otree(vg=vg, id="giftex", overwrite.templates = TRUE)
    
  stage = vg$stages[[1]]  
  
  #otree resetdb
  #otree runserver
  #otree startapp your_app_name
  #otree startapp giftexchange_base
  #otree runserver
}

examples.make.otree.dir = function() {
  setwd("D:/libraries/XEconDB/projects/UltimatumGame/")
  make.otree.dir(wait = TRUE)
}

make.otree.dir = function(parent.dir=getwd(), dir="oTree", wait=FALSE, adapt.settings.py = TRUE) {
  wd = getwd()
  setwd(parent.dir)
  com = paste0("otree startproject --noinput ",dir)
  system(com, wait = wait)
  otree.dir = file.path(parent.dir, dir)
  setwd(otree.dir)
    
  txt = readLines("settings.py")
  row = which(str.starts.with(txt,"SESSION_CONFIGS = ["))
  txt[row] = paste0(txt[row],"\n#. START XEcon Games\n\n#. END XEcon Games")
  writeLines(txt, "settings.py")
  cat("\nGenerated otree directory ",otree.dir)
    

  setwd(wd)
}

# insert a vg into settings.py
insert.vg.into.settings.py = function(vg, otree.dir) {
  
  file = file.path(otree.dir, "settings.py")
  txt = readLines(file)
  restore.point("insert.vg.into.settings.py")

  code = paste0(
"#.. START ", vg$id,"
{
'name': '",vg$id,"',
'display_name': '",vg$id,"',
'num_demo_participants': ",vg$params$numPlayers,",
'app_sequence': ['",vg$id,"'],
},
#.. END ", vg$id
)
  # check if the game is already registered and overwrite
  start.line = paste0("#.. START ", vg$id)
  end.line = paste0("#.. END ", vg$id)
  
    
  start.row = which(txt == start.line)
  
  if (length(start.row)>0) {
    end.row = which(txt == end.line)
    txt = txt[-setdiff(start.row:end.row,start.row)]
    insert.row = start.row
    txt[insert.row] = code
  } else {
    insert.row = which(txt == "#. START XEcon Games")[1]
    txt[insert.row] = paste0(txt[insert.row],"\n",code)
  }
  writeLines(txt, file)
  
}

otree.app.skeleton = function(id, otree.dir, wait=TRUE,...) {
	restore.point("otree.app.skeleton")
  wd = getwd()
  setwd(otree.dir)
  com = paste0("otree startapp ",id)
  system(com, wait = wait)
  setwd(wd)
}

otree.resetdb = function(otree.dir, wait=TRUE, intern=TRUE,...) {
  wd = getwd()
  setwd(otree.dir)
  com = "otree resetdb --noinput"
  res = system(com, wait = wait,intern=intern,invisible=FALSE,...)
  setwd(wd)
  res
}

otree.runserver = function(otree.dir, wait=FALSE,...) {
  wd = getwd()
  setwd(otree.dir)
  com = "otree runserver"
  system(com, wait = wait,invisible=FALSE,...)
  setwd(wd)
}


jg.to.otree = function(jg, otree.dir=getwd(), overwrite.templates=FALSE,...) {
  restore.point("jg.to.otree")
  rg = jg.to.rg(jg)
  
  variants = rg$variants
  
  for (variant in variants) {
    vg = rg.to.vg(rg,variant=variant)
    vg.to.otree(vg=vg, otree.dir=otree.dir, overwrite.templates = overwrite.templates,...)
  }
  
}

vg.to.otree = function(vg, otree.dir=getwd(), overwrite.templates=FALSE, id=NULL, create.app.skeleton=TRUE, insert.into.settings.py = TRUE, msg.id=NULL) {
  restore.point("vg.to.otree")
  
	if (is.null(id))
		id = tolower(paste0(vg$gameId,"_", vg$variant))
	
  vg$id = id
  
  if (!dir.exists(file.path(otree.dir,id))) {
    if (create.app.skeleton) {
    	if (!is.null(msg.id))
    	  timedMessage(msg.id,paste0("Export ",vg$id," to otree... call 'otree startapp ",id, "' to create app skeleton"), millis = Inf)
      msg = try(otree.app.skeleton(id=id, otree.dir=otree.dir))
    	if (!is.null(msg.id))
    	  timedMessage(msg.id,paste0("Export ",vg$id," to otree... output of 'otree startapp ",id, "':<br>", paste0(msg, collapse="\n")), millis = Inf)
    }
  }
  
  otree.make.dirs(vg$id, otree.dir)
  setwd(file.path(otree.dir,vg$id))

  # add wait information
  vg$stages = lapply(vg$stages, function(stage) {
  	restore.point("vg.to.otree.inner.loop")
  	if (is.character(stage$observe))
    	stage$observe = setdiff(stage$observe,"")
    stage$otree.wait = length(stage$observe) > 0 | length(stage$nature)>0 | length(stage$compute)>0
    #stage$waitId = paste0("WaitFor",stage$name)
    stage
  })

  if (!is.null(msg.id)) timedMessage(msg.id,paste0("Export  ",vg$id," to otree: write models.py..."), millis = Inf)
    
  models.py = otree.models(vg)
  writeLines(models.py,"models.py")

  if (!is.null(msg.id)) timedMessage(msg.id,paste0("Export  ",vg$id," to otree: write views.py..."), millis = Inf)

  views.py = otree.views(vg)
  writeLines(views.py,"views.py")

  if (!is.null(msg.id)) timedMessage(msg.id,paste0("Export  ",vg$id," to otree: write form templates..."), millis = Inf)
  
  # create templates
  templ.dir = file.path(otree.dir,vg$id,"templates",vg$id)
  
  for (stage in vg$stages) {
    templ = otree.template(stage)
    file = file.path(templ.dir,paste0(stage$name,".html"))
    if (file.exists(file) & !overwrite.templates) {
      file = paste0(file,".new.html")
    }
    writeLines(templ, file)
  }

    
  if (insert.into.settings.py) {
  	if (!is.null(msg.id)) timedMessage(msg.id,paste0("Export ",vg$id," to otree:: adapt settings.py..."), millis = Inf)
    try(insert.vg.into.settings.py(vg=vg, otree.dir=otree.dir))
  }
  
}

otree.make.dirs = function(id, otree.dir = getwd()) {
  dir.create(file.path(otree.dir,id),showWarnings = FALSE)  
  dir.create(file.path(otree.dir,id,"templates"),showWarnings = FALSE)  
  dir.create(file.path(otree.dir,id,"templates",id),showWarnings = FALSE)  
  
}

otree.views = function(vg) {
  restore.point("otree.views")
  
  head = paste0(
'from otree.api import Currency as c, currency_range
from ._builtin import Page, WaitPage
from . import models
from .models import Constants
import random
from xeconfuns import *

class Introduction(Page):
    pass

  ')
  pages = lapply(vg$stages, otree.page, vg=vg)
  pages = paste0(pages, collapse = "\n")
  
  stages = get.names(vg$stages)
  
  page_seq = unlist(lapply(vg$stages, function(stage) {
    if(isTRUE(stage$otree.wait)) {
      return(c("WaitPage",stage$name))
    }
    stage$name
  }))
  footer = paste0('
page_sequence = [\n',paste0("\t",page_seq, collapse=",\n"),'
]
  ')
  
  code = paste0(head, pages, footer)
  code
}

otree.page = function(stage,vg) {
  restore.point("otree.page")
  #if (stage$name == "delegationChoicePlayer") stop()
	
  st = stage
  
  
  actions = get.names(st$actions)
  
  nature.code = ""
  if (length(st$nature)>0) {
    nature.code = lapply(st$nature,otree.nature)
    
    nature.code = sc("\t\t\t",nature.code,collapse="\n")
    nature.code = sc("\n\t\t\t# moves of nature = random variables\n", nature.code,"\n")
  }

  
  compute.code = ""
  if (length(st$compute)>0) {
    compute.code = lapply(st$compute,otree.compute)
    
    compute.code = sc("\t\t\t",compute.code,collapse="\n")
    compute.code = sc("\n\t\t\t# compute = deterministic transformations\n", compute.code,"\n")
  }
  
  page.actions = sc(lapply(st$actions, otree.page.action), collapse="\n\n")
  
  condition = st$condition
  if (is.call(condition) | is.name(condition)) {
  	condition = r2otree.formula(condition)
  }
  if (nchar(condition)==0) condition="True"
  
  observe = r2otree.set(st$observe)
  
  player = r2otree.set(st$player)
  
  
  code = paste0('
class ',st$name,'(Page):
	"""',st$descr,'
  XEcon specification:',
	sc('\n\tplayer: ', player),
	sc('\n\tcondition: ', condition),
	sc('\n\tobserve: ', observe),
	sc('\n\tnature: ', sc(get.names(st$nature),collapse=", ")),
	sc('\n\tactions: ', sc(get.names(st$actions),collapse=", ")),
	sc('\n\tcompute: ',sc(get.names(st$compute),collapse=", ")),'
	"""
	form_model = models.Group
	form_fields = ',r2py.vec(actions),'

	def is_displayed(self):
		condition = ',condition,' 
		if condition and "',st$name,'" not in self.group.initialized_pages:
			self.group.initialized_pages.append("',st$name,'")', nature.code, compute.code,'
		do_display = condition and self.player.id_in_group in  as_iterable(',player,')
		return do_display
', page.actions,'
  ')
}

otree.models = function(vg) {
  restore.point("otree.models")
  
  n = vg$params$numPlayers
  
  params = vg$params[setdiff(names(vg$params),"numPlayers")]
  params.class = vg$vars.class[names(params)]

  params.def = otree.params.def(params)
  
  vars = setdiff(vg$vars, c(names(params),"variant","numPlayers"))
  vars.class = vg$vars.class[vars]
  
  vars.def = otree.vars.def(vars, classes=vars.class)
  
  payoff.code = paste0('\t\tself.get_player_by_id(',1:n,').payoff = self.payoff_',1:n, collapse="\n")
    
  id = paste0(vg$gameId, "_",vg$variant)
  
head = paste0('
from otree.api import (
  models, widgets, BaseConstants, BaseSubsession, BaseGroup, BasePlayer, Currency as c, currency_range
)
import random

doc = """
Game variant ',id,'
automatically generated by XEconDB.
"""

class Constants(BaseConstants):
	name_in_url = "',id,'"
	players_per_group = ',vg$params$numPlayers,'
	num_rounds = 1
	instructions_template = "',id,'/Instructions.html"

class Subsession(BaseSubsession):
 	pass

class Group(BaseGroup):
	# helper list to keep track which stages
	# are initialized 
	initialized_pages = []  

	# parameters of this game variant
	# they are defined for simplicity in group
',params.def,'
	# define all variables here
',vars.def,'



	# currently only implemented for 2 players
	def set_payoffs(self):
',payoff.code,'


class Player(BasePlayer):
 	pass
')

}

make.tabs = function(tabs) {
  if (is.numeric(tabs)) tabs = paste0(rep("\t",tabs),collapse="")
  tabs
}

otree.vars.def = function(vars,classes=NULL,choices=NULL, tabs=1) {
  restore.point("otree.params.def")
  
  tabs = make.tabs(tabs)
  s = unlist(lapply(seq_along(vars), function(i) {
    field = r.to.django.field(class = classes[[i]],choices=choices[[i]])
    paste0(tabs,vars[i]," = models.",field)
  }))
  paste0(s, collapse="\n")
} 


otree.params.def = function(params, tabs=1) {
  restore.point("otree.params.def")
  
  tabs = make.tabs(tabs)
  s = unlist(lapply(names(params), function(f) {
    field = r.to.django.field(initial=params[[f]])
    paste0(tabs,f," = models.",field)
  }))
  paste0(s, collapse="\n")
} 

r.to.django.field = function(initial=NULL, choices=NULL,class=NULL) {
  restore.point("r.to.django.field")
  
  if (is.null(class)) {
    if (!is.null(choices)) 
    	class = class(choices)
    if (!is.null(initial)) class = class(initial)
  }
  quotes = if (class=="character") '"' else ''
  
  # r booleans are TRUE and FALSE
  # python booleans are True and Talse
  if (class=="logical" & !is.null(initial)) {
  	if (isTRUE(initial)) {
  		initial="True"
  	} else if (isTRUE(!initial)) {
  		initial="False"
  	}
  }
  
  
  field = r.class.to.django.field.name(class)
  code = paste0(field,"(")
  comma = ""
  if (!is.null(initial)) {
    code = paste0(code,comma, "initial=",quotes,initial,quotes)
    comma = ","
  }
  if (!is.null(choices)) {
    code = paste0(code,comma, "choices=",r2py.vec(choices))
    comma = ","
  }
  code = paste0(code,")")
  code
}

r2py.vec = function(x, collapse=", ") {
  quotes = if (is.character(x)) '"' else ''
  paste0("[",paste0(quotes,x,quotes, collapse=collapse),"]")
}

r.class.to.django.field.name = function(class) {
  
  if (class=="integer") return("IntegerField")
  if (class=="numeric") return("FloatField")
  if (class=="character") return("CharField")
  if (class=="factor") return("CharField")
  if (class=="logical") return("BooleanField")
  
  stop(paste0(class, " not yet implemented."))
}

get.names = function(li) {
  if (!is.null(names(li))) return(names(li))
  names = sapply(li, function(el) el$name)
  names
}

otree.page.action = function(action, prefix="self.group.") {
  restore.point("otree.page.action")
  
  # need to generate
  # action_choices or action_min action_max functions
  # need some r2py translation for sequences
  var = action$name
  fs = formula.to.field.specs(action$set)
  code = lapply(names(fs), function(field) {
  	val = fs[[field]]
  	if (field == "choices") {
  		formula = r2otree.set(val)
  	} else {
	  	if (is.character(val)) {
	  		formula = paste0('"',val,'"')
	  	} else {
				formula = r2otree.formula(val)
	  	}
  	}
    paste0('
	def ',var,'_',field,'(self):
		return ',formula,'
    ')
  })
  code = sc(code, collapse = "")
  code
}

formula.to.field.specs = function(call) {
  restore.point("formula.to.field.specs")
  
  #call = quote(0:give)
  if (call[[1]] == ":") {
    min = call[[2]]
    max = call[[3]]
    return(list(min=min,max=max))
  }
  return(list(choices=call))
}


otree.nature = function(nature, prefix="self.group.") {
  restore.point("otree.nature")
  
	
  pyset = r2otree.set(nature$set, prefix=prefix)
  if (!identical(nature$probs,"")) {
    pyprobs = r2otree.set(nature$probs, prefix=prefix)
    pyprobs = paste0(", weights = ", pyprobs)
  } else {
    pyprobs = ""
  }
  
  var = nature$name
  code = paste0('random.choices(',pyset, pyprobs,', k=1)[0]')
  #paste0(prefix,var,".initial = ",code)
  paste0(prefix,var," = ",code)
}


otree.compute = function(compute, prefix="self.group.") {
  restore.point("otree.compute")
  
  pyform = r2otree.formula(compute$formula, prefix=prefix)
  var = compute$name
  #paste0(prefix,var,".initial = ",pyform)
  paste0(prefix,var," = ",pyform)
}

r2otree.set = function(set, prefix=paste0(if(add.self) "self.","group."), add.self=TRUE) {
	if (is.call(set) | is.name(set)) {
		return(r2otree.formula(set, prefix=prefix, add.self=add.self))
	}
	# deal with atoms
	r2py.vec(set)
}

r2otree.formula = function(call,  prefix=paste0(if(add.self) "self.","group."), add.self=TRUE) {
  restore.point("r2otree.formula")
  
	
	if (!is.call(call) & !is.name(call)) {
		if (is.character(call)) return(paste0('"',call,'"'))
		return(call)		
	}
	
	
  #call = quote((cake-give)+back*factor)
  call = r2otree.fun(call)
  
  vars = find.variables(call)
  pvars = r2otree.var(vars,prefix=prefix)
  subst = lapply(as.list(pvars), as.name)
  names(subst)=vars
  
  scall = substitute.call(call, subst)
  py = deparse1(scall)
  py = gsub("^","**",py, fixed=TRUE)
  py = gsub("!","not ",py, fixed=TRUE)
  py = gsub(" & "," and ",py, fixed=TRUE)
  py = gsub("&"," and ",py, fixed=TRUE)
  py = gsub(" | "," or ",py, fixed=TRUE)
  py = gsub("|"," or ",py, fixed=TRUE)
  py
}

r2otree.var = function(var, prefix=paste0(if(add.self) "self.","group."), add.self=TRUE) {
  pvar = paste0(prefix,var)
  pvar
}

# replace common r functions with corresponding
# python functions known by otree
r2otree.fun = function(call) {
  restore.point("r2otree.fun")
  if (is.name(call)) return(call)
  
  fun.name = call[[1]]
  new = NULL
  ccall = call
  for (i in setdiff(1:length(call),1)) {
    ccall[[i]] = r2otree.fun(call[[i]])
  }
  
  if (fun.name == ":") {
    restore.point("r2otree.fun.:")
    substitute(list(range(x,y,1)), list(x=ccall[[2]],y=ccall[[3]]))
  } else if (fun.name == "seq") {
    restore.point("r2otree.fun.seq")
  	# to do need to adapt for different versions of seq
  	# currently assume 3rd argument is by
    substitute(list(range(x,y,z)), list(x=ccall[[2]],y=ccall[[3]],z=ccall[[4]]))
  } else if (fun.name == "c") {
    #restore.point("r2otree.fun.c")
		#ccall[[1]] = as.call("`[`")
		return(ccall)
  } else {
    ccall
  }
}

otree.template = function(stage) {
  restore.point("otree.template")
  st = stage
  obs = setdiff(st$observe,"")

  obs.code = ""
  if (length(obs)>0) {
    obs.code = paste0(obs,": {{group.",obs,"}}", collapse = "\n\n")
    obs.code = paste0('
<h4>Observations</h4>
', obs.code,'
    ')
  }
  
  actions = get.names(st$actions)
  actions.code = ""
  if (length(actions)>0) {
    actions.code = paste0('{% formfield group.',actions,' with label="',actions,':" %}')
    actions.code = paste0('
<h4>Make your choices</h4><p>
', actions.code)
    
  }
  
  
  txt = paste0('
{% extends "global/Page.html" %}
{% load staticfiles otree_tags %}

{% block title %}
Stage: ', st$name,'
<br>
Player: {{player.id_in_group}}
{% endblock %}

{% block content %}
', obs.code,'
', actions.code,'

<p>
{% next_button %}
</p>

{% endblock %}
')
  txt
}