var xecon = {

struc: {},
nodeCounter: 0,
games: {},
copiedNode: null,
duplCounter: 0,

init: function(struc) {
  xecon.struc = struc;
  xecon.initGameTreeMenus();
},

initGame: function(gameId, content) {
  var treeId =  "xsGameTree_"+gameId;
  var varparId =  "xsVarPar_"+gameId;
  var game = {gameId: gameId, treeId: treeId, varparId:varparId, content: content};
  
  var wasLoaded = xecon.games.hasOwnProperty(gameId);
  
  xecon.games[gameId] = game;
  
  if (!wasLoaded) {
  	xecon.initGameTree(game);
  } else {
  	//alert("Reload game " + gameId);
  	xecon.reloadGameTree(game);
  }
  //$(function(){xecon.initGameVarPar(game)});
},

initGameVarPar: function(game) {
  var data = game.content.game.varpar;
  data[0][0] = "variants↓ params→";

	
  var opts = {
  	data: data,
    width: "100%",
    height: "200",
    contextMenu: true,
    allowInsertColumn: true,
    allowDeleteColumn: true,
    allowInsertRow: true,
    fixedColumnsLeft: 1,
    fixedRowsTop: 2,
    
    manualColumnResize: true,
    manualColumnMove: true,
    /*
    manualRowResize: true,
    manualColumnMove: true,
    manualRowMove: true,    
    */
    cell: [
      {row: 0, col: 0, readOnly: true}
    ]
  };
  
  var hot = new Handsontable(document.getElementById(game.varparId),opts);
  //hot.loadData(data);
  game.varparHot = hot;
},

parseAndSendGame: function(gameId, mode) {
  var game = xecon.games[gameId];
  var root = $("#"+game.treeId).fancytree("getRootNode");
  var content = xecon.parseRootContent(root);
  Shiny.onInputChange("parseGameEvent", {eventId:"parseGameEvent",id: "parseGameEvent", value: content, json: JSON.stringify(content,null,4), gameId: gameId, mode:mode, nonce: Math.random()});
},

parseAndSendNode: function(node,xs) {
  var content = xecon.parseNodeContent(node,1);
  Shiny.onInputChange("parseNodeEvent", {eventId:"parseNodeEvent",id: "parseNodeEvent", value: content,nonce: Math.random()});

},

parseSpecialNodeContent: function(node, level) {
  var xs = node.data.xs;
  if (xs.nodeType === "varpar") {
    var game = xecon.games[xs.info.gameId];
    var data = null;
    if (typeof game.varparHot==='undefined') {
      data = game.content.game.varpar;
    } else {
      data = game.varparHot.getData();
    }
    return(data);
  }
  return null;
},


parseRootContent: function(root) {
  var content = {};
  var nodes = root.children;
  for (var j=0; j<nodes.length; j++) {
    var cc = xecon.parseNodeContent(nodes[j],1);
    var name = nodes[j].data.xs.fieldName;
    content[name] = cc;
  }
  return content;  
},

parseNodeContent: function(node, level) {
  var xs = node.data.xs;

  if (level>10) {
    alert("parseNodeContent nested too deeply!");
    return(null);
  }

  console.log("parseNodeContent" + xs.title);

  if (xs.special === true) {
    return(xecon.parseSpecialNodeContent(node, level));
  }


  if (xs.isAtom) {
    var value = $("#"+xs.inputId).val();
    if (typeof value === 'undefined') value = xs.value;
    return(value);
  } else if (xs.isList) {
    var cn = node.getChildren();
    var cont = [];
    if (!isArray(cn)) return(cont);
    for (var i = 0; i < cn.length; i++) {
      cont.push(xecon.parseNodeContent(cn[i], level+1));
    }
    return(cont);
  // node with fields
  } else {
    var cn = node.getChildren();
    var cont = {};
    if (!isArray(cn)) return(cont);
    for (var i = 0; i < cn.length; i++) {
      var cxs = cn[i].data.xs;
      cont[cxs.fieldName] = xecon.parseNodeContent(cn[i], level+1);
    }
    return(cont);
  }
},

gameContextMenuClick: function(key,opt,nodeType) {
  var node = $.ui.fancytree.getNode(opt.$trigger[0]);
  var xs = node.data.xs;
  xecon.runGameTreeCommand(key,node,xs);
},

runGameTreeCommand: function(cmd, node,xs) {
  if (cmd=="add") {
    if (!xs.isList) {
      xecon.addNode(node,xs,"after");
    } else {
      xecon.addListChildNode(node,xs,"child");
    }
  } else if (cmd=="addBefore") {
    xecon.addNode(node,xs,"before");
  } else if (cmd=="delete") {
    node.remove();
  } else if (cmd=="parse") {
    xecon.parseAndSendNode(node,xs);
  } else if (cmd=="copy") {
  	xecon.copyNode(node,xs);
  	return;
  } else if (cmd=="duplicate") {
  	xecon.duplicateNode(node,xs);
  	return;
  } else if (cmd=="paste") {
  	xecon.pasteNode(node,xs);
  }
  xecon.initGameTreeMenus();
},

copyNode: function(node,xs) {
	//cnode = JSON.parse(JSON.stringify(node));
	xecon.copiedNode =  {
		nodeData: node.toDict(true),
		nodeType: xs.nodeType
	};
},

pasteNode: function(node,xs) {
	//var cnode = xs.copiedNode.node;
	var cnode = xecon.copiedNode.nodeData;
	
	if (cnode === null) {
		alert("No node has been copied.");
		return;
	}
	var ntype = xs.nodeType;
	var ctype = xecon.copiedNode.nodeType;
	
	var mode = "fail";
	if (ctype == "stage") {
		if (ntype == "stage") mode = "after";
		if (ntype == "stages") mode = "in";
	} else if (ctype == "action") {
		if (ntype == "action") mode = "after";
		if (ntype == "actions") mode = "in";
	} else if (ctype == "randomVar") {
		if (ntype == "randomVar") mode = "after";
		if (ntype == "randomVars") mode = "in";
	} else if (ctype == "computation") {
		if (ntype == "computation") mode = "after";
		if (ntype == "computations") mode = "in";
	}
	
	if (mode === "fail") {
		alert("Cannot paste node of type "+ ctype + " into / after node of type "+ntype+".");
		return;
	} else if (mode === "after") {
		node.addNode(cnode,"after");
		//cnode.copyTo(node, "after");
	} else if (mode === "in") {
		node.addNode(cnode,"child");
		//cnode.copyTo(node,"child");
	}
},

duplicateNode: function(node,xs) {
	var pn = node.getParent();
	nodeType = xs.nodeType;
	nodeData = node.toDict(true);

	var content = xecon.parseNodeContent(node,1);
	//parseNode: function(level, parentKey, type, content, field, info)
  var nn = xecon.parseNode(1,xs.parentKey,xs.nodeType,content,xs.field, xs.info);
  nn.expanded = true;
  node.addNode(nn,"after");
	return;
	
	// old code does not work
	// adapt inputId and keys
	xecon.duplCounter = xecon.duplCounter+1; 
	oldid = nodeData.key;
	newid = oldid+"_dup"+xecon.duplCounter;
	nodeData = xecon.changeNodeDataInputId(nodeData, oldid,newid);
	node.addNode(nodeData,"after");
},

changeNodeDataInputId: function(nodeData, oldid, newid) {
	// 
	var nd = nodeData;
	
	if (typeof(nd.key) !== 'undefined') {
		nd.key = nd.key.replace(oldid, newid);
		nd.data.xs.inputId = nd.data.xs.inputId.replace(oldid, newid);

		if (typeof(nd.children) !== 'undefined') {
			for (var i = 0; i < nd.children.length; i++) {
		    nd.children[i] = xecon.changeNodeDataInputId(nd.children[i],oldid, newid);
			}
		}
	}
	return nd;
},


addNode: function(node, xs, mode) {
  var pn = node.getParent();
  var nn = xecon.parseNode(1,xs.parentKey,xs.nodeType,{},{}, xs.info);
  nn.expanded = true;
  node.addNode(nn,mode);
},

addListChildNode: function(node, xs, mode) {
  var pn = node.getParent();
  var nn = xecon.parseNode(1,node.key,xs.childType,{},{},xs.info);
  nn.expanded = true;
  node.addNode(nn,mode);
},


initGameTree: function(game) {
  var struc = xecon.struc;
  var id = game.treeId;
  var content = game.content;
  var tree = {
    extensions: ["dnd", "table", "gridnav"],
    table: {nodeColumnIdx: 0, indentation: 16},
    // maybe delete gridnav options
    gridnav: {autofocusInput: false, handleCursorKeys: true},
    
    
    click: function(e, data) {
        var nodeId = data.node.key;
        Shiny.onInputChange("click", {eventId: "click", id: id, objType: "fancytree_node", nodeId: nodeId, data: data.node.data, nonce: Math.random()});
    },
    renderColumns: function(e, data){
        var node = data.node,
            cols = $(node.tr).find(">td");
        
        var xs = node.data.xs;    
        var html = "";
        
        if (xs.nodeType === "varparTable") {
          var game = xecon.games[xs.info.gameId];
          //alert("render varpar");
          
          //html = '<div width="100%" height="300" style="resize: both; overflow: auto;"><div id="'+game.varparId+'"></div></div>';
          html = '<div id="'+game.varparId+'"></div>'+
            '<div class="gameTreeMsg_'+game.gameId+'" id="'+xs.inputId+'__Msg" style="margin:0, padding:0, color: #aa0000;"></div>';          
          cols.eq(1).html(html);
          xecon.initGameVarPar(game);
          return;
        }
        
        if (xs.inputType === "text") {
          value = as_atom(xs.value);
          html = '<div class="xs_input_div"> <input class="xs_input" type="text" name ="'+xs.inputId+'" id ="'+xs.inputId+'" value="'+value+'"></div>';
        } else if (xs.inputType === "textArea") {
          value = String(as_atom(xs.value));
          var valueRows = value.split(/\r\n|\r|\n/).length;
          var rows=1;
          if (valueRows > rows) rows=valueRows;
          
          html = '<div class="xs_input_div"> <textarea class="xs_input" id ="'+xs.inputId+'" rows="'+rows+'">'+value+'</textarea></div>';
        }
        if (typeof xs.inputId !== 'undefined') {
          html = html + '<div class="gameTreeMsg_'+xs.info.gameId+'" id="'+xs.inputId+'__Msg" style="margin:0, padding:0, color: #aa0000;"></div>';
        }
        
        cols.eq(1).html(html);
        
    },
    dnd: {
        autoExpandMS: 99999,
        draggable: { // modify default jQuery draggable options
          zIndex: 1000,
          scroll: true,
          containment: "parent",
          revert: "invalid"
        },
        preventRecursiveMoves: true, // Prevent dropping nodes on own descendants
        preventVoidMoves: true, // Prevent dropping nodes 'before self', etc.
    
        dragStart: function(node, data) {
          // This function MUST be defined to enable dragging for the tree.
          // Return false to cancel dragging of node.
    //    if( data.originalEvent.shiftKey ) ...          
    //    if( node.isFolder() ) { return false; }
          return true;
        },
        dragEnter: function(node, data) {
          /* data.otherNode may be null for non-fancytree droppables.
           * Return false to disallow dropping on node. In this case
           * dragOver and dragLeave are not called.
           * Return 'over', 'before, or 'after' to force a hitMode.
           * Return ['before', 'after'] to restrict available hitModes.
           * Any other return value will calc the hitMode from the cursor position.
           */
          // Prevent dropping a parent below another parent (only sort
          // nodes under the same parent):
          if(node.parent !== data.otherNode.parent){
            return false;
          }
          // Don't allow dropping *over* a node (would create a child). Just
          // allow changing the order:
          return ["before", "after"];
          // Accept everything:
          return true;
        },
        dragExpand: function(node, data) {
          // return false to prevent auto-expanding parents on hover
        },
        dragOver: function(node, data) {
        },
        dragLeave: function(node, data) {
        },
        dragStop: function(node, data) {
        },
        dragDrop: function(node, data) {
          // This function MUST be defined to enable dropping of items on the tree.
          // data.hitMode is 'before', 'after', or 'over'.
          // We could for example move the source to the new target:
          data.otherNode.moveTo(node, data.hitMode);
        }
      }

  };
  
  var nodes = xecon.parseNodeFields(1,"game","game",content.game,{gameId: game.gameId});
  tree.source = nodes;
  $("#"+id).fancytree(tree);

  //initGameTreeMenus();

},

reloadGameTree: function(game) {
  var struc = xecon.struc;
  var id = game.treeId;
  var content = game.content;

  var nodes = xecon.parseNodeFields(1,"game","game",content.game,{gameId: game.gameId});
	//debugger;
	var tree = $("#"+id).fancytree("getTree");  
  tree.reload(nodes);

  //initGameTreeMenus();

},


getAtomNodeInputType: function(st, field, type) {
  if (typeof st !== 'undefined') {
    if (typeof st.inputType !== 'undefined') {
      return(st.inputType);
    }
  }
  if (typeof field !== 'undefined') {
    if (typeof field.inputType !== 'undefined') {
      return(field.inputType);
    }
    if (isArray(field.set)) {
      return("select");
    }
  }
  if (type === "numeric") {
    return("numeric");
  }
  return("text");
},

parseSpecialNode: function(level, parentKey, type, content, field, info) {
  if (type==="varpar") {
    var title= "Variants, Params";
    var varparId =  "xsVarPar_"+info.gameId;
    var key= info.gameId+"_varParNode";
    var xs = {title: title, special: true, isList: true, isAtom: false, nodeType: type, info: info, fieldName: "varpar"};
    
    var childxs = {special: true, nodeType: "varparTable", info: info,fieldName: "varparTable", inputId: varparId};
    var childnode = {key: info.gameId+"_varParTableNode", title: "", folder: false, extraClasses: "gameNodeType_"+type, children: null, xs: childxs, icon: ""};
    
    var node = {key: key, title: title, folder: true, expanded: false, extraClasses: "gameNodeType_"+type, children: [childnode], xs: xs};
    return(node);
  }
},

parseNode: function(level, parentKey, type, content, field, info) {
  if (level>20) throw("parseNode is nested too deeply!");

  var st = xecon.struc[type];
  if (typeof field === "undefined") field = {};
  if (typeof st === "undefined") st = {};
  
  var title = with_default(field.name,type)+" "+with_default(content.name,"");
  xecon.nodeCounter++;
  var key = parentKey + "_" + type + xecon.nodeCounter;
  
  if (field.special === true) {
    return(xecon.parseSpecialNode(level, parentKey, type, content, field, info));
  }
  var isList = as_atom(field.isList,false);
  var isAtom = false;
  if (!isList) {
    if (typeof st === 'undefined') {
      isAtom = true;
    } else if (!isArray(st.fields)) {
      isAtom = true;
    }
  }

  var inputId = "input__" + key;
  if (isAtom) {
    inputType = xecon.getAtomNodeInputType(st, field, type);
    var choices = with_default(field.set, null);
    var xs = {title: title, fieldName: field.name, isList: false, isAtom: true, nodeType: type, inputType: inputType, inputId: inputId, choices: choices, value: content, parentKey: key, info: info, field: field};
    var node = {key: key, title: title, folder: false, extraClasses: "gameNodeType_"+type, children: null, xs: xs};
    return(node);
  }
  var expanded = st.expanded === true | field.expanded === true;
  
  if (isList) {
    folder = true;
    children = [];
    var childType = field.childType;
    
    if (isArray(content)) {
      for (var j=0; j<content.length; j++) {
        var child = xecon.parseNode(level+1, key, childType,content[j],{}, info);
        children.push(child);
      }
    }
    var xs = {title: title, fieldName: field.name, isList: true, isAtom: false, nodeType: type, inputType: "none", inputId: inputId, parentKey: key, value: null, childType: childType, info: info, field: field};
    var node = {key: key, title: title, folder: true, expanded: expanded, extraClasses: "gameNodeType_"+type, children: children, xs: xs};
    return(node);
  }
  
  // Node with fields
  var children = xecon.parseNodeFields(level+1,key,type, content, info);
  var xs = {title: title, fieldName: field.name,isList: false,isAtom: false, nodeType: type, inputType: "none", inputId: inputId, parentKey: key, value: null,info: info, field: field};
  var node = {key: key, title: title, folder: true, extraClasses: "gameNodeType_"+type, children: children, expanded: expanded, xs: xs};
  return(node);
},

parseNodeFields: function(level,parentKey, type, content, info) {
  var struc = xecon.struc;
  var st = struc[type];
  var fields = st.fields;
  var nodes = [];
  
  for (var i=0; i<fields.length; i++) {
    var field = fields[i];
    var name = as_atom(field.name);
    var fieldContent = with_default(content[name],"");
    var fieldType = as_atom(field.type, name);
    var node = xecon.parseNode(level+1, parentKey, fieldType, fieldContent,field, info);
    nodes.push(node);
  }
  return nodes;
},

initGameTreeMenus: function() {
  $(function() {
    $.contextMenu({
      selector: '.gameNodeType_stages span.fancytree-title', 
      callback: function(key, opt) {
        xecon.gameContextMenuClick(key,opt,"stages");
      },
      items: {
        "parse": {name: "Parse Stages"},
        "add": {name: "Add Stage", icon: "edit"},
        "paste": {name: "Paste stage at end", icon: "paste"},
        "sep1": "---",
        "template": {
          "name": "Stage Templates", 
        }
      }
    });


    $.contextMenu({
      selector: '.gameNodeType_stage span.fancytree-title', 
      callback: function(key, opt) {
        xecon.gameContextMenuClick(key,opt,"stage");
      },
      items: {
        "parse": {name: "Parse Stage"},
        "addBefore": {name: "Add Stage Before", icon: "edit"},
        "add": {name: "Add Stage After", icon: "edit"},
        "duplicate": {name: "Duplicate Stage", icon: "copy"},
        /*
        "copy": {name: "Copy Stage", icon: "copy"},
        "paste": {name: "Paste Stage Below", icon: "paste"},
        */
        "delete": {name: "Delete Stage", icon: "delete"},
        "sep1": "---",
        "template": {
          "name": "Stage Templates", 
        }
      }
    });


    $.contextMenu({
      selector: '.gameNodeType_actions span.fancytree-title', 
      callback: function(key, opt) {
        xecon.gameContextMenuClick(key,opt,"actions");
      },
      items: {
        "parse": {name: "Parse Actions"},
        "add": {name: "Add Action", icon: "edit"},
        /*
        "copy": {name: "Copy All Actions", icon: "copy"},
        "paste": {name: "Paste", icon: "paste"},
        */
      }
    });

    $.contextMenu({
      selector: '.gameNodeType_action span.fancytree-title', 
      callback: function(key, opt) {
        xecon.gameContextMenuClick(key,opt,"action");
      },
      items: {
        "parse": {name: "Parse Action"},
        "add": {name: "Add Action", icon: "edit"},
        "duplicate": {name: "Duplicate", icon: "copy"},
        /*
        "copy": {name: "Copy", icon: "copy"},
        "paste": {name: "Paste", icon: "paste"},
        */
        "delete": {name: "Delete", icon: "delete"},
        "sep1": "---",
        "template": {
          "name": "Action Templates", 
          "items": {
              "acceptAction": {"name": "accept"}
          }
        }
      }
    });

    $.contextMenu({
      selector: '.gameNodeType_nature span.fancytree-title', 
      callback: function(key, opt) {
        xecon.gameContextMenuClick(key,opt,"nature");
      },
      items: {
        "add": {name: "Add Move of Nature", icon: "edit"},
        /*
        "copy": {name: "Copy All Moves of Natures", icon: "copy"},
        "paste": {name: "Paste", icon: "paste"},
        */
      }
    });

    $.contextMenu({
      selector: '.gameNodeType_randomVar span.fancytree-title', 
      callback: function(key, opt) {
        xecon.gameContextMenuClick(key,opt,"randomVar");
      },
      items: {
        "add": {name: "Add Move of Nature", icon: "edit"},
        "duplicate": {name: "Duplicate", icon: "copy"},
        /*
        "copy": {name: "Copy", icon: "copy"},
        "paste": {name: "Paste", icon: "paste"},
        */
        "delete": {name: "Delete", icon: "delete"},
        "sep1": "---",
        "template": {
          "name": "Templates", 
          "items": {
          }
        }
      }
    });

   $.contextMenu({
      selector: '.gameNodeType_compute span.fancytree-title', 
      callback: function(key, opt) {
        xecon.gameContextMenuClick(key,opt,"compute");
      },
      items: {
        "add": {name: "Add Computation", icon: "edit"},
        /*
        "copy": {name: "Copy All Computations", icon: "copy"},
        "paste": {name: "Paste", icon: "paste"},
        */
        "sep1": "---",
        "addPayoffs": {name: "Add Payoffs", icon: "edit"}
      }
    });
 
    
    $.contextMenu({
      selector: '.gameNodeType_transformation span.fancytree-title', 
      callback: function(key, opt) {
        xecon.gameContextMenuClick(key,opt,"action");
      },
      items: {
        "add": {name: "Add Computation", icon: "edit"},
        "duplicate": {name: "Duplicate", icon: "copy"},
        /*
        "copy": {name: "Copy", icon: "copy"},
        "paste": {name: "Paste", icon: "paste"},
        */
        "delete": {name: "Delete", icon: "delete"},
        "sep1": "---",
        "addPayoffs": {name: "Add Payoffs", icon: "edit"}
      }
    });
    
    
  });
    
  return;

},

clearGameTreeErrors: function(gameId) {
  $(".gameTreeMsg_"+gameId).html("");
},

// show errors messages that R determined when passing game tree
showGameTreeErrors: function(gameId,log) {
  //alert(JSON.stringify(log))
  var game = xecon.games[gameId];
  var root = $("#"+game.treeId).fancytree("getRootNode");
  // clear old messages
  $(".gameTreeMsg_"+gameId).html("");

  for (key in log) {
    var keyVec = key.split(",");
    var node = xecon.findNodeByKeyVec(root,keyVec,true);
    var msgId = node.data.xs.inputId + "__Msg";
    $("#"+msgId).html('<p style="color: #a00;">'+log[key]+'</p>');
  }
  
},

findNodeByKeyVec: function(node, keys, expand) {
  if (expand === true) node.setExpanded(true);
  if (keys.length==0) return(node);
  var key = keys.shift();
  
  var numKey = parseInt(key);
  var child;
  if (!isNaN(numKey)) {
    child = node.children[numKey-1];
  } else {
    child = xecon.findChildNodeByFieldName(node, key);
  }
  return xecon.findNodeByKeyVec(child,keys, expand);
},

findChildNodeByFieldName: function(node, fieldName) {
  var children = node.children;
  for (var i = 0; i< children.length; i++) {
    var child = children[i];
    if (child.data.xs.fieldName === fieldName) return(child);
  }
  return(null);
}

}; // end xecon




function as_atom(value, defaultVal) {
  if (typeof value === 'undefined') return(defaultVal);
  if (value === null) return(null);
  if (value.constructor === Array) return(value[0]);
  return(value);
}

function with_default(value, defaultVal) {
  if (typeof value === 'undefined') return(defaultVal);
  return(value);
}

function isArray(value) {
  if (typeof value === 'undefined') return(false);
  if (value === null) return(false);
  if (value.constructor === Array) return(true);
  return false;
}
