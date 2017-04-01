var xep = {

struc: {},
nodeCounter: 0,
projs: {},

init: function(struc) {
  xep.struc = struc;
  xep.initProjTreeMenus();
},

initProj: function(projId, content) {
  var treeId =  "xsProjTree_"+projId;
  var varparId =  "xsVarPar_"+projId;
  var proj = {projId: projId, treeId: treeId, content: content};  
  xep.projs[projId] = proj;
  xep.initProjTree(proj);
},


projContextMenuClick: function(key,opt,nodeType) {
  var node = $.ui.fancytree.getNode(opt.$trigger[0]);
  var xs = node.data.xs;
  xep.runProjTreeCommand(key,node,xs);
},

runProjTreeCommand: function(cmd, node,xs) {
  if (cmd=="add") {
    if (!xs.isList) {
      xep.addNode(node,xs,"after");
    } else {
      xep.addListChildNode(node,xs,"child");
    }
  } else if (cmd=="addBefore") {
    xep.addNode(node,xs,"before");
  } else if (cmd=="delete") {
    node.remove();
  } else if (cmd=="parse") {
    xep.parseAndSendNode(node,xs);
  }
  xep.initProjTreeMenus();
},


initProjTree: function(proj) {
  var struc = xep.struc;
  var id = proj.treeId;
  var content = proj.content;
  var tree = {
    extensions: ["dnd"],
    table: {indentation: 16},

    click: function(e, data) {
        var nodeId = data.node.key;
        Shiny.onInputChange("click", {eventId: "click", id: id, objType: "fancytree_node", nodeId: nodeId, data: data.node.data, nonce: Math.random()});
    }  
  };
  
  var nodes = xep.parseNodeFields(1,"proj","proj",content.proj,{projId: proj.projId});
  tree.source = nodes;
  $("#"+id).fancytree(tree);

  //initProjTreeMenus();

},

initProjTreeMenus: function() {
  $(function() {
    $.contextMenu({
      selector: '.projNodeType_stages span.fancytree-title', 
      callback: function(key, opt) {
        xep.projContextMenuClick(key,opt,"stages");
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
      selector: '.projNodeType_stage span.fancytree-title', 
      callback: function(key, opt) {
        xep.projContextMenuClick(key,opt,"stage");
      },
      items: {
        "parse": {name: "Parse Stage"},
        "addBefore": {name: "Add Stage Before", icon: "edit"},
        "add": {name: "Add Stage After", icon: "edit"},
        "copy": {name: "Copy Stage", icon: "copy"},
        "paste": {name: "Paste Stage Below", icon: "paste"},
        "delete": {name: "Delete Stage", icon: "delete"},
        "sep1": "---",
        "template": {
          "name": "Stage Templates", 
        }
      }
    });


    $.contextMenu({
      selector: '.projNodeType_actions span.fancytree-title', 
      callback: function(key, opt) {
        xep.projContextMenuClick(key,opt,"actions");
      },
      items: {
        "parse": {name: "Parse Actions"},
        "add": {name: "Add Action", icon: "edit"},
        "copy": {name: "Copy All Actions", icon: "copy"},
        "paste": {name: "Paste", icon: "paste"},
      }
    });

    $.contextMenu({
      selector: '.projNodeType_action span.fancytree-title', 
      callback: function(key, opt) {
        xep.projContextMenuClick(key,opt,"action");
      },
      items: {
        "parse": {name: "Parse Action"},
        "add": {name: "Add Action", icon: "edit"},
        "copy": {name: "Copy", icon: "copy"},
        "paste": {name: "Paste", icon: "paste"},
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
      selector: '.projNodeType_nature span.fancytree-title', 
      callback: function(key, opt) {
        xep.projContextMenuClick(key,opt,"nature");
      },
      items: {
        "add": {name: "Add Move of Nature", icon: "edit"},
        "copy": {name: "Copy All Moves of Natures", icon: "copy"},
        "paste": {name: "Paste", icon: "paste"},
      }
    });

    $.contextMenu({
      selector: '.projNodeType_randomVar span.fancytree-title', 
      callback: function(key, opt) {
        xep.projContextMenuClick(key,opt,"randomVar");
      },
      items: {
        "add": {name: "Add Move of Nature", icon: "edit"},
        "copy": {name: "Copy", icon: "copy"},
        "paste": {name: "Paste", icon: "paste"},
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
      selector: '.projNodeType_compute span.fancytree-title', 
      callback: function(key, opt) {
        xep.projContextMenuClick(key,opt,"compute");
      },
      items: {
        "add": {name: "Add Computation", icon: "edit"},
        "copy": {name: "Copy All Computations", icon: "copy"},
        "paste": {name: "Paste", icon: "paste"},
        "sep1": "---",
        "addPayoffs": {name: "Add Payoffs", icon: "edit"}
      }
    });
 
    
    $.contextMenu({
      selector: '.projNodeType_transformation span.fancytree-title', 
      callback: function(key, opt) {
        xep.projContextMenuClick(key,opt,"action");
      },
      items: {
        "add": {name: "Add Computation", icon: "edit"},
        "copy": {name: "Copy", icon: "copy"},
        "paste": {name: "Paste", icon: "paste"},
        "delete": {name: "Delete", icon: "delete"},
        "sep1": "---",
        "addPayoffs": {name: "Add Payoffs", icon: "edit"}
      }
    });
    
    
  });
    
  return;

},

// show errors messages that R determined when passing proj tree
showProjTreeErrors: function(projId,log) {
  //alert(JSON.stringify(log))
  var proj = xep.projs[projId];
  var root = $("#"+proj.treeId).fancytree("getRootNode");
  // clear old messages
  $(".projTreeMsg_"+projId).html("");

  for (key in log) {
    var keyVec = key.split(",");
    var node = xep.findNodeByKeyVec(root,keyVec,true);
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
    child = xep.findChildNodeByFieldName(node, key);
  }
  return xep.findNodeByKeyVec(child,keys, expand);
},

findChildNodeByFieldName: function(node, fieldName) {
  var children = node.children;
  for (var i = 0; i< children.length; i++) {
    var child = children[i];
    if (child.data.xs.fieldName === fieldName) return(child);
  }
  return(null);
}

}; // end xep


