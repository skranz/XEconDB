form_UltimatumGame_offerStage = function(values, sfg, stage, observe, player, var, ...) {
  
  
  
}

offerStage:
  formType: "defaultForm" 
  title: "Offer Stage"
  explain: "You are a proposer."
  actions:
    offer:
      label: "Please choose your offer."
      setLabel:
  
acceptStage:
  formType: "defaultForm" 
  title: "Accept Stage"
  explain: "You are a responder."
  observe: >
    The proposer 
  actions:
    accept:
      label: "Do you want to accept or reject the offer?"
      setLabel:
        - [1, "accept"]
        - [0, "reject"]
