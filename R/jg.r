
# an empty game with given gameId
empty.jg = function(gameId) {
  paste0('
  {"game": {
    "gameId": "', gameId,'",
    "gameInfo": {
        "label": "",
        "tags": "",
        "descr": "",
        "articles": "",
        "variantOf": ""
    },
    "varpar": [
        [
            "variants<U+2193> params<U+2192>",
            "numPlayers",
            "descr"
        ],
        [
            "base",
            "2",
            "The base variant"
        ]
    ],
    "stages": [
       {
            "name": "resultsStage",
            "player": "[1,2]",
            "condition": "",
            "observe": "",
            "nature": [],
            "actions": [],
            "special": {
                "beliefs": [],
                "freetext": []
            },
            "compute": [
                {
                    "name": "payoff_1",
                    "formula": "=0"
                },
                {
                    "name": "payoff_2",
                    "formula": "=0"
                }
            ]
        }
    ]
}}
  ')
}