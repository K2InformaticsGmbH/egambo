function init(container, width, height) {
    "use strict";
    // This code is executed once and it should initialize the graph, the
    // available parameters are:


    // Run the query:
    // select tid, tname from egambo.egGameType

    // Enabled bots, update as new ones are added.
    var opponents = [
        {id: 1, name: "Bot 1"},
        {id: 2, name: "Bot 2"},
        {id: -1, name: 'None'},
        {id: 5, name: "Connect 4 Bot"}
    ];

    var opponent = 1;

    function initGraph() {
        return {};
    }

    function extractGameTypes(rows, types) {
        rows.forEach(function(row) {
            types[row.tid_1] = {
                name: row.tname_2
            };
        });
        return types;
    }

    // Background and viewport settings
    var viewport = container
        .append('div')
        .style('padding', '25px 0px 0px 0px')
        .style('font-size', '20px')
        .style('font-famlily', 'Impact')
        .style('background-color', '#1C848B')
        .style('color', 'black');

    // Opponent title
    viewport
        .append('div')
        .style('background-color', '#B7B7B7')
        .style('width', '60%')
        .style('height', '32px')
        .style('border-radius', '50px')
        .style('border', '2px dashed black')
        .style('text-align', 'center')
        .style('margin-left', '20%')
        .style('margin-bottom', '30px')
        .text('Opponent');
    
    // Opponent section
    var opponentDiv = viewport
        .append('div')
        .style('margin-bottom', '30px')
        .style('margin-left', '20%')
        .style('width', '60%')
        .style('display', 'inline-block');
    
    /*var opponentSelect = viewport
        .append('select')
        .style('width', '60%')
        .style('margin-left', '20%')
        .style('margin-bottom', '50px')
        .style('font-size', '14px');
    */

    // Game type title
    viewport
        .append('div')
        .style('background-color', '#B7B7B7')
        .style('width', '60%')
        .style('height', '32px')
        .style('border-radius', '50px')
        .style('border', '2px dashed black')
        .style('text-align', 'center')
        .style('margin-left', '20%')
        .style('margin-bottom', '30px')
        .text('Game Type');
    
    // Game type select
    var typesSelect = viewport
        .append('select')
        .style('width', '60%')
        .style('margin-left', '20%')
        .style('margin-bottom', '30px')
        .style('font-size', '14px');
    
    // Start button
    viewport.append('div')
        .style('background-color', '#2DD962')
        .style('width', '40%')
        .style('border-radius', '50%')
        .style('text-align', 'center')
        .style('margin-left', '30%')
        .style('cursor', 'pointer')
        .on('click', startGame)
        .text('Start');

    function startGame() {
        var gameType = typesSelect.property('value');
        var parameters = {':binstr_GameType': {typ: 'binstr', val: gameType}};
        var view = 'egambo.start(GameType)';
        if(opponent !== -1) {
            parameters[':integer_OpponentAccountId'] = {typ: 'integer', val: opponent.toString()};
            view = 'egambo.start(GameType,OpponentAcc)';
        }
        helper.browse(view, parameters);
    }

    function resize(w, h) {
        viewport
            .style('width', w + "px")
            .style('height', h + "px");
    }

    function entries(obj) {
        var res = [];
        for(var k in obj) {
            obj[k].id = k;
            res.push(obj[k]);
        }
        return res;
    }

    function applyOpponentStyle() {
        opponentDiv.selectAll('span')
            .style('background-color', function(d) {
                if(d.id === opponent) {
                    return 'orange';
                }
                return 'grey';
            })
            .style('color', function(d) {
                if(d.id === opponent) {
                    return 'white';
                }
                return 'black';
            });
    }

    function selectOpponent(d) {
        opponent = d.id;
        applyOpponentStyle();
    }

    resize(width, height);
    
    var gameTypes = initGraph();
    return {
        on_data: function(rows) {
            if(rows.length === 0) {
                return;
            }

            // Set opponent static data for now...
            /*opponentSelect.selectAll('option')
                .data(opponents, function(d) { return d.id; })
                .enter()
                .append('option')
                .text(function(d) { return d.name; });
            */

            // Add new opponents
            opponentDiv.selectAll('span')
                .data(opponents, function(d) { return d.id; })
                .enter()
                .append('span')
                .style('cursor', 'pointer')
                .style('width', function(d) {
                    if(d.id === 5) {
                        return '90%';
                    }
                    return '30%';
                })
                .style('text-align', 'center')
                .style('font-size', '18px')
                .style('height', '28px')
                .style('padding-top', '3px')
                .style('display', 'inline-block')
                .style('border-radius', '20px 0px')
                .style('margin-left', function(d) {
                    if(d.id === 1) {
                        return '0px';
                    }
                    return '5%';
                })
                .style('overflow', 'hidden')
                .style('margin-bottom', '5px')
                .style('float', 'left')
                .on('click', selectOpponent)
                .text(function(d) { return d.name; });
            
            // Apply selected opponent style to selected button
            applyOpponentStyle();

            // Process rows and add draw here.
            console.log("the new rows arrived", rows);
            gameTypes = extractGameTypes(rows, gameTypes);

            typesSelect.selectAll('option')
                .data(entries(gameTypes), function(d) { return d.id; })
                .enter()
                .append('option')
                .attr('value', function(d) { return d.id; })
                .text(function(d) { return d.name; });
        },
        on_resize: resize,
        on_reset: function() {
            // Called when the button clear the graph is clicked.
            viewport.selectAll('div > *').remove();
            gameTypeList = initGraph();
        },
        on_close: function() {
            // This should cleanup event listeners and element added
            // outside the container, the container itself will be removed
            // after this function call.
        }
    };
}
