function init(container, width, height) {
    "use strict";
    // This code is executed once and it should initialize the graph, the
    // available parameters are:

    // Run the query:
    // select gid, tid, cid, players, bots, ctime, ialiases, imovers, space, stime, etime, status, nmovers, naliases, nscores from egGame where hd(players) = user or safe_integer(hd(tl(players))) = user

    // Enabled bots, update as new ones are added.
    function initGraph() {
        return {};
    }

    function extractGameList(rows, list) {
        rows.forEach(function(row) {
            list[row.gid_1] = {
                name: row.tid_2,
                movers: row.nmovers_13,
                alias: row.naliases_14,
                status: row.status_12
            };
        });
        return list;
    }

    // Background and viewport settings
    var viewport = container
        .append('div')
        .style('font-size', '16px')
        .style('font-famlily', 'Impact')
        .style('background-color', '#1C848B')
        .style('color', 'black');
    
    // Header of the game list table
    var header = viewport
        .append('div')
        .style('width', '100%')
        .style('background-color', '#B7B7B7')
        .style('height', '25px')
        .style('border-bottom', '1px black dashed');
    
    // Header content
    header
        .append('span')
        .style('display', 'inline-block')
        .style('width', '30%')
        .style('text-align', 'center')
        .text('Game Id');
    
    header
        .append('span')
        .style('display', 'inline-block')
        .style('width', '20%')
        .style('text-align', 'center')
        .style('border-left', '1px solid')
        .text('Name');
    
    header
        .append('span')
        .style('display', 'inline-block')
        .style('width', '15%')
        .style('text-align', 'center')
        .style('border-left', '1px solid')
        .text('Movers');
    
    header
        .append('span')
        .style('display', 'inline-block')
        .style('width', '15%')
        .style('text-align', 'center')
        .style('border-left', '1px solid')
        .text('Alias');
    
    header
        .append('span')
        .style('display', 'inline-block')
        .style('width', '19%')
        .style('text-align', 'center')
        .style('border-left', '1px solid')
        .text('Status');

    // Section for the content of the game list
    var content = viewport
        .append('div')
        .style('height', 'calc(100% - 25px)')
        .style('overflow-y', 'auto');

    function openGame(d) {
        helper.browse('egambo.d3.Board(GameId)', {
            ':integer_GameId': {typ: 'integer', val: d.id}
        });
    }

    function resize(w, h) {
        viewport
            .style('width', w + "px")
            .style('height', h + "px");
    }

    function entriesByStatus(obj) {
        var res = {
            playing: [],
            forming: [],
            finished: []
        };
        for(var k in obj) {
            obj[k].id = k;
            var status = obj[k].status;
            if(status === 'playing' || status === 'forming' || status === 'finished') {
                res[status].push(obj[k]);
            }
        }
        return res;
    }

    resize(width, height);
    
    var gameList = initGraph();
    return {
        on_data: function(rows) {
            if(rows.length === 0) {
                return;
            }

            // Process rows and add draw here.
            console.log("the new rows arrived", rows);
            gameList = extractGameList(rows, gameList);

            var listByStatus = entriesByStatus(gameList);

            // Add other status entries too...
            var games = content.selectAll('div')
                .data(listByStatus.playing, function(d) { return d.id; });
            
            games.exit().remove();

            var newGames = games.enter()
                .append('div')
                .style('width', '100%')
                .style('background-color', 'white');

            newGames
                .append('span')
                .style('display', 'inline-block')
                .style('width', '30%')
                .style('text-align', 'center')
                .style('border-right', '1px solid')
                .style('-webkit-user-select', 'text')  /* Chrome 49+ */
                .style('-moz-user-select', 'text')     /* Firefox 43+ */
                .style('-ms-user-select', 'text')      /* No support yet */
                .style('user-select', 'text')
                .attr('class', 's-playing-id');
            
            newGames
                .append('span')
                .style('display', 'inline-block')
                .style('width', '20%')
                .style('text-align', 'center')
                .style('border-right', '1px solid')
                .attr('class', 's-playing-name');
            
            newGames
                .append('span')
                .style('display', 'inline-block')
                .style('width', '15%')
                .style('text-align', 'center')
                .style('border-right', '1px solid')
                .attr('class', 's-playing-movers');
            
            newGames
                .append('span')
                .style('display', 'inline-block')
                .style('width', '15%')
                .style('text-align', 'center')
                .style('border-right', '1px solid')
                .attr('class', 's-playing-alias');
            
            newGames
                .append('span')
                .style('display', 'inline-block')
                .style('width', '19%')
                .style('text-align', 'center')
                .style('cursor', 'pointer')
                .on('click', openGame)
                .attr('class', 's-playing-status');

            // Upate information for existing games
            var allGames = content.selectAll('div');

            allGames.select('.s-playing-id')
                .text(function(d) {
                    return d.id;
                });

            allGames.select('.s-playing-name')
                .text(function(d) {
                    return d.name;
                });

            allGames.select('.s-playing-movers')
                .text(function(d) {
                    return d.movers;
                });

            allGames.select('.s-playing-alias')
                .text(function(d) {
                    return d.alias;
                });

            allGames.select('.s-playing-status')
                .style('background-color', '#2DD962')
                .style('border-radius', '50%')
                .text(function(d) {
                    return d.status;
                });

        },
        on_resize: resize,
        on_reset: function() {
            // Called when the button clear the graph is clicked.
            viewport.selectAll('div > *').remove();
            gameList = initGraph();
        },
        on_close: function() {
            // This should cleanup event listeners and element added
            // outside the container, the container itself will be removed
            // after this function call.
        }
    };
}
