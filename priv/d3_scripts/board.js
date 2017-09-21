function init(container, width, height) {
    "use strict";
    // This code is executed once and it should initialize the graph, the
    // available parameters are:


    // Run the query:
    // select time, gid, msgtype, to_json(message) as msg from egGameMsg where gid = :integer_GameId

    // virtual coordinates drawing arc radius
    var size = 1000;
    // token radius in virtual coordinates
    var nradius = 50;
    var animDuration = 500;

    var parseMessage = function(message) {
        try {
            return JSON.parse(message);
        } catch (e) {
            throw parseError(message);
        }
    };

    function extractBoard(rows) {
        var row = rows.pop();
        var msg = parseMessage(row.msg_4);
        var board = [];
        for(var i = 0; i < msg.height; ++i){
            for(var j = 0; j < msg.width; ++j) {
                var id = i*msg.width+j;
                var token = msg.board[id];
                board.push({
                    id: id,
                    gid: row.gid_2,
                    token: msg.board[id],
                    status: msg.status,
                    x: j*(nradius*2),
                    y: i*(nradius*2)
                });
            }
        }
        return board;
    }


    var vBox = {
        x: -nradius * 2,
        y: -nradius * 2,
        w: size + nradius * 2,
        h: size + nradius * 2
    };

    var margin = { top: 10, right: 10, bottom: 10, left: 10 }; // physical margins in px

    var svg = container
        .append('svg')
        .attr('viewBox', vBox.x + ' ' + vBox.y + ' ' + vBox.w + ' ' + vBox.h)
        .attr('preserveAspectRatio', 'xMidYMax meet')
        .style('margin-top', margin.top + 'px')
        .style('margin-right', margin.right + 'px')
        .style('margin-bottom', margin.bottom + 'px')
        .style('margin-left', margin.left + 'px');

    function resize(w, h) {
        var cheight = h - (margin.top + margin.bottom);
        var cwidth = w - (margin.left + margin.right);
        svg.attr('width', cwidth)
            .attr('height', cheight);
    }

    resize(width, height);

    var tokenColor = {
        X: "#DB210E",
        O: "#FFFF21",
        " ": "white"
    }
    
    function move(d) {
        if(d.token === " " && d.status === "playing") {
            helper.browse('egambo.play(GameId,CellInt)', {
                ':integer_GameId': {typ: "integer", val: d.gid},
                ':integer_CellId': {typ: "integer", val: d.id.toString()}
            });
        }
    }

    var firstData = true;
    return {
        on_data: function(data) {
            if(data.length === 0) {
                return;
            }

            // Process data and add draw here.
            console.log("the new data arrived", data);
            var board = extractBoard(data);
            svg.selectAll('circle')
                .data(board, function(d) { return d.id; })
                .enter()
                .append('circle')
                .attr('r', nradius)
                .attr('cx', function(d) { return d.x; })
                .attr('cy', function(d) { return d.y; })
                .style('stroke', 'black')
                .style('stroke-width', 3)
                .on('click', move);
            
            // Update new moves
            svg.selectAll('circle')
                .style('fill', function(d) {
                    return tokenColor[d.token];
                })
        },
        on_resize: resize,
        on_reset: function() {
            // Called when the button clear the graph is clicked.
            svg.selectAll('svg > *').remove();
        },
        on_close: function() {
            // This should cleanup event listeners and element added
            // outside the container, the container itself will be removed
            // after this function call.
        }
    };
}
