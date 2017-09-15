function init(container, width, height) {
    "use strict";
    // This code is executed once and it should initialize the graph, the
    // available parameters are:

    // Expected data w, h, data

    // query:
    // select 9 as w, 1 as h, :binstr_pattern as data from dual

    // virtual coordinates drawing arc radius
    var size = 1000;
    // token radius in virtual coordinates
    var nradius = 50;
    var animDuration = 500;

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

    var parseMessage = function(message) {
        // Remove erlang binary delimitators
        message = message.replace("<<", "").replace(">>", "");
        message = message.split("{").join("[").split("}").join("]");
        try {
            return JSON.parse(message);
        } catch (e) {
            return message;
        }
    };

    var max;
    function extractBoard(rows) {
        max = 0;
        var row = rows.pop();
        var data = parseMessage(row.data_3);
        var height = row.h_2;
        var width = row.w_1;
        var board = [];
        // Fit the data to the screen
        var sizeW = width * nradius * 2;
        var sizeH = height * nradius * 2;
        var vBox = {
            x: -nradius * 2,
            y: -nradius * 2,
            w: sizeW + nradius * 2,
            h: sizeH + nradius * 2
        };
        svg.attr('viewBox', vBox.x + ' ' + vBox.y + ' ' + vBox.w + ' ' + vBox.h);


        if(isMoves(data)) {
            data.reverse();
            // Initialize an empty board before adding the moves...
            for(var i = 0; i < height*width; ++i) {
                board.push({
                    id: i,
                    token: " ",
                    label: " ",
                    x: (i % width) * (nradius * 2),
                    y: Math.floor(i/width) * (nradius * 2)
                })
            }
            for(var i = 0; i < data.length; ++i) {
                var token = String.fromCharCode(data[i][0]);
                var id = data[i][1];
                var x = (id % width) * (nradius * 2);
                var y = Math.floor(id/width) * (nradius * 2);
                board[id] = {
                    id: id,
                    token: token,
                    label: token + " " + (i+1),
                    x: x,
                    y: y
                };
            }
        } else {
            data = data.map(function(element) {
                if(Array.isArray(element)) {
                    return element[1] / element[0];
                } else {
                    return element;
                }
            });
            for(var i = 0; i < height; ++i) {
                for(var j = 0; j < width; ++j) {
                    var id = i * width + j;
                    var token = data[id];
                    if (Math.abs(token) > max) {
                        max = Math.abs(token);
                    }
                    board.push({
                        id: id,
                        token: token,
                        label: token,
                        x: j * (nradius * 2),
                        y: i * (nradius * 2)
                    });
                }
            }
        }
        return board;
    }

    function isMoves(data) {
        return data.every(function(element) {
            return (element[0] == 88 || element[0] == 79);
        });
    }

    function resize(w, h) {
        var cheight = h - (margin.top + margin.bottom);
        var cwidth = w - (margin.left + margin.right);
        svg.attr('width', cwidth)
            .attr('height', cheight);
    }

    resize(width, height);

    var tokenColor = {
        X: "#DB210E",
        O: "#3352FF",
        $: "black",
        "*": "#2BE9A7",
        " ": "white"
    }

    function getColor(token) {
        if(tokenColor[token]) {
            return tokenColor[token];
        } else if(token < 0) {
            return tokenColor.X;
        } else if(token > 0) {
            return tokenColor.O;
        } else {
            return tokenColor[" "];
        }
    }

    function getSize(token) {
        if(tokenColor[token]) {
            return nradius;
        } else {
            var scaled = Math.max(Math.abs(token) * nradius / max, 1);
            if (Number.isNaN(scaled)) {
                return nradius;
            } else {
                return scaled;
            }
        }
    }

    function getLabel(d) {
        var f = +d.label;
        if(Number.isNaN(f) || d.label === " ") {
            return d.label;
        }
        return f.toFixed(1);
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
                .attr('r', function(d) {
                    return getSize(d.token);
                })
                .attr('cx', function(d) { return d.x; })
                .attr('cy', function(d) { return d.y; })
                .style('stroke', 'black')
                .style('stroke-width', 3)
                .style('fill', function(d) {
                    return getColor(d.token);
                });

            // Add label
            svg.selectAll('text')
                .data(board, function(d) { return d.id; })
                .enter()
                .append('text')
                .style('font-size', '24px')
                .attr('text-anchor', 'middle')
                .text(getLabel)
                .attr('x', function(d) { return d.x; })
                .attr('y', function(d) { return d.y + 8; });
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
