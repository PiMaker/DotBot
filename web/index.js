var stage = null;
var canvas = null;

// For debugging only ($.extend(true))
var testModel = {
    width: 80,
    height: 40,
    elements: [{
        type: "grass",
        x: 1,
        y: 2
    }, {
        type: "grass",
        x: 4,
        y: 4
    }, {
        type: "red",
        x: 2,
        y: 10,
        life: 12
    }, {
        type: "blue",
        x: 4,
        y: 4,
        life: 45
    }, {
        type: "blue",
        x: 6,
        y: 1,
        life: 2
    }, {
        type: "beamRed",
        x1: 2,
        y1: 10,
        x2: 6,
        y2: 10
    }, {
        type: "beamBlue",
        x1: 4,
        y1: 4,
        x2: 40,
        y2: 20
    }]
};

var model = null;
var newModel = null;

var blueImg = new Image();
blueImg.src = "blueImg.png";
var redImg = new Image();
redImg.src = "redImg.png";

// Initializes necessary elements for drawing
function initVis() {
    canvas = document.getElementById("canvas");
    canvas.width = document.body.clientWidth;
    canvas.height = document.body.clientHeight;
    stage = new createjs.Stage(canvas);
    createjs.Ticker.setFPS(30);
    createjs.Ticker.addEventListener("tick", stage);
    drawModel(testModel);
}

// Draws a model to the screen (canvas)
function drawModel(model) {
    model.elements = model.elements.filter(function(x) {
        return x.removed !== true;
    });
    drawGrass(model);
    drawGrid(model.width, model.height);
    drawCritters(model);
    drawBeams(model);
    stage.update();
}

// Draws only the grass in the model
function drawGrass(model) {
    var squareWidth = canvas.width / model.width;
    var squareHeight = canvas.height / model.height;
    for (element of model.elements) {
        if (element.type === "grass") {
            square = new createjs.Shape();
            square.graphics.beginFill("Lime");
            square.graphics.drawRect(0, 0, squareWidth, squareHeight);
            square.x = element.x * squareWidth;
            square.y = element.y * squareHeight;
            stage.addChild(square);
        }
    }
}

// Draws all elements of a model
function drawCritters(model) {
    var squareWidth = canvas.width / model.width;
    var squareHeight = canvas.height / model.height;
    for (element of model.elements) {
        if (element.type === "red" || element.type === "blue") {
            var bitmap = new createjs.Bitmap(element.type === "red" ? redImg : blueImg);
            bitmap.x = element.x * squareWidth;
            bitmap.y = element.y * squareHeight;
            bitmap.scaleX = squareWidth/128;
            bitmap.scaleY = squareHeight/128;
            stage.addChild(bitmap);
            var text = new createjs.Text("" + element.life, "10px Arial", "yellow");
            text.x = bitmap.x;
            text.y = bitmap.y;
            stage.addChild(text);
        }
    }
}

// Draws all beams
function drawBeams(model) {
    var squareWidth = canvas.width / model.width;
    var squareHeight = canvas.height / model.height;
    for (element of model.elements) {
        if (element.type === "beamRed" || element.type == "beamBlue") {
            line = new createjs.Shape();
            line.graphics.beginStroke(element.type.substr(4, 4));
            line.graphics.moveTo(element.x1 * squareWidth + squareWidth / 2, element.y1 * squareHeight + squareHeight / 2);
            line.graphics.lineTo(element.x2 * squareWidth + squareWidth / 2, element.y2 * squareHeight + squareHeight / 2);
            line.graphics.endStroke();
            stage.addChild(line);
            createjs.Tween.get(line, {
                    loop: false
                })
                .to({
                    alpha: 0
                }, 1000, createjs.Ease.getPowOut(3))
        }
    }
}

// Draws a grid pattern
function drawGrid(width, height) {
    var square;
    var squareWidth = canvas.width / width;
    var squareHeight = canvas.height / height;
    for (var x = 0; x < width; x++) {
        for (var y = 0; y < height; y++) {
            square = new createjs.Shape();
            square.graphics.beginStroke("LightGray");
            square.graphics.drawRect(0, 0, squareWidth, squareHeight);
            square.x = x * squareWidth;
            square.y = y * squareHeight;
            stage.addChild(square);
        }
    }
}

// Pull the trigger
initVis();
