FrequencyMeter = function (cpuName) {
    this.name          = cpuName;
    this.canvas        = document.getElementById(cpuName);
    this.canvas.width  = 361;
    this.canvas.height = 130;

    this.context       = this.canvas.getContext("2d");
    this.bgColor       = "#040404";
    this.darkPixel     = "#0f3700";
    this.litPixel      = "#358800";
    this.deadCross     = "#ff0000";
    this.font          = "12px Courier";
    this.pixelWidth    = 5;
    this.crossWidth    = 10;
    this.space         = 1;
    this.lines         = 10;
    this.columns       = 60;
    this.textSpace     = 14;

    this.minFreq       = 0;
    this.maxFreq       = 0;
    this.range         = 0;
    this.samples       = new Array();
};

FrequencyMeter.prototype.draw = function (obj) {
    this.addSample(obj);
    this.drawBase(obj.algorithm, obj.minFreq + " KHz", 
		  obj.maxFreq + " KHz", obj.curFreq + " KHz");
    for (var i = 0; i < this.samples.length; ++i) {
	this.fillColumn(i, this.normalize(this.samples[i]));
    }
};

FrequencyMeter.prototype.drawAnim = function (table) {
    this.drawBase("-", "-", "-", "-");
    for (var i = 0; i < 60; ++i) {
	this.fillColumn(i, table[i]);
    }
};

FrequencyMeter.prototype.drawDead = function () {
    this.drawBase("-", "-", "-", "-");
    this.context.beginPath();
    this.context.lineWidth = this.crossWidth;
    this.context.strokeStyle = this.deadCross;
    this.context.moveTo(0, 0);
    this.context.lineTo(this.canvas.width, this.canvas.height);
    this.context.moveTo(0, this.canvas.height);
    this.context.lineTo(this.canvas.width, 0);
    this.context.stroke();

    // Also reset all stuff related to samples and let the meter
    // "start from the beginning"
    this.minFreq = 0;
    this.maxFreq = 0;
    this.range = 0;
    this.samples = new Array();
};

FrequencyMeter.prototype.normalize = function (n) {
    return n > this.minFreq ? 
	Math.ceil(((n - this.minFreq) / this.range) * 10) : 1;
};

FrequencyMeter.prototype.fillBg = function () {
    this.context.fillStyle = this.darkPixel;
    this.context.fillRect(0, 0, this.canvas.width, this.canvas.height);
    this.context.stroke();
};

FrequencyMeter.prototype.drawGrid = function () {
    this.context.beginPath();
    this.context.lineWidth = this.space;
    this.context.strokeStyle = this.bgColor;
    var y = this.startY();
    for (var i = 0; i < this.lines + 1; ++i) {
	this.context.moveTo(0, y);
	this.context.lineTo(this.canvas.width, y);
	y -= this.unitStep();
    }
    
    // Need to rollback 'y' one step
    y += this.unitStep();
    var x = this.startX();
    for (var i = 0; i < this.columns + 1; ++i) {
	this.context.moveTo(x, y);
	this.context.lineTo(x, this.canvas.height);
	x += this.unitStep();
    }
    this.context.stroke();
};

FrequencyMeter.prototype.drawTextBox = function (algorithm, 
						 min, max, freq) {
    this.context.font = this.font;
    this.context.fillStyle = this.litPixel;
    this.context.fillText("Unit        : " 
			  + this.name, 
			  this.unitStep(), this.textSpaceing());
    this.context.fillText("Algorithm   : " 
			  + algorithm, 
			  this.unitStep(), 2 * this.textSpaceing());
    this.context.fillText("Min/max freq: " 
			  + min + "/" + max, 
			  this.unitStep(), 3 * this.textSpaceing());
    this.context.fillText("Current freq: " 
			  + freq, 
			  this.unitStep(), 4 * this.textSpaceing());
};

FrequencyMeter.prototype.drawBase = function (algorithm, min, max, freq) {
    this.fillBg();
    this.drawGrid();
    this.drawTextBox(algorithm, min, max, freq);
};

FrequencyMeter.prototype.startY = function () {
    return this.canvas.height - this.space - 1;
};

FrequencyMeter.prototype.startX = function () {
    return 0;
};

FrequencyMeter.prototype.textSpaceing = function () {
    return this.textSpace;
};

FrequencyMeter.prototype.unitStep = function () {
    return this.pixelWidth + this.space;
};

FrequencyMeter.prototype.fillColumn = function (column, value) {
    this.context.beginPath();
    this.context.fillStyle = this.litPixel;
    var x = this.startX() + this.unitStep() * column;
    var y = this.startY() - this.pixelWidth;
    for (var i = 0; i < value; ++i) {
	this.context.fillRect(x, y, this.pixelWidth, this.pixelWidth);
	y -= this.unitStep();
    }
    this.context.stroke();
};

FrequencyMeter.prototype.addSample = function (obj) {
    this.minFreq = obj.minFreq;
    this.maxFreq = obj.maxFreq;
    this.range = obj.maxFreq - obj.minFreq;

    this.samples.push(obj.curFreq);
    if (this.samples.length > this.columns) {
	this.samples.splice(0, 1);
    }    
};

genAnimationTable = function () {
    var animationTable = new Array();
    var iterator       = 0;

    // Create the base array
    var t0 = new Array(60);
    for (var i = 0; i < 60; ++i) {
	t0[i] = 1;
    }
    animationTable[iterator++] = t0;

    // Count up all columns to ten ...
    for (var i = 2; i <= 10; ++i) {
	animationTable[iterator] = 
	    animationTable[iterator - 1].map(function (n) {return n + 1});
	++iterator;
    }

    // ... and count down randomly
    while (animationTable[iterator - 1].some(function (n) {return n > 0})) {
	animationTable[iterator] =
	    animationTable[iterator - 1].map(function (n) {
		return (n > 0 && Math.random() > 0.25) ? n - 1 : n
	    });
	++iterator;
    }

    return animationTable;
};
