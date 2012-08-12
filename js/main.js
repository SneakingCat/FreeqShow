/*
** Copyright (C) 2012, Patrik Sandahl
** sneakingcatsw@gmail.com
*/
pageLoaded = function (cpus) {
    var meterMap = new Array();
    cpus.forEach(function(cpu) {meterMap[cpu] =  new FrequencyMeter(cpu)});
    animateMeters(meterMap, function () {

	var ws = createWebSocket();
	var tmo = createTimeout(meterMap);

	ws.onopen = function () {
	    clearInterval(tmo);
	};
	ws.onmessage = function (event) {
	    var array = JSON.parse(event.data);
	    array.forEach(function (obj) {
		meterMap[obj.cpu].draw(obj);
	    });
	};
	ws.onclose = function () {
	    drawAllDead(meterMap);
	};
	ws.onerror = function () {
	    drawAllDead(meterMap);
	};
    });
};

createTimeout = function (meterMap) {
    return setInterval(function () {
	drawAllDead(meterMap);
    }, 1500);
};

drawAllDead = function (meterMap) {
    for (var i in meterMap) {
	meterMap[i].drawDead();
    }
};

createWebSocket = function () {
    var uri = 'ws://' + window.location.hostname + ':9160/';
    var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
    return new Socket(uri);
};

animateMeters = function (meterMap, f) {
    animationTable = genAnimationTable();
    var tableIndex = 0;
    var timeVar = setInterval(function () {
	if (tableIndex < animationTable.length) {
	    for (var i in meterMap) {
		meterMap[i].drawAnim(animationTable[tableIndex]);
	    }
	    ++tableIndex;
	} else {
	    clearInterval(timeVar);
	    f();
	}
    }, 60);
};

