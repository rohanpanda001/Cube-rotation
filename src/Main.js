"use strict";

exports.animate = function (ctx) {
    return function (callback) {
        function loop () {
            callback(ctx)();
            window.requestAnimationFrame(loop);
        }

        window.requestAnimationFrame(loop);
        return function () {}
    }
}

exports.logVal = function (a) {
    return function() {
        console.log(a);
    }
}

exports.addEventListener = function (canvas) {
    return function (eventType) {
        return function(callback) {
            function eventHandler(e) {
                callback(e)();
                // console.log(e);
            }
            // console.log(canvas);
            // canvas.addEventListner(eventType, eventHandler);
            document.getElementById("canvas").addEventListener(eventType,eventHandler);
            return function () {};  
        }
    }
} 

exports.stopListener= function(ctx) {
    return function(callback) {
        function eventHandler(e) {
            callback(e)();
            // console.log(e);
        }
    document.getElementById(ctx).addEventListener("mouseup",eventHandler);
    return function () {};
    }
}