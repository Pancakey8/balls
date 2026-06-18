function openWindow() {
    const canvas = document.getElementById("game");
    const ctx = canvas.getContext("2d");
    return {canvas, ctx};
}

function closeWindow(win) {}

function mkVec2(x, y) { return {x, y}; }

function mkColor(r, g, b, a) { return {r, g, b, a}; }

function drawCircle(win, center, color, rad) {
    console.table({center,color,rad});
    win.ctx.beginPath();
    win.ctx.arc(center.x, center.y, rad, 0, 2 * Math.PI, false);
    win.ctx.fillStyle = "rgba(" + color.r + ", " + color.g + ", " + color.b + ", " + (color.a / 255) + ")";
    win.ctx.fill();
}

function fillWindow(win, color) {
    console.table({color});
    win.ctx.save();
    win.ctx.setTransform(1, 0, 0, 1, 0, 0);
    win.ctx.clearRect(0, 0, win.canvas.width, win.canvas.height)
    win.ctx.fillStyle = "rgba(" + color.r + "," + color.g + "," + color.b + "," + (color.a / 255) + ")";
    win.ctx.fillRect(0, 0, win.canvas.width, win.canvas.height);
    win.ctx.restore();
}

function beginDraw(win) {
    fillWindow(win, {r: 0, g: 0, b: 0, a: 255});
}

function endDraw(win) {}

