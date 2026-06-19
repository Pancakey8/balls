function eventHandlers(win) {
    win.mouse = { left: false, middle: false, right: false };
    win.events = [];

    win.canvas.addEventListener("mousedown", (e) => {
	const left = (e.buttons & 1) === 1;
	const leftMouse = { button: 0, x: e.offsetX, y: e.offsetY };
	if (left && !win.mouse.left) {
	    win.events.push({ kind: 2, mouse: leftMouse });
	} else if (left && win.mouse.left) {
	    win.events.push({ kind: 0, mouse: leftMouse });
	}

	const middle = (e.buttons & 4) === 4;
	const middleMouse = { button: 1, x: e.offsetX, y: e.offsetY };
	if (middle && !win.mouse.middle) {
	    win.events.push({ kind: 2, mouse: middleMouse });
	} else if (middle && win.mouse.middle) {
	    win.events.push({ kind: 0, mouse: middleMouse });
	}

	const right = (e.buttons & 2) === 2;
	const rightMouse = { button: 2, x: e.offsetX, y: e.offsetY };
	if (right && !win.mouse.right) {
	    win.events.push({ kind: 2, mouse: rightMouse });
	} else if (right && win.mouse.right) {
	    win.events.push({ kind: 0, mouse: rightMouse });
	}

	win.mouse.left = left;
	win.mouse.middle = middle;
	win.mouse.right = right;
    });

    win.canvas.addEventListener("mouseup", (e) => {
	const left = (e.buttons & 1) === 1;
	const leftMouse = { button: 0, x: e.offsetX, y: e.offsetY };
	if (!left && win.mouse.left) {
	    win.events.push({ kind: 1, mouse: leftMouse });
	}

	const middle = (e.buttons & 4) === 4;
	const middleMouse = { button: 1, x: e.offsetX, y: e.offsetY };
	if (!middle && win.mouse.middle) {
	   win.events.push({ kind: 1, mouse: middleMouse });
	}

	const right = (e.buttons & 2) === 2;
	const rightMouse = { button: 2, x: e.offsetX, y: e.offsetY };
	if (!right && win.mouse.right) {
	    win.events.push({ kind: 1, mouse: rightMouse });
	}

	win.mouse.left = left;
	win.mouse.middle = middle;
	win.mouse.right = right;
    });
}

function openWindow() {
    const canvas = document.getElementById("game");
    const ctx = canvas.getContext("2d");
    const win = {canvas, ctx};
    eventHandlers(win);
    return win;
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

function hasEvent(ev) { return +(ev !== null); }

function getEventKind(ev) { return ev.kind; }

function getMouseEventButton(ev) { return ev.mouse.button; }

function getMouseEventX(ev) { return ev.mouse.x; }

function getMouseEventY(ev) { return ev.mouse.y; }

function toFrame(_, f) { requestAnimationFrame((_) => f()); }

function pollEvent(win) {
    const ev = (win.events.length == 0) ? null : win.events[0];
    win.events.shift();
    console.table({ ev });
    return ev;
}
