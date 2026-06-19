import { renderImageData, pixelToComplex, fitDefaultView } from './modules/engine.js';

const canvas = document.getElementById('c');
const ctx = canvas.getContext('2d');

const cxRange = document.getElementById('cxRange');
const cyRange = document.getElementById('cyRange');
const cxNum = document.getElementById('cxNum');
const cyNum = document.getElementById('cyNum');
const iterNum = document.getElementById('iterNum');
const resetBtn = document.getElementById('reset');

let cx = parseFloat(cxNum.value);
let cy = parseFloat(cyNum.value);
let maxIter = parseInt(iterNum.value, 10);
let centerX = 0, centerY = 0;
let zoom = 1.5;

// declare renderPending before any uses
let renderPending = false;

function requestRender(){
  if (renderPending) return;
  renderPending = true;
  requestAnimationFrame(async ()=>{
    renderPending = false;
    await doRender();
  });
}

async function doRender(){
  const w = canvas.width, h = canvas.height;
  const config = { width: w, height: h, cx, cy, maxIter, centerX, centerY, zoom };
  const img = renderImageData(config);
  ctx.putImageData(img, 0, 0);
}

function resizeCanvas(){
  const size = Math.min(window.innerWidth-40, window.innerHeight-80, 900);
  canvas.width = canvas.height = Math.max(200, Math.round(size));
  requestRender();
}
window.addEventListener('resize', resizeCanvas);
resizeCanvas();

// UI sync helpers
function syncToUI(){
  cxNum.value = cx.toFixed(6); cyNum.value = cy.toFixed(6);
  cxRange.value = cx; cyRange.value = cy;
  iterNum.value = maxIter;
}

cxRange.addEventListener('input', ()=>{
  cx = parseFloat(cxRange.value); syncToUI(); requestRender();
});
cyRange.addEventListener('input', ()=>{
  cy = parseFloat(cyRange.value); syncToUI(); requestRender();
});
cxNum.addEventListener('change', ()=>{
  cx = parseFloat(cxNum.value); syncToUI(); requestRender();
});
cyNum.addEventListener('change', ()=>{
  cy = parseFloat(cyNum.value); syncToUI(); requestRender();
});
iterNum.addEventListener('change', ()=>{
  maxIter = Math.max(50, parseInt(iterNum.value,10)||300); requestRender();
});

resetBtn.addEventListener('click', ()=>{
  cx = -0.8; cy = 0.156; maxIter = 300; centerX = 0; centerY = 0; zoom = 1.5;
  syncToUI(); requestRender();
});

canvas.addEventListener('click', (e)=>{
  const r = canvas.getBoundingClientRect();
  const mx = e.clientX - r.left, my = e.clientY - r.top;
  const p = pixelToComplex(mx, my, canvas.width, canvas.height, centerX, centerY, zoom);
  const factor = 0.7;
  cx = parseFloat((p.x * factor).toFixed(6));
  cy = parseFloat((p.y * factor).toFixed(6));
  syncToUI(); requestRender();
});

canvas.addEventListener('wheel', (e)=>{
  e.preventDefault();
  const r = canvas.getBoundingClientRect();
  const mx = e.clientX - r.left, my = e.clientY - r.top;
  const before = pixelToComplex(mx, my, canvas.width, canvas.height, centerX, centerY, zoom);
  const zoomFactor = e.deltaY > 0 ? 1.15 : 1/1.15;
  zoom *= zoomFactor;
  const after = pixelToComplex(mx, my, canvas.width, canvas.height, centerX, centerY, zoom);
  centerX += before.x - after.x;
  centerY += before.y - after.y;
  requestRender();
}, { passive: false });

// initial view and render
({ centerX, centerY, zoom } = fitDefaultView());
syncToUI();
requestRender();

