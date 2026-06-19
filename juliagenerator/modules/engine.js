// small rendering engine: exports helpers + renderImageData

export function fitDefaultView(){
  return { centerX: 0, centerY: 0, zoom: 1.5 };
}

export function pixelToComplex(px, py, width, height, centerX, centerY, zoom){
  const aspect = width / height;
  const scale = zoom;
  const x = centerX + ((px - width/2) / (width/2)) * scale * aspect;
  const y = centerY + ((py - height/2) / (height/2)) * scale;
  return { x, y };
}

function hsvToRgb(h, s, v){
  const c = v * s;
  const hp = h / 60;
  const x = c * (1 - Math.abs(hp % 2 - 1));
  let r=0,g=0,b=0;
  if (0 <= hp && hp < 1) [r,g,b]=[c,x,0];
  else if (1 <= hp && hp < 2) [r,g,b]=[x,c,0];
  else if (2 <= hp && hp < 3) [r,g,b]=[0,c,x];
  else if (3 <= hp && hp < 4) [r,g,b]=[0,x,c];
  else if (4 <= hp && hp < 5) [r,g,b]=[x,0,c];
  else [r,g,b]=[c,0,x];
  const m = v - c;
  return [Math.floor((r+m)*255), Math.floor((g+m)*255), Math.floor((b+m)*255)];
}

function getColor(iter, zx, zy, maxIter){
  if (iter >= maxIter) return [0,0,0];
  const zn = Math.sqrt(zx*zx + zy*zy);
  const nu = Math.log(Math.log(Math.max(zn, 2))) / Math.log(2);
  const smooth = iter + 1 - nu;
  const hue = (360 * smooth / maxIter) % 360;
  return hsvToRgb(hue, 0.8, 1.0);
}

// synchronous full-image renderer
export function renderImageData({ width, height, cx, cy, maxIter, centerX, centerY, zoom }){
  const img = new ImageData(width, height);
  const data = img.data;
  let ptr = 0;
  for (let py = 0; py < height; py++){
    for (let px = 0; px < width; px++){
      const p = pixelToComplex(px, py, width, height, centerX, centerY, zoom);
      let zx = p.x, zy = p.y;
      let iter = 0;
      while (zx*zx + zy*zy <= 4 && iter < maxIter){
        const xt = zx*zx - zy*zy + cx;
        zy = 2*zx*zy + cy;
        zx = xt;
        iter++;
      }
      const col = getColor(iter, zx, zy, maxIter);
      data[ptr++] = col[0];
      data[ptr++] = col[1];
      data[ptr++] = col[2];
      data[ptr++] = 255;
    }
  }
  return img;
}

