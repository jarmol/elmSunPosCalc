Below is a minimal 3-file ES-module example (no Web Worker) that reproduces the interactive Julia set with sliders + numeric inputs. Save three files in the same folder and serve them with your local server

How to run

Serve the folder via your local server (modules require HTTP/HTTPS). Example: python -m http.server from the folder and open http://localhost:8000/.
Open index.html in the browser. Adjust cx/cy with sliders or numbers, click canvas to set c, and wheel to zoom.

Notes

This simple module layout keeps engine code separate and returns an ImageData so the main module remains responsible for DOM and UI.
If you later need responsiveness on large canvases, convert renderImageData into chunked rendering or move it to a Worker.
.
