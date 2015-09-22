; Program to accept and return two points for use in zooming. 
; 
; OUTPUT: 
;   output = structure containing the x center location, y center location, and zoom factor (for use with plot_map)
; 
; James Paul Mason
; 2012/2/15

PRO ZoomOnSelectedSquare, output=output

print, 'Right click upper-left and lower-right corners of a square to zoom on.'

output = create_struct('xCen',fltarr(1),'yCen',fltarr(1),'zoomFOV',fltarr(1)) 
xArray = fltarr(2) & yArray = fltarr(2)

FOR i=0,1 DO BEGIN
  
  cursor,x,y,/down
  IF !MOUSE.BUTTON EQ 4 THEN BEGIN
    xArray(i)=x
    yArray(i)=y
  ENDIF $ ;right clicked
  ELSE BEGIN 
    print, 'Use right click for zooming.'
    return
  ENDELSE
  
ENDFOR ;i loop

output.xCen = mean(xArray)
output.yCen = mean(yArray)

IF abs(xArray(1)-xArray(0)) GT abs(yArray(1)-yArray(0)) THEN $
  output.zoomFOV = abs(xArray(1)-xArray(0)) ELSE $
  output.zoomFOV = abs(yArray(1)-yArray(0))

END