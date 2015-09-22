; Program to interpolate distance between selected points in an existing graphics window
; Ideal for use with plot_map in solarsoft
; 
; OUTPUT: 
;   outlen = the total computed length in whatever units the graphics display is using
;   outpoints = 2 element array of length 2*multi to store the click up and down user selected locations. 
;               Array structured as a series of [x,y] points.
; 
; Provided by Amir Caspi (originally called select.pro)
; 2012/2/15
; 
; MODIFICATION HISTORY:
;   2012/2/29: James Paul Mason
;     Added an oplot diamond for each place the user clicks
;   2012/3/1: James Paul Mason
;     Added outpoints output array
; 

PRO ClickToComputeLength,multi=multi,outlen=outlen,outpoints=outpoints

checkvar,multi,2
totlen = 0
outpoints = create_struct('x',fltarr(multi),'y',fltarr(multi))

FOR i=0,(multi-1)>1 DO BEGIN
  cursor,x1,y1,/down
  
  ; store the points for output 
  outpoints.x(i) = x1 & outpoints.y(i) = y1
  
  ; Overplot a diamond in the locations the user selected
  TEK_COLOR
  oplot,[x1],[y1],psym=4,color=2 ; oplot requires x,y be arrays

  ; Compute and display length
  print,'('+num2str(x1)+', '+num2str(y1)+')'
  IF i GT 0 THEN BEGIN 
    x2 = outpoints.x(i-1) & y2 = outpoints.y(i-1)
    len = sqrt((x2-x1)^2+(y2-y1)^2)
    totlen = totlen + len
    print,'segment length: '+num2str(len)
  ENDIF ; i > 0
  
ENDFOR ;i loop

; Output
outlen=totlen
outpoints=outpoints

END