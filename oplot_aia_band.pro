;
;	EVE Analysis Code
;	-----------------
;
;	Purpose:  	Overplot aia bands on existing plot
;
;	Input:  	ybottom		Y value for bottom of band box
;				ytop		Y value for top of band box
;
;	Options:	color		Option to specify the color
;				charsize	Option to specify the character size
;				charthick	Option to specify the character thickness
;				loud		Option to display information
;
;	Calls:		none
;
;	Plot:		over plots boxes for AIA bands
;
;	History:	12/10/10  T. Woods
;	          2013/5/7 James Paul Mason - updated to work with IDL 8 plot function, using text and polygon functions instead of xyouts and polyfill
;
pro oplot_aia_band, ybottom, ytop, color=color, charsize=charsize, charthick=charthick, loud=loud

if n_params() lt 2 then begin
  print, 'USAGE:  oplot_aia_band, ybottom, ytop, color=color, charsize=charsize, charthick=charthick, /loud'
  return
endif

if keyword_set(charsize) then cs=charsize else cs=1.6
if keyword_set(charthick) then ct=charthick else ct=1.0

if keyword_set(loud) then begin
  print, 'OPLOT_AIA_BAND optional parameters are:'
  print, '    charsize   = ', cs
  print, '    charthick  = ', ct
  print, ' '
endif

;
; define AIA bands FWHM - web site lists line center, guess 2 nm bandpass ???
;
band = [ [9.4+[-1.,1]/2.], [13.1+[-1.,1]/2.], [17.1+[-1.,1]/2.], [19.3+[-1.,1]/2.], [21.1+[-1.,1]/2.], [30.4+[-1.,1]], [33.5+[-1.,1]], $
		[160.+[-10.,10]], [170.+[-10.,10]] ]
center = reform(band[0,*] + band[1,*])/2.

;
;	over plot the line identification above the existing line peaks
;
wgd = where( (center ge !x.crange[0]) and (center le !x.crange[1]), numgd )
if (numgd lt 2) then begin
  print, 'ERROR finding any AIA bands to identify'
  return
endif

for k=0,numgd-1 do begin
  ii = wgd[k]
  xx = [band[0,ii],band[1,ii],band[1,ii],band[0,ii],band[0,ii]]
  yy = [ybottom, ybottom, ytop, ytop, ybottom]
  ;if keyword_set(color) then polyfill, xx, yy, color=color else polyfill, xx, yy
  a = polygon(xx, yy, /FILL_BACKGROUND, FILL_COLOR = JPMColors(k*5), FILL_TRANSPARENCY = 80, /DATA)
endfor

xx = 25.5
yy = ybottom
if keyword_set(color) then xyouts,xx,yy,'AIA',charsize=cs,charthick=ct,align=0.5,color=color $
;else xyouts,xx,yy,'AIA',charsize=cs,charthick=ct,align=0.5
else t = text(xx, yy, 'AIA', font_size = 14, /DATA)

return
end
