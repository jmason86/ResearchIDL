;
;	EVE Analysis Code
;	-----------------
;
;	Purpose:  	Overplot line identification on existing plot
;
;	Input:  	wave		Wavelength (nm)
;				sp			Spectrum (whatever units)
;
;	Options:	linewave	Wavelength of lines id (or uses default set)
;				linename	Name of lines id (or uses default set)
;				/charsize	Option to specify character size (default is 1.4)
;				/charthick	Option to specify character thickness (default is 1.0)
;				/linecolor	Option to specify line id color
;				/yfactor	Option to specify Y factor above line peak (default is 1.1)
;				/waveoffset Option to specify wavelength offset for line id
;				/loud		Option to print debug messages
;
;	Calls:		none
;
;	Plot:		over plots the line identification on existing plot
;
;	History:	9/12/10  T. Woods
;	          5/7/2013 J. Mason Changed the output to work with IDL 8's plot function using the text function instead of xyouts
;
pro oplot_line_id, wave, sp, linewave, linename, linecolor=linecolor, charsize=charsize, charthick=charthick, $
					yfactor=yfactor, waveoffset=waveoffset, loud=loud

if n_params() lt 2 then begin
  print, 'USAGE:  oplot_line_id, wave, sp [, linewave, linename]'
  return
endif

if n_params() lt 4 then begin
  ;  define the default line id - intended for SDO EVE MEGS (5-105 nm) at 0.1 nm resolution
  ;  Note that deleted  Ne VII 127, Fe VIII 131, and Fe X 177 due to over crowding
  linewave = [9.39, 13.29, 14.87, 17.11, $
  			18.04, 19.51, 20.20, 21.13, 25.63, $
  			28.42, 30.38, 33.54, 36.08, 36.81, $
  			44.37, 46.52, 49.94, 52.58, 55.44, 58.43, $
  			59.96, 62.49, 62.97, 71.85, 76.51, 77.04, $
  			79.02, 97.25, 97.70, 102.57, 103.19 ]
  linename = [ 'Fe XVIII', 'Fe XX', 'Ne V', 'Fe IX', $
  			'Fe XI', 'Fe XII', 'Fe XIII', 'Fe XIV', 'He II', $
  			'Fe XV', 'He II', 'Fe XVI', 'Fe XVI', 'Mg IX', $
  			'Mg IX', 'Ne VII', 'Si XII', 'O III', 'O IV', 'He I', $
  			'O III', 'Mg X', 'O V', 'O II', 'N IV', 'Ne VIII', $
  			'O IV', 'H I', 'C III', 'H I', 'O VI' ]
endif

if keyword_set(charsize) then cs=charsize else cs=1.4
if keyword_set(charthick) then ct=charthick else ct=1.0

doLineColor = 0
if keyword_set(linecolor) then begin
  if n_elements(linecolor) eq n_elements(linewave) then doLineColor=2 else doLineColor=1
endif

if not keyword_set(waveoffset) then waveoffset = (!x.crange[1] - !x.crange[0])/100.
if not keyword_set(yfactor) then yfactor = 1.1

if keyword_set(loud) then begin
  print, 'OPLOT_LINE_ID optional parameters are:'
  print, '    waveoffset = ', waveoffset
  print, '    yfactor    = ', yfactor
  print, '    charsize   = ', cs
  print, '    charthick  = ', ct
  print, '    doLineColor= ', doLineColor
  print, '    number ids = ', n_elements(linewave)
  print, ' '
endif

;
;	over plot the line identification above the existing line peaks
;
wgd = where( (linewave ge min(wave)) and (linewave le max(wave)), numgd )
if (numgd lt 2) then begin
  print, 'ERROR finding any lines to identify'
  return
endif

for k=0,numgd-1 do begin
  wline = linewave[wgd[k]]
  ; find line peak within 0.5 nm
  ww = where( (wave ge (wline-0.5)) and (wave le (wline+0.5)), numww )
  if (numww lt 3) then begin
    ; failed, so try finding line peak within 1 nm
    ww = where( (wave ge (wline-1)) and (wave le (wline+1)), numww )
  endif
  if (numww lt 3) then begin
    ; failed, so try finding line peak within 2 nm
    ww = where( (wave ge (wline-2)) and (wave le (wline+2)), numww )
  endif
  if (numww ge 3) then begin
    ; only plot line ID if found at least 3 data points in spectrum
    temp = max( sp[ww], wpeak )
    yy = temp * yfactor
    xx = wline + waveoffset
    if (doLineColor eq 2) then begin
      xyouts, xx, yy, linename[wgd[k]], orient=90., charsize=cs, charthick=ct, color=linecolor[wgd[k]]
    endif else if (doLineColor eq 1) then begin
      xyouts, xx, yy, linename[wgd[k]], orient=90., charsize=cs, charthick=ct, color=linecolor[0]
    endif else begin
      ;xyouts, xx, yy, linename[wgd[k]], orient=90., charsize=cs, charthick=ct
      t = text(xx, yy, linename[wgd[k]], orientation = 90., /DATA, font_size = 14, COLOR = JPMColors(k+20))
    endelse
  endif
endfor

return
end
