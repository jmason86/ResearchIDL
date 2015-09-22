;
;	EVE Analysis Code
;	-----------------
;
;	Purpose:  	Plot MEGS L2 Lines Time Series (flare phases)
;
;	Input:  	yyyydoy1	Start date (Year / Day of Year)
;				yyyydoy2	End date (if same as yyyydoy1 then reads just one day)
;
;	Options:	/goes		GOES XRS input data,  NOAA 1-min data as follows:
;							 goes[5,*] = sod, goes[7,*] = XRS-B irradiance
;				/xrange		Set X-Range for plot in hours
;				/title		Set plot Title string
;				/level		Specify the flare level to make full color scale available (default = 0.05)
;				/lineplot	Option to plot as lines instead of color bars and in units of energy
;				/jpeg		Option to make JPEG image of plot
;				/save		Option to save the raw data for later restoring
;				/fe_only	Option to only do Fe lines
;
;	Output:		tsdata		Optional output that gives Time Series (TS) data of each line
;				rawdata		Optional output of full EVE L2 lines
;				peaks		Optional output of peak times
;				meta		Optional output of EVE L2 meta data
;
;	Calls:		merge_evl	IDL procedure located in $eve_code/idl_lib/ on evesci2
;
;	Plot:		Plot time series of Fe emissions from cool to hot Temperature from MEGS-A
;
;	History:	6/23/10  T. Woods, Code based on plot_megs_ts.pro.
;

pro eve_megs_lines_ts, yyyydoy1, yyyydoy2, goes=goes, peaks=peaks, xrange=xrange, title=title, debug=debug, $
					level=level, tsdata=tsdata, rawdata=rawdata, meta=meta, $
					lineplot=lineplot, jpeg=jpeg, save=save, fe_only=fe_only

  ;
  ;  check inputs
  ;
  if n_params() lt 1 then begin
     print, 'USAGE:  eve_megs_lines_ts, yyyydoy1, yyyydoy2 [, /goes, xrange=xrange, title=title, $ '
     print, '                     level=level, tsdata=tsdata, rawdata=rawdata, meta=meta, peaks=peaks ] '
     return
  endif
  
  ;
  ;	check input days
  ;
  if (n_params() lt 2) then yyyydoy2 = yyyydoy1
  if (yyyydoy1 lt 2010085L) or (yyyydoy2 le 2010085L) then begin
    print, 'ERROR in having valid input dates. Exiting'
    return
  endif
  if (yyyydoy2 lt yyyydoy1) then begin
    ytemp = yyyydoy2
    yyyydoy2 = yyyydoy1
    yyyydoy1 = ytemp
  endif
  numdays = yyyydoy2 - yyyydoy1 + 1L
  
  ;
  ; set options
  ;
  xrfull = [0,24*numdays]
  if keyword_set(xrange) then xr=xrange else xr=xrfull
  if keyword_set(title) then mtitle=title else mtitle=' '
  
  ; set flare level for color saturation  
  full_level = 0.05
  if keyword_set(level) then full_level = level
  
;
;	read the results from EVE L2 Lines merged set saved for the mission
;
common eve_lines_last, datalines, metalines, yd_range
if (n_elements(yd_range) lt 2) then yd_range = [ 0L, 0L ]
if (n_elements(datalines) lt 2) or (yyyydoy1 lt yd_range[0]) or (yyyydoy1 gt yd_range[1]) $
          or (yyyydoy2 lt yd_range[0]) or (yyyydoy2 gt yd_range[1]) then begin
  evedir = getenv('eve_data_merged')
  if strlen(evedir) lt 1 then evedir = getenv('projects')+'/SDO_EVE/data/merged/' $
  else evedir = evedir + '/'
  fname = 'eve_lines_*.sav'
  filelist = file_search( evedir + fname, count=fcount )
  if (fcount lt 1) then begin
    print, 'EVE_MEGS_LINES_TS:  ERROR finding EVE Lines save set.  Exiting...' 
    return
  endif
  ;
  ;  search file list to find which one to read
  ;		first make list of dates for each file found
  ;		then find the file with most overlap
  file_yd = lonarr( 2, fcount )
  best_k = 0L
  best_cnt = 0L
  for k=0, fcount-1 do begin
    spos = strpos( filelist[k], 'eve_lines_' )
    file_yd[0,k] = long( strmid(filelist[k],spos+10, 7) )
    file_yd[1,k] = long( strmid(filelist[k],spos+10+8, 7) )
    dcnt = 0L
    if (yyyydoy1 ge file_yd[0,k]) and (yyyydoy1 le file_yd[1,k]) then begin
      dcnt = file_yd[1,k] - yyyydoy1
    endif
    if (yyyydoy2 ge file_yd[0,k]) and (yyyydoy2 le file_yd[1,k]) then begin
      dcnt2 = yyyydoy2 - file_yd[0,k]
      if (dcnt2 gt dcnt) then dcnt = dcnt2
    endif
    if (dcnt gt best_cnt) then begin
      best_k = k
      best_cnt = dcnt
    endif
  endfor
  ;
  ;  now read file (restore IDL save set)
  ;
  yd_range[0] = file_yd[0,best_k]
  yd_range[1] = file_yd[1,best_k]
  fevelines = filelist[best_k]
  if keyword_set(loud) then print, 'EVE_MEGS_LINES_TS: reading ', fevelines
  restore, fevelines	; datalines, meta
  ; metalines = meta.linesmeta
endif
 
  ;
  ;	convert SOD to hours
  ;
  day = datalines.yyyydoy - yyyydoy1
  thourfull = day * 24. + datalines.sod/3600.
  wgdtime = where((thourfull ge xr[0]) and (thourfull le xr[1]), ntime)
  if (ntime lt 2) then begin
    print, 'ERROR finding valid (positive) time, exiting...'
    if keyword_set(debug) then stop, 'Check out ERROR'
    return  
  endif
  thour = thourfull[wgdtime]
  year1 = long(yyyydoy1/1000.)
  xtitle = 'Time (hours of '+strtrim(year1,2)+'/'+strtrim(long(yyyydoy1 mod year1),2)+')'
    
  ;
  ;	prep plot and define colors
  ;
  setplot
  ncolors = 250
  cc = rainbow( 7 )
  
  ;
  ;	Define the emission lines (configured for Version 1 of EVE Lines Level 2)
  ;
  wseta = where( (metalines.wave_center ge 6) and (metalines.wave_center lt 38), nseta )
  if (nseta lt 2) then begin
    print, 'ERROR finding MEGS-A lines, exiting...'
    if keyword_set(debug) then stop, 'Check out ERROR'
    return
  endif
  
  ename = metalines[wseta].name
  ewave = metalines[wseta].wave_center
  etemp = metalines[wseta].logt
  
  ;  sort so lines go from cool to hot
  asort = sort( etemp )
  if keyword_set(debug) then print, 'Lines Temperature Sorted index: ', asort
  if keyword_set(fe_only) then begin
    ; do only the Fe lines
    asort = [ 11, 1, 3, 4, 5, 6, 7, 8, 10, 12, 0, 2 ]
  endif
  ename = ename[asort]
  ewave = ewave[asort]
  etemp = etemp[asort]
  eindex = wseta[asort]
  
  enum = n_elements(ewave)
  epeaktime = fltarr(enum)
  epeaklevel = fltarr(enum)
  epeakcom = fltarr(enum)  ; center of mass
  for j=0L,enum-1 do ename[j] = strtrim(ename[j],2)+' '+strtrim(long(ewave[j]*10.+0.5),2)
  numtime = n_elements(thour)
  ts = fltarr(enum,numtime)
  tsnorm = ts
  
  ;
  ;	Make the Time Series (TS) data array
  ;
  print, 'Emission   Max-Level    Max-Time_hour   COM-Time_hour'
  ans = ' '
  ymax = 0.05  ; for %
  ymax = 1E-6  ; for energy lineplot
  nsmooth = 5  ; for smoothing over time
  
  for k=0,enum-1 do begin
    wgd = where( (datalines[wgdtime].line_irradiance[eindex[k]] gt 0), numgd )
    if (numgd gt 1) then begin
      ts[k,wgd] = datalines[wgdtime[wgd]].line_irradiance[eindex[k]]
      ;  fill bad results with interpolated results
      wbad = where( (datalines[wgdtime].line_irradiance[eindex[k]] le 0), numbad )
      if (numbad ge 1) then begin
        ts[k,wbad] = interpol( ts[k,wgd], thour[wgd], thour[wbad] )
      endif
      ;  smooth more over time
      ts[k,*] = smooth(ts[k,*],nsmooth)
      ;  make normalized time series too
      ;  normalize with minimum (almost minimum) and "full_level"
      tstemp = reform(ts[k,*])
      wgg = where( (thour ge xr[0]) and (thour le xr[1]) and (tstemp gt 0), numgg)
      if (numgg gt 1) then tstemp = reform(tstemp[wgg])
      ym = median(tstemp)
      for j=0,4 do if (n_elements(ym) gt 4) then ym = median(tstemp[where(tstemp lt ym)])
      ;  smooth the data by 3 minutes
      tsnorm[k,*] = smooth(ts[k,*] - ym, 3)
      ; only allow emissions that have > 5% flare variation to have full color scale
      wgg = where( (thour ge xr[0]) and (thour le xr[1]) and (tsnorm[k,*] gt 0), numgg)
      if (numgg gt 1) then begin
        kemax = max(tsnorm[k,wgg],wmax)
        ktmax = thour[wgg[wmax]]
        kcom = total( reform(tsnorm[k,wgg]) * thour[wgg] ) / total(tsnorm[k,wgg])
        ; if k eq 11 then stop, 'Check out kcom for Fe XVI 335...'
      endif else begin
        kemax = max(tsnorm[k,*],wmax)
        ktmax = thour[wmax]
        kcom = ktmax ; not valid to do center of mass
      endelse
      ;  save peak time
      epeaktime[k] = ktmax
      epeaklevel[k] = kemax
      epeakcom[k] = kcom
      ;  prepare for plot now
      if ((kemax/ym) gt full_level) then colornorm = ncolors else colornorm = ncolors/2.
      if keyword_set(lineplot) then colornorm = kemax   ;  percent = kemax / ym, energy = kemax
      tsnorm[k,*] = tsnorm[k,*] * colornorm / kemax
      if keyword_set(lineplot) and (k gt 2) then ymax = max( [ymax, kemax] )
      print, ename[k], kemax*1E6, ktmax, kcom, format='(A14,F8.3,2F9.3)'
      if keyword_set(debug) then begin
        yname = strtrim(ename[k],2) + ' ' + strtrim(long(ewave[k]*10.),2)
        plot, thour, ts[k,*]*1000., xtitle=xtitle, xrange=xrfull, xs=1, ys=1, ytitle='Irradiance', title=yname
        oplot, !x.crange,ym*1000.*[1,1],line=2
        oplot, ktmax*[1,1],!y.crange,line=1
        read, 'Next ', ans
      endif
    endif
  endfor

  if keyword_set(lineplot) then begin
    yrline = [0, long(ymax*10.+1)*10.]		; for percentage
    if (yrline[1] gt 100) then yrline[1] = 100.
    yrline = [0, ymax*1E6+1.]				; for energy units
    if (yrline[1] gt 20) then yrline[1] = 20.
  endif
  
  ;
  ;	Option to Read / Use GOES 1-m data in plot
  ;
  doGOES = 0
  goespeaklevel = 0.
  goespeaktime = 0.
  if keyword_set(goes) then begin
     if (n_elements(goes) lt 5) then begin
       dgoes = read_dat( strtrim(yyyydoy1,2)+'_GOES_XRS_1m.dat' )
       if (n_elements(dgoes) ge 5) and (numdays gt 1) then begin
         for j=1L,numdays-1 do begin
           goes2 = read_dat( strtrim(yyyydoy1+j,2)+'_GOES_XRS_1m.dat' )
           if (n_elements(goes2) gt 5) then begin
             goes2[5,*] = goes2[5,*] + j * 24. * 3600.
             dgoes = [ [dgoes], [goes2] ]
           endif
         endfor
       endif
     endif else dgoes = goes
     if (n_elements(dgoes) lt 5) then begin
       doGOES = 0
     endif else begin
       doGOES = 1
       ;  GOES time in minutes
       gthour = dgoes[5,*]/3600.
       gnorm = dgoes[7,*]
       ;  only use the data within MEGS data time (thour)
       wgg = where( (gthour ge min(thour)) and (gthour le max(thour)), numgg )
       if (numgg gt 2) then begin
         gthour = gthour[wgg]
         gnorm = gnorm[wgg]
       endif
       ;  make normalized time series too
       ym = median(gnorm)
       for j=0,4 do if (n_elements(ym) gt 4) then ym = median(gnorm[where(gnorm lt ym)])
       gnorm = gnorm - ym
       wgg = where( (gthour ge xr[0]) and (gthour le xr[1]), numgg)
       if (numgg gt 1) then begin
         goesmax = max(gnorm[wgg],wmax) 
         gtmax = gthour[wgg[wmax]]
       endif else begin
         goesmax = max(gnorm,wmax)
         gtmax = gthour[wmax]
       endelse
       goespeaktime = gtmax
       goespeaklevel = goesmax
       if keyword_set(lineplot) then colornorm = yrline[1] else colornorm = ncolors
       gnorm = gnorm * colornorm / goesmax
       print, 'GOES XRS ', gtmax
       endelse
  endif
  print, ' '
  
  ;
  ;	save peak times in "peaks"
  ;
  peaks = { name: ename, wavelength: ewave, logTemp: etemp, index: eindex, peakTime: epeaktime, $
  			peakLevel: epeaklevel, peakCOM: epeakcom, $
  			goes_peakTime: goespeaktime, goes_peakLevel: goespeaklevel }

  ;
  ; make plot - two options:  1) color bar for each line  or  2) line plots on top of each other
  ;  
  
  if keyword_set(lineplot) then begin
  ;
  ;  line plot version  in units of energy 
  ;		(used to be percent:  ytitle='Variability (%)' & tsfactor = 100.)
  ;
  ytitle='Variability (microW/m!U2!N)' & tsfactor = 1.E6
  plot, thour, tsnorm[0,*], /nodata, $
    xrange=xr, xs=1, yrange=yrline, ys=1, $
    ytitle=ytitle, xtitle=xtitle, title=mtitle

  xx = !x.crange[0]*0.95 + !x.crange[1]*0.05
  yy = !y.crange[1]
  dy = (!y.crange[1] - !y.crange[0])/(enum+2.)
  yy = yy - dy
  cs = 1.8
  
  if (doGOES ne 0) then begin
    oplot, gtmax*[1,1],!y.crange, line=2	; mark where GOES XRS has maximum
    oplot, gthour, gnorm, thick=3			; black line
    xyouts, xx, yy + dy*1.5, 'GOES XRS', charsize=cs
  endif

  cc = rainbow(enum+1)
  for k=enum-1,0,-1 do begin
    tstemp = tsnorm[k,*]*tsfactor
    if (epeaklevel[k]*tsfactor gt yrline[1]) then tstemp = tstemp * yrline[1]/(epeaklevel[k]*tsfactor)
    oplot, thour, tstemp, color=cc[k]
    xyouts, xx, yy - dy*k, ename[k], charsize=cs, color=cc[k]
  endfor
  
  if keyword_set(jpeg) then begin
    jfile = 'eve_lines_plot_'+strtrim(yyyydoy1,2)+'_'+strtrim(long(xr[0]>0),2)+'-'+strtrim(long(xr[1]),2)+'UT.jpg'
    print, 'Writing JPEG image to ', jfile
    im3 = tvrd(true=1)
    write_jpeg, jfile, im3, true=1, quality=90.
  endif
  
  endif else begin
  ;
  ;  color bar version of plot
  ;
  if (doGOES ne 0) then begin
    yr = [0,enum+2]
    cstep = long(ncolors/(enum+1))
    yticks=enum+2
    ytickname = [ ' ', ename, 'GOES XRS', ' ' ]  
    ytickname2 = [ ' ', string(etemp,format='(F4.2)'), ' ', ' ']
  endif else begin
    yr = [0,enum+1]
    cstep = long(ncolors/enum)
    yticks=enum+1
    ytickname = [ ' ', ename, ' ' ]
    ytickname2 = [ ' ', string(etemp,format='(F4.2)'), ' ']
  endelse
  
  xmargin = [9,6]
  ymargin = [3,2]
  
  plot, thour, tsnorm[0,*], /nodata, $
    xrange=xr, xs=1, yrange=yr, ys=8, $
    yticks=yticks, ytickname=ytickname, yticklen=1E-5, xmargin=xmargin, ymargin=ymargin, $
    ytitle=' ', xtitle=xtitle, title=mtitle

  axis, yaxis=1, yticklen=1E-5, yticks=yticks, ytickname=ytickname2, ytitle='log(Temp)'
    
  if (doGOES ne 0) then begin
    oplot, gtmax*[1,1],!y.crange	; mark where GOES XRS has maximum
  endif
  
  cc = rainbow(256,/image)
  
  ;  define plot window position
  px1 = long(xmargin[0]*!d.x_ch_size*!p.charsize) + 1
  py1 = long(ymargin[0]*!d.y_ch_size*!p.charsize)
  px2 = long(!d.x_size-xmargin[1]*!d.x_ch_size*!p.charsize) - 1
  py2 = long(!d.y_size-ymargin[1]*!d.y_ch_size*!p.charsize)
  numx = px2-px1+1
  numy = py2-py1+1
  numy1 = numy / yticks
  py1 = py1 + long(numy1/2.)
  box1 = fltarr( numx, long(numy1-4) )	; box for single emission
  xrg = !x.crange
  xbox1 = findgen( numx ) * (xrg[1]-xrg[0]) / numx + xrg[0]
  
  wgdx = where( (thour ge xrg[0]) and (thour le xrg[1]), numgdx )
  if (numgdx lt 2) then return	; nothing to plot
  
  for k=0,enum-1 do begin
    ; oplot, thour, tsnorm[k,*], color=cc[k*cstep]
    ;
    ;	fill box1 with value from tsnorm[k,*]
    ;
    box1[*,0] = interpol( (tsnorm[k,*] > 0), thour, xbox1 )
    for j=0,numx-1 do box1[j,*] = box1[j,0]
    ;  put box1 image onto plot
    tv, box1, px1, long(py1 + k*numy1)
  endfor
  
  ;
  ;  Add GOES XRS
  ;
  if keyword_set(goes) then begin
    ;  fill box1
    box1[*,0] = interpol( (gnorm > 0), gthour, xbox1 )
    wbad = where( xbox1 gt max(gthour), numbad )
    if (numbad gt 1) then box1[wbad,0] = 0
    for j=0,numx-1 do box1[j,*] = box1[j,0]
    ;  put box1 image onto plot
    tv, box1, px1, long(py1 + enum*numy1)
  endif
  
  ;
  ;	add color bar
  ;
  colorbar = fltarr(long(!d.x_ch_size*!p.charsize),ncolors)
  for j=0,ncolors-1 do colorbar[*,j] = j
  yoffset = py1 - numy1/2 + (numy - ncolors)/2.
  tv, colorbar, 0, yoffset

  if keyword_set(jpeg) then begin
    jfile = 'eve_lines_ts_'+strtrim(yyyydoy1,2)+'_'+strtrim(long(xr[0]>0),2)+'-'+strtrim(long(xr[1]),2)+'UT.jpg'
    print, 'Writing JPEG image to ', jfile
    im3 = tvrd(true=1)
    write_jpeg, jfile, im3, true=1, quality=90.
  endif

  endelse  ; end of color bar plot
    
  ;
  ;	save "tsdata"
  ;
  tsdata = { hour : fltarr(ntime), irr : fltarr(enum, ntime), irr_norm : fltarr(enum, ntime), $
  		name : strarr(enum), temp : fltarr(enum), wave : fltarr(enum) }
  tsdata.hour = thour
  tsdata.irr = ts
  tsdata.irr_norm = tsnorm
  tsdata.name = ename
  tsdata.temp = etemp
  tsdata.wave = ewave
  
  if keyword_set(debug) then stop, 'DEBUG eve_megs_lines_ts.pro ...'
 
return
end
