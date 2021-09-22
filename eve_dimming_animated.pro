;+
; NAME:
;   eve_dimming_animated
;
; PURPOSE:
;   Export all of the images needed to animate dimming profiles. Originally built to overlay on a spatial image movie of dimming (from AIA) 
;   to exemplify the process for ESCAPE's website. 
;
; INPUTS:
;   None; but requires access to my "Bare bones" .sav file of EVE data
;
; OPTIONAL INPUTS:
;   None
;
; KEYWORD PARAMETERS:
;   None
;
; OUTPUTS:
;   images saved to disk to be compiled into a movie by another application (e.g., Adobe Premiere)
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Requires eve_lines_2010121-2014146 MEGS-A Mission Bare Bones.sav
;
; EXAMPLE:
;   Just run it!
;-
PRO eve_dimming_animated

; Defaults
fontSize = 28
thickness = 4
saveloc = '/Users/jmason86/Dropbox/Research/ResearchScientist_LASP/Analysis/Dimming/EVE Dimming Movie/'

restore, '/Users/jmason86/Dropbox/Research/Data/SDO/EVE/eve_lines_2010121-2014146 MEGS-A Mission Bare Bones.sav'

; Restrict to time of interest: the dimming event from my 2014 paper: 2010-08-07 moderate event
good_indices = where(jd GE jpmiso2jd('2010-08-07T17:00:00Z') and jd LE jpmiso2jd('2010-08-07T23:00:00Z'))
jd = jd[good_indices]
iso = iso[good_indices]
irradiance = irradiance[*, good_indices]

; Get rid of those god damn -1 bad data flags
irradiance[where(irradiance EQ -1)] = !VALUES.F_NAN

; Extract just the lines to plot
irr171 = irradiance[3, *]
irr180 = irradiance[5, *]
irr195 = irradiance[6, *]

; Convert to percent change
irr171 = perdiff(irr171[0], irr171)
irr180 = perdiff(irr180[0], irr180)
irr195 = perdiff(irr195[0], irr195)

; Convert time to just elapsed hours
time = (jd - jd[0]) * 24.

w = window(DIMENSIONS = [1200, 1200], background_color='black', FONT_SIZE=fontSize, /BUFFER)
p1 = plot(time, irr195, thick=thickness, /CURRENT, font_size=fontSize, 'white', font_color='white', position=[0.13, 0.74, 0.95, 0.99], $
          xshowtext=0, xrange=minmax(time), xcolor='white', $
          yrange=[-4, 4], ycolor='white', $
          axis_style=4)
p10 = plot(p1.xrange, [0, 0], '2--', color='grey', /OVERPLOT)
p10.order, /SEND_TO_BACK
p1y = axis('Y', target=p1, color='white', minor=0, tickfont_size=fontSize, location=0)
p2 = plot(time, irr180, thick=thickness, /CURRENT, font_size=fontSize, 'white', font_color='white', position=[0.13, 0.41, 0.95, 0.66], $
          xshowtext=0, xrange=minmax(time), xcolor='white', $
          yrange=[-4, 4], ycolor='white', $
          axis_style=4)
p20 = plot(p1.xrange, [0, 0], '2--', color='grey', /OVERPLOT)
p20.order, /SEND_TO_BACK
p2y = axis('Y', target=p2, color='white', minor=0, tickfont_size=fontSize, location=0)
p3 = plot(time, irr171, thick=thickness, /CURRENT, font_size=fontSize, 'white', font_color='white', position=[0.13, 0.08, 0.95, 0.33], $
          xtitle='elapsed time [hours]', xrange=minmax(time), xcolor='white', xminor=0, $
          yrange=[-4, 4], ycolor='white', yminor=0, $
          axis_style=1)
p30 = plot(p1.xrange, [0, 0], '2--', color='grey', /OVERPLOT)
p30.order, /SEND_TO_BACK
t1 = text(0.062, 0.87, 'Fe XII 195 Å !C 1.3 MK', orientation=90, font_size=fontSize, color='white', alignment=0.5)
t2 = text(0.062, 0.54, 'Fe XI 180 Å !C 1.1 MK', orientation=90, font_size=fontSize, color='white', alignment=0.5)
t3 = text(0.062, 0.20, 'Fe IX 171 Å !C 0.6 MK', orientation=90, font_size=fontSize, color='white', alignment=0.5)

n_frames = n_elements(time)
FOR i = 0, n_frames - 1 DO BEGIN
  p1.setdata, time[0:i], irr195[0:i]
  p2.setdata, time[0:i], irr180[0:i]
  p3.setdata, time[0:i], irr171[0:i]
  p1.save, saveloc + '/frames/' + JPMPrintNumber(i, /NO_DECIMALS) + '.png', /TRANSPARENT, RESOLUTION=100
ENDFOR



STOP

END