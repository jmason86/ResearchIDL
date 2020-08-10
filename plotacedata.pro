;+
; NAME:
;   PlotAceData
;
; PURPOSE:
;   Make a plot of ACE data for the MagEx proposal
;
; INPUTS:
;   None. But does actually require the ACE data .txt files to be downloaded from
;     1) Magnetic field data: http://www.srl.caltech.edu/cgi-bin/dib/rundibviewmagl2/ACE/ASC/DATA/level2/mag?mag_data_4min_year2014.hdf!hdfref;tag=1962,ref=3,s=0
;     2) 
;
; OPTIONAL INPUTS:
;   None
;
; KEYWORD PARAMETERS:
;   MAKE_ASCII_TEMPLATE: Set this to create new ASCII templates for reading the source data
;
; OUTPUTS:
;   Stack plot of ACE data
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Requires ACE data
;
; EXAMPLE:
;   Just run it!
;
; MODIFICATION HISTORY:
;   2018-08-16: James Paul Mason: Wrote script.
;-
PRO PlotAceData, MAKE_ASCII_TEMPLATE = MAKE_ASCII_TEMPLATE

; Setup
magPathFilename = '/Users/jmason86/Dropbox/Research/Data/ACE/ACE_MAG_Data.txt'
windPathFilename = '/Users/jmason86/Dropbox/Research/Data/ACE/ACE_SWICS_Proton_Data.txt'
windowDimensions = [650, 300]

; Make ASCII read template
IF keyword_set(MAKE_ASCII_TEMPLATE) THEN BEGIN
  aceMagTemplate = ascii_template(magPathFilename)
  aceWindTemplate = ascii_template(windPathFilename)
  
  save, aceMagTemplate, FILENAME = '/Users/jmason86/Dropbox/Research/Data/ACE/ACE_MAG_Template.sav'
  save, aceWindTemplate, FILENAME = '/Users/jmason86/Dropbox/Research/Data/ACE/ACE_Wind_Template.sav'
ENDIF ELSE BEGIN
  restore, '/Users/jmason86/Dropbox/Research/Data/ACE/ACE_MAG_Template.sav'
  restore, '/Users/jmason86/Dropbox/Research/Data/ACE/ACE_Wind_Template.sav'
ENDELSE

; Read the data
mag = read_ascii(magPathFilename, TEMPLATE = aceMagTemplate)
wind = read_ascii(windPathFilename, TEMPLATE = aceWindTemplate)

; Convert magnetic field dates to jd
doyMag = fix(mag.doy_fod)
yyyydoy = strtrim(mag.year, 2) + strtrim(doyMag, 2)
yyyymmdd = JPMyyyydoy2yyyymmdd(yyyydoy, /RETURN_STRING)
fod = mag.doy_fod - doyMag
hhmmss = JPMfod2hhmmss(fod)
jdMag = JPMiso2jd(yyyymmdd + ' ' + hhmmss)

; Convert wind dates to jd
doyWind = fix(wind.doy_fod)
yyyydoy = strtrim(wind.year, 2) + strtrim(doyWind, 2)
yyyymmdd = JPMyyyydoy2yyyymmdd(yyyydoy, /RETURN_STRING)
fod = wind.doy_fod - doyWind
hhmmss = JPMfod2hhmmss(fod)
jdWind = JPMiso2jd(yyyymmdd + ' ' + hhmmss)

; Get separate days to plot
day1MagIndices = where(doyMag EQ 83)
day1WindIndices = where(doyWind EQ 83)
day2MagIndices = where(doyMag EQ 88)
day2WindIndices = where(doyWind EQ 88)

; Filter out bad wind data
badProtonDensityIndices = where(wind.proton_density LT -2000)
wind.proton_density[badProtonDensityIndices] = !VALUES.F_NAN
badProtonSpeedIndices = where(wind.proton_speed LT -2000)
wind.proton_speed[badProtonSpeedIndices] = !VALUES.F_NAN

; Plot data for day 1
w = window(background_color = 'black', DIMENSIONS = windowDimensions)
p1 = plot(jdMag[day1MagIndices], mag.b_z_gse[day1MagIndices], 'white', /CURRENT, FONT_COLOR = 'white', FONT_SIZE = 20, THICK = 3, $
          TITLE = 'Southward Mag Field [nT]', $
          XCOLOR = 'white', XTICKNAME = [''], $
          YCOLOR = 'white', $
          BACKGROUND_TRANSPARENCY = 100)
p1.save, 'Mag Field Earth.png', /TRANSPARENT, RESOLUTION = 300

w = window(background_color = 'black', DIMENSIONS = windowDimensions)
p2 = plot(jdWind[day1WindIndices], wind.proton_density[day1WindIndices], 'white', /CURRENT, FONT_COLOR = 'white', FONT_SIZE = 20, THICK = 3, $
          TITLE = 'Solar Wind Density [cm$^{-3}$]', $
          XCOLOR = 'white', XTICKNAME = [''], $
          YCOLOR = 'white', YMAJOR = 4, $
          BACKGROUND_TRANSPARENCY = 100)
p2.save, 'Proton Density Earth.png', /TRANSPARENT, RESOLUTION = 300

w = window(background_color = 'black', DIMENSIONS = windowDimensions)
p3 = plot(jdWind[day1WindIndices], wind.proton_speed[day1WindIndices], 'white', /CURRENT, FONT_COLOR = 'white', FONT_SIZE = 20, THICK = 3, $
          TITLE = 'Solar Wind Speed [km s$^{-1}$]', $
          XCOLOR = 'white', XTICKNAME = [''], $
          YCOLOR = 'white', YMAJOR = 4, $
          BACKGROUND_TRANSPARENCY = 100)
p3.save, 'Proton Speed Earth.png', /TRANSPARENT, RESOLUTION = 300

; Plot data for day 2
w = window(background_color = 'black', DIMENSIONS = windowDimensions)
p4 = plot(jdMag[day2MagIndices], mag.b_z_gse[day2MagIndices], 'white', /CURRENT, FONT_COLOR = 'white', FONT_SIZE = 20, THICK = 3, $
          TITLE = 'Southward Mag Field [nT]', $
          XCOLOR = 'white', XTICKNAME = [''], $
          YCOLOR = 'white', $
          BACKGROUND_TRANSPARENCY = 100)
p4.save, 'Mag Field L5.png', /TRANSPARENT, RESOLUTION = 300

w = window(background_color = 'black', DIMENSIONS = windowDimensions)
p5 = plot(jdWind[day2WindIndices], wind.proton_density[day2WindIndices], 'white', /CURRENT, FONT_COLOR = 'white', FONT_SIZE = 20, THICK = 3, $
          TITLE = 'Solar Wind Density [cm$^{-3}$]', $
          XCOLOR = 'white', XTICKNAME = [''], $
          YCOLOR = 'white', YMAJOR = 4, $
          BACKGROUND_TRANSPARENCY = 100)
p5.save, 'Proton Density L5.png', /TRANSPARENT, RESOLUTION = 300

w = window(background_color = 'black', DIMENSIONS = windowDimensions)
p6 = plot(jdWind[day2WindIndices], wind.proton_speed[day2WindIndices], 'white', /CURRENT, FONT_COLOR = 'white', FONT_SIZE = 20, THICK = 3, $
          TITLE = 'Solar Wind Speed [km s$^{-1}$]', $
          XCOLOR = 'white', XTICKNAME = [''], $
          YCOLOR = 'white', YMAJOR = 4, $
          BACKGROUND_TRANSPARENCY = 100)
p6.save, 'Proton Speed L5.png', /TRANSPARENT, RESOLUTION = 300

STOP

END