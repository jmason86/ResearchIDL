PRO PlotExposureTimes

loc = '/Users/jama6159/Documents/Research/Data/SDO/AIA/FullDisk/2011216_04AUG_0357_M9.3/prepped/'
cd, loc
readcol, 'filesToProcess.txt', files, format='a', /silent
wavelengths = strmid(files,7,3,/reverse_offset)
files171 = where(wavelengths EQ '171')
files193 = where(wavelengths EQ '193') 

exposure171 = fltarr(120)
exposure193 = fltarr(120)

FOR i = 0, 119 DO BEGIN
  hd = headfits(files(files171(i)))
  exposure171(i) = sxpar(hd, 'EXPTIME')
  hd = headfits(files(files193(i)))
  exposure193(i) = sxpar(hd, 'EXPTIME')

ENDFOR

p1 = plot(exposure171, 'r2', title = 'AIA Exposure Times for 2011216_04AUG_0357_M9.3', NAME = '171Å')
p2 = plot(exposure193, 'b2', /OVERPLOT, NAME = '193Å')
leg = legend(TARGET = [p1, p2], POSITION = [0.8,0.7])
STOP
p1.close

END