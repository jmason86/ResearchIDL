PRO QuickComparePSFNoPSF
aiaFilesNoPSF = file_search('/Users/jmason86/Desktop/2010219_07AUG_1824_M1.0_PSF/*lev1.fits')
aiaFilesYesPSF = file_search('/Users/jmason86/Desktop/2010219_07AUG_1824_M1.0_PSF/*psf.fits')
aiaFilesNoPSF = aiafilesnopsf[100:200]
aiaFilesYesPSF = aiaFilesYesPSF[100:200]

imgNoPSF = fltarr(4096, 4096, 101)
imgNoPSFExposureFixed = fltarr(4096, 4096, 101)
imgYesPSF = fltarr(4096, 4096, 101)

FOR i = 0, 100 DO BEGIN 
  read_sdo, aiaFilesNoPSF[i], header, tmp, /SILENT
  imgNoPSF[*, *, i] = tmp
  imgNoPSFExposureFixed[*, *, i] = tmp/header.exptime
  imgYesPSF[*, *, i] = readfits(aiaFilesYesPSF[i], /SILENT)
ENDFOR

imgNoPSFTotal = total(total(imgNoPSF, 2), 1)
imgNoPSFExposureFixedTotal = total(total(imgNoPSFExposureFixed, 2), 1)
imgYesPSFTotal = total(total(imgYesPSF, 2), 1)

p1 = plot(imgNoPSFTotal/imgNoPSFTotal[0], '3', $
          TITLE = '171Å Disk Total Comparison', $
          YTITLE = 'Normalized Total', $
          XTITLE = 'Index', $
          NAME = 'Raw')
p2 = plot(imgYesPSFTotal/imgYesPSFTotal[0], 'r2', /OVERPLOT, NAME = 'With PSF No Exposure Correction')
p3 = plot(imgNoPSFExposureFixedTotal/imgNoPSFExposureFixedTotal[0], 'b2--', /OVERPLOT, NAME = 'Raw with Exposure Correction')
leg = legend(TARGET = [p1, p2, p3], POSITION = [0.95, 0.95], /RELATIVE)

save, imgNoPSFTotal, imgNoPSFExposureFixedTotal, imgYesPSFTotal, filename = 'bla.sav'

END