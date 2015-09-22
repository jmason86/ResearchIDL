PRO Read193
aia193Files = file_search('/Users/jmason86/Desktop/2010219_07AUG_1824_M1.0_PSF/prepped/*193A*rot.fits', COUNT = count193)
aia193Array = fltarr(1024, 1024, count193)
headers = strarr(221, 298)

FOR i = 0, count193 - 1 DO BEGIN
  aia193Array[*, *, i] = readfits(aia193Files[i], '.fits', headers[*, i])
  ;IF i EQ 0 THEN all193Headers = header193 ELSE all193Headers = [all193Headers, header193]
  message, /INFO, '193Å fits reading ' + strtrim(float(i + 1) / count193 * 100., 2) + '% complete'
ENDFOR

i1 = image(sqrt(aia193Array[*, *, 0]), dimensions = [1024, 1024])
FOR i = 0, count193 - 1 DO BEGIN
  i1 = image(sqrt(aia193Array[*, *, i]), /CURRENT)
  t = text(0.07, 0.05, string(i+1) + ' of ' + strtrim(count193,2), color = 'white')
  wait, 0.5
  t.delete
ENDFOR

STOP
END