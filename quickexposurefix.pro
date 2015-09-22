PRO QuickExposureFix
saveloc = '/Users/jama6159/Documents/Research/Data/SDO/AIA/FullDisk/2011215_03AUG_1348_M6.0/prepped/1k/ExposureCorrected/'

aiaFiles = file_search('/Users/jama6159/Documents/Research/Data/SDO/AIA/FullDisk/2011215_03AUG_1348_M6.0/prepped/1k/*.fits', COUNT = count)

FOR i = 0, count - 1 DO BEGIN
  read_sdo, aiaFiles[i], header, image
  exptime = header.exptime
  IF aiaFiles[i] EQ '/Users/jama6159/Documents/Research/Data/SDO/AIA/FullDisk/2011215_03AUG_1348_M6.0/prepped/1k/AIA2011-08-03T13_57_171A_rot_1k.fits' $
    OR aiaFiles[i] EQ '/Users/jama6159/Documents/Research/Data/SDO/AIA/FullDisk/2011215_03AUG_1348_M6.0/prepped/1k/AIA2011-08-03T13_56_171A_rot_1k.fits' THEN STOP
  image = image/exptime
  

  parsedFilename = ParsePathAndFilename(aiaFiles[i])
  
  mwritefits, header, image, outfile = saveloc + parsedFilename.Filename
  message, /INFO, 'Writing ' + strtrim(float(i + 1) / count * 100., 2) + '% complete'
ENDFOR

END