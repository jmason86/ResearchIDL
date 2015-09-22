; Program to generate a histogram of the time difference between the peaks of two spectral line emissions during a flare.
; Originally developed for the Fe XIV 211Å line and Fe XX 133Å line. 
;
; REQUIREMENTS: Must have the Flare Catalog mounted. /eve_analysis/testing/analysis/Flare_catalog/
;
; James Paul Mason
; 2012/2/1 

PRO HistogramOfDifferenceInPeakFlareTimes

; load the flare catalog
CD, '/Volumes/Flare Catalog/IDL_savesets'
RESTORE,'merged_flare_catalog.sav'

; obtain the JD peak times for 133 and 211Å lines from the flares in the catalog
timesAll = flare_catalog.evl.evl_lines.peak_time_jd
times133 = reform(timesAll[2,*])
times211 = reform(timesAll[8,*])
flareClass = flare_catalog.goes.peak_class
flareIds = flare_catalog.evl.flare_id

; remove bad data
differenceUnmodified = (times211-times133)*24 ;store difference with the bad data remaining
badIndices = where(times133 NE -999. AND times211 NE -999)
times133 = times133(badIndices) & times211 = times211(badIndices)

; find the difference in peak times of the two lines, converted to hours
difference = (times211-times133)*24
difference = reform(difference)

; create histogram data and plot
;h1 = histogram(difference, binsize=1/12., locations=locs)
;b1 = barplot(locs,h1, fill_color='blue', xrange=[-1,5],$
;             title='Histogram: Difference between peak time of 211Å and 133Å in flare',$
;             xtitle='Difference (hours)',$
;             ytitle='Number (from flare catalog)')


; create and save list of flares for further study
flaresOfInterestIndices = where(differenceUnmodified LE 0.5 AND flareClass GE 'C8.0')
flaresOfInterestIds = flareIds[flaresOfInterestIndices]
;saveloc = '/Users/jmason86/Dropbox/Research/Data/Cooling Rate Flare Loop Length Analysis'
;;spawn, 'rm '+saveloc+'flaresOfInterestIds.txt'
;close, 1 & openw, 1, saveloc+'flaresOfInterestIds.txt', width = 200, /append
;FOR i=0,n_elements(flaresOfInterestIds)-1 DO printf,1,flaresOfInterestIds(i)
;close,1


STOP
END