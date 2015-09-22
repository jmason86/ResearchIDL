; Script for feature categorization intensity levels binning.
; Originally developed for finding Meudon K3 intensity levels. 
; 
; INPUT: accumulated histogram data for each µ ring: y-axis ranging 0-1
;        intensity values in accumulated histogram: x-axis ranging dependent on histogram bins
; OUTPUT: intensity "central bin" thresholds for each µ
;        
; James Paul Mason
; 4/6/2011


PRO intbin, observatory
IF n_elements(observatory) EQ 0 THEN observatory='MDON'

;Define the percentage bins for each feature category
Abini = 0.0  & Abinf = 0.1125  
Bbini = 0.1125  & Bbinf = 0.75 
Dbini = 0.75 & Dbinf = 0.97
Fbini = 0.97 & Fbinf = 1.0

;define arrays to hold intensity values for all µ
allAbin = fltarr(10) & allBbin = fltarr(10)
allDbin = fltarr(10) & allFbin = fltarr(10)
allHbin = fltarr(10) & allPbin = fltarr(10)

FOR i=0,9 DO BEGIN

  ;read in data 
  mustr = strcompress(string(i),/remove_all)
  IF observatory EQ 'Rome' OR observatory EQ 'ROME' OR observatory EQ 'rome' THEN BEGIN
    readcol, 'Dropbox/Research/Data/LevelsDetermination/acchist'+mustr+'Rome20070510.dat',$
                              yaxfi1,yaxfi2,yaxfi3,yaxfi4,yaxfi5,yaxfi6,yaxfi7,yaxfi8,/silent
    readcol, 'Dropbox/Research/Data/LevelsDetermination/intmu'+mustr+'Rome20070510.dat',$
                            intfi1,intfi2,intfi3,intfi4,intfi5,intfi6,intfi7,intfi8,/silent
  ENDIF ELSE $
  IF observatory EQ 'mlso' OR observatory EQ 'MLSO' OR observatory EQ 'MaunaLoa' THEN BEGIN
    readcol, 'Dropbox/Research/Data/LevelsDetermination/acchist'+mustr+'MLSO20070510.dat',$
                              yaxfi1,yaxfi2,yaxfi3,yaxfi4,yaxfi5,yaxfi6,yaxfi7,yaxfi8,/silent
    readcol, 'Dropbox/Research/Data/LevelsDetermination/intmu'+mustr+'MLSO20070510.dat',$
                            intfi1,intfi2,intfi3,intfi4,intfi5,intfi6,intfi7,intfi8,/silent
  ENDIF ELSE BEGIN
    readcol, 'Dropbox/Research/Data/LevelsDetermination/acchist'+mustr+'Meudon20070510.dat',$
                              yaxfi1,yaxfi2,yaxfi3,yaxfi4,yaxfi5,yaxfi6,yaxfi7,yaxfi8,/silent
    readcol, 'Dropbox/Research/Data/LevelsDetermination/intmu'+mustr+'Meudon20070510.dat',$
                            intfi1,intfi2,intfi3,intfi4,intfi5,intfi6,intfi7,intfi8,/silent  
  ENDELSE
  
  ;Find the x-axis value of the top of the Bbin (corresponding to yax=0.75)
  ;Note single fi value is chosen.
  closeBbinf = closest(Bbinf,yaxfi2)
  y1 = yaxfi2(closeBbinf-1) & y2 = yaxfi2(closeBbinf+1)
  x1 = intfi2(closeBbinf-1) & x2 = intfi2(closeBbinf+1)
  Bbin_inttop = findabscissa(x1,y1,x2,y2,Bbinf)

  ;Intensity bin "centers" A-F are on an evenly spaced grid 
  space = Bbin_inttop - 1 ;Determine the half-bin spacing
  Bbin_center = 1
  Abin_center = Bbin_center-2*space
  Dbin_center = Bbin_center+2*space
  Fbin_center = Dbin_center+2*space
  Hbin_center = Fbin_center+2*space
  ;Intensity bin "center" P has new spacing
  ;Existent Rome levels say ~H/P=0.8
  ;In order to use this Rome level, must scale MDON. Scaling diff for all mus
  IF observatory NE 'MDON' THEN $
    Pbin_center = Hbin_center/0.77 $
  ELSE BEGIN
    IF i EQ 0 THEN BEGIN 
      spacescaled = (1.5 * alog(Bbin_inttop) + 1.008) - 1 
      Hscaled = Bbin_center+6*spacescaled
    ENDIF
    IF i EQ 1 THEN Hscaled = Bbin_center + 6*((1.5 * alog(Bbin_inttop) + 1.000) - 1)
    IF i EQ 2 THEN Hscaled = Bbin_center + 6*((1.4 * alog(Bbin_inttop) + 1.000) - 1)
    IF i EQ 3 THEN Hscaled = Bbin_center + 6*((1.3 * alog(Bbin_inttop) + 1.000) - 1)
    IF i EQ 4 THEN Hscaled = Bbin_center + 6*((1.3 * alog(Bbin_inttop) + 1.000) - 1)
    IF i EQ 5 THEN Hscaled = Bbin_center + 6*((1.1 * alog(Bbin_inttop) + 1.006) - 1)
    IF i EQ 6 THEN Hscaled = Bbin_center + 6*((1.1 * alog(Bbin_inttop) + 1.008) - 1)
    IF i EQ 7 THEN Hscaled = Bbin_center + 6*((0.9 * alog(Bbin_inttop) + 1.000) - 1)
    IF i EQ 8 THEN Hscaled = Bbin_center + 6*((0.8 * alog(Bbin_inttop) + 1.009) - 1)
    IF i EQ 9 THEN Hscaled = Bbin_center + 6*((1.0 * alog(Bbin_inttop) + 1.000) - 1)    
     ;old method    
;    IF i EQ 2 THEN Hscaled = 1.4 * alog(Hbin_center) + 1.000
;    IF i EQ 3 THEN Hscaled = 1.3 * alog(Hbin_center) + 1.000
;    IF i EQ 4 THEN Hscaled = 1.3 * alog(Hbin_center) + 1.000
;    IF i EQ 5 THEN Hscaled = 1.1 * alog(Hbin_center) + 1.006
;    IF i EQ 6 THEN Hscaled = 1.1 * alog(Hbin_center) + 1.008
;    IF i EQ 7 THEN Hscaled = 0.9 * alog(Hbin_center) + 1.000
;    IF i EQ 8 THEN Hscaled = 0.8 * alog(Hbin_center) + 1.009
;    IF i EQ 9 THEN Hscaled = 1.0 * alog(Hbin_center) + 1.000
    Pbin_center = Hscaled/0.77
  ENDELSE
  
  ;Put value in array for output outside of loop
  allAbin(i) = Abin_center
  allBbin(i) = Bbin_center
  allDbin(i) = Dbin_center
  allFbin(i) = Fbin_center
  allHbin(i) = Hbin_center
  allPbin(i) = Pbin_center
 
  ;NOTE: MDON mu7 should probably avoid fi2
  ;      MDON mu8 and 9 are pretty spread, maybe average?
  
ENDFOR ;i loop

;open and set up file for output
IF observatory EQ 'Rome' OR observatory EQ 'ROME' or observatory EQ 'rome' THEN BEGIN
  spawn, 'rm Dropbox/Research/Data/LevelsDetermination/ROMEintlevels.tab'
  close,1 & openw,1,'Dropbox/Research/Data/LevelsDetermination/ROMEintlevels.tab',width=200
  printf,1,'Intensity Table:ROME/PSPT_'
ENDIF ELSE $
IF observatory EQ 'mlso' OR observatory EQ 'MLSO' OR observatory EQ 'MaunaLoa' THEN BEGIN
  spawn, 'rm Dropbox/Research/Data/LevelsDetermination/MLSOintlevels.tab'
  close,1 & openw,1,'Dropbox/Research/Data/LevelsDetermination/MLSOintlevels.tab',width=200
  printf,1,'Intensity Table:MLSO/PSPT_'
ENDIF ELSE BEGIN
  spawn, 'rm Dropbox/Research/Data/LevelsDetermination/MDONintlevels.tab'
  close,1 & openw,1,'Dropbox/Research/Data/LevelsDetermination/MDONintlevels.tab',width=200
  printf,1,'Intensity Table:Meudon_K3'
ENDELSE
printf,1,'Filter:?'
printf,1,'Filter offset:0.0'
printf,1,'Filter Units:?, Intensity Units:?'
printf,1,'Models:6 Mus:10'
printf,1,'Mu:  1.0000  0.9000  0.8000  0.7000  0.6000  0.5000  0.4000  0.3000  0.2000  0.1000'

;output the intensity thresholds
printf,1,'Model:A', allAbin(0),allAbin(1),allAbin(2),allAbin(3),allAbin(4),$
                    allAbin(5),allAbin(6),allAbin(7),allAbin(8),allAbin(9)
printf,1,'Model:B', allBbin(0),allBbin(1),allBbin(2),allBbin(3),allBbin(4),$
                    allBbin(5),allBbin(6),allBbin(7),allBbin(8),allBbin(9)
printf,1,'Model:D', allDbin(0),allDbin(1),allDbin(2),allDbin(3),allDbin(4),$
                    allDbin(5),allDbin(6),allDbin(7),allDbin(8),allDbin(9)
printf,1,'Model:F', allFbin(0),allFbin(1),allFbin(2),allFbin(3),allFbin(4),$
                    allFbin(5),allFbin(6),allFbin(7),allFbin(8),allFbin(9)
printf,1,'Model:H', allHbin(0),allHbin(1),allHbin(2),allHbin(3),allHbin(4),$
                    allHbin(5),allHbin(6),allHbin(7),allHbin(8),allHbin(9)
printf,1,'Model:P', allPbin(0),allPbin(1),allPbin(2),allPbin(3),allPbin(4),$
                    allPbin(5),allPbin(6),allPbin(7),allPbin(8),allPbin(9)

close,1
END