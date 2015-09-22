;+
; NAME:
;   JPMMergeACEData
;
; PURPOSE:
;   Read in, concatenate, and save ACE data as downloaded from their website (http://www.srl.caltech.edu/ACE/ASC/level2/lvl2DATA_MAG-SWEPAM.html). 
;   This is the 64 second averaged, Mag/SWEPAM merged data. It downloads as a standard text file with tab-delimited columns. 
;
; INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   None
;
; KEYWORD PARAMETERS:
;   None
;
; OUTPUTS:
;   IDL Save file with ACE data for time and proton density. 
;   year, doy, hr, min, sec are all UT
;   fp_year: fractional year
;   fp_doy: fractional day of year
;   Np: proton density [cm^-3]
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Requires readcol.pro
;
; EXAMPLE:
;   Just run it! 
;
; MODIFICATION HISTORY:
;   2015/02/19: James Paul Mason: Wrote script.
;-
PRO JPMMergeACEData

FOR i = 0, 4 DO BEGIN
  aceName = 'ACE20' + JPMPrintNumber(10 + i)
  
  ; Read data
  readcol, '/Volumes/Archer4TB/ACE/' + aceName, yearTmp, doyTmp, hrTmp, minTmp, secTmp, fp_yearTmp, fp_doyTmp, NpTmp, Tp, Alpha_ratio, Vp, V_rtn_r, V_rtn_t, V_rtn_n, $
    V_gse_x, V_gse_y, V_gse_z, V_gsm_x, V_gsm_y, V_gsm_z, B_rtn_r, B_rtn_t, B_rtn_n, B_gse_x, B_gse_y, $
    ;                                          B_gse_z, B_gsm_x, B_gsm_y, B_gsm_z, Bmag, Lambda4, Delta, dBrms
    SKIPLINE = 46, /SILENT
  
  ; Concatenate data
  IF i EQ 0 THEN BEGIN
    year = yearTmp
    doy = doyTmp
    hr = hrTmp
    min = minTmp
    sec = secTmp
    fp_year = fp_yearTmp
    fp_doy = fp_doyTmp
    Np = NpTmp
  ENDIF ELSE BEGIN
    year = [year, yearTmp]
    doy = [doy, doyTmp]
    hr = [hr, hrTmp]
    min = [min, minTmp]
    sec = [sec, secTmp]
    fp_year = [fp_year, fp_yearTmp]
    fp_doy = [fp_doy, fp_doyTmp]
    Np = [Np, NpTmp]
  ENDELSE
ENDFOR

; Get rid of bad data
goodIndices = where(Np NE -9999.90)
year = year[goodIndices]
doy = doy[goodIndices]
hr = hr[goodIndices]
min = min[goodIndices]
sec = sec[goodIndices]
fp_year = fp_year[goodIndices]
fp_doy = fp_doy[goodIndices]
Np = Np[goodIndices]

; Output concatenated data
save, year, doy, hr, min, sec, fp_year, fp_doy, Np, FILENAME = 'ACE2010-2014.sav', /COMPRESS

END