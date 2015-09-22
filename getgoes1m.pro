;+
; NAME:
;   getgoes1m
;
; PURPOSE:
;   Read the GOES 1-min data save set and extract data for single day
;
; CATEGORY:
;   Called from get_goes_temp.pro for XPS processing
;
; CALLING SEQUENCE:  
;   data = getgoes1m( date [, channel, /sectime, /hourtime, /daytime] )
;
; INPUTS:
;   date	YYYYDOY
;
;   channel = 'S' for Short channel = 0.5-4 Angstrom
;           or 'L' for Long  channel =  1 -8 Angstrom <default>
;           or 'F' or 'FL' for Flare index Long"
;           or 'FS' for Flare index Short"
;           or 'G' for Goes number =  10 or 12"
;           or 'A' for all values (S, L, FS, FL, G)"
;
;   /sectime is option to convert GPS time into seconds of day'
;   /hourtime is option to convert GPS time into hours of day'
;   /daytime is option to convert GPS time into fraction of day'
;	
; OUTPUTS:  
;   data	Array of temperature results  or  -1.0 (NO_DATA) if no GOES data found
;			data[0,*] = time (GPS default or other if time option given)
;			data[1,*] = data values (LONG flux default, specified by 'channel'
;	
; COMMON BLOCKS:
;	common  getgoes1m_block, goes, lastyear
;				goes = GOES data structure for a full year
;				lastyear = year for the "goes" variable
;
; PROCEDURE:
;   1.  Check that parameters are valid
;   2.  Get the GOES data for given date (common block)
;	3.  Extract requested data
;   4.  Set return variable (data)
;
; MODIFICATION HISTORY:
;   5/19/06  Tom Woods  First version
;
;-

function getgoes1m, date, channel, sectime=sectime, hourtime=hourtime, daytime=daytime

;
;   1.  Check that parameters are valid
;
NO_DATA = -1.0	; default value to return if NO DATA found

if (n_params() lt 1) then begin
  print, ' '
  print, 'USAGE: data = getgoes1m( date [, channel, /sectime, /hourtime, /daytime] )'
  print, ' '
  print, '   date = YYYYDOY'
  print, ' '
  print, "   channel = 'S' for Short channel = 0.5-4 Angstrom"
  print, "          or 'L' for Long  channel =  1 -8 Angstrom <default>"
  print, "          or 'F' or 'FL' for Flare index Long"
  print, "          or 'FS' for Flare index Short"
  print, "          or 'G' for Goes number =  10 or 12"
  print, "          or 'A' for all values (S, L, FS, FL, G)"
  print, ' '
  print, '   /sectime is option to convert GPS time into seconds of day'
  print, '   /hourtime is option to convert GPS time into hours of day'
  print, '   /daytime is option to convert GPS time into fraction of day'
  print, ' '
  return, NO_DATA
endif

;  assume date is YYYYDOY
year = long(date / 1000L)
if (year lt 1981) or (year gt 2013) then begin
  print, 'GETGOES1M() ERROR:  invalid DATE - YYYYDOY format expected and for years 1981-2013'
  return, NO_DATA
endif
gps_start = jd2gps(yd2jd(date))
gps_end = gps_start + 24L*3600L		; one full day


if (n_params() lt 2) then channel = 'L'
ch = strupcase(strmid(channel,0,2))
ch1 = strmid(ch,0,1)
if (ch1 ne 'F') then ch = ch1

;
;   2.  Get the GOES data for given date (common block)
;
;	Common Block for GOES data (restore if YEAR changes)
;
;	Data Restored
;		goes.time				GPS time (seconds)
;		goes.short				Irradiance 0.05-0.4 nm  W/m^2
;		goes.long				Irradiance 0.1 -0.8 nm  W/m^2
;		goes.sat				GOES Satellite number (10 or 12)
;		goes.flare_idx_long		Phil's Flare Index for LONG channel
;		goes.flare_idx_short	Phil's Flare Index for SHORT channel
;
common  getgoes1m_block, goes, lastyear
if size(lastyear,/type) eq 0 then lastyear = year
if size(goes,/type) eq 0 or lastyear ne year then begin
  gdir = getenv('see_analysis')
  if (strlen(gdir) gt 0) then gdir = gdir + '/goes/' $
  else gdir = '/titus/timed/analysis/goes/'		; force to TITUS directory
  sfile = 'goes_1mdata_widx_'+strtrim(year,2)+'.sav'
  print, 'GETGOES1M():  restoring ', sfile
  restore, gdir + sfile
  lastyear = year
endif

;
;	3.  Extract requested data
;
wgd = where( (goes.time ge gps_start) and (goes.time lt gps_end) )
if (wgd[0] eq -1) then begin
  print, 'GETGOES1M() ERROR: No Data found'
  return, NO_DATA
endif

if (ch eq 'A') then data = dblarr(6,n_elements(wgd))  $
else data = dblarr(2,n_elements(wgd))

;
;	Get TIME
;
if keyword_set(sectime) then begin
  data[0,*] = (gps2jd(double(goes[wgd].time)) - yd2jd(date))*24.D0*3600.
endif else if keyword_set(hourtime) then begin
  data[0,*] = (gps2jd(double(goes[wgd].time)) - yd2jd(date))*24.D0
endif else if keyword_set(daytime) then begin
  data[0,*] = gps2jd(double(goes[wgd].time)) - yd2jd(date)
endif else begin
  data[0,*] = goes[wgd].time	; GPS time
endelse

;
;	Get Data Value (flux, flare index, or Satellite number)
;
case ch of
  'L':  data[1,*] = goes[wgd].long
  'S':  data[1,*] = goes[wgd].short
  'G':  data[1,*] = goes[wgd].sat
  'F':  data[1,*] = goes[wgd].flare_idx_long
  'FL': data[1,*] = goes[wgd].flare_idx_long
  'FS': data[1,*] = goes[wgd].flare_idx_short
  'A':  begin
  		data[1,*] = goes[wgd].short
  		data[2,*] = goes[wgd].long
  		data[3,*] = goes[wgd].flare_idx_short
  		data[4,*] = goes[wgd].flare_idx_long
  		data[5,*] = goes[wgd].sat
  		end
  else: data[1,*] = goes[wgd].long   ; default
endcase

;   4.  Set return variable (data)
return, data
end

