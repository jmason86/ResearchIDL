;docformat = 'rst' 

;+
;	Determine previous day in yyyydoy format that correctly accounts
;	for year boundaries including leap years.
;
;
;PROCEDURE:
;   1) Check parameters. If less than 2 arguments are passed, return
;   usage.
;   2) Separate doy from yyyy for thisday.
;   3) If doy = 1, then check for leap year.
;   4) Assemble date in yyyydoy format, and return.
;
; MODIFICATION HISTORY:
;   10-20-00 DLW Placed under CVS management.
;
; $Log: prev_yyyydoy.pro,v $
; Revision 3.0  2011/03/22 15:24:47  dlwoodra
; version_3.0_commit
;
; Revision 2.0  2010/06/23 18:10:43  dlwoodra
; version_2.0_commit
;
; Revision 1.1.1.1  2009/05/28 21:58:01  evesdp
; Imported Sources
;
; Revision 7.0  2004/07/08 23:02:57  turkk
; commit of version 7.0
;
; Revision 6.0  2003/03/05 19:32:44  dlwoodra
; version 6 commit
;
; Revision 5.20  2002/09/06 23:21:34  see_sw
; commit of version 5.0
;
; Revision 4.0  2002/05/29 18:10:01  see_sw
; Release of version 4.0
;
; Revision 3.0  2002/02/01 18:55:27  see_sw
; version_3.0_commit
;
; Revision 1.1.1.1  2000/11/21 21:49:18  dlwoodra
; SEE Code Library Import
;
;
;idver='$Id: prev_yyyydoy.pro,v 3.0 2011/03/22 15:24:47 dlwoodra Exp $'
;:Params:
; thisday: in, required
; prevday: out, required
;:Categories:
;  utility
pro eve_prev_yyyydoy, thisday, prevday

if n_params() ne 2 then begin
    print,''
    print,'USAGE: prev_yyyydoy, yyyydoy_day, yyyydoy_previous'
    print,''
    print,'  prev_yyyydoy handles new year boundary and'
    print,'  corrects for all possible leap years. '
endif

year = thisday/1000l           ;4-digit year
doy  = thisday - (year*1000l)  ;day of year

;handle year boundary
if doy eq 1 then begin

    ;get previous year
    year = year - 1l

    ;is year a leap year? (handles ALL cases)
    if ((year mod 400 eq 0) or ((year mod 4 eq 0) and (year mod 100) ne 0)) $
      then leap_cor = 1l else leap_cor = 0l

    ;correct doy for the extra day in leap years
    doy = 366l + leap_cor
endif


prevday = (year*1000l) + doy - 1l

return
end
