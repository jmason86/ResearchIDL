;docformat = 'rst' 

;+
;Find the next year/day of year after the input parameter, properly taking into
;account leap years, year boundary wraparound, and so forth
;
;This routine properly handles the Gregorian calendar leap-year rules, so 
;1900 and 2100 are not leap years, but 2000 is.
;
;:Params:
;  thisday: in, required
;  nextday: out, required
;-
pro eve_next_yyyydoy, thisday, nextday

if n_params() ne 2 then begin
    print,''
    print,'USAGE: next_yyyydoy, yyyydoy_day, yyyydoy_next'
    print,''
    print,'  next_yyyydoy handles new year boundary and'
    print,'  corrects for all possible leap years. '
endif

year = thisday/1000l           ;4-digit year
doy  = thisday - (year*1000l)  ;day of year
if doy gt 366 then begin
    print,'Day of year greater than 366 is not possible.'
    print,'Returning'
    return
endif

;handle year boundary
if doy eq 365 then begin
    ;is year a leap year? (handles ALL cases)
    if ((year mod 400 eq 0) or ((year mod 4 eq 0) and (year mod 100) ne 0)) $
      then leap_cor = 1l else leap_cor = 0l

    ;correct doy for the extra day in leap years
    if leap_cor eq 1 then doy = 365 else begin
        ;get next year
        year = year + 1l
        doy = 0
    endelse
endif
if doy eq 366 then begin
    ;get next year
    year = year + 1l
    doy = 0
endif

nextday = (year*1000l) + doy + 1l

return
end
