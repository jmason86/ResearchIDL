;+
; NAME:
;   convert_goes_events_netcdf_to_csv
;
; PURPOSE:
;   Ingest the sci_xrsf-l2-flsum_g15_s20100331_e20200304_v1-0-0.nc file from NCEI, which is the GOES/XRS data without the fudge factor, run through their new algorithm for flare identification. 
;   Then put it in a format useful for our C-PhLARE paper. 
;
; INPUTS:
;   None (except the source file)
;
; OPTIONAL INPUTS:
;   None
;
; KEYWORD PARAMETERS:
;   None
;
; OUTPUTS:
;   Formatted csv file to disk
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   None
;
; EXAMPLE:
;   Just run it!
;-
PRO convert_goes_events_netcdf_to_csv

filename = 'sci_xrsf-l2-flsum_g15_s20100331_e20200304_v1-0-0.nc'

read_ncdf, '/Users/masonjp2/Downloads/' + filename, data, attrib=attr

t_start_jd = jpmiso2jd('2000-01-01T12:00:00Z')
days_since_start = data.time / 86400.
time_jd = t_start_jd + days_since_start

flare_peak_indices = where(data.status EQ 'EVENT_PEAK')
flare_class = data.flare_class[flare_peak_indices]
xrsb_flux = data.xrsb_flux[flare_peak_indices]
time_jd = time_jd[flare_peak_indices]

time_indices = where(time_jd GE jpmiso2jd('2011-01-14') AND time_jd LE jpmiso2jd('2018-02-10'))
flare_class = flare_class[time_indices]
xrsb_flux = xrsb_flux[time_indices]
time_jd = time_jd[time_indices]
time_iso = jpmjd2iso(time_jd)

write_csv, filename + '.csv', time_iso, flare_class, xrsb_flux, header=['Time [ISO8601]', 'Flare Class', 'Peak Flux [W/m2]']

END