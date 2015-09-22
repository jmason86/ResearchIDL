;docformat = 'rst' 

;+
;Merge EVE Level 2 Lines files into one long EVE record, optionally averaging 
;consecutive measurements
;
;:Params:
;  start_yyyydoy: in, required
;    Start day to use in building the merge. Date is specified as a 7-digit 
;    year/day of year value, for instance 2012/254 Sep 10 is 2012254
;  stop_yyyydoy: in, required
;    End day to use in building the merge. Files up to and including 23:59:59
;    on the day in question are used. If start and stop dates are the same,
;    all files on that day will be included.
;:Keywords:
;  n_average: in, optional
;    If set, each consecutive bundle of n_average measurments are averaged 
;    together. For instance, if n_average is passed in as 6, each consecutive
;    group of 6 10-second measurements is averaged together, resulting
;    in output which is a minute average. 
;  meta: out, optional
;    Metadata from last file that was successfully read
;  verbose: in, optional
;    Set this switch to make this print more results in the IDL log. This is 
;    just passed as-is to functions used internally, as this function produces
;    no log output itself
;  files: out, optional
;    Array of strings, each one being the name of one file which this routine read and merged
;:Categories:
;  user
;-
function eve_merge_evl,start_yyyydoy,stop_yyyydoy,n_average=n_average,meta=meta,verbose=verbose,files=files
  if ~keyword_set(n_average) then n_average=1
  n_average=long(n_average)
  n_days=eve_yd_to_jd(stop_yyyydoy)-eve_yd_to_jd(start_yyyydoy)+1

  for jd=eve_yd_to_jd(start_yyyydoy),eve_yd_to_jd(stop_yyyydoy) do begin
    this_yd=eve_jd_to_yd(jd)
    this_yyyy=this_yd/1000
    this_doy=this_yd mod 1000
    for hour=0,23 do begin
      infn=file_search(string(format='(%"%s/level2/%04d/%03d/EVL_L2_%07d_%02d_*.fit.gz")',getenv("EVE_DATA"),this_yyyy,this_doy,this_yd,hour),count=count)
      if count gt 0 then begin
        l2l=eve_read_whole_fits(infn,verbose=verbose)
        meta={linesmeta:       l2l.linesmeta,         $
              linesmeta_header:l2l.linesmeta_header,  $
              bandsmeta:       l2l.bandsmeta,         $
              bandsmeta_header:l2l.bandsmeta_header,  $
              diodemeta:       l2l.diodemeta,         $
              diodemeta_header:l2l.diodemeta_header,  $
              quadmeta:        l2l.quadmeta,          $
              quadmeta_header: l2l.linesmeta_header,  $
              linesdata_header:l2l.linesdata_header   }
        if n_elements(result) eq 0 then begin
           result=l2l.linesdata 
           files=infn
        endif else begin
           result=[temporary(result),l2l.linesdata]
           files=[temporary(files),infn]
        endelse
      end ;else stop
    end
  end

  if n_average gt 1 then begin
    n_new_result=n_elements(result)/n_average
    new_result=result[0:*:n_average] ;This way we get all the timestamps and stuff
    for i=0L,n_new_result-1 do begin
      new_result[i].line_irradiance =eve_average_no_fill(result[i*n_average:(i+1)*n_average-1].line_irradiance, 2)
      new_result[i].band_irradiance =eve_average_no_fill(result[i*n_average:(i+1)*n_average-1].band_irradiance, 2)
      new_result[i].diode_irradiance=eve_average_no_fill(result[i*n_average:(i+1)*n_average-1].diode_irradiance,2)
      new_result[i].quad_fraction   =eve_average_no_fill(result[i*n_average:(i+1)*n_average-1].quad_fraction,   2)
    end
    ;Not properly combining stdevs, so just fill them
    new_result.diode_stdev=-1
    new_result.quad_stdev=-1

    result=new_result
  end 
  return,result
end

