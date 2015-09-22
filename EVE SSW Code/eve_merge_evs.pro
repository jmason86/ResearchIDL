;docformat = 'rst' 

;+
;Merge EVE Level 2 spectrum files into one long EVE record, optionally averaging 
;consecutive spectra
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
;    If set, each consecutive bundle of n_average spectra are averaged 
;    together. For instance, if n_average is passed in as 6, each consecutive
;    group of 6 10-second spectra is averaged together, resulting
;    in output which is has minute average spectra. 
;  meta: out, optional
;    Metadata from last file that was successfully read
;  verbose: in, optional
;    Set this switch to make this print more results in the IDL log. This is 
;    just passed as-is to functions used internally, as this function produces
;    no log output itself
;    
;:Returns:
;An array of structures, each of the same form as an L2S spectrum structure
;
;:Categories:
;  user
;-
function eve_merge_evs,start_yyyydoy,stop_yyyydoy,n_average=n_average,meta=meta,verbose=verbose
  if ~keyword_set(n_average) then n_average=1
  n_new_result=360L/n_average
  n_days=eve_yd_to_jd(stop_yyyydoy)-eve_yd_to_jd(start_yyyydoy)+1

  for j=0L,n_days-1 do begin
    jd=eve_yd_to_jd(start_yyyydoy)+j
    this_yd=eve_jd_to_yd(jd)
    this_yyyy=this_yd/1000
    this_doy=this_yd mod 1000
    for hour=0,23 do begin
      
      infn=file_search(string(format='(%"%s/level2/%04d/%03d/EVS_L2_%07d_%02d_*.fit.gz")',getenv("EVE_DATA"),this_yyyy,this_doy,this_yd,hour),count=count)
      if count gt 0 then begin
        l2s=eve_read_whole_fits(infn,verbose=verbose)
        meta={spectrummeta:       l2s.spectrummeta,         $
              spectrummeta_header:l2s.spectrummeta_header,  $
              spectrum_header:    l2s.spectrum_header   }
        this_result=l2s.spectrum
        if n_average gt 1 then begin
          new_result=this_result[0:*:n_average] ;This way we get all the timestamps and stuff
          for i=0,n_new_result-1 do begin
            new_result[i].irradiance =eve_average_no_fill(this_result[i*n_average:(i+1)*n_average-1].irradiance, 2)
          end
          this_result=new_result
        end 
        if n_elements(result) eq 0 then begin
          result=replicate(this_result[0],long(n_days)*24*n_new_result)
        end
        result[(j*24+hour)*n_new_result:(j*24+hour+1)*n_new_result-1]=this_result
      end else begin
        message,"Didn't read spectrum - status code "+string(s)
      end
    end
  end

  return,result
end
