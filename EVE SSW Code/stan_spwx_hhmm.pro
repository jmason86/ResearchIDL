;docformat = 'rst'
;+
;Example routine demonstrating the routines eve_merge_evs(), stan_bands(), and eve_process_spectrum(). This program
;takes a date as input, then reads the files for that day with eve_merge_evs() wich also is used to compute a 1-minute average. 
;It then calls eve_process_spectrum(), which in turn calls stan_bands() on each of the 1440 spectra for the day.
;Finally it produces a text file report similar in structure to the EVE Level 0CS space weather product, with one row
;for each minute of the day, and one column for each Stan band.
;  
;:Params:
;  yyyydoy: in, required
;    Date to produce Stan Bands report, in yyyydoy format
;:Returns:
;  none, but creates a file megs_stan_bands_yyyydoy.txt in the current directory
;   
;:Categories:
;  example
;-
pro stan_spwx_hhmm,yyyydoy
  set_plot,'z'
  result=dblarr(22,24*60)-1
  l2s=eve_merge_evs(yyyydoy,yyyydoy, n=6) ; Load all spectra for the day and make 1-minute averages
  junk=stan_bands(low=stan_min)
  result=eve_process_spectrum('stan_bands',l2s)

  w=where(result eq 0.0,count)
  if count gt 0 then result[w]=-1

  oufn=string(format='(%"megs_stan_bands_%07d.txt")',yyyydoy)
  print,oufn
  openw,ouf,oufn,/get_lun

  printf,ouf,"; DATA_list: "+file_basename(oufn)
  printf,ouf,"; Created: "+systime(/utc)+" UTC"
  printf,ouf,"; Origin: SDO/EVE Science Processing and Operations Center, LASP/CU"
  printf,ouf,"; Units: ph/s/cm^2"
  printf,ouf,"; Source: SDO-EVE MEGS-A and MEGS-B instruments, http://lasp.colorado.edu/eve/data_access"
  printf,ouf,"; Reference: Solomon, S. C., and L. Qian (2005), 'Solar extreme-ultraviolet irradiance for general circulation models',"
  printf,ouf,";            J. Geophys. Res., 110, A10306, doi:10.1029/2005JA011160"
  printf,ouf,"; Product: Custom 'Stan Bands', produced from EVE Level 2, 1-minute averages"
  printf,ouf,"; Missing data: -1.000e+000"
  printf,ouf,"; Columns: First column is time in HHMM format"
  printf,ouf,";   Each subsequent column is labeled with the lower bound of a Stan band."
  printf,ouf,";   Upper bound is lower bound of next higher band. Upper bound of last band is 105nm."
  printf,ouf,"; Overlapping bands: There are two bands marked 65.0, three at 79.8, and three at 91.3."
  printf,ouf,";   For 65.0, the left column is medium N2 cross section and the right one is high."
  printf,ouf,";   For the others, from left to right they are low, medium, and high."
  column_head=";  YYYYDOY"
  column_head=";     HHMM"
  column_line=";---------"

  for i=0,n_elements(stan_min)-1 do begin
    column_head=column_head+string(format='(%"    %8.2f")',stan_min[i])
    column_line=column_line+'------------'
  end
  printf,ouf,column_head
  printf,ouf,column_line
  printf,ouf,string(format='(%"   %07d")',yyyydoy)
  for i=0,23 do for j=0,59 do begin
    column_line=string(format='(%"      %02d%02d")',i,j)
    for k=0,n_elements(stan_min)-1 do begin
      column_line=column_line+string(format='(%" %11.3e")',result[k,i*60+j])
    end
    printf,ouf,column_line
  end
  free_lun,ouf
end
