;docformat = 'rst'
;+
;Generate a text file space weather report in a form similar to that of EVE Level 0CS. Intended
;as an example of how to use the Stan bands code.
;
;:Params:
;  start: in, required
;    Date to start at, in yyyydoy format
;  finish: in, required
;    Date to finish on, in yyyydoy format
;
;:Description:
;  The output is a text file Space Weather report with one line per day, covering the date range specified.
;  The first column is the date in YYYYDOY format, followed by one column for each of the 22 Stan bands.
;-
pro stan_spwx,start,finish
;  set_plot,'z'
  device,decompose=0
  loadct,39
  t=lindgen(finish-start+1)+start
  print,t
  result=dblarr(22,n_elements(t))-1
  for i=0,n_elements(t)-1 do begin
    yyyy=t[i]/1000
    doy=t[i] mod 1000
    fn=string(format='(%"%s/level3/%04d/EVE_L3_%07d_%03d_%02d.fit")',getenv('EVE_DATA'),yyyy,t[i],2,1)
    l3=eve_read_whole_fits(fn)
    print,t[i]

      plot,l3.spectrummeta.wavelength,l3.data.sp_irradiance>0,/ylog,yrange=[1e-7,1e-2]
      result[*,i]=stan_bands(/l2,l3.data.sp_irradiance,low=stan_min,high=stan_max)
      x=[[stan_min],[stan_max]]
      x=reform(transpose(x),44)
      y=[[result[*,i]/(stan_max-stan_min)],[result[*,i]/(stan_max-stan_min)]]
      y=reform(transpose(y),44)
      oplot,x,y,color=254

  end
  w=where(result eq 0.0,count)
  if count gt 0 then result[w]=-1

  oufn=string(format='(%"megs_stan_bands_%07d_%07d.txt")',start,finish)
  print,oufn
  openw,ouf,oufn,/get_lun

  printf,ouf,"; DATA_list: "+file_basename(oufn)
  printf,ouf,"; Created: "+systime(/utc)+" UTC"
  printf,ouf,"; Origin: SDO/EVE Science Processing and Operations Center, LASP/CU"
  printf,ouf,"; Units: ph/s/cm^2"
  printf,ouf,"; Source: SDO-EVE MEGS-A and MEGS-B instruments, http://lasp.colorado.edu/eve/data_access"
  printf,ouf,"; Reference: Solomon, S. C., and L. Qian (2005), 'Solar extreme-ultraviolet irradiance for general circulation models',"
  printf,ouf,";            J. Geophys. Res., 110, A10306, doi:10.1029/2005JA011160"
  printf,ouf,"; Product: Custom 'Stan Bands', produced from EVE Level 3, 1-day averages"
  printf,ouf,"; Missing data: -1.00e+00"
  printf,ouf,"; Columns: First column is date in YYYYDOY format"
  printf,ouf,";   Each subsequent column is labeled with the lower bound of a Stan band."
  printf,ouf,";   Upper bound is lower bound of next higher band. Upper bound of last band is 105nm."
  printf,ouf,"; Overlapping bands: There are two bands marked 65.0, three at 79.8, and three at 91.3."
  printf,ouf,";   For 65.0, the left column is medium N2 cross section and the right one is high."
  printf,ouf,";   For the others, from left to right they are low, medium, and high."
  column_head=";  YYYYDOY"
  column_line=";---------"

  for i=0,n_elements(stan_min)-1 do begin
    column_head=column_head+string(format='(%"    %8.2f")',stan_min[i])
    column_line=column_line+'------------'
  end
  printf,ouf,column_head
  printf,ouf,column_line

  for i=0,n_elements(t)-1 do begin
    column_line=string(format='(%"   %07d")',t[i])
    for j=0,n_elements(stan_min)-1 do begin
      column_line=column_line+string(format='(%" %11.4e")',result[j,i])
    end
    printf,ouf,column_line
  end
  free_lun,ouf
end
