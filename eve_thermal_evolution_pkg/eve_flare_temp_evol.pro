; PURPOSE: To plot time series of 130+ EVE isolated emission lines grouped by
;          temperatures from in delta-log(T)=0.1 bins from 4.5 to 7.2.
;
; OTHER NOTES:
;      The EVE data should be located in this directory path:
;                 ~/EVE/data/level2/YYYY/DOY/
;                        YYYY is the year (e.g. 2010)
;                        DOY is the day of year (e.g. 001 for Jan 1)
; KEYWORDS:
;      megs_smth: Set the wavelength range for boxcar smoothing, in
;                 0.02 nm bin steps (default is no smoothing).  Note
;                 that each 'emission line' is the sum of the peak
;                 wavelength bin plus each +/- 2, 0.02 nm wavelength
;                 bins on each side to get the EVE 0.1 nm spectral resolution.
;      time_smth: Set the time range for boxcar smoothing, in sec
;                 (default is 30 sec - or three, 10-sec EVE
;                 integrations)
;      open_ps: to automatically output a .ps figure to the directory:
;                 ~/EVE/data/analysis/plots/
;      nhrs: set the number of extra hours after the flare hour to plot the
;                 time series (default is 0, so only the single hour
;                 entered is plotted)
;      min_nhrs: set the number of extra hours before the flare hour
;                 to plot the time series (default is 0)
;      abs_mag: plot as the absolute magnitude of the radiance,
;                 (default is all temperature bins normalized from 0
;                 to 1)
;      yr: enter the year of the flare, if not set then will prompt
;                 for the year to be entered in the code
;      month: enter the month of the flare, if month and day are not
;                 set, then it will prompt for it to be entered 
;      day: enter the day of the month of the flare, if month and day
;                 are not set or doy is not set, then it will prompt
;                 for it to be enterd
;      fl_doy: enter the day-of-year of the flare, if not set, or month
;                 and day are not set, it will prompt for it to be entered.
;      fl_hr: enter the peak hour of the flare, if not set it will
;                 prompt for it to be entered
;      abs_log: to plot the absolute magnitude of the flare radiance on a
;                 /ylog axis
;      only_7: plots only 7 arbitrary temperatures bins, with most of
;                 the cooler bins combined.
;      only_2: plots only 2 large temperature bins
;      only_6: plots only 6 temperature bins spaced at deltalog(T)=0.4
;                 K bins.  This is the setting for the Chamberlin et
;                 al paper (in preparation)
;      only_10: plots only 10 temperature bins, where only the cooler
;                 tempeartures are bined 
;      only_12: plots only 12 temperature bins spaced at
;                 deltalog(T)=0.2 K bins.
;      peak_temp: plots diamonds at the maximum point of the times
;                 series for each bin, showing the time of the peak
;                 temperature.
;      st_min: set the start minute for plotting and for the energy
;                 calculation if keyword 'calc_energy' is set
;      end_min: set the end minute for plotting and for the energy
;                 calculation if keyword 'calc_energy' is set
;      use_manual: use the list (can be modified in this code) of
;                 manually entered wavelengths and temperatures
;                 instead of the autmatically found set described in
;                 Chamberlin et al (in preparation)
;      qual_fact: set the quality factor (described in Chamberlin et
;                 al, in prep) to a more isothermal quality (default
;                 is 0.3) 
;      fe_only: only use the Fe emission lines if various ionization
;                 states for the time series plots   
;      version: use a different version of the EVE data (default is 2)
;      calc_energy: calculate the radiated energy in all the different
;                 temperature bins as well as the total 7-37 nm
;                 radiated energy.  This also fits a Gaussian to the
;                 log(T)=4.9 bin (dominated by He II 30.4 nm) to
;                 determine the impulsive phase radiated emissions,
;                 then used the residual as the gradual phase
;                 emissions. This will also calculate the 0.1-7nm
;                 radiated emissions from ESP if the keyword /esp is
;                 set.
;	force_ip_t: fore the peak IP time for computing IP and GP
;	          energies if keyword /calc_energy is set. It must be a string
;             	  in the form 'hh:mm:ss.ms'
;       save_idl: saves the temperature bin times sereis to an IDL
;                 savefile.
;       megsb: add in the MEGS-B emission lines (if MEGS-B was making
;                 measurements during that time)
;       goes_times: use the NOAA-defined GOES start and stop times for
;                 the flare instead of entering using st_min, fl_hr, 
;                 nhrs, and end_min.  NOTE that this will overide any
;                 of those other specified times if set.
;       debug: stops at the end of the code for dubbing
;       esp: plots the EVE/ESP time series for the diodes during the
;                 same time.  This also needs to be set in order to
;                 get radiated energies in the 0.1-7nm range when the
;                 'calc_energies' keyword is set.
;       aia: plots the EVE-derived AIA times series for the given
;                 time/flare.  NOTE that this is NOT the AIA
;                 lightcurves, but what AIA should see if the counts
;                 were integrated over thier FOV.  It is derived by
;                 folding the AIA response funciton with the EVE
;                 spectra to get the derived AIA irradiance
;                 lightcurve.
;       plot_contour: plots a countour 'smear-a-gram' of the time
;                 series plots.  
;       rhessi_em: plots the RHESSI Emission Measure if available and
;                 saved (must get this outside and independently of
;                 this code)   
;       goes: overplots the GOES lightcurve, if available and
;                 independently obtained outside of this code)
;       ip_en_2sig: calculates the impulsive energy width as 2 sigma
;                 of the fit, instead of just the total energy under the IP
;                 gaussian (therefore give energy in the Impulsive phase vs
;                 impulsive emissions)
;       no_sub_cont: don't subtract the underlying continuum emission from the
;                 bound-bound emission enhancements for each line
;
; CALLED PROCEEDURES/FUNCTIONS:
;      independent_color.pro (Not a standard SSW routine)
;      open_ps.pro (Not a standard SSW routine)
;      close_ps.pro (Not a standard SSW routine)
;      ymd_to_yd.pro (Not a standard SSW routine)
;      yd_to_ymd.pro (Not a standard SSW routine)
;      mrdfits.pro
;      read_generic_fits.pro (Not a standard SSW routine)
;      rainbow.pro (Not a standard SSW routine)
;      pr_gev.pro
;      
; RESORED files:
;       eve_isothermal_emissions_nodup.sav
;       eve_isothermal_emissions_wmb.sav (only if /megsb keyword is
;                   set)
;       EVE data
;
; MODIFICATION HISTORY:
;       P. Chamberlin, 18-Aug-2011: Initial Release
;       James Paul Mason, 2012/1/24: Customized paths for personal use
;


pro eve_flare_temp_evol, megs_smth=megs_smth, time_smth=time_smth, ps_out=ps_out, $
	nhrs=nhrs, min_nhrs=min_nhrs, abs_mag=abs_mag, yr=yr, month=month, day=day, fl_doy=fl_doy, fl_hr=fl_hr, $
	abs_log=abs_log, only_7=only_7, only_2=only_2, only_6=only_6, only_10=only_10, only_12=only_12, $
	st_min=st_min, end_min=end_min, use_manual=use_manual, qual_fact=qual_fact, fe_only=fe_only, $
	version=version, calc_energy=calc_energy, force_ip_t=force_ip_t, save_idl=save_idl, megsb=megsb, $
        goes_times=goes_times, debug=debug, esp=esp, aia=aia, plot_contour=plot_contour, $
        rhessi_em=rhessi_em, rhessi_t=rhessi_t, goes=goes, peak_temp=peak_temp, ip_en_2sig=ip_en_2sig, $
        no_sub_cont=no_sub_cont

;add_path, '/Users/jmason86/IDLWorkspace81/Research/eve_thermal_evolution_pkg'
add_path, '/Users/jama6159/Dropbox/IDLWorkspace81/Research/eve_thermal_evolution_pkg/'

if not keyword_set(min_nhrs) then min_nhrs=0 ; number of hours to plot before peak hour
if not keyword_set(nhrs) then nhrs=0 ; number of hours to plot after peak hour
if not keyword_set(megs_smth) then megs_smth=1; 5 ; smooth in wv, bins
if not keyword_set(time_smth) then time_smth=50 ; 240 ; smooth in time, sec
if not keyword_set(qual_fact) then qual_fact=0.3 ; Quality factor from CHIANTI analysis
if not keyword_set(version) then version=2
esp_time_smth=0 ; Now using Level 2 data at 10 sec bins; time_smth*4.
megs_time_smth=time_smth/10.
aia_time_smth=time_smth/10.
thk=2.0


if keyword_set(use_manual) then begin

   wv_plt_wv = [9.39,10.15,10.39,10.83,11.44,11.71,11.74,11.86,11.99,12.18,12.87,13.29,13.57, 18.21,$
	19.20,20.38,20.42,21.13,21.58,21.91,24.91,25.10,25.51,25.63,26.29,26.47,28.36,28.41,29.19,30.37, $
	33.54, 30.57, 29.73, 16.81,6.39, 6.48, 7.20, 7.34, 8.54, 11.79, 13.12, 13.72, 14.05, 24.49, $
	14.27, 14.33, 15.25, 15.66, 15.87,16.48,16.74,16.81, 17.05, 17.10, 17.21, 17.30, 17.39,17.45, $
	17.52,17.72,17.97,18.02,18.18, 18.28, 18.41, 18.45, 18.48, 18.52, 18.68, 19.44, 19.51, 19.59, $
	19.78, 20.09, 20.15, 20.20, 20.38, 20.72, 20.86, 20.93, 21.21, 21.41, 21.69, 22.31, 23.24, 23.51, $
	23.95, 23.73,23.78,23.85,24.30, $ ;]
	45.18,45.22,53.70,53.81,54.11,55.33,55.39,55.40,55.45,58.43,59.66,59.95,62.49, $
	62.58,64.51,90.4,91.27,91.33]
   wv_plt_temp = [6.9,7.0,6.9,7.0,7.1,7.1,7.1,7.1,7.0,7.1,7.1,7.2,7.1, 6.2,$
	7.2,6.3,6.2,6.3,6.8,6.3,6.8,6.8,7.2,4.9,6.8,6.3,5.1,6.4,6.9,4.9, $
	6.8, 5.1, 5.2, 5.7,6.7, 6.7, 5.8,6.7, 5.8, 7.2, 5.7, 6.7, 6.7, 5.9, $
	5.5, 5.5,5.5,6.6,6.1,6.6,5.7,5.7,6.4, 5.9, 5.4,5.5,5.9,6.1,$
	6.1,6.1,6.2,6.2,6.7,6.7, 5.5, 6.1, 6.2, 5.7, 6.2, 6.6, 6.2, 5.7, $
	6.0, 6.7, 6.2, 6.3, 6.3, 5.2, 6.8, 5.3, 6.4, 5.2, 5.9,6.5,6.5, 5.6, $
	6.5, 4.9, 6.6, 5.2, 4.9, $; ]
	4.9,4.9,4.5,4.9,5.2,5.2,5.6,5.6,5.2,4.5,5.1,4.9,6.8, $
	5.2,4.7,4.6,4.5,5.0]
endif else begin  ; Use the emissions determination method described by Chamberlin et al. (in prep)
 	restore, 'eve_isothermal_emissions_nodup.sav' ; Made in 'eve_megs_ts_chianti_get_iso.pro'; 
	if keyword_set(megsb) then restore, 'eve_isothermal_emissions_wmb.sav' ; add addition of MEGS-B emissions if available
	if keyword_set(fe_only) then begin
		ion_info_el=strmid(ion_info,5,2)
		gd_iso=where(ion_info_el eq 'Fe')
	endif else begin
		gd_iso=where(blend_frac le qual_fact) ; only use emissions less than the specified quality factor
	endelse
	wv_plt_wv=wv_ar[gd_iso]
	wv_plt_temp=temp_ar[gd_iso]
endelse

wv_plt=transpose([[wv_plt_wv],[wv_plt_temp]])


;
; Enter in the date and time of the flare if not entered as keywords
;   Can enter as day-of-year or month day
;
if not keyword_set(yr) then read, yr, prompt='Enter Flare Year: '
yr=fix(yr)
if not keyword_set(fl_doy) and not keyword_set(month) then begin
    read, fl_doy, prompt='Enter Flare Day of Year (or 0 if you would rather enter Month and Day): '
    if fl_doy le 0 then begin
            read, month, prompt='Enter Flare Month (numerically): '
            read, day, prompt='Enter Flare Day: '
    endif
endif
if not keyword_set(fl_hr) then read, fl_hr, prompt='Enter Flare start hour: '
fl_min_max=30
; Convert date to ydoy and ymd as both are needed
if fl_doy le 0 then begin
         ymd_to_yd, yr, month, day, yyyydoy
         fl_doy=strmid(strtrim(yyyydoy,2),4,3)
endif else begin
         yyyydoy=yr*1000l+fl_doy
         yd_to_ymd, yyyydoy, year, month, day
endelse
ymd=yr*10000l+month*100l+day

	
; Make sure day-of-year as a string is 3 characters
if fl_doy lt 10 then begin
	fl_doy='00'+strmid(strtrim(fl_doy,2),0,1)
endif else if fl_doy lt 100 then begin
	fl_doy='0'+strmid(strtrim(fl_doy,2),0,2)
endif else begin
	fl_doy=strmid(strtrim(fl_doy,2),0,3)
endelse

; Get DD-MMM-YY Formated date
case month of
      1: jst_date=strtrim(day,2)+'-jan-'+strmid(strtrim(yr,2),2,2)
      2: jst_date=strtrim(day,2)+'-feb-'+strmid(strtrim(yr,2),2,2)
      3: jst_date=strtrim(day,2)+'-mar-'+strmid(strtrim(yr,2),2,2)
      4: jst_date=strtrim(day,2)+'-apr-'+strmid(strtrim(yr,2),2,2)
      5: jst_date=strtrim(day,2)+'-may-'+strmid(strtrim(yr,2),2,2)
      6: jst_date=strtrim(day,2)+'-jun-'+strmid(strtrim(yr,2),2,2)
      7: jst_date=strtrim(day,2)+'-jul-'+strmid(strtrim(yr,2),2,2)
      8: jst_date=strtrim(day,2)+'-aug-'+strmid(strtrim(yr,2),2,2)
      9: jst_date=strtrim(day,2)+'-sep-'+strmid(strtrim(yr,2),2,2)
      10: jst_date=strtrim(day,2)+'-oct-'+strmid(strtrim(yr,2),2,2)
      11: jst_date=strtrim(day,2)+'-nov-'+strmid(strtrim(yr,2),2,2)
      12: jst_date=strtrim(day,2)+'-dec-'+strmid(strtrim(yr,2),2,2)
endcase
;
; Flare times given by NOAA SWPC if keyword /use_goes is set - nice
;   to standardize radiated energy values with /calc_energy keyword
;
if keyword_set(goes_times) then begin
   pr_gev, jst_date, outfile='temp_prgev.dat'
   openr, unit, 'temp_prgev.dat', /get_lun
   line=''
   goodline=''
   while ~ EOF(unit) do begin     
        readf, unit, line
        pr_st_hr=strmid(line,11,2)
        if pr_st_hr eq fl_hr then begin
           goodline=line
        endif
     endwhile
   if strlen(goodline) eq 0 then begin ; If didn't find the hour for startime, look at peak time
      free_lun, unit
      openr, unit, 'temp_prgev.dat', /get_lun
      line=''
      while ~ EOF(unit) do begin     
        readf, unit, line
        pr_st_hr=strmid(line,17,2)
        if pr_st_hr eq fl_hr then begin
           goodline=line
        endif
     endwhile
   endif
   free_lun, unit

   ; If no GOES flare is found, then abort
   if strlen(goodline) eq 0 then begin
      print, 'No GOES file found for given day/hour, aborting...'
      goto, endpro
   endif

   ; Define the start hr/min and the end hr/min
   ;   Note that this will also overide
   ;   other st_min, end_min, fl_hr, and nhrs keywords
   st_hr=fix(strmid(goodline,11,2))
   end_hr=fix(strmid(goodline,23,2))
   nhrs=end_hr-st_hr
   st_min=fix(strmid(goodline,14,2))
   end_min=fix(strmid(goodline,26,2))
   fl_hr=st_hr
endif
; Make sure hour as a string is 2 characters
if fl_hr lt 10 then begin
	fl_hr='0'+strmid(strtrim(fl_hr,2),0,1)
endif else begin
	fl_hr=strmid(strtrim(fl_hr,2),0,2)
endelse 
fl_hr1=fl_hr

if keyword_set(esp) or keyword_set(aia) then begin

;
;  ESP + MEGS-P  -  Note that ESP data in Level 2 is 10 second average data, use level 1 to get 0.25 sec data
;
; Read ESP and other 'line' data 
flnm='~/EVE/data/level2/'+strtrim(yr,2)+'/'+fl_doy+'/EVL_L2_'+strtrim(yr,2)+fl_doy+'_'+fl_hr+'_00'+strtrim(fix(version),2)+'_01.fit.gz'
; Check to see if files exist. If not, it will download both the EVL*
; and EVS* files (needed later)
get_eve_data, timerange=[jst_date+' '+fl_hr+':00:00',jst_date+' '+strtrim(fl_hr+nhrs,2)+':00:00'], year=yr, doy=fl_doy 
esp_data_info=mrdfits(flnm,3,/unsigned) ;ESPQ      ESP171    ESP257    ESP304    ESP366    MEGSP1216
diode_data=mrdfits(flnm,5,/unsigned)
if (keyword_set(st_min) and not keyword_set(end_min)) then begin	
		gd_esp=where(diode_data.sod gt 0.0 and diode_data.diode_irradiance[0] lt 1.0 and diode_data.diode_irradiance gt 0.0 $
			and diode_data.diode_irradiance[4] gt 0.0 and diode_data.sod ge st_min*60.+fl_hr*3600.)
endif else if (keyword_set(end_min) and not keyword_set(st_min) and not keyword_set(nhrs)) then begin
		gd_esp=where(diode_data.sod gt 0.0 and diode_data.diode_irradiance[0] lt 1.0 and diode_data.diode_irradiance gt 0.0 $
			and diode_data.diode_irradiance[4] gt 0.0 and diode_data.sod le end_min*60.+fl_hr*3600.)
endif else if (keyword_set(end_min) and keyword_set(st_min) and not keyword_set(nhrs)) then begin
		gd_esp=where(diode_data.sod gt 0.0 and diode_data.diode_irradiance[0] lt 1.0 and diode_data.diode_irradiance gt 0.0 $
			and diode_data.diode_irradiance[4] gt 0.0 and diode_data.sod le end_min*60.+fl_hr*3600. $
			and diode_data.sod ge st_min*60.+fl_hr*3600.)
endif else if (keyword_set(end_min) and keyword_set(st_min) and keyword_set(nhrs)) then begin
		gd_esp=where(diode_data.sod gt 0.0 and diode_data.diode_irradiance[0] lt 1.0 and diode_data.diode_irradiance gt 0.0 $
			and diode_data.diode_irradiance[4] gt 0.0 and diode_data.sod ge st_min*60.+fl_hr*3600.)
endif else begin
		gd_esp=where(diode_data.sod gt 0.0 and diode_data.diode_irradiance[0] lt 1.0 and diode_data.diode_irradiance gt 0.0 $
			and diode_data.diode_irradiance[4] gt 0.0)
endelse
esp_quad=diode_data[gd_esp].diode_irradiance[0]
esp_18=diode_data[gd_esp].diode_irradiance[1]
esp_26=diode_data[gd_esp].diode_irradiance[2]
esp_30=diode_data[gd_esp].diode_irradiance[3]
esp_36=diode_data[gd_esp].diode_irradiance[4]
;esp_dark=esp_data[gd_esp].ch_d
esp_sod=diode_data[gd_esp].sod
esp_tai=diode_data[gd_esp].tai
megsp_121=diode_data[gd_esp].diode_irradiance[5]

endif ; with keyword_set(esp)

if keyword_set(aia) then begin
;
;  AIA data lightcurves
;
aia_data=mrdfits(flnm,2,/unsigned) ; AIA_A94  AIA_A133 AIA_A171 AIA_A195 AIA_A211 AIA_W304 AIA_W335
aia_094=diode_data[gd_esp].band_irradiance[0]
aia_133=diode_data[gd_esp].band_irradiance[1]
aia_171=diode_data[gd_esp].band_irradiance[2]
aia_195=diode_data[gd_esp].band_irradiance[3]
aia_211=diode_data[gd_esp].band_irradiance[4]
aia_304=diode_data[gd_esp].band_irradiance[5]
aia_335=diode_data[gd_esp].band_irradiance[6]
endif ; with keyword_set(aia)

;
;  Read RHESSI EM and Temp data, if available
;
rhessi_yes=0
a=findfile(strtrim(ymd,2)+'*')
if strlen(a) gt 1 and (keyword_set(rhessi_t) or keyword_set(rhessi_em)) then begin
	rhessi_yes=1
	restore, a[0]
	gps_to_utc, time, 13, 2010, 127, timeutc, mnth, dy, hh, mm, ss
	rhessi_time=timeutc/3600.
	rhessi_temp=temp*11.6
	rhessi_em=em
endif
	

;
;  MEGS Data
;
; Read MEGS Level 2 Spectrum data (EVS)
dataLoc='/Users/jmason86/Documents/Research/Data/EVE/'
flnm=dataLoc+'EVS_L2_'+strtrim(yr,2)+fl_doy+'_'+fl_hr+'_00'+strtrim(fix(version),2)+'_01.fit.gz' ; JPM: Customized to personal local directory

if version eq 1 then begin 
	data1=mrdfits(flnm,1,hdr,/unsigned) 
	wave=data1.wavelength 
endif else begin
	data1=read_generic_fits(flnm)
	wave=data1.spectrummeta.wavelength
endelse

if version eq 1 then data2=mrdfits(flnm,2,hdr,/unsigned) else data2=mrdfits(flnm,3,hdr,/unsigned)
if (keyword_set(st_min) and not keyword_set(end_min)) then begin
	gd_times=where(data2.sod ge st_min*60.+fl_hr*3600.)
endif else if (keyword_set(end_min) and not keyword_set(st_min) and not keyword_set(nhrs)) then begin
	gd_times=where(data2.sod ge 0.0 and data2.sod le end_min*60.+fl_hr*3600.)
endif else if (keyword_set(end_min) and keyword_set(st_min) and not keyword_set(nhrs)) then begin
	gd_times=where(data2.sod ge st_min*60.+fl_hr*3600. and data2.sod le end_min*60.+fl_hr*3600.)
endif else if (keyword_set(end_min) and keyword_set(st_min) and keyword_set(nhrs)) then begin
	gd_times=where(data2.sod ge st_min*60.+fl_hr*3600.)
endif else begin
	gd_times=where(data2.sod ge 0.0)
endelse

spect=transpose(data2[gd_times].irradiance)
time=[data2[gd_times].tai]

;
; Get the data for number of hours (nhrs) after the peak hour
;
dy_bnd_fact=0.0
for b=1,nhrs do begin
	;
	;  EVE Data +nth hour
	;
	fl_hr=fl_hr+1
	if fl_hr eq 24 then begin
		fl_doy=fl_doy+1
		dy_bnd_fact=dy_bnd_fact+86400.
		if fl_doy lt 10 then begin
			fl_doy='00'+strmid(strtrim(fl_doy,2),0,1)
		endif else if fl_doy lt 100 then begin
			fl_doy='0'+strmid(strtrim(fl_doy,2),0,2)
		endif else begin
			fl_doy=strmid(strtrim(fl_doy,2),0,3)
		endelse
		fl_hr='00'
	endif
	if fl_hr lt 10 then begin
		fl_hr='0'+strmid(strtrim(fl_hr,2),0,1)
	endif else begin
		fl_hr=strmid(strtrim(fl_hr,2),0,2)
	endelse 
	;
	; Read MEGS Level 2 data 
	flnm='~/EVE/data/level2/'+strtrim(yr,2)+'/'+fl_doy+'/EVS_L2_'+strtrim(yr,2)+fl_doy+'_'+fl_hr+'_00'+strtrim(fix(version),2)+'_01.fit.gz'
	if version eq 1 then data2=mrdfits(flnm,2,hdr,/unsigned) else data2=mrdfits(flnm,3,hdr,/unsigned)
	if b eq nhrs and keyword_set(end_min) then gd_times=where(data2.sod ge 0.0 and data2.sod le end_min*60.+fl_hr*3600.) $
		else gd_times=where(data2.sod ge 0.0)

	;
	; Put together all of the hours of data
	;
	spect=[spect,transpose(data2[gd_times].irradiance)]
	time=[time,data2[gd_times].tai]

        if keyword_set(esp) or keyword_set(aia) then begin
	;
	;  ESP + MEGS-P  -  Note that ESP data in Level 2 is 10 second average data, use level 1 to get 0.25 sec data
	;
	; Read ESP and other 'line' data 
	flnm='~/EVE/data/level2/'+strtrim(yr,2)+'/'+fl_doy+'/EVL_L2_'+strtrim(yr,2)+fl_doy+'_'+fl_hr+'_00'+strtrim(fix(version),2)+'_01.fit.gz'
	esp_data_info=mrdfits(flnm,3,/unsigned) ;ESPQ      ESP171    ESP257    ESP304    ESP366    MEGSP1216
	diode_data=mrdfits(flnm,5,/unsigned)
	if b eq nhrs and keyword_set(end_min) then begin
		gd_esp=where(diode_data.sod gt 0.0 and diode_data.diode_irradiance[0] lt 1.0 and diode_data.diode_irradiance[0] gt 0.0 $
			and diode_data.diode_irradiance[4] gt 0.0 and diode_data.sod le end_min*60.+fl_hr*3600.)
	endif else begin
		gd_esp=where(diode_data.sod gt 0.0 and diode_data.diode_irradiance[0] lt 1.0 and diode_data.diode_irradiance[0] gt 0.0 $
			and diode_data.diode_irradiance[4] gt 0.0)
	endelse
	esp_quad=[esp_quad,diode_data[gd_esp].diode_irradiance[0]]
	esp_18=[esp_18,diode_data[gd_esp].diode_irradiance[1]]
	esp_26=[esp_26,diode_data[gd_esp].diode_irradiance[2]]
	esp_30=[esp_30,diode_data[gd_esp].diode_irradiance[3]]
	esp_36=[esp_36,diode_data[gd_esp].diode_irradiance[4]]
	esp_sod=[esp_sod,diode_data[gd_esp].sod]
	esp_tai=[esp_tai,diode_data[gd_esp].tai]
	megsp_121=[megsp_121,diode_data[gd_esp].diode_irradiance[5]]
        endif

        if keyword_set(aia) then begin
	;
	;  AIA data lightcurves
	;
	aia_data=mrdfits(flnm,2,/unsigned) ; AIA_A94  AIA_A133 AIA_A171 AIA_A195 AIA_A211 AIA_W304 AIA_W335
	aia_094=[aia_094,diode_data.band_irradiance[0]]
	aia_133=[aia_133,diode_data.band_irradiance[1]]
	aia_171=[aia_171,diode_data.band_irradiance[2]]
	aia_195=[aia_195,diode_data.band_irradiance[3]]
	aia_211=[aia_211,diode_data.band_irradiance[4]]
	aia_304=[aia_304,diode_data.band_irradiance[5]]
	aia_335=[aia_335,diode_data.band_irradiance[6]]
        endif
endfor

;
; Get the data for number of hours before peak hour (min_nhrs)
;
fl_hr=fl_hr1
for b=1,min_nhrs do begin
	;
	;  EVE Data -nth hour
	;
	fl_hr=fl_hr-1
	if fl_hr eq -1 then begin
		fl_doy=fl_doy-1
		if fl_doy lt 10 then begin
			fl_doy='00'+strmid(strtrim(fl_doy,2),0,1)
		endif else if fl_doy lt 100 then begin
			fl_doy='0'+strmid(strtrim(fl_doy,2),0,2)
		endif else begin
			fl_doy=strmid(strtrim(fl_doy,2),0,3)
		endelse
		fl_hr=23
	endif
	if fl_hr lt 10 then begin
		fl_hr='0'+strmid(strtrim(fl_hr,2),0,1)
	endif else begin
		fl_hr=strmid(strtrim(fl_hr,2),0,2)
	endelse 
	;
	; Read MEGS Level 2 data 
	flnm='~/EVE/data/level2/'+strtrim(yr,2)+'/'+fl_doy+'/EVS_L2_'+strtrim(yr,2)+fl_doy+'_'+fl_hr+'_00'+strtrim(fix(version),2)+'_01.fit.gz'
	if version eq 1 then data2=mrdfits(flnm,2,hdr,/unsigned) else data2=mrdfits(flnm,3,hdr,/unsigned)
	gd_times=where(data2.sod ge 0.0)

	;
	; Put together all of the hours of data
	;
	spect=[transpose(data2[gd_times].irradiance),spect]
	time=[data2[gd_times].tai,time]

endfor

; Calculate the integrated 6.5-7.0 nm time series energy
spect_tmp=spect>0.0
int_time_series=total(spect,2,/double)


if keyword_set(esp) then begin
;
; Normalize ESP Spectra
;
; 	Quad
tmp_sp=esp_quad
gd_ir=where(tmp_sp gt 0.0)
tmp_sp=tmp_sp[gd_ir]
tmp_sp=tmp_sp-min(tmp_sp) ; (0.98*median(tmp_sp)); 
if not keyword_set(abs_mag) then tmp_sp=tmp_sp/max(tmp_sp)
esp_quad=tmp_sp
; 	18 nm
tmp_sp=esp_18
tmp_sp=tmp_sp[gd_ir]
gd_ir_sm=where(tmp_sp gt 0.0)
tmp_sp=tmp_sp-min(tmp_sp[gd_ir_sm]) ; (0.98*median(tmp_sp)); 
if not keyword_set(abs_mag) then tmp_sp=tmp_sp/max(tmp_sp)
esp_18=tmp_sp
; 	26 nm
tmp_sp=esp_26
tmp_sp=tmp_sp[gd_ir]
gd_ir_sm=where(tmp_sp gt 0.0)
tmp_sp=tmp_sp-min(tmp_sp[gd_ir_sm]) ; (0.98*median(tmp_sp)); 
if not keyword_set(abs_mag) then tmp_sp=tmp_sp/max(tmp_sp)
esp_26=tmp_sp
; 	30 nm
tmp_sp=esp_30
tmp_sp=tmp_sp[gd_ir]
gd_ir_sm=where(tmp_sp gt 0.0)
tmp_sp=tmp_sp-min(tmp_sp[gd_ir_sm]) ; (0.98*median(tmp_sp)); 
if not keyword_set(abs_mag) then tmp_sp=tmp_sp/max(tmp_sp)
esp_30=tmp_sp
; 	36 nm
tmp_sp=esp_36
tmp_sp=tmp_sp[gd_ir]
gd_ir_sm=where(tmp_sp gt 0.0)
tmp_sp=tmp_sp-min(tmp_sp[gd_ir_sm]) ; (0.98*median(tmp_sp)); 
if not keyword_set(abs_mag) then tmp_sp=tmp_sp/max(tmp_sp)
esp_36=tmp_sp

esp_sod=esp_sod[gd_ir]
esp_tai=esp_tai[gd_ir]

endif ; with keyword_set(esp)

if keyword_set(aia) then begin
;
; Normalize AIA Spectra
;
; 	094
tmp_sp=smooth(aia_094, aia_time_smth, /edge_truncate)
gd_ir=where(tmp_sp gt 0.0 and esp_sod ge fl_hr1*3600. and esp_sod le (fl_hr1+1.+nhrs)*3600.)
tmp_sp=tmp_sp[gd_ir]
tmp_sp=tmp_sp-min(tmp_sp) ; (0.98*median(tmp_sp)); 
if not keyword_set(abs_mag) then tmp_sp=tmp_sp/max(tmp_sp)
aia_094=tmp_sp
; 	133
tmp_sp=smooth(aia_133, aia_time_smth, /edge_truncate)
gd_ir=where(tmp_sp gt 0.0 and esp_sod ge fl_hr1*3600. and esp_sod le (fl_hr1+1.+nhrs)*3600.)
tmp_sp=tmp_sp[gd_ir]
tmp_sp=tmp_sp-min(tmp_sp) ; (0.98*median(tmp_sp)); 
if not keyword_set(abs_mag) then tmp_sp=tmp_sp/max(tmp_sp)
aia_133=tmp_sp
; 	171
tmp_sp=smooth(aia_171, aia_time_smth, /edge_truncate)
gd_ir=where(tmp_sp gt 0.0 and esp_sod ge fl_hr1*3600. and esp_sod le (fl_hr1+1.+nhrs)*3600.)
tmp_sp=tmp_sp[gd_ir]
tmp_sp=tmp_sp-min(tmp_sp) ; (0.98*median(tmp_sp)); 
if not keyword_set(abs_mag) then tmp_sp=tmp_sp/max(tmp_sp)
aia_171=tmp_sp
; 	195
tmp_sp=smooth(aia_195, aia_time_smth, /edge_truncate)
gd_ir=where(tmp_sp gt 0.0 and esp_sod ge fl_hr1*3600. and esp_sod le (fl_hr1+1.+nhrs)*3600.)
tmp_sp=tmp_sp[gd_ir]
tmp_sp=tmp_sp-min(tmp_sp) ; (0.98*median(tmp_sp)); 
if not keyword_set(abs_mag) then tmp_sp=tmp_sp/max(tmp_sp)
aia_195=tmp_sp
; 	211
tmp_sp=smooth(aia_211, aia_time_smth, /edge_truncate)
gd_ir=where(tmp_sp gt 0.0 and esp_sod ge fl_hr1*3600. and esp_sod le (fl_hr1+1.+nhrs)*3600.)
tmp_sp=tmp_sp[gd_ir]
tmp_sp=tmp_sp-min(tmp_sp) ; (0.98*median(tmp_sp)); 
if not keyword_set(abs_mag) then tmp_sp=tmp_sp/max(tmp_sp)
aia_211=tmp_sp
; 	304
tmp_sp=smooth(aia_304, aia_time_smth, /edge_truncate)
gd_ir=where(tmp_sp gt 0.0 and esp_sod ge fl_hr1*3600. and esp_sod le (fl_hr1+1.+nhrs)*3600.)
tmp_sp=tmp_sp[gd_ir]
tmp_sp=tmp_sp-min(tmp_sp) ; (0.98*median(tmp_sp)); 
if not keyword_set(abs_mag) then tmp_sp=tmp_sp/max(tmp_sp)
aia_304=tmp_sp
; 	335
tmp_sp=smooth(aia_335, aia_time_smth, /edge_truncate)
gd_ir=where(tmp_sp gt 0.0 and esp_sod ge fl_hr1*3600. and esp_sod le (fl_hr1+1.+nhrs)*3600.)
tmp_sp=tmp_sp[gd_ir]
tmp_sp=tmp_sp-min(tmp_sp) ; (0.98*median(tmp_sp)); 
if not keyword_set(abs_mag) then tmp_sp=tmp_sp/max(tmp_sp)
aia_335=tmp_sp

endif ; with keyword_set(aia)

cc=independent_color()

; Interpolate bad megs data then smooth
nwv=n_elements(spect[0,*])
ntimes=n_elements(spect[*,0])
for j=0,nwv-1 do begin
	bd_data=where(spect[*,j] le 0.0)
	if bd_data[0] ne -1 then begin
		spect[bd_data,j]=(spect[bd_data-2,j]+spect[bd_data+2,j])/2.
	endif 
endfor

spect=smooth(spect,[megs_time_smth, megs_smth], /edge_truncate) ; NOTE: default has no smooth in spectral dimension, 30 seconds in time
                                                                ; JPM: /edge changed to /edge_truncate (believe IDL 8 included additional edge keywords)


;
; Subtract off the pre-flare background, and normalize the spectra to range from 0-1 if the keyword /abs_mag is NOT set
;
nplt_wv=n_elements(wv_plt[0,*])
plt_sp_ts=fltarr(ntimes,nplt_wv)
nel_spect=n_elements(spect[*,0])
if nel_spect lt 35 then num_smth=nel_spect*0.75 else num_smth=30;0

; Subtract the pre-flare integrated value from the integrated time series
int_time_series=int_time_series-min(smooth(int_time_series,num_smth,/edge_truncate)) ; JPM: using /edge_truncate to account for additional edge keywords in IDL 8

; Find the total energy for each wavlength
for k=0,nplt_wv-1 do begin
	gd_wv=where(wave ge wv_plt[0,k])
        if not keyword_set(no_sub_cont) then begin ; subtract off the underlying contiuum enhancements to get the pure line enhancements only
           cont_spect=fltarr(ntimes,5)
           for t=0,ntimes-1 do begin
              yb=min(spect[t,gd_wv[0]-12:gd_wv[0]-3],wminb) ; irradiance value for the blue side continuum (minimum value for 1 nm to the left)
              yr=min(spect[t,gd_wv[0]+3:gd_wv[0]+12],wminr) ; irradiance value for the red side continuum (minimum value for 1 nm to the right)
              min_sp_timet_b=min(spect[t,gd_wv[0]-2:gd_wv[0]])
              min_sp_timet_r=min(spect[t,gd_wv[0]:gd_wv[0]+2])
              if yb gt min_sp_timet_b then yb=min_sp_timet_b ; if no strong preflare emission (e.g for T>3MK), just use min
              if yr gt min_sp_timet_r then yr=min_sp_timet_r
              fit_cont=linfit([wave[gd_wv[0]-12+wminb],wave[gd_wv[0]+3+wminr]],[yb,yr])
              cont_spect[t,*]=fit_cont[0]+wave[gd_wv[0]-2:gd_wv[0]+2]*fit_cont[1]
              ;stop
           endfor
           tmp_sp=total(spect[*,gd_wv[0]-2:gd_wv[0]+2]-cont_spect,2) ; subtract off the continua contributions then total
        endif else begin
           tmp_sp=total(spect[*,gd_wv[0]-2:gd_wv[0]+2],2) 
        endelse
	tmp_sp=tmp_sp*0.2; Units still in W/m^2/nm , but new bin size (/nm) is 0.1 nm; 0.02nm/0.1nm=0.2
	gd_ir=where(tmp_sp gt 0.0)
	if gd_ir[0] ne -1 then begin
		tmp_sp=tmp_sp-min(smooth(tmp_sp[gd_ir],num_smth,/edge_truncate)) ; (0.98*median(tmp_sp)); (0.98*median(tmp_sp)); 
		                                                                 ; JPM: using /edge_truncate to account for additional edge keywords in IDL 8 
		if not keyword_set(abs_mag) then tmp_sp=tmp_sp/max(tmp_sp) ;max(smooth(tmp_sp[gd_ir],180,/edge))
	endif
	plt_sp_ts[*,k]=tmp_sp
        ;stop
endfor

if keyword_set(calc_energy) then begin ; Subtract of background for all wavelengths to get total IP and GP energies in the EVE range
	; MEGS Data (already done for ESP)
	plt_sp_ts_all=spect*0.0
	for k=0,nwv-1 do begin
		tmp_sp=reform(spect[*,k])
		gd_ir=where(tmp_sp gt 0.0)
		if gd_ir[0] ne -1 then begin
			if n_elements(gd_ir) gt num_smth then begin
                           tmp_sp=tmp_sp-min(smooth(tmp_sp[gd_ir],num_smth,/edge_truncate)) ; (0.98*median(tmp_sp)); (0.98*median(tmp_sp)); 
                                                                                            ; JPM: using /edge_truncate to account for additional edge keywords in IDL 8
                        endif else begin
                           tmp_sp=tmp_sp-min(smooth(tmp_sp[gd_ir],n_elements(gd_ir)*0.5,/edge_truncate)) ; (0.98*median(tmp_sp)); (0.98*median(tmp_sp));                          
                        endelse
			if not keyword_set(abs_mag) then tmp_sp=tmp_sp/max(tmp_sp) ;max(smooth(tmp_sp[gd_ir],180,/edge))
		endif
		plt_sp_ts_all[*,k]=tmp_sp
	endfor
endif

plt_sp_orig=plt_sp_ts

;
; Find the average spectra for each log(T)=0.1 K bin from log(T) range of 4.5 to 7.3 K
;
temp_dec_ar=findgen(73-45)*0.1+4.5
ntmps=n_elements(temp_dec_ar)
temp_dec_sp=fltarr(ntimes,ntmps)
nlines_per_bin=intarr(ntmps)
for z=0,ntmps-1 do begin
	gdtmps=where(wv_plt[1,*] ge temp_dec_ar[z]-0.05 and wv_plt[1,*] lt temp_dec_ar[z]+0.05 and median(plt_sp_ts,dimension=1) gt 0.0)
	if gdtmps[0] ne -1 then begin
                nlines_per_bin[z]=n_elements(gdtmps)
		if n_elements(gdtmps) gt 1 then begin
			if keyword_set(abs_mag) then begin
				;stop
				temp_dec_sp[*,z]=total(plt_sp_ts[*,gdtmps],2) ; Don't divide to conserve energy for absolute magnitude calc
			endif else begin
				temp_dec_sp[*,z]=total(plt_sp_ts[*,gdtmps],2)/n_elements(gdtmps) ; Divide to keep normalized
			endelse
		endif else begin
			temp_dec_sp[*,z]=plt_sp_ts[*,gdtmps]
		endelse
	endif
endfor
plt_sp_ts=temp_dec_sp
nplt_wv=n_elements(temp_dec_ar)
wv_plt=fltarr(2,nplt_wv)
wv_plt[1,*]=temp_dec_ar		

;
; Bin and plot only 7 temperatures if keyword /only_7 is set
;   
if keyword_set(only_7) then begin
	temp_dec_ar=[4.9, $ ; 4.5-5.0; 0:5
		5.3, $ ; 5.1-5.7; 6:12
		5.9, $ ; 5.8-6.1; 13:16
		6.3, $ ; 6.2-6.4; 17:19
		6.7, $ ; 6.5-6.8; 20:23
		6.9, $ ; 6.9-7.0; 24:25
		7.1] ; 7.1-7.2; 26:27
	plt_sp_ts_tmp=fltarr(n_elements(reform(plt_sp_ts[*,0])),7)	
	ntmps=7
        nlines_per_temp_bin=intarr(ntmps)
	if keyword_set(abs_mag) then begin
		plt_sp_ts_tmp[*,0]=total(plt_sp_ts[*,0:5], 2)-min(smooth(total(plt_sp_ts[*,0:5],2)>0.0,num_smth,/edge_truncate)) 
		plt_sp_ts_tmp[*,1]=total(plt_sp_ts[*,6:12], 2)-min(smooth(total(plt_sp_ts[*,6:12],2)>0.0,num_smth,/edge_truncate)) 
		plt_sp_ts_tmp[*,2]=total(plt_sp_ts[*,13:16], 2)-min(smooth(total(plt_sp_ts[*,13:16],2)>0.0,num_smth,/edge_truncate)) 
		plt_sp_ts_tmp[*,3]=total(plt_sp_ts[*,17:19], 2)-min(smooth(total(plt_sp_ts[*,17:19],2)>0.0,num_smth,/edge_truncate)) 
		plt_sp_ts_tmp[*,4]=total(plt_sp_ts[*,20:23], 2)-min(smooth(total(plt_sp_ts[*,20:23],2)>0.0,num_smth,/edge_truncate)) 
		plt_sp_ts_tmp[*,5]=total(plt_sp_ts[*,24:25], 2)-min(smooth(total(plt_sp_ts[*,24:25],2)>0.0,num_smth,/edge_truncate)) 
		plt_sp_ts_tmp[*,6]=total(plt_sp_ts[*,26:27], 2)-min(smooth(total(plt_sp_ts[*,26:27],2)>0.0,num_smth,/edge_truncate)) 
	endif else begin		
		plt_sp_ts_tmp[*,0]=total(plt_sp_ts[*,0:5], 2)/6.
		plt_sp_ts_tmp[*,1]=total(plt_sp_ts[*,6:12], 2)/6.
		plt_sp_ts_tmp[*,2]=total(plt_sp_ts[*,13:16], 2)/5.
		plt_sp_ts_tmp[*,3]=total(plt_sp_ts[*,17:19], 2)/3.
		plt_sp_ts_tmp[*,4]=total(plt_sp_ts[*,20:23], 2)/4.
		plt_sp_ts_tmp[*,5]=total(plt_sp_ts[*,24:25], 2)/2.
		plt_sp_ts_tmp[*,6]=total(plt_sp_ts[*,26:27], 2)/2.
        endelse
        nlines_per_temp_bin[0]=total(nlines_per_bin[0:5])
        nlines_per_temp_bin[1]=total(nlines_per_bin[6:12])
        nlines_per_temp_bin[2]=total(nlines_per_bin[13:16])
        nlines_per_temp_bin[3]=total(nlines_per_bin[17:19])
        nlines_per_temp_bin[4]=total(nlines_per_bin[20:23])
        nlines_per_temp_bin[5]=total(nlines_per_bin[24:25])
        nlines_per_temp_bin[6]=total(nlines_per_bin[26:27])
	plt_sp_ts=plt_sp_ts_tmp
endif

;
; Bin and plot only the cool temperatures if keyword /only_10 is set
;
if keyword_set(only_10) then begin
	temp_dec_ar=[4.9, $ ; 4.5-5.0; 0:5
		5.3, $ ; 5.1-5.6; 6:11
		5.9, $ ; 5.7-6.1; 12:16
		6.3, $ ; 6.2-6.5; 17:20
		6.6, $ ; 6.6-6.7; 21:22
		6.8, $ ; 6.8; 23
		6.9, $ ; 6.9; 24
		7.0, $ ; 7.0; 25
		7.1, $ ; 7.1; 26
		7.2] ;, $ ; 7.2; 27
		;7.3] ; 7.3; 28
	plt_sp_ts_tmp=fltarr(n_elements(reform(plt_sp_ts[*,0])),10)	
	ntmps=10
        nlines_per_temp_bin=intarr(ntmps)
	if keyword_set(abs_mag) then begin
		plt_sp_ts_tmp[*,0]=total(plt_sp_ts[*,0:5], 2)-min(smooth(total(plt_sp_ts[*,0:5],2)>0.0,num_smth,/edge_truncate)) ; -total(plt_sp_ts[0:9,0:5])/10.
		plt_sp_ts_tmp[*,1]=total(plt_sp_ts[*,6:11], 2)-min(smooth(total(plt_sp_ts[*,6:11],2)>0.0,num_smth,/edge_truncate)) ;-total(plt_sp_ts[0:9,6:11])/10.
		plt_sp_ts_tmp[*,2]=total(plt_sp_ts[*,12:16], 2)-min(smooth(total(plt_sp_ts[*,12:16],2)>0.0,num_smth,/edge_truncate)) ;-total(plt_sp_ts[0:9,12:16])/10.
		plt_sp_ts_tmp[*,3]=total(plt_sp_ts[*,17:20], 2)-min(smooth(total(plt_sp_ts[*,17:20],2)>0.0,num_smth,/edge_truncate)) ;-total(plt_sp_ts[0:9,17:20])/10.
		plt_sp_ts_tmp[*,4]=total(plt_sp_ts[*,21:22], 2)-min(smooth(total(plt_sp_ts[*,21:22],2)>0.0,num_smth,/edge_truncate)) ;-total(plt_sp_ts[0:9,21:22])/10.
		plt_sp_ts_tmp[*,5]=plt_sp_ts[*,23];-total(plt_sp_ts[0:9,23])/10.
		plt_sp_ts_tmp[*,6]=plt_sp_ts[*,24];-total(plt_sp_ts[0:9,24])/10.
		plt_sp_ts_tmp[*,7]=plt_sp_ts[*,25];-total(plt_sp_ts[0:9,25])/10.
		plt_sp_ts_tmp[*,8]=plt_sp_ts[*,26];-total(plt_sp_ts[0:9,26])/10.
		plt_sp_ts_tmp[*,9]=plt_sp_ts[*,27];-total(plt_sp_ts[0:9,27])/10.
		;plt_sp_ts_tmp[*,10]=plt_sp_ts[*,28];-total(plt_sp_ts[0:9,28])/10.
	endif else begin		
		plt_sp_ts_tmp[*,0]=total(plt_sp_ts[*,0:5], 2)/6.
		plt_sp_ts_tmp[*,1]=total(plt_sp_ts[*,6:11], 2)/6.
		plt_sp_ts_tmp[*,2]=total(plt_sp_ts[*,12:16], 2)/5.
		plt_sp_ts_tmp[*,3]=total(plt_sp_ts[*,17:20], 2)/4.
		plt_sp_ts_tmp[*,4]=total(plt_sp_ts[*,21:22], 2)/2.
		plt_sp_ts_tmp[*,5]=plt_sp_ts[*,23]
		plt_sp_ts_tmp[*,6]=plt_sp_ts[*,24]
		plt_sp_ts_tmp[*,7]=plt_sp_ts[*,25]
		plt_sp_ts_tmp[*,8]=plt_sp_ts[*,26]
		plt_sp_ts_tmp[*,9]=plt_sp_ts[*,27]
		;plt_sp_ts_tmp[*,10]=plt_sp_ts[*,28]
        endelse
        nlines_per_temp_bin[0]=total(nlines_per_bin[0:5])
        nlines_per_temp_bin[1]=total(nlines_per_bin[6:11])
        nlines_per_temp_bin[2]=total(nlines_per_bin[12:16])
        nlines_per_temp_bin[3]=total(nlines_per_bin[17:20])
        nlines_per_temp_bin[4]=total(nlines_per_bin[21:22])
        nlines_per_temp_bin[5]=nlines_per_bin[23]
        nlines_per_temp_bin[6]=nlines_per_bin[24]
        nlines_per_temp_bin[7]=nlines_per_bin[25]
        nlines_per_temp_bin[8]=nlines_per_bin[26]
        nlines_per_temp_bin[9]=nlines_per_bin[27]
	plt_sp_ts=plt_sp_ts_tmp
endif

; Bin and plot only 2 large temperature bins if /only_2 is set
if keyword_set(only_2) then begin
	temp_dec_ar=[4.9, $ ; 4.5-5.7
		6.3] ; 5.8-7.3
	plt_sp_ts_tmp=fltarr(n_elements(reform(plt_sp_ts[*,0])),2)	
	ntmps=2
        nlines_per_temp_bin=intarr(ntmps)
	if keyword_set(abs_mag) then begin
		plt_sp_ts_tmp[*,0]=total(plt_sp_ts[*,0:12], 2)-total(plt_sp_ts[0:9,0:12])/10.
		plt_sp_ts_tmp[*,1]=total(plt_sp_ts[*,13:28], 2)-total(plt_sp_ts[0:9,13:28])/10.
	endif else begin		
		plt_sp_ts_tmp[*,0]=total(plt_sp_ts[*,0:12], 2)/13.
		plt_sp_ts_tmp[*,1]=total(plt_sp_ts[*,13:28], 2)/16.
	endelse
        nlines_per_temp_bin[0]=total(nlines_per_bin[0:12])
        nlines_per_temp_bin[1]=total(nlines_per_bin[13:28])
	plt_sp_ts=plt_sp_ts_tmp
endif

;
; Bin and plot 6 temperature bins of equal deltalog(T) 
;    This is the keyword used in the Chamberlin et al (in prep) paper
;
if keyword_set(only_6) then begin
	temp_dec_ar=[5.0, $ ; 4.5-5.25; 0:6 ; NOTE: no emission below 4.9 if only MEGS-A
		5.4, $ ; 5.25-5.65; 7:10
		5.8, $ ; 5.65-6.05; 11:14
		6.2, $ ; 6.05-6.45; 15:18
		6.6, $ ; 6.45-6.85; 19:22
		7.0] ; 6.85-7.25; 23:26
	plt_sp_ts_tmp=fltarr(n_elements(reform(plt_sp_ts[*,0])),6)	
	ntmps=6
        nlines_per_temp_bin=intarr(ntmps)
	if keyword_set(abs_mag) then begin
		plt_sp_ts_tmp[*,0]=total(plt_sp_ts[*,0:6], 2)-min(smooth(total(plt_sp_ts[*,0:6],2)>0.0,num_smth,/edge_truncate)) 
		plt_sp_ts_tmp[*,1]=total(plt_sp_ts[*,7:10], 2)-min(smooth(total(plt_sp_ts[*,7:10],2)>0.0,num_smth,/edge_truncate)) 
		plt_sp_ts_tmp[*,2]=total(plt_sp_ts[*,11:14], 2)-min(smooth(total(plt_sp_ts[*,11:14],2)>0.0,num_smth,/edge_truncate)) 
		plt_sp_ts_tmp[*,3]=total(plt_sp_ts[*,15:18], 2)-min(smooth(total(plt_sp_ts[*,15:18],2)>0.0,num_smth,/edge_truncate)) 
		plt_sp_ts_tmp[*,4]=total(plt_sp_ts[*,19:22], 2)-min(smooth(total(plt_sp_ts[*,19:22],2)>0.0,num_smth,/edge_truncate)) 
		plt_sp_ts_tmp[*,5]=total(plt_sp_ts[*,23:27], 2)-min(smooth(total(plt_sp_ts[*,23:27],2)>0.0,num_smth,/edge_truncate)) 
	endif else begin		
		plt_sp_ts_tmp[*,0]=total(plt_sp_ts[*,0:6], 2)/7.
		plt_sp_ts_tmp[*,1]=total(plt_sp_ts[*,7:10], 2)/4.
		plt_sp_ts_tmp[*,2]=total(plt_sp_ts[*,11:14], 2)/4.
		plt_sp_ts_tmp[*,3]=total(plt_sp_ts[*,15:18], 2)/4.
		plt_sp_ts_tmp[*,4]=total(plt_sp_ts[*,19:22], 2)/4.
		plt_sp_ts_tmp[*,5]=total(plt_sp_ts[*,23:27], 2)/5.
	endelse
        nlines_per_temp_bin[0]=total(nlines_per_bin[0:6])
        nlines_per_temp_bin[1]=total(nlines_per_bin[7:10])
        nlines_per_temp_bin[2]=total(nlines_per_bin[11:14])
        nlines_per_temp_bin[3]=total(nlines_per_bin[15:18])
        nlines_per_temp_bin[4]=total(nlines_per_bin[19:22])
        nlines_per_temp_bin[5]=total(nlines_per_bin[23:27])
	plt_sp_ts=plt_sp_ts_tmp
endif
;
; Bin and plot 12 temperature bins of equal deltalog(T) of 0.2 dex
;
if keyword_set(only_12) then begin
	temp_dec_ar=[4.95, $ ; 4.5-5.05; 0:4 ; NOTE: no emission below 4.9 if only MEGS-A
		5.15, $ ; 5.05-5.25; 5:6
		5.35, $ ; 5.25-5.45; 7:8
		5.55, $ ; 5.45-5.65; 9:10
		5.75, $ ; 5.65-5.85; 11:12
		5.95, $ ; 5.85-6.05; 13:14
		6.15, $ ; 6.05-6.25; 15:16
		6.35, $ ; 6.25-6.45; 17:18
		6.55, $ ; 6.45-6.65; 19:20
		6.75, $ ; 6.65-6.85; 21:22
		6.95, $ ; 6.85-7.05; 23:24
		7.15] ; 7.05-7.25; 25:26
	plt_sp_ts_tmp=fltarr(n_elements(reform(plt_sp_ts[*,0])),12)	
	ntmps=12
        nlines_per_temp_bin=intarr(ntmps)
	if keyword_set(abs_mag) then begin
		plt_sp_ts_tmp[*,0]=total(plt_sp_ts[*,0:4], 2)-min(smooth(total(plt_sp_ts[*,0:4],2)>0.0,num_smth,/edge_truncate)) 
		plt_sp_ts_tmp[*,1]=total(plt_sp_ts[*,5:6], 2)-min(smooth(total(plt_sp_ts[*,5:6],2)>0.0,num_smth,/edge_truncate)) 
		plt_sp_ts_tmp[*,2]=total(plt_sp_ts[*,7:8], 2)-min(smooth(total(plt_sp_ts[*,7:8],2)>0.0,num_smth,/edge_truncate)) 
		plt_sp_ts_tmp[*,3]=total(plt_sp_ts[*,9:10], 2)-min(smooth(total(plt_sp_ts[*,9:10],2)>0.0,num_smth,/edge_truncate)) 
		plt_sp_ts_tmp[*,4]=total(plt_sp_ts[*,11:12], 2)-min(smooth(total(plt_sp_ts[*,11:12],2)>0.0,num_smth,/edge_truncate)) 
		plt_sp_ts_tmp[*,5]=total(plt_sp_ts[*,13:14], 2)-min(smooth(total(plt_sp_ts[*,13:14],2)>0.0,num_smth,/edge_truncate)) 
		plt_sp_ts_tmp[*,6]=total(plt_sp_ts[*,15:16], 2)-min(smooth(total(plt_sp_ts[*,15:16],2)>0.0,num_smth,/edge_truncate)) 
		plt_sp_ts_tmp[*,7]=total(plt_sp_ts[*,17:18], 2)-min(smooth(total(plt_sp_ts[*,17:18],2)>0.0,num_smth,/edge_truncate)) 
		plt_sp_ts_tmp[*,8]=total(plt_sp_ts[*,19:20], 2)-min(smooth(total(plt_sp_ts[*,19:20],2)>0.0,num_smth,/edge_truncate)) 
		plt_sp_ts_tmp[*,9]=total(plt_sp_ts[*,21:22], 2)-min(smooth(total(plt_sp_ts[*,21:22],2)>0.0,num_smth,/edge_truncate)) 
		plt_sp_ts_tmp[*,10]=total(plt_sp_ts[*,23:24], 2)-min(smooth(total(plt_sp_ts[*,23:24],2)>0.0,num_smth,/edge_truncate)) 
		plt_sp_ts_tmp[*,11]=total(plt_sp_ts[*,25:26], 2)-min(smooth(total(plt_sp_ts[*,25:26],2)>0.0,num_smth,/edge_truncate)) 
	endif else begin		
		plt_sp_ts_tmp[*,0]=total(plt_sp_ts[*,0:4], 2)/5.
		plt_sp_ts_tmp[*,1]=total(plt_sp_ts[*,5:6], 2)/2.
		plt_sp_ts_tmp[*,2]=total(plt_sp_ts[*,7:8], 2)/2.
		plt_sp_ts_tmp[*,3]=total(plt_sp_ts[*,9:10], 2)/2.
		plt_sp_ts_tmp[*,4]=total(plt_sp_ts[*,11:12], 2)/2.
		plt_sp_ts_tmp[*,5]=total(plt_sp_ts[*,13:14], 2)/2.
		plt_sp_ts_tmp[*,0]=total(plt_sp_ts[*,15:16], 2)/2.
		plt_sp_ts_tmp[*,1]=total(plt_sp_ts[*,17:18], 2)/2.
		plt_sp_ts_tmp[*,2]=total(plt_sp_ts[*,19:20], 2)/2.
		plt_sp_ts_tmp[*,3]=total(plt_sp_ts[*,21:22], 2)/2.
		plt_sp_ts_tmp[*,4]=total(plt_sp_ts[*,23:24], 2)/2.
		plt_sp_ts_tmp[*,5]=total(plt_sp_ts[*,25:26], 2)/2.
	endelse
        nlines_per_temp_bin[0]=total(nlines_per_bin[0:4])
        nlines_per_temp_bin[1]=total(nlines_per_bin[5:6])
        nlines_per_temp_bin[2]=total(nlines_per_bin[7:8])
        nlines_per_temp_bin[3]=total(nlines_per_bin[9:10])
        nlines_per_temp_bin[4]=total(nlines_per_bin[11:12])
        nlines_per_temp_bin[5]=total(nlines_per_bin[13:14])
        nlines_per_temp_bin[0]=total(nlines_per_bin[15:16])
        nlines_per_temp_bin[1]=total(nlines_per_bin[17:18])
        nlines_per_temp_bin[2]=total(nlines_per_bin[19:20])
        nlines_per_temp_bin[3]=total(nlines_per_bin[21:22])
        nlines_per_temp_bin[4]=total(nlines_per_bin[23:24])
        nlines_per_temp_bin[5]=total(nlines_per_bin[25:26])
	plt_sp_ts=plt_sp_ts_tmp
     endif

;
; Eliminate temperature bins that don't have any data
;
gd_ar=fltarr(ntmps)
for i=0,ntmps-1 do begin
   gd_test=max(plt_sp_ts[*,i])
   if gd_test gt 0.0 then gd_ar[i]=1
endfor
gd_place=where(gd_ar gt 0.0)
plt_sp_ts=plt_sp_ts[*,gd_place]
temp_dec_ar=temp_dec_ar[gd_place]
ntmps=n_elements(temp_dec_ar)

;
; Calculate total radiated energy for both the log(T)=4.9 and the log(T)=7.1-7.2 bins if keyword 'calc_energy' is set
;
if keyword_set(calc_energy) then begin
	; First for the specific energy bins
	energy_bin=fltarr(ntmps)
	energy_bin_ip=fltarr(ntmps)
	energy_bin_gp=fltarr(ntmps)
	ip_arr=fltarr(n_elements(plt_sp_ts[*,0]),ntmps)
	gp_arr=fltarr(n_elements(plt_sp_ts[*,0]),ntmps)
	for t=0, ntmps-1 do begin
		; Fit a gaussian to the IP to figure out IP energy
		if t eq 0 then begin
			if not keyword_set(force_ip_t) then begin
				if not keyword_set(only_6) then w_heii=where(temp_dec_ar eq 4.9) else w_heii=0
				mx_eb=max(plt_sp_ts[*,w_heii],wmx_ev) ; use He II peak to find IP peak for all wavelengths
			endif else begin 
				a=anytim(time[0], /vms, fiducial='tai')
				st_mx_ip_t=strmid(a[0],0,12)+force_ip_t
				wmx_ev_tmp=where(anytim(time, /vms, fiducial='tai') ge st_mx_ip_t)
				wmx_ev=wmx_ev_tmp[0]
			endelse
                        if keyword_set(ip_en_2sig) then begin
                           ; Fit the He II first to define the impulsive phase bins/time
                           fit_ar=fltarr(wmx_ev*2+1)
                           fit_ar[0:wmx_ev]=reform(plt_sp_ts[0:wmx_ev,w_heii])
                           for a=1,wmx_ev do fit_ar[wmx_ev+a]=plt_sp_ts[wmx_ev-a,w_heii]
                           if n_elements(fit_ar) lt 2 then goto, bd_gfit_t
                           yfit=gaussfit(findgen(n_elements(fit_ar)),fit_ar,g_coefs_heii,nterms=3)
                        endif
                endif
 		; Convert to ergs.  With irradiance in W/m^2/nm and 0.1 ang bin sizes (as of now, converted earlier from 0.02 nm bins), factor is 1.406e30
		nt=n_elements(plt_sp_ts[*,t])
                if not keyword_set(ip_en_2sig) then begin
                   energy_bin[t]=total(plt_sp_ts[*,t])*(1406.15)>0.0 ; *(10^27) - make sure to note this value in 'energy_bin' is 10^27!!!
                endif else begin
                  energy_bin[t]=total(plt_sp_ts[g_coefs_heii[1]-ceil(g_coefs_heii[2]):nt-1,t])*(1406.15)>0.0 ; *(10^27) -value in 'energy_bin' is 10^27!!!
                endelse
                if keyword_set(ip_en_2sig) then goto, skpfit_t
		fit_ar=fltarr(wmx_ev*2+1)
		fit_ar[0:wmx_ev]=reform(plt_sp_ts[0:wmx_ev,t])
		for a=1,wmx_ev do fit_ar[wmx_ev+a]=plt_sp_ts[wmx_ev-a,t]
		if n_elements(fit_ar) lt 2 then goto, bd_gfit_t
		yfit=gaussfit(findgen(n_elements(fit_ar)),fit_ar,g_coefs,nterms=3)
		if g_coefs[0] lt 0.0 or g_coefs[1] lt 0 or g_coefs[2] gt 500 then goto, bd_gfit_t
                skpfit_t:
                if temp_dec_ar[t] lt 6.9 or keyword_set(ip_en_2sig) then begin ; have not seen IP increases for log(T)>6.8, doesn't matter
                                ; if calculating energy by phase
                        if not keyword_set(ip_en_2sig) then begin
                           energy_bin_ip[t]=total(yfit)*(1406.15)>0.0 ; *(10^27) - make sure to note this value in 'energy_bin' is 10^27!!!
                        endif else begin ; Use the predefined IP time from 2-sigma gauss fit to He II bin
                           energy_bin_ip[t]=total(plt_sp_ts[g_coefs_heii[1]-ceil(g_coefs_heii[2]):g_coefs_heii[1]+$
                                                       ceil(g_coefs_heii[2]),t])*(1406.15)>0.0 ; 2-sigma
                        endelse
                        if wmx_ev*2 le n_elements(ip_arr[*,t]) then begin
				ip_arr[0:wmx_ev*2,t]=yfit
			endif else begin
				ip_arr[*,t]=yfit[0:n_elements(plt_sp_ts[*,t])-1]
                        endelse
                        ;stop
		endif
		bd_gfit_t:
		; Subtract IP from total time series to get GP
		if not keyword_set(ip_en_2sig) then begin
                   gp_arr[*,t]=reform(plt_sp_ts[*,t])-ip_arr[*,t]
                   energy_bin_gp[t]=total(gp_arr[*,t])*(1406.15)>0.0 ; *(10^27) - make sure to note this value in 'energy_bin' is 10^27!!!
                endif else begin
                   gp_arr[*,t]=reform(plt_sp_ts[*,t])
                   gp_arr[0:g_coefs_heii[1]+ceil(g_coefs_heii[2]),t]=0.0 ; Zero out IP and preflare times
                   energy_bin_gp[t]=total(gp_arr[*,t])*(1406.15)>0.0 ; *(10^27) - make sure to note this value in 'energy_bin' is 10^27!!!
                endelse
             endfor
        
	; Now for all wavelengths in the EVE range
	energy_bin_all=fltarr(nwv)
	energy_bin_all_ip=fltarr(nwv)
	energy_bin_all_gp=fltarr(nwv)
	ip_arr_all=spect*0.0
	gp_arr_all=ip_arr_all
	st_wv_tmp=where(wave ge 7.0)
	st_wv=st_wv_tmp[0]
	for t=st_wv, nwv-1 do begin
		gd_t=where(plt_sp_ts_all[*,t] gt 0.0)
		if gd_t[0] ne -1 then begin
			if not keyword_set(ip_en_2sig) then begin
                           energy_bin_all[t]=total(plt_sp_ts_all[*,t])*(1406.15) ; *(10^27) - make sure to note this value in 'energy_bin' is 10^27!!!
                        endif else begin
                           nt=n_elements(plt_sp_ts_all[*,t])
                           energy_bin_all[t]=total(plt_sp_ts_all[g_coefs_heii[1]-ceil(g_coefs_heii[2]):nt-1,t])*(1406.15) ; 'energy_bin' is 10^27!!!
                        endelse
                        if keyword_set(ip_en_2sig) then goto, skpfit
                        ; Fit a gaussian to the IP to figure out IP energy
			;if t eq 0 then mx_eb=max(plt_sp_ts[*,t],wmx_ev) ; keep using He II peak from above to find IP peak for all wavelengths
			fit_ar=fltarr(wmx_ev*2+1)
			fit_ar[0:wmx_ev]=reform(plt_sp_ts_all[0:wmx_ev,t])
			for a=1,wmx_ev do fit_ar[wmx_ev+a]=plt_sp_ts_all[wmx_ev-a,t]
			if n_elements(fit_ar) lt 2 then goto, bd_gfit
			yfit=gaussfit(findgen(n_elements(fit_ar)),fit_ar,g_coefs,nterms=3)
			if g_coefs[0] lt 0.0 or g_coefs[1] lt 0 or g_coefs[2] gt 500 then goto, bd_gfit
                        skpfit:
                        if not keyword_set(ip_en_2sig) then begin
                           energy_bin_all_ip[t]=total(yfit)*(1406.15) ; *(10^27) - make sure to note this value in 'energy_bin' is 10^27!!!
                        endif else begin
                           energy_bin_all_ip[t]=total(plt_sp_ts_all[g_coefs_heii[1]-ceil(g_coefs_heii[2]):g_coefs_heii[1]+$
                                                       ceil(g_coefs_heii[2]),t])*(1406.15)>0.0 ; 2-sigma
                      
                        endelse
                        if wmx_ev*2 le n_elements(ip_arr_all[*,t]) then begin
				ip_arr_all[0:wmx_ev*2,t]=yfit
			endif else begin
				ip_arr_all[*,t]=yfit[0:n_elements(plt_sp_ts_all[*,t])-1]
			endelse
			; Subtract IP from total time series to get GP
                        if not keyword_set(ip_en_2sig) then begin
                           gp_arr_all[*,t]=reform(plt_sp_ts_all[*,t])-ip_arr_all[*,t]
                           energy_bin_all_gp[t]=total(gp_arr_all[*,t])*(1406.15) ; *(10^27) - make sure to note this value in 'energy_bin' is 10^27!!! 
                       endif else begin
                           gp_arr_all[*,t]=reform(plt_sp_ts_all[*,t])
                           gp_arr_all[0:g_coefs_heii[1]+ceil(g_coefs_heii[2]),t]=0.0 ; Zero out IP and preflare times
                           energy_bin_all_gp[t]=total(gp_arr_all[*,t])*(1406.15)>0.0     ; *(10^27) - make sure to note this value is 10^27!!!
                      
                        endelse
                       ;print, energy_bin_all[t]-energy_bin_all_gp[t]-energy_bin_all_ip[t]; gt 0.5 then stop
                       ;if t eq 210 then stop
                       ;stop
			bd_gfit:
		endif
             endfor
        ;stop
	; Now for the ESP Quad Diode if keyword /esp is set
        if keyword_set(esp) then begin
	energy_bin_espq=0.0d
	energy_bin_espq_ip=0.0d
	energy_bin_espq_gp=0.0d
	ip_arr_espq=esp_quad*0.0d
	gp_arr_espq=ip_arr_espq
	gd_t=where(esp_quad gt 0.0)
	   if gd_t[0] ne -1 then begin
		energy_bin_espq=total(esp_quad)*(14060)>0.0; *(10^27) - make sure to note this value in 'energy_bin' is 10^27!!!
		; Fit a gaussian to the IP to figure out IP energy
		if not keyword_set(force_ip_t) then begin
			wmx_ev_tmp=where(esp_tai ge time[wmx_ev])
		endif else begin 
			wmx_ev_tmp=where(anytim(esp_tai, /vms, fiducial='tai') ge st_mx_ip_t)
		endelse
		wmx_ev=wmx_ev_tmp[0]
		fit_ar=fltarr(wmx_ev*2+1)
		fit_ar[0:wmx_ev]=esp_quad[0:wmx_ev]
		for a=1,wmx_ev do fit_ar[wmx_ev+a]=esp_quad[wmx_ev-a]
		if n_elements(fit_ar) lt 2 then goto, bd_gfit_espq
		yfit=gaussfit(findgen(n_elements(fit_ar)),fit_ar,g_coefs,nterms=3)
		if g_coefs[0] lt 0.0 or g_coefs[1] lt 0 or g_coefs[2] gt 500 then goto, bd_gfit_espq
		energy_bin_espq_ip=total(yfit)*(14060)>0.0; *(10^27) - make sure to note this value in 'energy_bin' is 10^27!!!
		if wmx_ev*2 le n_elements(ip_arr_espq)-1 then begin
			ip_arr_espq[0:wmx_ev*2]=yfit
		endif else begin
			ip_arr_espq=yfit[0:n_elements(esp_quad)-1]
		endelse
		; Subtract IP from total time series to get GP
		gp_arr_espq=esp_quad-ip_arr_espq
		energy_bin_espq_gp=total(gp_arr_espq)*(14060)>0.0; *(10^27) - make sure to note this value in 'energy_bin' is 10^27!!!
		bd_gfit_espq:
           endif
        endif    ; with keyword_set(esp)
endif

;
; Plot the time series for each of the Temperature ranges
;
window, 0
thk_lines=[7.3,6.3,4.9,5.9]
ps_fl_nm2='~/EVE/data/analysis/plots/flare_temp_ts_'+fl_doy+'_'+fl_hr1+'_l2'
if keyword_set(abs_mag) then ps_fl_nm2=ps_fl_nm2+'_am'
if keyword_set(abs_log) then ps_fl_nm2=ps_fl_nm2+'_log'
if keyword_set(only_7) then ps_fl_nm2=ps_fl_nm2+'_7t'
if keyword_set(only_6) then ps_fl_nm2=ps_fl_nm2+'_6t'
ps_fl_nm2=ps_fl_nm2+'.ps'
if keyword_set(ps_out) then open_ps, ps_fl_nm2, /landscape, /color
ntmps_tmp=ntmps ; set temporary variable as rainbow() will for ntmps<7 to be 7 when called
if keyword_set(only_7) then cc=rainbow(6) else cc=rainbow(ntmps)
ntmps=ntmps_tmp
if not keyword_set(abs_mag) then begin
   if keyword_set(only_10) then begin
	utplot, anytim(time, /vms, fiducial='tai'), plt_sp_ts[*,0], yr=[0,2], xs=1, $; xtitle='Hours of 2010, DOY '+fl_doy, $
		ytitle='Normalized Irradiance', charsize=1.5, $
			position=[0.10,0.12,0.94,0.98]
   endif else begin
	utplot, anytim(time, /vms, fiducial='tai'), plt_sp_ts[*,0], yr=[0,8], xs=1, $; xtitle='Hours of 2010, DOY '+fl_doy, $
		ytitle='Normalized Irradiance', charsize=1.5, $
			position=[0.10,0.12,0.94,0.98]
   endelse
	for x=0,ntmps-1 do begin
		gd=where(plt_sp_ts[*,x] gt 0.0)
		if gd[0] ne -1 then begin
			a=where(thk_lines eq temp_dec_ar[x])
			if a[0] ne -1 then begin
				outplot, anytim(time, /vms, fiducial='tai'), plt_sp_ts[*,x]+(0.2*x), color=cc[x], thick=2
			endif else begin
				outplot, anytim(time, /vms, fiducial='tai'), plt_sp_ts[*,x]+(0.2*x), color=cc[x]
			endelse
		endif
	endfor
endif else begin
	if keyword_set(abs_log) then begin
		; NOTE: mW/m^2 is the plot units, multiply by 10^6. to get W to mW, then 0.1 to eliminate the /nm (0.1 nm bins) =x100000.
		utplot, anytim(time, /vms, fiducial='tai'), plt_sp_ts[*,0]*100000.>0.0, xs=1, /ylog, yr=[1e-1,max(plt_sp_ts*100000.)], $
			ytitle='Absolute Irradiance Change (!7l!3W/m!U2!N)', charsize=1.5, $
			position=[0.11,0.12,0.94,0.98]
	endif else begin
		utplot, anytim(time, /vms, fiducial='tai'), plt_sp_ts[*,0]*100000., xs=1, yr=[0,max(plt_sp_ts*100000.)], $  
			ytitle='Absolute Irradiance Change (!7l!3W/m!U2!N)', charsize=1.5, ys=1, $
			position=[0.11,0.12,0.94,0.98], thick=2
	endelse
	for x=0,ntmps-1 do begin
		outplot, anytim(time, /vms, fiducial='tai'), plt_sp_ts[*,x]*100000., color=cc[x], thick=2
		if keyword_set(calc_energy) and x eq 0 then begin
			outplot, anytim(time, /vms, fiducial='tai'), ip_arr[*,x]*100000., color=cc[x], linestyle=2
			outplot, anytim(time, /vms, fiducial='tai'), gp_arr[*,x]*100000., color=cc[x], linestyle=2
		endif
	endfor

endelse

if keyword_set(only_7) then add_fact=0.70 else if keyword_set(only_2) then add_fact=0.85 $
	else if keyword_set(only_10) then add_fact=0.61 else if keyword_set(only_6) then add_fact=0.73 $
        else if keyword_set(only_12) then add_fact=0.56 else add_fact=0.08
xyouts, 0.94, 0.95, 'Log(T)', charsize=1.5, /normal

; Overplot the radiated enrgies for each temperature bin
if keyword_set(calc_energy) then begin
	if keyword_set(ps_out) then red_fact=0.07 else red_fact=0.04
	xyouts, 0.57-red_fact, 0.95, 'Ergs (x10!E27!N):', charsize=1.5, /normal
	xyouts, 0.69, 0.95, 'IP', charsize=1.5, /normal
	xyouts, 0.78, 0.95, 'GP', charsize=1.5, /normal
	xyouts, 0.87, 0.95, 'Tot', charsize=1.5, /normal
	xyouts, 0.552-red_fact, add_fact, 'Isotherm Only:', charsize=1.5, /normal
	xyouts, 0.68, add_fact, strmid(strtrim(total(energy_bin_ip>0.0),2),0,5), charsize=1.5, /normal
	xyouts, 0.77, add_fact, strmid(strtrim(total(energy_bin_gp>0.0),2),0,5), charsize=1.5, /normal
	xyouts, 0.86, add_fact, strmid(strtrim(total(energy_bin>0.0),2),0,5), charsize=1.5, /normal
	xyouts, 0.59-red_fact, add_fact-0.03, '7-37nm:', charsize=1.5, /normal
	xyouts, 0.67, add_fact-0.03, strmid(strtrim(total(energy_bin_all_ip>0.0),2),0,6), charsize=1.5, /normal
	xyouts, 0.77, add_fact-0.03, strmid(strtrim(total(energy_bin_all_gp>0.0),2),0,6), charsize=1.5, /normal
	xyouts, 0.86, add_fact-0.03, strmid(strtrim(total(energy_bin_all>0.0),2),0,6), charsize=1.5, /normal
	if keyword_set(esp) then begin
           xyouts, 0.573-red_fact, add_fact-0.06, '0.1-7nm:', charsize=1.5, /normal
	   xyouts, 0.66, add_fact-0.06, strmid(strtrim(total(energy_bin_espq_ip>0.0),2),0,7), charsize=1.5, /normal
	   xyouts, 0.76, add_fact-0.06, strmid(strtrim(total(energy_bin_espq_gp>0.0),2),0,7), charsize=1.5, /normal
	   xyouts, 0.86, add_fact-0.06, strmid(strtrim(total(energy_bin_espq>0.0),2),0,7), charsize=1.5, /normal
        endif
endif
for i=0,ntmps-1 do begin
	xyouts, 0.95, (0.03*(i+1)+add_fact), strmid(strtrim(temp_dec_ar[i],2),0,3), color=cc[i], $
		charsize=1.5, /normal
	if keyword_set(calc_energy) then begin
		xyouts, 0.68, (0.03*(i+1)+add_fact), strmid(strtrim(energy_bin_ip[i],2),0,4), color=cc[i], $
			charsize=1.5, /normal
		xyouts, 0.77, (0.03*(i+1)+add_fact), strmid(strtrim(energy_bin_gp[i],2),0,4), color=cc[i], $
			charsize=1.5, /normal
		xyouts, 0.87, (0.03*(i+1)+add_fact), strmid(strtrim(energy_bin[i],2),0,4), color=cc[i], $
			charsize=1.5, /normal
	endif
	maxval=max(smooth(plt_sp_ts[*,ntmps-1-i],1,/edge_truncate),wmax)
	if keyword_set(peak_temp) and wmax gt 0 and not keyword_set(abs_mag) then begin
		if keyword_set(only_10) or keyword_set(only_6) or keyword_set(only_7) then begin
			if keyword_set(abs_log) then begin
				outplot, [anytim(time[wmax],/vms, fiducial='tai'),anytim(time[wmax],/vms, fiducial='tai')], $
					[ (0.03*(i+1)+add_fact), (0.03*(i+1)+add_fact)], color=cc[ntmps-1-i], psym=4, thick=2, /normal
			endif else begin
				outplot, [anytim(time[wmax],/vms, fiducial='tai'),anytim(time[wmax],/vms, fiducial='tai')], $
					[1.9-0.15*i,1.9-0.15*i], color=cc[ntmps-1-i], psym=4, thick=2
			endelse
		endif else begin
			outplot, [anytim(time[wmax],/vms, fiducial='tai'),anytim(time[wmax],/vms, fiducial='tai')], $
				[3.9-0.15*i,3.9-0.15*i], color=cc[ntmps-1-i], psym=4, thick=2
		endelse
	endif else if keyword_set(peak_temp) and wmax gt 0 and keyword_set(abs_mag) then begin
		if keyword_set(abs_log) then begin
			outplot, [anytim(time[wmax],/vms, fiducial='tai'),anytim(time[wmax],/vms, fiducial='tai')], $
				[(0.03*(i+1)+add_fact), (0.03*(i+1)+add_fact)], color=cc[ntmps-1-i], psym=4, thick=2, /normal
		endif else begin
			outplot, [anytim(time[wmax],/vms, fiducial='tai'),anytim(time[wmax],/vms, fiducial='tai')], $
				[max(plt_sp_ts)*(0.93-i*0.036)*100000.,max(plt_sp_ts)*(0.93-i*0.036)*100000.], color=cc[ntmps-1-i], psym=4, thick=2
		endelse
	endif
endfor



if keyword_set(ps_out) then close_ps


;
; Make Bar Graph if keyword_set(plot_contour)
;  Will give errors if temperature binning (e.g. /only_7, /only_10,
;  etc.) keywords are set
if keyword_set(plot_contour) then begin
window, 0
ps_fl_nm='~/EVE/data/analysis/plots/flare_temp_cont_'+fl_doy+'_'+fl_hr1+'_l2'
if keyword_set(abs_mag) then ps_fl_nm=ps_fl_nm+'_am'
if keyword_set(calc_energy) then ps_fl_nm=ps_fl_nm+'_en'
ps_fl_nm=ps_fl_nm+'.ps'
if keyword_set(ps_out) then open_ps, ps_fl_nm, /landscape, /color
loadct, 5
nbar=20
br_plt_ar=br_plt_ar;>0.1
if keyword_set(rhessi_t) then ytitle_cont='log(T); RHESSI/2: x-points' else ytitle_cont='log(T)'
contour, plt_sp_ts, anytim(time, /time_only)/3600., reform(wv_plt[1,*]), /fill, xs=1, xtitle='Hours of 2010, DOY '+fl_doy, $
	charsize=1.7, ytitle=ytitle_cont, nlevels=20, ys=1, position=[0.08,0.1,0.92,0.95]
if keyword_set(rhessi_t) then oplot, rhessi_time, rhessi_temp/2., thick=2, psym=7
if keyword_set(rhessi_em) then begin
	rcc=300
	axis, yaxis=1, yr=[0,10], ytitle='RHESSI EM (10!E49!N cm!E-3!N) - Triangles', charsize =1.7, $
		color=rcc, /save
	plot, rhessi_time, rhessi_em, thick=2, color=rcc, /noerase, psym=5, position=[0.08,0.1,0.92,0.95], $
	charsize=1.7, xstyle=4, ystyle=4, xr=[fix(min(time/86400.*24.)),fix(max(time/86400.*24.))]
endif

if keyword_set(ps_out) then close_ps

endif ; with keyword_set(plot_contour)

timex=time/86400.*24.

; Define the ESP and/or AIA times if the respective keywords are set
if keyword_set(esp) or keyword_set(aia) then time_esp=esp_tai ; esp_sod/3600.


; Plot ESP Time series if keyword_set(esp)
if keyword_set(esp) then begin
window, 2
ps_fl_nm3='~/EVE/data/analysis/plots/flare_temp_esp_'+fl_doy+'_'+fl_hr1+'_l2'
if keyword_set(abs_mag) then ps_fl_nm3=ps_fl_nm3+'_am'
ps_fl_nm3=ps_fl_nm3+'.ps'
if keyword_set(ps_out) then open_ps, ps_fl_nm3, /landscape, /color
if keyword_set(abs_mag) then ytlt='Absolute Irradiance Change (mW/m!U2!N)' else ytlt='Normalized Irradiance'
utplot, anytim(time_esp,/vms, fiducial='tai'), smooth(esp_quad,esp_time_smth, /edge_truncate)*1000., charsize=1.5, xs=1, $ ;, xr=[fl_hr1,fl_hr1+1+nhrs]
	ytitle=ytlt, title='ESP Diodes', /ylog, yr=[1e-4,1] ; xtitle='Hours of 2010, DOY '+fl_doy, 
outplot, anytim(time_esp, /vms, fiducial='tai'), smooth(esp_18,esp_time_smth, /edge_truncate)*1000., color=cc[0.1/7.*ntmps]
outplot, anytim(time_esp, /vms, fiducial='tai'), smooth(esp_26,esp_time_smth, /edge_truncate)*1000., color=cc[3./7.*ntmps]
outplot, anytim(time_esp, /vms, fiducial='tai'), smooth(esp_30,esp_time_smth, /edge_truncate)*1000., color=cc[6./7.*ntmps]
xyouts, fl_hr1, 0.9*max(esp_quad), 'Black: 0.1-7 nm', charsize=1.5
xyouts, fl_hr1, 0.85*max(esp_quad), 'Red: 17-22 nm', charsize=1.5, color=cc[0.1/7.*ntmps]
xyouts, fl_hr1, 0.8*max(esp_quad), 'Green: 22-29 nm', charsize=1.5, color=cc[3./7.*ntmps]
xyouts, fl_hr1, 0.75*max(esp_quad), 'Blue: 27-32 nm', charsize=1.5, color=cc[6./7.*ntmps]
if keyword_set(ps_out) then close_ps
endif ; with keyword_set(esp)

; Plot AIA Time series if keyword_set(aia) is set
if keyword_set(aia) then begin
window, 3
ps_fl_nm4='~/EVE/data/analysis/plots/flare_temp_aia_'+fl_doy+'_'+fl_hr1+'_l2'
if keyword_set(abs_mag) then ps_fl_nm4=ps_fl_nm4+'_am'
ps_fl_nm4=ps_fl_nm4+'.ps'
if keyword_set(ps_out) then open_ps, ps_fl_nm4, /landscape, /color
if keyword_set(abs_mag) then begin
	utplot, anytim(time_esp,/vms, fiducial='tai'), aia_094, charsize=1.5, title='AIA Diodes', xs=1, yr=[0,max([aia_195,aia_211,aia_304])], $
		ytitle='Absolute Irradiance Change (W/m!U2!N)'  
endif else begin
	utplot, anytim(time_esp,/vms, fiducial='tai'), aia_094, charsize=1.5, title='AIA Diodes', xs=1, $
		ytitle='Normalized Irradiance' 
endelse
outplot, anytim(time_esp, /vms, fiducial='tai'), aia_133, color=cc[0.1/7.*ntmps]
outplot, anytim(time_esp, /vms, fiducial='tai'), aia_171, color=cc[1./7.*ntmps]
outplot, anytim(time_esp, /vms, fiducial='tai'), aia_195, color=cc[2./7.*ntmps]
outplot, anytim(time_esp, /vms, fiducial='tai'), aia_211, color=cc[3./7.*ntmps]
outplot, anytim(time_esp, /vms, fiducial='tai'), aia_304, color=cc[4./7.*ntmps]
outplot, anytim(time_esp, /vms, fiducial='tai'), aia_335, color=cc[6./7.*ntmps]
xyouts, fl_hr1, 0.9*max([aia_195,aia_211,aia_304]), 'Black: 94 Ang', charsize=1.5
xyouts, fl_hr1, 0.85*max([aia_195,aia_211,aia_304]), 'Red: 133 Ang', charsize=1.5, color=cc[0.1/7.*ntmps]
xyouts, fl_hr1, 0.8*max([aia_195,aia_211,aia_304]), 'Orange: 171 Ang', charsize=1.5, color=cc[1./7.*ntmps]
xyouts, fl_hr1, 0.75*max([aia_195,aia_211,aia_304]), 'Olive: 195 Ang', charsize=1.5, color=cc[2./7.*ntmps]
xyouts, fl_hr1, 0.7*max([aia_195,aia_211,aia_304]), 'Green: 211 Ang', charsize=1.5,  color=cc[3./7.*ntmps]
xyouts, fl_hr1, 0.65*max([aia_195,aia_211,aia_304]), 'Aqua: 304 Ang', charsize=1.5,  color=cc[4./7.*ntmps]
xyouts, fl_hr1, 0.6*max([aia_195,aia_211,aia_304]), 'Blue: 335 Ang', charsize=1.5,  color=cc[6./7.*ntmps]
if keyword_set(ps_out) then close_ps
endif ; with keyword_set(aia)

if keyword_set(save_idl) then begin
	save, time_esp, aia_133, aia_171, aia_195, aia_211, aia_304, aia_335, time, plt_sp_ts, temp_dec_ar, $
		esp_quad, int_time_series,file='./savesets/eve_flare_ts_'+fl_doy+'.sav
endif

if keyword_set(debug) then stop

endpro:

end
