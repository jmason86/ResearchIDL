;; Code to get AIA synoptic data, based on EVE flare catalog

PRO grab_aia_data_time, yyyydoy, start_hour, nhours

	tstart = start_hour
	jd = yd_to_jd(yyyydoy)
	wget_cmd = ''
	vso_cmd = ''

        aia_folders = ''
        month_string = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']

        for j=0, nhours-1 do begin
                if tstart+j ge 0 AND tstart+j le 23 then begin
                        yd = jd_to_yd(jd)
                        yd_to_ymd, yd, yy, mm, dd
         		year = string(yy, format='(I4)')
	                month = string(mm, format='(I02)')
                        day = string(dd, format='(I02)')
                        hh = (tstart+j)
                endif

                if tstart+j ge 24 then begin
                        yd = jd_to_yd(time_jd +1)
                        yd_to_ymd, yd, yy, mm, dd
			year = string(yy, format='(I4)')
                        month = string(mm, format='(I02)')
                        day = string(dd, format='(I02)')
                        hh = (tstart+j)-24
                endif

                hour = string(hh, format='(I02)')
                wget_cmd = [wget_cmd, year+'/'+month+'/'+day+'/H'+hour+'00']
		vso_cmd = [vso_cmd, year+'/'+month+'/'+day+'T'+hour]
        endfor

	wget_cmd = wget_cmd[1:*]
	wget_cmd = wget_cmd[sort(wget_cmd)]
	wget_cmd = wget_cmd[uniq(wget_cmd)]
	wget_cmd = reverse(wget_cmd)

	vso_cmd = vso_cmd[1:*]
	vso_cmd = vso_cmd[sort(vso_cmd)]
	vso_cmd = vso_cmd[uniq(vso_cmd)]
	vso_cmd = reverse(vso_cmd)

	for i=0, n_elements(wget_cmd)-1 do begin
	        spawn, 'mkdir /eve_analysis/idl_lib/aia/data/synoptic/jsoc.stanford.edu/data/aia/synoptic/'+wget_cmd[i]
        	tmp = file_search('/eve_analysis/idl_lib/aia/data/synoptic/jsoc.stanford.edu/data/aia/synoptic/'+wget_cmd[i], '*.fits')
	        if n_elements(tmp) lt 100 then begin
        	        spawn, 'wget -r -l 1 -A fits http://jsoc.stanford.edu/data/aia/synoptic/'+wget_cmd[i]
                	if n_elements(tmp) lt 100 then begin
                        	tmp = file_search('/eve_analysis/idl_lib/aia/data/synoptic/lev1/'+wget_cmd[i], '*.fits')
	                        if n_elements(tmp) lt 100 then begin
        	                        spawn, 'mkdir /eve_analysis/idl_lib/aia/data/synoptic/lev1/'+year
                	                spawn, 'mkdir /eve_analysis/idl_lib/aia/data/synoptic/lev1/'+year+'/'+month
                        	        spawn, 'mkdir /eve_analysis/idl_lib/aia/data/synoptic/lev1/'+year+'/'+month+'/'+day
                                	spawn, 'mkdir /eve_analysis/idl_lib/aia/data/synoptic/lev1/'+year+'/'+month+'/'+day+'/H'+hour+'00'
STOP
					ssw_jsoc_time2data, vso_cmd[i]+':00', vso_cmd[i]+':59', index, data, ds='aia.lev1', outdir_top='/eve_analysis/idl_lib/aia/data/synoptic/lev1/'+year+'/'+month+'/'+day+'/H'+hour+'00', /copy_only
 	                       endif
        	        endif
	        endif
	endfor


END
