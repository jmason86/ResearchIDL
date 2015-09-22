;; Read in all the derotated images, regrid to 1K by 1K, and save as a saveset

;; AIA 171
files = find_files('AIA*_0171_rot.fits', '/Users/Dinah/Desktop/FromJames')
nimg = n_elements(files)
img171 = fltarr(1024, 1024, nimg)  
for i=0, nimg-1 do begin
	statusline, strtrim(i,2)+' of '+strtrim(nimg)
	read_sdo, files[i], hdr, img 
	if i eq 0 then hdr171 = hdr else hdr171 = [hdr171, hdr]         
	img171[*,*,i] = congrid(img, 1024, 1024)
endfor

save, img171, hdr171, file='~/Desktop/EUV_dimming_workshop/IDL_savesets/a171_derot_james.sav'


files = find_files('aia.lev1.171A*rot.fits', '/Volumes/Flares2013/Barbara/171')
nimg = n_elements(files)
img171 = fltarr(1024, 1024, nimg)  
for i=0, nimg-1 do begin
	statusline, strtrim(i,2)+' of '+strtrim(nimg)
	read_sdo, files[i], hdr, img 
	if i eq 0 then hdr171 = hdr else hdr171 = [hdr171, hdr]         
	img171[*,*,i] = congrid(img, 1024, 1024)
endfor

save, img171, hdr171, file='~/Desktop/EUV_dimming_workshop/IDL_savesets/a171_derot_barbara.sav'


;; AIA 193
files = find_files('AIA*_0193_rot.fits', '/Users/Dinah/Desktop/FromJames')
nimg = n_elements(files)
img193 = fltarr(1024, 1024, nimg)  
for i=0, nimg-1 do begin
	statusline, strtrim(i,2)+' of '+strtrim(nimg)
	read_sdo, files[i], hdr, img 
	if i eq 0 then hdr193 = hdr else hdr193 = [hdr193, hdr]         
	img193[*,*,i] = congrid(img, 1024, 1024)
endfor

save, img193, hdr193, file='~/Desktop/EUV_dimming_workshop/IDL_savesets/a193_derot_james.sav'


files = find_files('aia.lev1.193A*rot.fits', '/Volumes/Flares2013/Barbara/193')
nimg = n_elements(files)
img193 = fltarr(1024, 1024, nimg)  
for i=0, nimg-1 do begin
	statusline, strtrim(i,2)+' of '+strtrim(nimg)
	read_sdo, files[i], hdr, img 
	if i eq 0 then hdr193 = hdr else hdr193 = [hdr193, hdr]         
	img193[*,*,i] = congrid(img, 1024, 1024)
endfor

save, img193, hdr193, file='~/Desktop/EUV_dimming_workshop/IDL_savesets/a193_derot_barbara.sav'


;; AIA 211
files = find_files('AIA*_0211_rot.fits', '/Users/Dinah/Desktop/FromJames')
nimg = n_elements(files)
img211 = fltarr(1024, 1024, nimg)  
for i=0, nimg-1 do begin
	statusline, strtrim(i,2)+' of '+strtrim(nimg)
	read_sdo, files[i], hdr, img 
	if i eq 0 then hdr211 = hdr else hdr211 = [hdr211, hdr]         
	img211[*,*,i] = congrid(img, 1024, 1024)
endfor

save, img211, hdr211, file='~/Desktop/EUV_dimming_workshop/IDL_savesets/a211_derot_james.sav'

files = find_files('aia.lev1.211A*rot.fits', '/Volumes/Flares2013/Barbara/211')
nimg = n_elements(files)
img211 = fltarr(1024, 1024, nimg)  
for i=0, nimg-1 do begin
	statusline, strtrim(i,2)+' of '+strtrim(nimg)
	read_sdo, files[i], hdr, img 
	if i eq 0 then hdr211 = hdr else hdr211 = [hdr211, hdr]         
	img211[*,*,i] = congrid(img, 1024, 1024)
endfor

save, img211, hdr211, file='~/Desktop/EUV_dimming_workshop/IDL_savesets/a211_derot_barbara.sav'

;; AIA 304
files = find_files('aia.lev1.304A*rot.fits', '/Volumes/Flares2013/Barbara/304')
nimg = n_elements(files)
img304 = fltarr(1024, 1024, nimg)  
for i=0, nimg-1 do begin
	statusline, strtrim(i,2)+' of '+strtrim(nimg)
	read_sdo, files[i], hdr, img 
	if i eq 0 then hdr304 = hdr else hdr304 = [hdr304, hdr]         
	img304[*,*,i] = congrid(img, 1024, 1024)
endfor

save, img304, hdr304, file='~/Desktop/EUV_dimming_workshop/IDL_savesets/a304_derot_barbara.sav'

END
