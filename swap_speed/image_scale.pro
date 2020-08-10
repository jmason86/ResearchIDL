; This routine scales plain, running difference and base difference images
; for subsequent displaying.
; LAST MODIFIED: 20041103 (AZ)
;

pro image_scale, image, t_array, frames, contrast, contrast_plain, sv = sv, plain = plain, lin = lin, bdi = bdi, rdi = rdi, percent = percent, big_font = big_font, image_scaled

size = size(image)
;stop

sz = size[1]
xsz = size[1]
ysz = size[2]

if keyword_set(bdi) then begin

im_db = image
time_array_db = t_array

im_db = im_db*contrast
im_db_scl= bytarr(xsz, ysz, frames-1)
;im_db_scl_reb= bytarr(128, 128, n_elements(needed_listing) - 1)
mx = max(im_db) & print, mx
mn = min(im_db) & print, mn
print, !d.table_size
print, !d.n_colors
max_color = float(min([!d.n_colors, !d.table_size]) - 1)
print, max_color
scl = max_color/alog10((mx < abs(mn))/1000.0)
;scl = max_color/alog10((mx > abs(mn))) ;before 2014


ltz = where(im_db lt 0.)
gtz = where(im_db gt 0.)

im_db(gtz) = (scl*(alog10(im_db(gtz) > 1.))) & print, minmax(im_db(gtz))
im_db(ltz) = -(scl*(alog10(abs(im_db(ltz)) > 1.))) & print, minmax(im_db(ltz))



areasatPos=where(im_db gt 255.)
areasatNeg=where(im_db lt (-255.))

im_db[areasatPos] = 255
im_db[areasatNeg] = -255


im_db_scl = byte((im_db+255.)/2.) 
;im_db_scl = bytscl(im_db,-7,7)


print, minmax(im_db_scl)


if not keyword_set(big_font) then im_db_scl = my_time_stamp(im_db_scl, 50, time_array_db, 10, 10) else im_db_scl = my_time_stamp(im_db_scl, 50, time_array_db, 10, 10, /big_font)
image_scaled = im_db_scl

endif

if keyword_set(rdi) then begin

im_diff = image
time_array_diff = t_array

im_diff = im_diff*contrast
im_diff_scl= bytarr(xsz, ysz, frames-1)
mx = max(im_diff) & print, mx
mn = min(im_diff) & print, mn
print, !d.table_size
print, !d.n_colors
max_color = float(min([!d.n_colors, !d.table_size]) - 1)
print, max_color
scl = max_color/alog10((mx > abs(mn)))

ltz = where(im_diff lt 0.)
gtz = where(im_diff gt 0.)

im_diff(gtz) = (scl*(alog10(im_diff(gtz) > 1.))) & print, minmax(im_diff(gtz))
im_diff(ltz) = -(scl*(alog10(abs(im_diff(ltz)) > 1.))) & print, minmax(im_diff(ltz))
not_needed = im_diff[*, *, 0] & save, not_needed, filename = 'not_needed.fts' ; & spawn
im_diff_scl = byte((im_diff+255.)/2.) 

; time-stamping and displaying the movie

if not keyword_set(big_font) then im_diff_scl = my_time_stamp(im_diff_scl,50, time_array_diff, 10, ysz-100) else im_diff_scl = my_time_stamp(im_diff_scl,50, time_array_diff, 10, ysz-100, /big_font)
image_scaled = im_diff_scl

endif

if keyword_set(percent) then begin

im_diff = image
time_array_diff = t_array

im_diff = im_diff*contrast
im_diff_scl= bytarr(xsz, ysz, frames-1)
mx = max(im_diff) & print, mx
mn = min(im_diff) & print, mn
print, !d.table_size
print, !d.n_colors
max_color = float(min([!d.n_colors, !d.table_size]) - 1)
print, max_color
scl = max_color/alog10((128.))

ltz = where(im_diff lt 0.)
gtz = where(im_diff gt 0.)

im_diff(gtz) = (scl*(alog10(im_diff(gtz) > 1. and im_diff(gtz) < 128.))) & print, minmax(im_diff(gtz))
im_diff(ltz) = -(scl*(alog10(abs(im_diff(ltz)) > 1. and im_diff(ltz) > 128.))) & print, minmax(im_diff(ltz))
;not_needed = im_diff[*, *, 0] & save, not_needed, filename = 'not_needed.fts' ; & spawn
im_diff_scl = byte((im_diff+255.)/2.) 
;im_diff_scl = bytscl(im_db,-7,7)
; time-stamping and displaying the movie

if not keyword_set(big_font) then im_diff_scl = my_time_stamp(im_diff_scl,50, time_array_diff, 10, 10) else im_diff_scl = my_time_stamp(im_diff_scl,50, time_array_diff, 10, 10, /big_font)
image_scaled = im_diff_scl

endif

if keyword_set(plain) then begin

if not keyword_set(lin) then begin

if not keyword_set(sv) then im = image else im = image*contrast_plain
time_array = t_array

   im_scl= bytarr(xsz, ysz, frames)
   mx = max(im)
   print, !d.table_size
   print, !d.n_colors
   max_color = float(min([!d.n_colors, !d.table_size]) - 1)
   print, max_color
   scl = max_color/alog10(mx)

    for i = 0, frames-1 do begin

      im_scl[*, *, i] = byte(scl*(alog10(im[*,*,i] > 1.)))

    endfor

endif else begin 

im = image
time_array = t_array

   im_scl= bytarr(xsz, ysz, frames)
   mx = max(im)
   print, !d.table_size
   print, !d.n_colors
   max_color = float(min([!d.n_colors, !d.table_size]) - 1)
   print, max_color
   scl = max_color/mx

    for i = 0, frames-1 do begin

      im_scl[*, *, i] = byte(scl*im[*,*,i])

    endfor

endelse

   if not keyword_set(big_font) then im_scl=my_time_stamp(im_scl,50, time_array, 10, 10) else im_scl=my_time_stamp(im_scl,50, time_array, 10, 10, /big_font)
   image_scaled = im_scl

endif

return
end
