; This routine calculates base difference images
;

pro bdiSWAP, im, time_array, im_db, time_array_db, short = short, step = step

if not keyword_set(step) then step = 1

sizes = size(im)
xsz = sizes[1]
ysz = sizes[2]
frames = n_elements(time_array)
im_db = fltarr(xsz, ysz, frames - 1)
time_array_db = strarr(frames - 1)

 for i = 0, frames - 2 do begin

  im_db[*, *, i] = im[*, *, i+1] - im[*, *, 0]
  if not keyword_set(short) then time_array_db[i] = time_array[i+1]+' - '+strmid(time_array[0], 11, 8) else time_array_db[i] = strmid(time_array[i+1], 11, 8) +' - '+strmid(time_array[i], 11, 8)
 
 endfor

return
end
