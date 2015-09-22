PRO xmov_arr, image, no_label=no_label, true=true

;; Read files and put frames into memory
	if keyword_set(true) then begin
		dims = [n_elements(image[0,*,0,0]), n_elements(image[0,0,*,0])]
                nframes = n_elements(image[0,0,0,*])
	endif else begin
		dims = [n_elements(image[*,0,0]), n_elements(image[0,*,0])]
		nframes = n_elements(image[0,0,*])
	endelse

	pixmap_id_array = lonarr(nframes)
	for i=0, nframes-1 do begin
		statusline, 'Reading Frame '+string(i+1, format='(I0'+string(floor(alog10(nframes))+1, format='(I1)')+')')+' of '+strtrim(nframes, 2)
		window, /free, /pixmap, xsize=dims[0], ysize=dims[1]
		pixmap_id_array[i] = !d.window
		if keyword_set(true) then tvscl, image[*,*,*,i], true=true else tvscl, image[*,*,i]
		if NOT keyword_set(no_label) then xyouts, 20, 20, 'Frame '+string(i+1, format='(I0'+string(floor(alog10(nframes))+1, format='(I1)')+')')+' of '+strtrim(nframes, 2), /device, charsize=2
	endfor

;; Create window
	window, /free, xsize=dims[0], ysize=dims[1], title='XMOV_JPG'
	display_window = !d.window

;; Display movie commands: byte(get_kbrd(/escape)) byte(get_kbrd(/key_name)

	print, ''
	print, ''
	print, 'XMOV_MJ2 Commands:'
	print, '      <e> or <q>   Quit'
	print, '        SPACEBAR   Pause/Resume'
	print, '     RIGHT arrow   Move to next frame'
	print, '      LEFT arrow   Move to previous frame'
	print, '        UP arrow   Increase frame rate'
	print, '      DOWN arrow   Decrease frame rate'
	print, ''

;; Initialize movie
	frame_rate = floor(nframes/30.)>1
	i = 0
	key = ''
	play = 1

;; List of keyboard commands
lbl_commands:
	case strmid(key, 0, 1) of
		'e': GOTO, lbl_exit
                'E': GOTO, lbl_exit
		'q': GOTO, lbl_exit
		'Q': GOTO, lbl_exit
		' ': begin
			play = 1-play
			if play eq 0 then GOTO, lbl_display else GOTO, lbl_play
		end
		'R': begin
			if play eq 0 then begin
				i = (i+1) mod nframes
				GOTO, lbl_display
			endif else GOTO, lbl_play
		end
		'L': begin
                        if play eq 0 then begin
				if i eq 0 then i = nframes-1 else i = (i-1) mod nframes
                                GOTO, lbl_display
                        endif else GOTO, lbl_play
                end
		'U': begin
			if play eq 1 then begin
				frame_rate = frame_rate+1. 
				GOTO, lbl_play
			endif else GOTO, lbl_display
		end
		'D': begin 
			if play eq 1 then begin
				frame_rate = (frame_rate-1.)>1. 
                                GOTO, lbl_play
			endif else GOTO, lbl_display
		end
		else: if play eq 1 then GOTO, lbl_play else GOTO, lbl_display
	endcase

;; Play movie
lbl_play:
	key = ''
	statusline, '                                   '
	statusline, 'Frame rate = '+strtrim(string(frame_rate, format='(I3)'), 2)+' frames/second'
        REPEAT BEGIN
                device, copy=[0, 0, dims[0], dims[1], 0, 0, pixmap_id_array[i]]
                i = (i+1) mod nframes
                wait, 1/(frame_rate*1.0)
                key = get_kbrd(0, /key_name)
        ENDREP UNTIL key ne ''
	GOTO, lbl_commands	

;; Display single image
lbl_display:	
	key = ''
	statusline, '                                   '
	statusline, 'Press SPACEBAR to resume playback'
	device, copy=[0, 0, dims[0], dims[1], 0, 0, pixmap_id_array[i]]

	key = get_kbrd(/key_name)
	GOTO, lbl_commands

;; On exit, clear memory
lbl_exit:
	statusline, '                                   '
	for i=0, nframes-1 do wdelete, pixmap_id_array[i]
	wdelete, display_window
	if keyword_set(second_panel) then wdelete, display_window2
END
