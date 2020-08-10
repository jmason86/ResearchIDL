; Filename = eve_read_whole_fits.pro
; CreationDate = Fri Dec 28 23:07:13 2012
; ORIGIN = SDO/EVE SPOC // LASP, University of Colorado, Boulder
; COMMENT = Only the eve_read_whole_fits.pro function was written by LASP.
; COMMENT = All other software in this package is available from GSFC.
; COMMENT = This file contains a complete package of IDL code that is
; COMMENT = known to be compatible with EVE data products.
; COMMENT = Most of the software is included with the IDL Astronomy
; COMMENT = Library maintained at GSFC. However, our data users have
; COMMENT = complained about errors and byte-swapping problems in the past,
; COMMENT = and we are addressing all of these issues with this package.
; COMMENT = Those problems are caused by from inconsistent software
; COMMENT = that is out of date. It is difficult for most people to even
; COMMENT = determine if their software is out of date since much of the
; COMMENT = needed software spread over several directories.
; COMMENT = This package is internally consistent.
; REFURL1 = http://lasp.colorado.edu/home/eve
; REFURL2 = http://idlastro.gsfc.nasa.gov/
;
;
forward_function fxparpos
;
;-------------------------------------------------------------
;+
; NAME:
;       REPCHR
; PURPOSE:
;       Replace all occurrences of one character with another in a text string.
; CATEGORY:
; CALLING SEQUENCE:
;       new = repchr(old, c1, [c2])
; INPUTS:
;       old = original text string.          in
;       c1 = character to replace.           in
;       c2 = character to replace it with.   in
;            default is space.
; KEYWORD PARAMETERS:
; OUTPUTS:
;       new = edited string.                 out
; COMMON BLOCKS:
; NOTES:
; MODIFICATION HISTORY:
;       R. Sterner.  28 Oct, 1986.
;       Johns Hopkins Applied Physics Lab.
;       RES 1 Sep, 1989 --- converted to SUN.
;       R. Sterner, 27 Jan, 1993 --- dropped reference to array.
;
; Copyright (C) 1986, Johns Hopkins University/Applied Physics Laboratory
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.  Other limitations apply as described in the file disclaimer.txt.
;-
;-------------------------------------------------------------
 
	FUNCTION REPCHR, OLD, C1, C2, help=hlp
 
	if (n_params(0) lt 2) or keyword_set(help) then begin
	  print,' Replace all occurrences of one character with another '+$
	    'in a text string.'
	  print,' new = repchr(old, c1, [c2])'
	  print,'   old = original text string.          in'
	  print,'   c1 = character to replace.           in'
	  print,'   c2 = character to replace it with.   in'
	  print,'        default is space.'
	  print,'   new = edited string.                 out'
	  return, -1
	endif
 
	B = BYTE(OLD)			   ; convert string to a byte array.
	CB1 = BYTE(C1)			   ; convert char 1 to byte.
	W = WHERE(B EQ CB1(0))		   ; find occurrences of char 1.
	IF W(0) EQ -1 THEN RETURN, OLD	   ; if none, return old string.
	IF N_PARAMS(0) LT 3 THEN C2 = ' '  ; default char 2 is space.
	CB2 = BYTE(C2)			   ; convert char 2 to byte.
	B(W) = CB2(0)			   ; replace char 1 by char 2.
	RETURN, STRING(B)		   ; return new string.
	END
pro fits_info, filename, SILENT=silent,TEXTOUT=textout, N_ext=n_ext, extname=extname
;+
; NAME:
;     FITS_INFO
; PURPOSE:
;     Provide information about the contents of a FITS file
; EXPLANATION:
;     Information includes number of header records and size of data array.
;     Applies to primary header and all extensions.    Information can be 
;     printed at the terminal and/or stored in a common block
;
;     This routine is mostly obsolete, and better results can be usually be
;     performed with FITS_HELP (for display) or FITS_OPEN (to read FITS 
;     information into a structure)
;
; CALLING SEQUENCE:
;     FITS_INFO, Filename, [ /SILENT , TEXTOUT = , N_ext =, EXTNAME= ]
;
; INPUT:
;     Filename - Scalar string giving the name of the FITS file(s)
;               Can include wildcards such as '*.fits', or regular expressions 
;               allowed by the FILE_SEARCH() function.     One can also search 
;               gzip compressed  FITS files, but their extension must
;               end in .gz or .ftz.
; OPTIONAL INPUT KEYWORDS:
;     /SILENT - If set, then the display of the file description on the 
;                terminal will be suppressed
;
;      TEXTOUT - specifies output device.
;               textout=1        TERMINAL using /more option
;               textout=2        TERMINAL without /more option
;               textout=3        <program>.prt
;               textout=4        laser.tmp
;               textout=5        user must open file, see TEXTOPEN
;               textout=7       append to existing <program.prt> file
;               textout = filename (default extension of .prt)
;
;               If TEXTOUT is not supplied, then !TEXTOUT is used
; OPTIONAL OUTPUT KEYWORDS:
;       N_ext - Returns an integer scalar giving the number of extensions in
;               the FITS file
;       extname - returns a list containing the EXTNAME keywords for each
;       		extension.
;
; COMMON BLOCKS
;       DESCRIPTOR =  File descriptor string of the form N_hdrrec Naxis IDL_type
;               Naxis1 Naxis2 ... Naxisn [N_hdrrec table_type Naxis
;               IDL_type Naxis1 ... Naxisn] (repeated for each extension) 
;               For example, the following descriptor 
;                    167 2 4 3839 4 55 BINTABLE 2 1 89 5
; 
;               indicates that the  primary header containing 167 lines, and 
;               the primary (2D) floating point image (IDL type 4) 
;               is of size 3839 x 4.    The first extension header contains
;               55 lines, and the  byte (IDL type 1) table array is of size
;               89 x 5.
;
;               The DESCRIPTOR is *only* computed if /SILENT is set.
; EXAMPLE:
;       Display info about all FITS files of the form '*.fit' in the current
;               directory
;
;               IDL> fits_info, '*.fit'
;
;       Any time a *.fit file is found which is *not* in FITS format, an error 
;       message is displayed at the terminal and the program continues
;
; PROCEDURES USED:
;       GETTOK(), MRD_SKIP, STRN(), SXPAR(), TEXTOPEN, TEXTCLOSE 
;
; SYSTEM VARIABLES:
;       The non-standard system variables !TEXTOUT and !TEXTUNIT will be  
;       created by FITS_INFO if they are not previously defined.   
;
;       DEFSYSV,'!TEXTOUT',1
;       DEFSYSV,'!TEXTUNIT',0
;
;       See TEXTOPEN.PRO for more info
; MODIFICATION HISTORY:
;       Written, K. Venkatakrishna, Hughes STX, May 1992
;       Added N_ext keyword, and table_name info, G. Reichert
;       Work on *very* large FITS files   October 92
;       More checks to recognize corrupted FITS files     February, 1993
;       Proper check for END keyword    December 1994
;       Correctly size variable length binary tables  WBL December 1994
;       EXTNAME keyword can be anywhere in extension header WBL  January 1998
;       Correctly skip past extensions with no data   WBL   April 1998
;       Converted to IDL V5.0, W. Landsman, April 1998
;       No need for !TEXTOUT if /SILENT D.Finkbeiner   February 2002
;       Define !TEXTOUT if needed.  R. Sterner, 2002 Aug 27
;       Work on gzip compressed files for V5.3 or later  W. Landsman 2003 Jan
;       Improve speed by only reading first 36 lines of header 
;       Count headers with more than 32767 lines         W. Landsman Feb. 2003
;       Assume since V5.3 (OPENR,/COMPRESS)   W. Landsman Feb 2004
;       EXTNAME keyword can be anywhere in extension header again 
;                         WBL/S. Bansal Dec 2004
;       Read more than 200 extensions  WBL   March 2005
;       Work for FITS files with SIMPLE=F   WBL July 2005
;       Assume since V5.4, fstat.compress available WBL April 2006
;       Added EXTNAME as an IDL keyword to return values. M. Perrin Dec 2007
;       make Ndata a long64 to deal with large files. E. Hivon Mar 2008
;       For GDL compatibility, first check if file is compressed  before using
;          OPENR,/COMPRESS  B. Roukema/WL    Apr 2010
;-
 On_error,2
 compile_opt idl2
 COMMON descriptor,fdescript

 if N_params() lt 1 then begin
     print,'Syntax - FITS_INFO, filename, [/SILENT, TEXTOUT=, N_ext=, EXTNAME=]'
     return
 endif

  defsysv,'!TEXTOUT',exists=ex                  ; Check if !TEXTOUT exists.
  if ex eq 0 then defsysv,'!TEXTOUT',1          ; If not define it.

    fil = file_search( filename, COUNT = nfiles) 
    if nfiles EQ 0 then message,'No files found'
; File is gzip compressed if it ends in .gz or .ftz 
    len = strlen(fil)
    ext = strlowcase(strmid(fil,transpose(len-3),3))
    compress = (ext EQ '.gz') or (ext EQ 'ftz')

   silent = keyword_set( SILENT )
   if ~silent then begin 
    if ~keyword_set( TEXTOUT ) then textout = !TEXTOUT    
    textopen, 'FITS_INFO', TEXTOUT=textout
   endif

 for nf = 0, nfiles-1 do begin

    file = fil[nf]

    openr, lun1, file, /GET_LUN, COMPRESS = compress[nf]
     
   N_ext = -1
    fdescript = ''
    nmax = 400 ; MDP was 100
    extname = strarr(nmax)

   ptr = 0l
   START:  
   ON_IOerror, BAD_FILE
   descript = ''
   
    test = bytarr(8)
   readu, lun1, test
 
   if N_ext EQ -1 then begin
        if string(test) NE 'SIMPLE  ' then goto, BAD_FILE
	simple = 1
   endif else begin
        if string(test) NE 'XTENSION' then goto, END_OF_FILE
	simple = 0
   endelse
   point_lun, lun1, ptr

;                               Read the header
   hdr = bytarr(80, 36, /NOZERO)
   N_hdrblock = 1
   readu, lun1, hdr
   ptr = ptr + 2880
   hd = string( hdr > 32b)

;                               Get values of BITPIX, NAXIS etc.
   bitpix = sxpar(hd, 'BITPIX', Count = N_BITPIX)
   if N_BITPIX EQ 0 then $ 
          message, 'WARNING - FITS header missing BITPIX keyword',/CON
   Naxis = sxpar( hd, 'NAXIS', Count = N_NAXIS)
   if N_NAXIS EQ 0 then message, $ 
           'WARNING - FITS header missing NAXIS keyword',/CON
   
   exten = sxpar( hd, 'XTENSION')
   Ext_type = strmid( strtrim( exten ,2), 0, 8)      ;Use only first 8 char
   gcount = sxpar( hd, 'GCOUNT') > 1
   pcount = sxpar( hd, 'PCOUNT')

   if strn(Ext_type) NE '0' then begin
        if (gcount NE 1) or (pcount NE 0) then $
             ext_type = 'VAR_' + ext_type
        descript = descript + ' ' + Ext_type
  endif

   descript = descript + ' ' + strn(Naxis)

   case BITPIX of
      8:   IDL_type = 1     ; Byte
     16:   IDL_type = 2     ; Integer*2
     32:   IDL_type = 3     ; Integer*4
    -32:   IDL_type = 4     ; Real*4 
    -64:   IDL_type = 5     ; Real*8
   ELSE: begin 
         message, ' Illegal value of BITPIX = ' + strn(bitpix) + $
         ' in header',/CON
         goto, SKIP
         end
   endcase

  if Naxis GT 0 then begin
         descript = descript + ' ' + strn(IDL_type)
         Nax = sxpar( hd, 'NAXIS*')
         if N_elements(Nax) LT Naxis then begin 
              message, $
                 'ERROR - Missing required NAXISi keyword in FITS header',/CON
                  goto, SKIP
         endif
         for i = 1, Naxis do descript = descript + ' '+strn(Nax[i-1])
  endif

  end_rec = where( strtrim(strmid(hd,0,8),2) EQ  'END')

  exname = sxpar(hd, 'extname', Count = N_extname)
  if N_extname GT 0 then extname[N_ext+1] = exname
  get_extname =  (N_ext GE 0) && (N_extname EQ 0) && ~keyword_set(SILENT)  

;  Read header records, till end of header is reached

  hdr = bytarr(80, 36, /NOZERO)
  while (end_rec[0] EQ -1) && (~eof(lun1) ) do begin
       readu,lun1,hdr
       ptr = ptr + 2880L
       hd1 = string( hdr > 32b)
       end_rec = where( strtrim(strmid(hd1,0,8),2) EQ  'END')
       n_hdrblock = n_hdrblock + 1
       if get_extname then begin
           exname = sxpar(hd1, 'extname', Count = N_extname)
           if N_extname GT 0 then begin
                extname[N_ext+1] = exname
               get_extname = 0
           endif
       endif 
  endwhile
 
 n_hdrec = 36L*(n_hdrblock-1) + end_rec[0] + 1L         ; size of header
 descript = strn( n_hdrec ) + descript

;  If there is data associated with primary header, then find out the size

 if Naxis GT 0 then begin
         ndata = long64(Nax[0])
         if naxis GT 1 then for i = 2, naxis do ndata=ndata*Nax[i-1]
 endif else ndata = 0

 nbytes = (abs(bitpix)/8) * gcount * (pcount + ndata)
 nrec = long(( nbytes +2879)/ 2880)


 
; Check if all headers have been read 

 if ( simple EQ 0 ) && ( strlen(strn(exten)) EQ 1) then goto, END_OF_FILE  

    N_ext = N_ext + 1
    if N_ext GT nmax then begin
              extname = [extname,strarr(nmax)]
              nmax = nmax*2
    endif


; Append information concerning the current extension to descriptor

    fdescript = fdescript + ' ' + descript

; Check for EOF
; Skip the headers and data records

    ptr = ptr + nrec*2880L
    if compress[nf] then mrd_skip,lun1,nrec*2880L else point_lun,lun1,ptr
    if ~eof(lun1) then goto, START
;
 END_OF_FILE:  

 extname = extname[0:N_ext]           ;strip off bogus first value
                                  ;otherwise will end up with '' at end

 if ~SILENT then begin
 printf,!textunit,file,' has ',strn(N_ext),' extensions'
 printf,!textunit,'Primary header: ',gettok(fdescript,' '),' records'

 Naxis = gettok( fdescript,' ' ) 

 If Naxis NE '0' then begin

 case gettok(fdescript,' ') of

 '1': image_type = 'Byte'
 '2': image_type = 'Integer*2'    
 '3': image_type = 'Integer*4'
 '4': image_type = 'Real*4'
 '5': image_type = 'Real*8'

 endcase

 image_desc = 'Image -- ' + image_type + ' array ('
 for i = 0,fix(Naxis)-1 do image_desc = image_desc + ' '+ gettok(fdescript,' ')
 image_desc = image_desc+' )'

 endif else image_desc = 'No data'
 printf,!textunit, format='(a)',image_desc

 if N_ext GT 0 then begin
  for i = 1,N_ext do begin

  printf, !TEXTUNIT, 'Extension ' + strn(i) + ' -- '+extname[i]

  header_desc = '               Header : '+gettok(fdescript,' ')+' records'
  printf, !textunit, format = '(a)',header_desc

  table_type = gettok(fdescript,' ')

  case table_type of
   'A3DTABLE' : table_desc = 'Binary Table'
   'BINTABLE' : table_desc = 'Binary Table'
   'VAR_BINTABLE': table_desc = 'Variable length Binary Table'
   'TABLE':     table_desc = 'ASCII Table'
    ELSE:       table_desc = table_type
  endcase

  table_desc = '               ' + table_desc + ' ( '
  table_dim = fix( gettok( fdescript,' ') )
  if table_dim GT 0 then begin
          table_type = gettok(fdescript,' ')
          for j = 0, table_dim-1 do $
                table_desc = table_desc + gettok(fdescript,' ') + ' '
  endif
  table_desc = table_desc + ')'

  printf,!textunit, format='(a)',table_desc
 endfor
 endif

  printf, !TEXTUNIT, ' '
  endif 
  SKIP: free_lun, lun1
  endfor
  if ~silent then textclose, TEXTOUT=textout
  return

 BAD_FILE:
     message, 'Error reading FITS file ' + file, /CON
    goto,SKIP
 end
FUNCTION FXMOVE, UNIT, EXTEN, SILENT = Silent, EXT_NO = ext_no, ERRMSG=errmsg

;+
; NAME:
;     FXMOVE
; PURPOSE:
;     Skip to a specified extension number or name in a FITS file
;
; CALLING SEQUENCE:
;     STATUS=FXMOVE(UNIT, EXT, /Silent)
;     STATUS=FXMOVE(UNIT, EXTNAME, /Silent, EXT_NO=, ERRMSG= )
;
; INPUT PARAMETERS:
;     UNIT     = An open unit descriptor for a FITS data stream.
;     EXTEN   = Number of extensions to skip.
;                              or
;             Scalar string giving extension name (in the EXTNAME keyword)           
; OPTIONAL INPUT PARAMETER:
;     /SILENT - If set, then any messages about invalid characters in the 
;               FITS file are suppressed.
; OPTIONAL OUTPUT PARAMETER:
;       ERRMSG  = If this keyword is present, then any error messages will be
;                 returned to the user in this parameter rather than
;                 depending on the MESSAGE routine in IDL.  If no errors are
;                 encountered, then a null string is returned.
;
; RETURNS:
;     0 if successful.
;    -1 if an error is encountered.
;
; COMMON BLOCKS:
;      None.
; SIDE EFFECTS:
;      Repositions the file pointer.
; PROCEDURE:
;      Each FITS header is read in and parsed, and the file pointer is moved
;      to where the next FITS extension header until the desired
;      extension is reached.
; PROCEDURE CALLS:
;      FXPAR(), MRD_HREAD, MRD_SKIP
; MODIFICATION HISTORY:
;      Extracted from FXPOSIT 8-March-2000 by T. McGlynn
;      Added /SILENT keyword  14-Dec-2000 by W. Landsman
;      Save time by not reading the full header  W. Landsman   Feb. 2003
;      Allow extension name to be specified, added EXT_NO, ERRMSG keywords
;         W. Landsman  December 2006
;      Make search for EXTNAME case-independent  W.Landsman March 2007 
;      Avoid round-off error for very large extensions N. Piskunov Dec 2007
;      Assume since V6.1 (/INTEGER keyword available to PRODUCT() ) Dec 2007
;      Capture error message from MRD_HREAD (must be used with post-June 2009
;        version of MRD-HREAD)   W. Landsman  July 2009
;-
     On_error, 2
     compile_opt idl2

         DO_NAME = SIZE( EXTEN,/TNAME) EQ 'STRING'
	 PRINT_ERROR = NOT ARG_PRESENT(ERRMSG)
         ERRMSG = ''
         IF DO_NAME THEN BEGIN 
	              FIRSTBLOCK = 0
		      EXT_NO = 9999
		      ENAME = STRTRIM( STRUPCASE(EXTEN), 2 )
		      ON_IOERROR, ALLOW_PLUN
		      POINT_LUN, -UNIT, DUM
		      ON_IOERROR, NULL
         ENDIF ELSE BEGIN 
	              FIRSTBLOCK = 1
		      EXT_NO = EXTEN
	ENDELSE 	            
		
        FOR I = 1, EXT_NO DO BEGIN
               
;
;  Read the next header, and get the number of bytes taken up by the data.
; 

                IF EOF(UNIT) THEN BEGIN 
		    IF DO_NAME THEN ERRMSG = $
	'Extension name ' + ename + ' not found in FITS file' ELSE ERRMSG = $	    
	'EOF encountered while moving to specified extension'
	        if PRINT_ERROR then message,errmsg	
		RETURN, -1
		ENDIF
           
                ; Can't use FXHREAD to read from pipe, since it uses
                ; POINT_LUN.  So we read this in ourselves using mrd_hread

                MRD_HREAD, UNIT, HEADER, STATUS, SILENT = Silent, $
		    FIRSTBLOCK=FIRSTBLOCK, ERRMSG = ERRMSG
                IF STATUS LT 0 THEN BEGIN 
		    IF PRINT_ERROR THEN MESSAGE,ERRMSG   ;Typo fix 04/10
		    RETURN, -1
		ENDIF    
                        
                ; Get parameters that determine size of data
                ; region.
                IF DO_NAME THEN IF I GT 1 THEN BEGIN
		       EXTNAME = STRTRIM(SXPAR(HEADER,'EXTNAME',COUNT=N_name),2)
			 if N_NAME GT 0 THEN $
			  IF ENAME EQ STRUPCASE(EXTNAME) THEN BEGIN
			        EXT_NO= I-1
				BLOCK = 1 + ((N_ELEMENTS(HEADER)-1)/36)
				POINT_LUN, -UNIT, CURR_POSS
				POINT_LUN, UNIT, CURR_POSS - BLOCK*2880 
			        BREAK
			ENDIF	
		ENDIF	         
                BITPIX = FXPAR(HEADER,'BITPIX')
                NAXIS  = FXPAR(HEADER,'NAXIS')
                GCOUNT = FXPAR(HEADER,'GCOUNT') 
                IF GCOUNT EQ 0 THEN GCOUNT = 1
                PCOUNT = FXPAR(HEADER,'PCOUNT')
                
                IF NAXIS GT 0 THEN BEGIN 
                        DIMS = FXPAR(HEADER,'NAXIS*')           ;Read dimensions
			NDATA = PRODUCT(DIMS,/INTEGER) 
                ENDIF ELSE NDATA = 0
                
                NBYTES = LONG64(ABS(BITPIX) / 8) * GCOUNT * (PCOUNT + NDATA)
;
;  Move to the next extension header in the file.
;
                NREC = (NBYTES + 2879) / 2880
                
                MRD_SKIP, UNIT, NREC*2880L 

        ENDFOR
	        
        RETURN, 0
ALLOW_PLUN:
        
        ERRMSG =  $
	'Extension name cannot be specified unless POINT_LUN access is available'
	if PRINT_ERROR then message,errmsg	
	RETURN, -1        
END
        FUNCTION FXPOSIT, XFILE, EXT_NO, readonly=readonly, COMPRESS=COMPRESS, $
                 SILENT = Silent, EXTNUM = extnum, ERRMSG= ERRMSG, $
		 LUNIT = lunit, UNIXPIPE= unixpipe, FPACK= fpack, $
		 NO_FPACK = no_fpack,HEADERONLY=headeronly
;+
; NAME:
;     FXPOSIT
; PURPOSE:
;     Return the unit number of a FITS file positioned at specified extension
; EXPLANATION:
;     The FITS file will be ready to be read at the beginning of the 
;     specified extension.    Either an extension number or extension name
;     can be specified.   Called by headfits.pro, mrdfits.pro
;
;     Modified in March 2009 to set the /SWAP_IF_LITTLE_ENDIAN keyword
;     when opening a file, and **may not be compatible with earlier versions**
; CALLING SEQUENCE:
;     unit=FXPOSIT(FILE, EXT_NO_OR_NAME, /READONLY, COMPRESS=program, 
;                       UNIXPIPE=, ERRMSG= , EXTNUM= , UNIT=, /SILENT
;                        /FPACK, /NO_FPACK
;
; INPUT PARAMETERS:
;     FILE    = FITS file name, scalar string.    If an empty string is supplied
;              then the user will be prompted for the file name.   The user
;              will also be prompted if a wild card is supplied, and more than 
;              one file matches the wildcard.
;     EXT_NO_OR_NAME  = Either the extension to be moved to (scalar 
;               nonnegative integer) or the name of the extension to read 
;               (scalar string)
;
; RETURNS:
;     Unit number of file or -1 if an error is detected.
;
; OPTIONAL INPUT KEYWORD PARAMETER:
;     COMPRESS - If this keyword is set and non-zero, then then treat
;                the file as compressed.  If 1 assume a gzipped file.
;                and use IDLs internal decompression facility.    For Unix 
;                compressed or bzip2 compressed files spawn off a process to 
;                decompress and use its output as the FITS stream.  If the 
;                keyword is not 1, then use its value as a string giving the 
;                command needed for decompression.
;     /FPACK - Signal that the file is compressed with the FPACK software. 
;               http://heasarc.gsfc.nasa.gov/fitsio/fpack/ ) By default, 
;               (FXPOSIT will assume that if the file name extension ends in 
;              .fz that it is fpack compressed.)     The FPACK software must
;               be installed on the system 
;     /NO_FPACK - The unit will only be used to read the FITS header.  In
;                 that case FPACK compressed files need not be uncompressed.
;      LUNIT -    Integer giving the file unit number.    Use this keyword if
;                you want to override the default use of GET_LUN to obtain
;                a unit number.
;     /READONLY - If this keyword is set and non-zero, then OPENR rather 
;                than OPENU will be used to open the FITS file.    Note that
;                 compressed files are always set to /READONLY
;     /SILENT    If set, then suppress any messages about invalid characters
;                in the FITS file.
;
; OPTIONAL OUTPUT KEYWORDS:
;       EXTNUM - Nonnegative integer give the extension number actually read
;               Useful only if the extension was specified by name.
;       ERRMSG  = If this keyword is present, then any error messages will be
;                 returned to the user in this parameter rather than
;                 depending on the MESSAGE routine in IDL.  If no errors are
;                 encountered, then a null string is returned.
;       UNIXPIPE - If set to 1, then the FITS file was opened with a UNIX pipe
;                rather than with the OPENR command.    This is only required 
;                 when reading a FPACK, bzip or Unix compressed file.   Note 
;                 that automatic byteswapping cannnot be set for a Unix pipe, 
;                 since the SWAP_IF_LITTLE_ENDIAN keyword is only available for the
;                 OPEN command, and it is the responsibilty of the calling 
;                 routine to perform the byteswapping.
; SIDE EFFECTS:
;      Opens and returns a file unit.
; PROCEDURE:
;      Open the appropriate file, or spawn a command and intercept
;      the output.
;      Call FXMOVE to get to the appropriate extension.
; PROCEDURE CALLS:
;      FXMOVE()
; MODIFICATION HISTORY:
;      Derived from William Thompson's FXFINDEND routine.
;      Modified by T.McGlynn, 5-October-1994.
;       Modified by T.McGlynn, 25-Feb-1995 to handle compressed
;          files.  Pipes cannot be accessed using FXHREAD so
;          MRD_HREAD was written.
;       W. Landsman 23-Apr-1997    Force the /bin/sh shell when uncompressing 
;       T. McGlynn  03-June-1999   Use /noshell option to get rid of processes left by spawn.
;                                  Use findfile to retain ability to use wildcards
;       W. Landsman 03-Aug-1999    Use EXPAND_TILDE under Unix to find file
;       T. McGlynn  04-Apr-2000    Put reading code into FXMOVE,
;                                  additional support for compression from D.Palmer.
;       W. Landsman/D.Zarro 04-Jul-2000    Added test for !VERSION.OS EQ 'Win32' (WinNT)
;       W. Landsman    12-Dec-2000 Added /SILENT keyword
;       W. Landsman April 2002     Use FILE_SEARCH for V5.5 or later
;       W. Landsman Feb 2004       Assume since V5.3 (OPENR,/COMPRESS available)
;       W. Landsman,W. Thompson, 2-Mar-2004, Add support for BZIP2 
;       W. Landsman                Don't leave open file if an error occurs
;       W. Landsman  Sep 2004      Treat FTZ extension as gzip compressed
;       W. Landsman  Feb 2006      Removed leading spaces (prior to V5.5)
;       W. Landsman  Nov 2006      Allow specification of extension name
;                                  Added EXTNUM, ERRMSG keywords
;       W. Landsman/N.Piskunov Dec 2007  Added LUNIT keyword
;       W. Landsman     Mar 2009   OPEN with /SWAP_IF_LITTLE_ENDIAN
;                                  Added UNIXPIPE output keyword
;       N. Rich        May 2009    Check if filename is an empty string
;       W. Landsman   May 2009     Support FPACK compressed files
;                                  Added /FPACK, /HEADERONLY keywords
;       W.Landsman    July 2009    Deprecated /HEADERONLY add /NO_FPACK
;       W.Landsman    July 2011    Check for SIMPLE in first 8 chars 
;               Use gunzip to decompress Unix. Z file since compress utility 
;               often not installed anymore)
;-
;
        On_Error,2
        compile_opt idl2  
;
;  Check the number of parameters.
;
        IF N_Params() LT 2 THEN BEGIN 
            PRINT,'SYNTAX:  UNIT = FXPOSIT(FILE, EXT_NO, /Readonly,' + $
	                   'ERRMSG= , /SILENT, compress=prog, LUNIT = lunit)'
            RETURN,-1
        ENDIF
        PRINTERR = ~ARG_PRESENT(ERRMSG)
	ERRMSG = ''
	UNIXPIPE=0
; The /headeronly keyword has been replaced with /no_fpack	
        if ~keyword_set(no_fpack) then no_fpack = keyword_set(headeronly)
	exten = ext_no

   	COUNT=0
	IF XFILE[0] NE '' THEN BEGIN 
             FILE = FILE_SEARCH(XFILE, COUNT=COUNT)  
	     IF COUNT GT 1 THEN $
	          FILE = DIALOG_PICKFILE(FILTER=XFILE, /MUST_EXIST, $
		         TITLE = 'Please select a FITS file')
	ENDIF ELSE BEGIN 
             FILE =DIALOG_PICKFILE(FILTER=['*.fit*;*.fts*;*.img*;*.FIT*'], $
	           TITLE='Please select a FITS file',/MUST_EXIST)
         ENDELSE
             COUNT = N_ELEMENTS(FILE)
	            

        IF COUNT EQ 0 THEN BEGIN
	    ERRMSG = 'Specified FITS File not found ' + XFILE[0]
	    IF PRINTERR THEN MESSAGE,ERRMSG,/CON 
            RETURN, -1   ; Don't print anything out, just report an error
	ENDIF    
        
        FILE = FILE[0]
;
;  Check if logical unit number is specified explicitly.
;
        IF KEYWORD_SET(LUNIT) THEN BEGIN 
	   UNIT=LUNIT 
	   GLUN = 0
	ENDIF ELSE BEGIN 
	    UNIT = -1
            GLUN = 1
       ENDELSE
; 
;  Check if this is a compressed file.
;
        UCMPRS = ' '
	IF KEYWORD_SET(compress) THEN BEGIN
	    IF strcompress(string(compress),/remo) eq '1' THEN BEGIN
	        compress = 'gunzip'
	    ENDIF
	    UCMPRS = compress;
	ENDIF ELSE IF KEYWORD_SET(FPACK) THEN $
	    UCMPRS = 'funpack'      $
	ELSE BEGIN
        
            LEN = STRLEN(FILE)
            IF LEN GT 3 THEN $
	        tail = STRLOWCASE(STRMID(file, len-3, 3))  $
	    ELSE tail = ' '
	    
            IF STRMID(tail,1,2) EQ '.z'  THEN $
                UCMPRS = 'gunzip'   $
	    ELSE IF (tail EQ '.gz') || (tail EQ 'ftz') THEN $
	        UCMPRS = 'gzip'       $
	    ELSE IF tail EQ 'bz2' THEN $
	        UCMPRS = 'bunzip2'     $
	    ELSE IF ~KEYWORD_SET(NO_FPACK) THEN $
	          IF tail EQ '.fz' THEN UCMPRS = 'funpack'	
	    
	ENDELSE

;  Handle compressed files which are always opened for Read only.

	IF UCMPRS EQ 'gzip' THEN BEGIN
	        
                OPENR, UNIT, FILE, /COMPRESS, GET_LUN=glun, ERROR = ERROR, $
		           /SWAP_IF_LITTLE       
                IF ERROR NE 0 THEN BEGIN
                        IF PRINTERR THEN PRINT,!ERROR_STATE.MSG ELSE $
			    ERRMSG = !ERROR_STATE.MSG 
                        RETURN,-1
                ENDIF
 
	ENDIF ELSE IF UCMPRS NE ' ' THEN BEGIN
; Handle FPACK compressed file.        If an extension name is supplied then
; first recursively call FXPOSIT to get the extension number.    Then open 
; the bidirectional pipe. 	
		        if UCMPRS EQ 'funpack' then begin
			if size(exten,/TNAME) EQ 'STRING' THEN BEGIN
			unit = fxposit( file, ext_no, /no_fpack,extnum=extnum)
			free_lun,unit
			exten = extnum
			endif 
			SPAWN, [UCMPRS,'-S',FILE], UNIT=UNIT, /NOSHELL 
			ENDIF else $
                        SPAWN, [UCMPRS,'-c',FILE], UNIT=UNIT, /NOSHELL
			UNIXPIPE = 1
                  
        ENDIF ELSE BEGIN
;
;  Go to the start of the file.
;
                IF KEYWORD_SET(READONLY) THEN $
                    OPENR, UNIT, FILE, GET_LUN=glun, ERROR = ERROR, $
                           /SWAP_IF_LITTLE ELSE                     $
                    OPENU, UNIT, FILE, GET_LUN=glun, ERROR = ERROR, $
                           /SWAP_IF_LITTLE 

                IF ERROR NE 0 THEN BEGIN
                        IF PRINTERR THEN PRINT,!ERROR_STATE.MSG ELSE $
			    ERRMSG = !ERROR_STATE.MSG 
                        RETURN,-1
                ENDIF
        ENDELSE
	
        IF SIZE(EXT_NO,/TNAME) NE 'STRING' THEN $
	      IF EXT_NO LE 0 THEN RETURN, UNIT

;For Uncompresed files test that the first 8 characters are 'SIMPLE'

        IF ucmprs EQ ' ' THEN BEGIN
          simple = BytArr(6)
	  READU,unit,simple
          if string(simple) NE 'SIMPLE' then begin 
                IF ~KEYWORD_SET(LUNIT) THEN Free_Lun, unit
	        ERRMSG = "ERROR - FITS File must begin with 'SIMPLE'" 
		if printerr THEN MESSAGE,errmsg,/CON
		return,-1
           endif 	
	point_lun,unit,0    	
	endif
	stat = FXMOVE(unit, exten, SILENT = Silent, EXT_NO = extnum, $
	ERRMSG=errmsg)

	IF stat LT 0 THEN BEGIN
            IF ~KEYWORD_SET(LUNIT) THEN Free_Lun, unit
	    IF PrintErr THEN MESSAGE,ErrMsg
	    RETURN, stat
	ENDIF ELSE RETURN, unit
END
pro mrd_hread, unit, header, status, SILENT = silent, FIRSTBLOCK = firstblock, $
    ERRMSG = errmsg,SKIPDATA=skipdata
;+
; NAME: 
;     MRD_HREAD
;
; PURPOSE: 
;     Reads a FITS header from an opened disk file or Unix pipe
; EXPLANATION:
;     Like FXHREAD but also works with compressed Unix files
;
; CALLING SEQUENCE: 
;     MRD_HREAD, UNIT, HEADER  [, STATUS, /SILENT, ERRMSG =, /FIRSTBLOCK ]
; INPUTS: 
;     UNIT    = Logical unit number of an open FITS file
; OUTPUTS: 
;     HEADER  = String array containing the FITS header.
; OPT. OUTPUTS: 
;     STATUS  = Condition code giving the status of the read.  Normally, this
;                 is zero, but is set to -1 if an error occurs, or if the
;                 first byte of the header is zero (ASCII null).
; OPTIONAL KEYWORD INPUT:
;      /FIRSTBLOCK - If set, then only the first block (36 lines or less) of 
;                the FITS header are read into the output variable.   If only
;                size information (e.g. BITPIX, NAXIS) is needed from the
;                header, then the use of this keyword can save time.  The 
;                file pointer is still positioned at the end of the header,
;                even if the /FIRSTBLOCK keyword is supplied.
;      /SILENT - If set, then warning messages about any invalid characters in
;                the header are suppressed.
;     /SKIPDATA - If set, then the file point is positioned at the end of the
;                HDU after the header is read, i.e. the following data block
;                is skipped.   Useful, when one wants to the read the headers
;                of multiple extensions.
; OPTIONAL OUTPUT PARAMETER:
;       ERRMSG  = If this keyword is present, then any error messages will be
;                 returned to the user in this parameter rather than
;                 depending on the MESSAGE routine in IDL.  If no errors are
;                 encountered, then a null string is returned.
; RESTRICTIONS: 
;      The file must already be positioned at the start of the header.  It
;      must be a proper FITS file.
; SIDE EFFECTS: 
;       The file ends by being positioned at the end of the FITS header, unless
;       an error occurs.
; REVISION HISTORY:
;      Written,  Thomas McGlynn                     August 1995
;      Modified, Thomas McGlynn		     January 1996
;         Changed MRD_HREAD to handle Headers which have null characters
;          A warning message is printed out but the program continues.
;          Previously MRD_HREAD would fail if the null characters were
;          not in the last 2880 byte block of the header.  Note that
;          such characters are illegal in the header but frequently
;          are produced by poor FITS writers.
;      Added /SILENT keyword   W. Landsman   December 2000
;      Added /FIRSTBLOCK keyword  W. Landsman   February 2003
;      Added ERRMSG, SKIPDATA keyword W. Landsman          April 2009
;      Close file unit even after error message   W.L.  October 2010
;-
  On_error,2
  compile_opt idl2
  printerr = ~arg_present(errmsg)
  errmsg = ''

  block = string(replicate(32b, 80, 36))
		
  Nend = 0                  ;Signal if 'END     ' statement is found
  nblock = 0

  while Nend EQ 0 do begin
		
; Shouldn't get eof in middle of header.
       if eof(unit) then begin
                errmsg = 'EOF encountered in middle of FITS header'
		if printerr then message,errmsg,/CON
		free_lun, unit
		status = -1
		return
		endif
		
	on_ioerror, error_return
	readu, unit, block
	on_ioerror, null

; Check that there aren't improper null characters in strings that are causing 
; them to be truncated.   Issue a warning but continue if problems are found.

	w = where(strlen(block) ne 80, Nbad)
	if (Nbad GT 0) then begin
		if ~keyword_set(SILENT) then message, /INF, $
                            'Warning-Invalid characters in header'
		block[w] = string(replicate(32b, 80))
	endif	       
	w = where(strmid(block, 0, 8) eq 'END     ', Nend)
        if nblock EQ 0 then begin
               header = Nend GT 0 ?  block[ 0:w[0] ] : block
	       nblock = nblock + 1
        endif else $
	       if ~keyword_set(firstblock) then $
	         header = Nend GT 0 ? [header,block[0:w[0]]] : [header, block]
			
	endwhile
		
        if keyword_set(skipdata) then begin 
                bitpix = fxpar(header,'bitpix')
                naxis  = fxpar(header,'naxis')
                gcount = fxpar(header,'gcount') 
                if gcount eq 0 then gcount = 1
                pcount = fxpar(header,'pcount')
               
                if naxis gt 0 then begin 
                        dims = fxpar(header,'naxis*')           ;read dimensions
  			ndata = product(dims,/integer)
                endif else ndata = 0
                
                nbytes = long64(abs(bitpix) / 8) * gcount * (pcount + ndata)
	        mrd_skip, unit, nbytes
	endif	
	status = 0
	return
error_return:
        status = -1
	errmsg = 'END Statement not found in FITS header'
        if printerr then message, 'ERROR ' + errmsg	
	return
end
			
;+
; NAME:
;     MRDFITS
;
; PURPOSE:
;     Read all standard FITS data types into arrays or structures.
;
; EXPLANATION:
;      Further information on MRDFITS is available at
;      http://idlastro.gsfc.nasa.gov/mrdfits.html 
;
;      **This version requires a post March 2009 version of fxposit.pro**
; CALLING SEQUENCE:
;      Result = MRDFITS( Filename/FileUnit,[Exten_no/Exten_name, Header],
;                       /FPACK, /NO_FPACK, /FSCALE , /DSCALE , /UNSIGNED,
;                       ALIAS=strarr[2,n], /USE_COLNUM,
;                       /NO_TDIM, ROWS = [a,b,...], $
;                       /POINTER_VAR, /FIXED_VAR, EXTNUM= 
;                       RANGE=[a,b], COLUMNS=[a,b,...]), ERROR_ACTION=x,
;                       COMPRESS=comp_prog, STATUS=status, /VERSION, 
;                       /EMPTYSTRING )
;
; INPUTS:
;      Filename = String containing the name of the file to be read or
;                 file number of an open unit.  If an empty string is supplied,
;                 then user will be prompted for the file name.    The user
;                 will also be prompted if a wild card is given in the file
;                 name, and there is more than one file name match.
;                 If the file name ends in .gz or .fz (or .Z on Unix systems)
;                 the file will be dynamically decompressed.
;                                    or
;      FiluUnit = An integer file unit which has already been
;                 opened for input.  Data will be read from this
;                 unit and the unit will be left pointing immediately
;                 after the HDU that is read.  Thus to read a compressed
;                 file with many HDU's a user might do something like:
;                      lun=fxposit(filename, 3)  ; Skip the first three HDU's
;                      repeat begin
;                          thisHDU = mrdfits(lun, 0, hdr, status=status)
;                          ... process the HDU ...
;                      endrep until status lt 0
;
;      Exten_no= Extension number to be read, 0 for primary array.
;                 Assumed 0 if not specified.
;                 If a unit rather than a filename
;                 is specified in the first argument, this is
;                 the number of HDU's to skip from the current position.
;      Exten_name - Name of the extension to read (as stored in the EXTNAME
;                 keyword).   This is a slightly slower method then specifying
;                 the extension number.
; OUTPUTS:
;      Result = FITS data array or structure constructed from
;               the designated extension.  The format of result depends
;               upon the type of FITS data read.
;             Non-group primary array or IMAGE extension:
;               A simple multidimensional array is returned with the
;               dimensions given in the NAXISn keywords.
;             Grouped image data with PCOUNT=0.
;               As above but with GCOUNT treated as NAXIS(n+1).
;             Grouped image data with PCOUNT>0.
;               The data is returned as an array of structures.  Each
;               structure has two elements.  The first is a one-dimensional
;               array of the group parameters, the second is a multidimensional
;               array as given by the NAXIS2-n keywords.
;             ASCII and BINARY tables.
;               The data is returned as a structure with one column for
;               each field in the table.  The names of the columns are
;               normally taken from the TTYPE keywords (but see USE_COLNUM).
;               Bit field columns
;               are stored in byte arrays of the minimum necessary
;               length.  Spaces and invalid characters are replaced by 
;               underscores, and other invalid tag names are converted using
;               the IDL_VALIDNAME(/CONVERT_ALL) function.
;               Columns specified as variable length columns are stored
;               with a dimension equal to the largest actual dimension
;               used.  Extra values in rows are filled with 0's or blanks.
;               If the size of the variable length column is not
;               a constant, then an additional column is created giving the 
;               size used in the current row.  This additional column will 
;               have a tag name of the form L#_"colname" where # is the column
;               number and colname is the column name of the variable length
;               column.   If the length of each element of a variable length 
;               column is 0 then the column is deleted.
;
;
; OPTIONAL OUTPUT:
;       Header = String array containing the header from the FITS extension.
;
; OPTIONAL INPUT KEYWORDS:
;       ALIAS    The keyword allows the user to specify the column names
;                to be created when reading FITS data.  The value of
;                this keyword should be a 2xn string array.  The first
;                value of each pair of strings should be the desired
;                tag name for the IDL column.  The second should be
;                the FITS TTYPE value.  Note that there are restrictions
;                on valid tag names.  The order of the ALIAS keyword
;                is compatible with MWRFITS.
;       COLUMNS - This keyword allows the user to specify that only a
;                subset of columns is to be returned.  The columns
;                may be specified either as number 1,... n or by
;                name or some combination of these two.
;                If /USE_COLNUM is specified names should be C1,...Cn.
;                The use of this keyword will not save time or internal
;                memory since the extraction of specified columns
;                is done after all columns have been retrieved from the
;                FITS file.      Structure columns are returned in the order
;                supplied in this keyword.
;       COMPRESS - This keyword allows the user to specify a
;                decompression program to use to decompress a file that
;                will not be automatically recognized based upon
;                the file name.
;       /DSCALE - As with FSCALE except that the resulting data is
;                stored in doubles.
;       /EMPTYSTRING - There was a bug in memory management for IDL versions 
;                 prior to V8.0, causing a memory leak when reading
;                 empty strings in a FITS table.   Setting /EMPTYSTRING will
;                 avoid this problem by first reading strings into bytes and
;                 then converting.   However, there is a performance penalty.                 
;       ERROR_ACTION - Set the on_error action to this value (defaults
;                to 2).
;       /FIXED_VAR- Translate variable length columns into fixed length columns
;                and provide a length column for truly varying columns.
;                This was only behavior prior to V2.5 for MRDFITS and remains
;                the default (see /POINTER_VAR)
;       /FPACK - If set, then assume the FITS file uses FPACK compression 
;                (http://heasarc.gsfc.nasa.gov/fitsio/fpack/).     MRDFITS
;                will automatically detect FPACK compressed files, but it is
;                more efficient to supply the /FPACK keyword.   A file with an
;                extension of .fz is assumed to be Fpack compressed.
;       /NO_FPACK - If present, then MRDFITS will not uncompress an extension
;                compressed with FPACK, but will just read the compressed
;                binary stream. 
;       /FSCALE - If present and non-zero then scale data to float
;                numbers for arrays and columns which have either
;                non-zero offset or non-unity scale.
;                If scaling parameters are applied, then the corresponding
;                FITS scaling keywords will be modified.
;       NO_TDIM  - Disable processing of TDIM keywords.  If NO_TDIM
;                is specified MRDFITS will ignore TDIM keywords in
;                binary tables.
;       /POINTER_VAR- Use pointer arrays for variable length columns.
;                In addition to changing the format in which
;                variable length arrays are stored, if the pointer_var
;                keyword is set to any value other than 1 this also disables
;                the deletion of variable length columns. (See /FIXED_VAR)
;                Note that because pointers may be present in the output
;                structure, the user is responsible for memory management
;                when deleting or reassigning the structure (e.g. use HEAP_FREE
;                first).
;       RANGE  - A scalar or two element vector giving the start
;                and end rows to be retrieved.  For ASCII and BINARY
;                tables this specifies the row number.  For GROUPed data
;                this will specify the groups.  For array images, this
;                refers to the last non-unity index in the array.  E.g.,
;                for a 3 D image with NAXIS* values = [100,100,1], the
;                range may be specified as 0:99, since the last axis
;                is suppressed.  Note that the range uses IDL indexing
;                So that the first row is row 0.
;                If only a single value, x, is given in the range,
;                the range is assumed to be [0,x-1].
;       ROWS -  A scalar or vector specifying a  specific row or rows to read 
;               (first row is 0).   For example to read rows 0,
;               12 and 23 only, set ROWS=[0,12,23].   Valid for images, ASCII 
;               and binary tables, but not GROUPed data.   For images
;               the row numbers refer to the last non-unity index in the array.
;               Note that the use of the ROWS will not improve the speed of
;               MRDFITS since the entire table will be read in, and then subset
;               to the specified rows.     Cannot be used at the same time as 
;               the RANGE keyword
;       /SILENT - Suppress informative messages.
;       STRUCTYP - The structyp keyword specifies the name to be used
;                for the structure defined when reading ASCII or binary
;                tables.  Generally users will not be able to conveniently
;                combine data from multiple files unless the STRUCTYP
;                parameter is specified.  An error will occur if the
;                user specifies the same value for the STRUCTYP keyword
;                in calls to MRDFITS in the same IDL session for extensions
;                which have different structures.
;       /UNSIGNED - For integer data with appropriate zero points and scales
;                read the data into unsigned integer arrays.
;       /USE_COLNUM - When creating column names for binary and ASCII tables
;                MRDFITS attempts to use the appropriate TTYPE keyword
;                values.  If USE_COLNUM is specified and non-zero then
;                column names will be generated as 'C1, C2, ... 'Cn'
;                for the number of columns in the table.
;       /VERSION Print the current version number
;
; OPTIONAL OUTPUT KEYWORDS:
;       EXTNUM - the number of the extension actually read.   Useful if the
;                 user specified the extension by name.
;       OUTALIAS - This is a 2xn string array where the first column gives the
;                actual structure tagname, and the second gives the
;                corresponding FITS keyword name (e.g. in the TTYPE keyword).   
;                This array can be passed directly to
;                the alias keyword of MWRFITS to recreate the file originally
;                read by MRDFITS.
;       STATUS - A integer status indicating success or failure of
;                the request.  A status of >=0 indicates a successful read.
;                Currently
;                    0 -> successful completion
;                   -1 -> error
;                   -2 -> end of file
;
; EXAMPLES:
;       (1) Read a FITS primary array:
;               a = mrdfits('TEST.FITS')    or
;               a = mrdfits('TEST.FITS', 0, header)
;       The second example also retrieves header information.
;
;       (2) Read rows 10-100 of the second extension of a FITS file.
;               a = mrdfits('TEST.FITS', 2, header, range=[10,100])
;
;       (3) Read a table and ask that any scalings be applied and the
;       scaled data be converted to doubles.  Use simple column names,
;       suppress outputs.
;               a = mrdfits('TEST.FITS', 1, /dscale, /use_colnum, /silent)
;
;       (4) Read rows 3, 34 and 52 of a binary table and request that 
;           variable length columns be stored as a pointer variable in the 
;           output structure
;              a = mrdfits('TEST.FITS',1,rows=[3,34,52],/POINTER)
  
; RESTRICTIONS:
;       (1)     Cannot handle data in non-standard FITS formats.
;       (2)     Doesn't do anything with BLANK or NULL values or
;               NaN's.  They are just read in.  They may be scaled
;               if scaling is applied.
; NOTES:
;       This multiple format FITS reader is designed to provide a
;       single, simple interface to reading all common types of FITS data.
;       MRDFITS DOES NOT scale data by default.  The FSCALE or DSCALE
;       parameters must be used.
;
;       Null values in an FITS ASCII table are converted to NaN (floating data),
;       or -2147483647L (longwords) or '...' (strings).   
;
; PROCEDURES USED:
;       The following procedures are contained in the main MRDFITS program.
;           MRD_IMAGE           -- Generate array/structure for images.
;           MRD_READ_IMAGE      -- Read image data.
;           MRD_ASCII           -- Generate structure for ASCII tables.
;           MRD_READ_ASCII      -- Read an ASCII table.
;           MRD_TABLE           -- Generate structure for Binary tables.
;           MRD_READ_TABLE      -- Read binary table info.
;           MRD_READ_HEAP       -- Read variable length record info.
;           MRD_SCALE           -- Apply scaling to data.
;           MRD_COLUMNS         -- Extract columns.
;
;        Other ASTRON Library routines used
;           FXPAR(), FXADDPAR, FXPOSIT, FXMOVE(), MATCH, MRD_STRUCT(), MRD_SKIP 
;
; MODIfICATION HISTORY:
;       V1.0 November 9, 1994 ----  Initial release.
;          Creator: Thomas A. McGlynn
;       V1.1 January 20, 1995 T.A. McGlynn
;          Fixed bug in variable length records.
;          Added TDIM support -- new routine mrd_tdim in MRD_TABLE.
;       V1.2
;          Added support for dynamic decompression of files.
;          Fixed further bugs in variable length record handling.
;       V1.2a
;          Added NO_TDIM keyword to turn off TDIM processing for
;          those who don't want it.
;          Bug fixes: Handle one row tables correctly, use BZERO rather than
;               BOFFSET.     Fix error in scaling of images.  
;       V1.2b 
;          Changed MRD_HREAD to handle null characters in headers.
;       V2.0 April 1, 1996
;          -Handles FITS tables with an arbitrary number of columns.
;          -Substantial changes to MRD_STRUCT to allow the use of
;          substructures when more than 127 columns are desired.
;          -All references to table columns are now made through the
;          functions MRD_GETC and MRD_PUTC.  See description above.
;          -Use of SILENT will now eliminate compilation messages for
;          temporary functions.
;          -Bugs in handling of variable length columns with either
;          a single row in the table or a maximum of a single element
;          in the column fixed.
;          -Added support for DCOMPLEX numbers in binary tables (M formats) for
;          IDL versions above 4.0.  
;          -Created regression test procedure to check in new versions.
;          -Added error_action parameter to allow user to specify
;          on_error action.  This should allow better interaction with
;          new CHECK facility.  ON_ERROR statements deleted from
;          most called routines.
;          - Modified MRDFITS to read in headers containing null characters
;          with a warning message printed.
;       V2.0a April 16, 1996
;          - Added IS_IEEE_BIG() checks (and routine) so that we don't
;          worry about IEEE to host conversions if the machine's native
;          format is IEEE Big-endian.
;       V2.1 August 24, 1996
;          - Use resolve_routine for dynamically defined functions
;          for versions > 4.0
;          - Fix some processing in random groups format.
;          - Handle cases where the data segment is--legally--null.
;          In this case MRDFITS returns a scalar 0.
;          - Fix bugs with the values for BSCALE and BZERO (and PSCAL and
;          PZERO) parameters set by MRDFITS.
;       V2.1a April 24, 1997  Handle binary tables with zero length columns
;       V2.1b May 13,1997 Remove whitespace from replicate structure definition
;       V2.1c May 28,1997 Less strict parsing of XTENSION keyword
;       V2.1d June 16, 1997 Fixed problem for >32767 entries introduced 24-Apr
;       V2.1e Aug 12, 1997 Fixed problem handling double complex arrays
;       V2.1f Oct 22, 1997 IDL reserved words can't be structure tag names
;       V2.1g Nov 24, 1997 Handle XTENSION keywords with extra blanks.
;       V2.1h Jul 26, 1998 More flexible parsing of TFORM characters
;       V2.2 Dec 14, 1998 Allow fields with longer names for
;                        later versions of IDL.
;                        Fix handling of arrays in scaling routines.
;                        Allow >128 fields in structures for IDL >4.0
;                        Use more efficient structure copying for
;                        IDL>5.0
;       V2.2b June 17, 1999 Fix bug in handling case where
;                           all variable length columns are deleted
;                           because they are empty.
;       V2.3 March 7, 2000 Allow user to supply file handle rather
;                          than file name.
;                          Added status field.
;                          Now needs FXMOVE routine
;       V2.3b April 4, 2000
;                          Added compress option (from D. Palmer)
;       V2.4  July 4, 2000 Added STATUS=-1 for "File access error" (Zarro/GSFC)
;       V2.4a May 2, 2001  Trim binary format string   (W. Landsman)
;       V2.5 December 5, 2001 Add unsigned, alias, 64 bit integers. version, $
;                           /pointer_val, /fixed_var.
;       V2.5a Fix problem when both the first and the last character
;            in a TTYPEnn value are invalid structure tag characters
;       V2.6 February 15, 2002 Fix error in handling unsigned numbers, $
;                           and 64 bit unsigneds. (Thanks to Stephane Beland)
;       V2.6a September 2, 2002 Fix possible conflicting data structure for
;                          variable length arrays (W. Landsman)
;       V2.7 July, 2003  Added Rows keyword (W. Landsman)
;       V2.7a September  2003 Convert dimensions to long64 to handle huge files
;       V2.8 October 2003 Use IDL_VALIDNAME() function to ensure valid tag names
;                         Removed OLD_STRUCT and TEMPDIR keywords W. Landsman
;       V2.9 February 2004 Added internal MRD_FXPAR procedure for faster
;                     processing of binary table headers E. Sheldon
;       V2.9a March 2004 Restore ability to read empty binary table W. Landsman
;             Swallow binary tables with more columns than given in TFIELDS
;       V2.9b Fix to ensure order of TFORMn doesn't matter
;       V2.9c Check if extra degenerate NAXISn keyword are present W.L. Oct 2004 
;       V2.9d Propagate /SILENT to MRD_HREAD, more LONG64 casting W. L. Dec 2004
;       V2.9e Add typarr[good] to fix a problem reading zero-length columns
;             A.Csillaghy, csillag@ssl.berkeley.edu (RHESSI)
;       V2.9f Fix problem with string variable binary tables, possible math 
;             overflow on non-IEEE machines  WL Feb. 2005 
;       V2.9g Fix problem when setting /USE_COLNUM   WL Feb. 2005
;       V2.10 Use faster keywords to BYTEORDER  WL May 2006
;       V2.11  Add ON_IOERROR, CATCH, and STATUS keyword to MRD_READ_IMAGE to
;             trap EOF in compressed files DZ  Also fix handling of unsigned 
;             images when BSCALE not present  K Chu/WL   June 2006 
;       V2.12 Allow extension to be specified by name, added EXTNUM keyword
;                     WL    December 2006
;       V2.12a Convert ASCII table column to DOUBLE if single precision is
;                 insufficient  
;       V2.12b Fixed problem when both /fscale and /unsigned are set 
;                  C. Markwardt    Aug 2007
;       V2.13  Use SWAP_ENDIAN_INPLACE instead of IEEE_TO_HOST and IS_IEEE_BIG
;                W. Landsman Nov 2007
;       V2.13a One element vector allowed for file name W.L. Dec 2007
;       V2.13b More informative error message when EOF found W.L. Jun 2008
;       V2.14  Use vector form of VALID_NUM(), added OUTALIAS keyword
;                                       W.L. Aug 2008
;       V2.15  Use new FXPOSIT which uses on-the-fly byteswapping W.L. Mar 2009
;       V2.15a Small efficiency updates to MRD_SCALE W.L. Apr 2009
;       V2.15b Fixed typo introduced Apr 2009
;       V2.15c Fix bug introduced Mar 2009  when file unit used W.L. July 2009
;       V2.16  Handle FPACK compressed files    W. L. July 2009
;       V2.17  Use compile_opt hidden on all routines except mrdfits.pro W.L. July 2009
;       V2.18  Added /EMPTYSTRING keyword W. Landsman August 2009
;       V2.18a Fix Columns keyword output, A. Kimball/ W. Landsman Feb 2010
;       V2.18b Fix bug with /EMPTYSTRING and multidimensional strings
;                             S. Baldridge/W.L. August 2010
;       V2.18c Fix unsigned bug caused by compile_opt idl2 WL  Nov 2010
;       V2.19  Use V6.0 operators WL Nov 2010
;       V2.19a Fix complex data conversion in variable length tables WL Dec 2010 
;       V2.19b Fix bug with /FSCALE introduced Nov 2010 WL Jan 2011
;       V2.19c Fix bug with ROWS keyword introduced Nov 2010 WL Mar 2011
;       V2.20  Convert Nulls in ASCII tables, better check of duplicate keywords
;-
PRO mrd_fxpar, hdr, xten, nfld, nrow, rsize, fnames, fforms, scales, offsets
compile_opt idl2, hidden
;
;  Check for valid header.  Check header for proper attributes.
;
  S = SIZE(HDR)
  IF ( S[0] NE 1 ) OR ( S[2] NE 7 ) THEN $
    MESSAGE,'FITS Header (first parameter) must be a string array'

  xten = fxpar(hdr, 'XTENSION')
  nfld = fxpar(hdr, 'TFIELDS')
  nrow = long64(fxpar(hdr, 'NAXIS2'))
  rsize = long64(fxpar(hdr, 'NAXIS1'))

  ;; will extract these for each
  names = ['TTYPE','TFORM', 'TSCAL', 'TZERO']
  nnames = n_elements(names)

;  Start by looking for the required TFORM keywords.    Then try to extract it 
;  along with names (TTYPE), scales (TSCAL), and offsets (TZERO)
   
 keyword = STRMID( hdr, 0, 8)

;
;  Find all instances of 'TFORM' followed by
;  a number.  Store the positions of the located keywords in mforms, and the
;  value of the number field in n_mforms
;

  mforms = WHERE(STRPOS(keyword,'TFORM') GE 0, n_mforms)
  if n_mforms GT nfld then begin
        message,/CON, $
        'WARNING - More columns found in binary table than specified in TFIELDS'
        n_mforms = nfld
        mforms = mforms[0:nfld-1]
  endif


  IF ( n_mforms GT 0 ) THEN BEGIN
      numst= STRMID(hdr[mforms], 5 ,3)      
 
      igood = WHERE(VALID_NUM(numst,/INTEGER), n_mforms)
      IF n_mforms GT 0 THEN BEGIN
          mforms = mforms[igood]
          number = fix( numst[igood])
          numst = numst[igood]
      ENDIF

  ENDIF ELSE RETURN              ;No fields in binary table

  ;; The others
  fnames = strarr(n_mforms)
  fforms = strarr(n_mforms) 
  scales = dblarr(n_mforms)
  offsets = dblarr(n_mforms)

  ;;comments = strarr(n_mnames)

  fnames_names  = 'TTYPE'+numst
  scales_names  = 'TSCAL'+numst
  offsets_names = 'TZERO'+numst
  number = number -1    ;Make zero-based


  match, keyword, fnames_names, mkey_names, mnames, count = N_mnames

  match, keyword, scales_names, mkey_scales, mscales, count = N_mscales

  match, keyword, offsets_names, mkey_offsets, moffsets,count = N_moffsets

  FOR in=0L, nnames-1 DO BEGIN 

      CASE names[in] OF
          'TTYPE': BEGIN 
              tmatches = mnames 
              matches = mkey_names
              nmatches = n_mnames
              result = fnames
          END 
          'TFORM': BEGIN 
              tmatches = lindgen(n_mforms)
              matches = mforms
              nmatches = n_mforms
              result = fforms
          END 
          'TSCAL': BEGIN 
              tmatches = mscales
              matches = mkey_scales
              nmatches = n_mscales
              result = scales
          END 
          'TZERO': BEGIN 
              tmatches = moffsets
              matches = mkey_offsets
              nmatches = n_moffsets
              result = offsets
          END 
          ELSE: message,'What?'
      ENDCASE 

      ;;help,matches,nmatches

;
;  Extract the parameter field from the specified header lines.  If one of the
;  special cases, then done.
;
      IF nmatches GT 0 THEN BEGIN 
          
          ;; "matches" is a subscript for hdr and keyword.
          ;; get just the matches in line
          
          line = hdr[matches]
          svalue = STRTRIM( STRMID(line,9,71),2)
          
          FOR i = 0, nmatches-1 DO BEGIN
              IF ( STRMID(svalue[i],0,1) EQ "'" ) THEN BEGIN

                  ;; Its a string
                  test = STRMID( svalue[i],1,STRLEN( svalue[i] )-1)
                  next_char = 0
                  off = 0
                  value = ''
;
;  Find the next apostrophe.
;
NEXT_APOST:
                  endap = STRPOS(test, "'", next_char)
                  IF endap LT 0 THEN MESSAGE,         $
                    'WARNING: Value of '+nam+' invalid in '+ " (no trailing ')", /info
                  value = value + STRMID( test, next_char, endap-next_char )
;
;  Test to see if the next character is also an apostrophe.  If so, then the
;  string isn't completed yet.  Apostrophes in the text string are signalled as
;  two apostrophes in a row.
;
                  IF STRMID( test, endap+1, 1) EQ "'" THEN BEGIN    
                      value = value + "'"
                      next_char = endap+2      
                      GOTO, NEXT_APOST
                  ENDIF

                 
;
;  If not a string, then separate the parameter field from the comment field.
;
              ENDIF ELSE BEGIN
                  ;; not a string
                  test = svalue[I]
                  slash = STRPOS(test, "/")
                  IF slash GT 0 THEN  test = STRMID(test, 0, slash)
                 
;
;  Find the first word in TEST.  Is it a logical value ('T' or 'F')?
;
                  test2 = test
                  value = GETTOK(test2,' ')
                  test2 = STRTRIM(test2,2)
                  IF ( value EQ 'T' ) THEN BEGIN
                      value = 1
                  END ELSE IF ( value EQ 'F' ) THEN BEGIN
                      value = 0
                  END ELSE BEGIN
;
;  Test to see if a complex number.  It's a complex number if the value and the
;  next word, if any, both are valid numbers.
;
                      IF STRLEN(test2) EQ 0 THEN GOTO, NOT_COMPLEX
                      test2 = GETTOK(test2,' ')
                      IF VALID_NUM(value,val1) && VALID_NUM(value2,val2) $
                        THEN BEGIN
                          value = COMPLEX(val1,val2)
                          GOTO, GOT_VALUE
                      ENDIF
;
;  Not a complex number.  Decide if it is a floating point, double precision,
;  or integer number.  If an error occurs, then a string value is returned.
;  If the integer is not within the range of a valid long value, then it will 
;  be converted to a double.  
;
NOT_COMPLEX:
                      ON_IOERROR, GOT_VALUE
                      value = test
                      IF ~VALID_NUM(value) THEN GOTO, GOT_VALUE

                      IF (STRPOS(value,'.') GE 0) || (STRPOS(value,'E') $
                                                      GE 0) || (STRPOS(value,'D') GE 0) THEN BEGIN
                          IF ( STRPOS(value,'D') GT 0 ) || $
                            ( STRLEN(value) GE 8 ) THEN BEGIN
                              value = DOUBLE(value)
                          END ELSE value = FLOAT(value)
                      ENDIF ELSE BEGIN
                          lmax = long64(2)^31 - 1
                          lmin = -long64(2)^31
                          value = long64(value)
                          if (value GE lmin) && (value LE lmax) THEN $
                            value = LONG(value)
                      ENDELSE
                      
;
GOT_VALUE:
                      ON_IOERROR, NULL
                  ENDELSE
              ENDELSE           ; if string
;
;  Add to vector if required.
;
              
              result[tmatches[i]] = value
             
          ENDFOR

          CASE names[in] OF
              'TTYPE': fnames[number] = strtrim(result, 2)
              'TFORM': fforms[number] = strtrim(result, 2)
              'TSCAL': scales[number] = result
              'TZERO': offsets[number] = result
              ELSE: message,'What?'
          ENDCASE 

;
;  Error point for keyword not found.
;
      ENDIF
;



  ENDFOR 
END
  

; Get a tag name give the column name and index
function  mrd_dofn, name, index, use_colnum, alias=alias
compile_opt idl2, hidden
    ; Check if the user has specified an alias.

    name = N_elements(name) EQ 0 ? 'C' + strtrim(index,2) : strtrim(name) 
    if keyword_set(alias) then begin
	sz = size(alias)
	
	if (sz[0] eq 1 || sz[0] eq 2) && (sz[1] eq 2) && (sz[sz[0]+1] eq 7) $
	   then begin
	    w = where( name eq alias[1,*], Nw)
	    if Nw GT 0 then name = alias[0,w[0]];
	endif
    endif
    ; Convert the string name to a valid variable name.  If name 
    ; is not defined generate the string Cnn when nn is the index 
    ; number.

    table = 0
     if ~use_colnum && (N_elements(name) GT 0)  then begin 
        if size(name,/type) eq 7 then begin 
            str = name[0] 
        endif else str = 'C'+strtrim(index,2) 
     endif else str = 'C'+strtrim(index,2) 
  
    return, IDL_VALIDNAME(str,/CONVERT_ALL) 
 
end 

;***************************************************************



; Parse the TFORM keyword and return the type and dimension of the 
; data. 
pro mrd_doff, form, dim, type 
compile_opt idl2, hidden 
    ; Find the first non-numeric character. 
 
    len = strlen(form) 
 
    if len le 0 then return 
 
    i = stregex( form, '[^0-9]')       ;Position of first non-numeric character
 
    if i lt 0 then return              ;Any non-numeric character found?
 
    if i gt 0 then begin 
        dim = long(strmid(form, 0, i)) 
        if dim EQ 0l then dim = -1l
    endif else dim = 0
 
    type = strmid(form, i, 1) 
end 



;*********************************************************************

;  Check that this name is unique with regard to other column names.

function mrd_chkfn, name, namelist, index 
 compile_opt idl2, hidden
    ; 
    ; 

    maxlen = 127

    if strlen(name) gt maxlen then name = strmid(name, 0, maxlen) 
    if ~array_equal(namelist eq name,0b ) then begin
    
        ; We have found a name conflict. 
        ; 
            name = 'gen$name_'+strcompress(string(index+1),/remove_all) 
    endif 

    return, name 
 
end

; Find the appropriate offset for a given unsigned type.
; The type may be given as the bitpix value or the IDL
; variable type.

function mrd_unsigned_offset, type
compile_opt idl2, hidden
    	    
    if (type eq 12) || (type eq 16) then begin
	return, uint(32768)
    endif else if (type eq 13) || (type eq 32) then begin
	return, ulong('2147483648')
    endif else if (type eq 15) || (type eq 64) then begin
	return, ulong64('9223372036854775808');
    endif
    return, 0
end



; Can we treat this data as unsigned?

function mrd_chkunsigned, bitpix, scale, zero, unsigned=unsigned
compile_opt idl2, hidden
    if ~keyword_set(unsigned) then return, 0
    
    ; This is correct but we should note that
    ; FXPAR returns a double rather than a long.
    ; Since the offset is a power of two
    ; it is an integer that is exactly representable
    ; as a double.  However, if a user were to use
    ; 64 bit integers and an offset close to but not
    ; equal to 2^63, we would erroneously assume that
    ; the dataset was unsigned...

    if scale eq 1 then begin
	if (bitpix eq 16 && zero eq 32768L) ||                   $
	   (bitpix eq 32 && zero eq 2147483648UL) ||        $
	   (bitpix eq 64 && zero eq 9223372036854775808ULL) then return,1
    endif
    
    return, 0
end

; Is this one of the IDL unsigned types?
function mrd_unsignedtype, data
 compile_opt idl2, hidden      
 type = size(data,/type) 
 
    if (type eq 12) || (type eq 13) || (type eq 15) then return, type $
                                                    else return, 0
    
end
    
; Return the currrent version string for MRDFITS
function mrd_version
compile_opt idl2, hidden
    return, '2.20 '
end
;=====================================================================
; END OF GENERAL UTILITY FUNCTIONS ===================================
;=====================================================================


; Parse the TFORM keyword and return the type and dimension of the
; data.
pro mrd_atype, form, type, slen
compile_opt idl2, hidden

    ; Find the first non-numeric character.


    ; Get rid of blanks.
    form = strcompress(form,/remove_all)
    len = strlen(form)
    if len le 0 then return

    type = strmid(form, 0,1)
    length = strmid(form,1,len-1)
    ;
    ; Ignore the number of decimal places.  We assume that there
    ; is a decimal point.
    ;
    p = strpos(length, '.')
    if p gt 0 then length = strmid(length,0,p)

   if strlen(length) gt 0 then slen = fix(length) else slen = 1
   if (type EQ 'F') || (type EQ 'E') then $     ;Updated April 2007
        if (slen GE 8) then type = 'D'

end


; Read in the table information.
pro mrd_read_ascii, unit, range, nbytes, nrows, nfld, typarr, posarr,   $
     lenarr, nullarr, table, old_struct=old_struct, rows=rows
compile_opt idl2, hidden
    ;
    ; Unit          Unit to read data from.
    ; Range         Range of  to be read
    ; Nbytes        Number of bytes per row.
    ; Nrows         Number of rows.
    ; Nfld          Number of fields in structure.
    ; Typarr        Array indicating type of variable.
    ; Posarr        Starting position of fields (first char at 0)
    ; Lenarr        Length of fields
    ; Nullarr       Array of null values
    ; Table         Table to read information into.
    ; Old_struct    Should recursive structure format be used?

    bigstr = bytarr(nbytes, range[1]-range[0]+1)

    if range[0] gt 0 then mrd_skip, unit, nbytes*range[0]
    readu,unit, bigstr
    if N_elements(rows) GT 0 then bigstr = bigstr[*,rows-range[0]] 

    ; Skip to the end of the data area.

    nSkipRow = nrows - range[1] - 1
    nskipB = 2880 - (nbytes*nrows) mod 2880
    if nskipB eq 2880 then nskipB = 0

    mrd_skip, unit, nskipRow*nbytes+nskipB

    s1 = posarr-1
    s2 = s1 + lenarr - 1
    for i=0, nfld-1 do begin
        flds = strtrim(bigstr[s1[i]:s2[i],* ]) 
       if nullarr[i] ne '' then begin
	    
		curr_col = table.(i)
           w = where(flds NE strtrim(nullarr[i]), Ngood)
	 
            if Ngood GT 0 then begin
                if N_elements(w) EQ 1 then w = w[0]
                if typarr[i] eq 'I' then begin
                    curr_col[w] = long(flds[w])
                endif else if typarr[i] eq 'E' || typarr[i] eq 'F' then begin
                    curr_col[w] = float(flds[w])
                endif else if typarr[i] eq 'D' then begin
                    curr_col[w] = double(flds[w])
                endif else if typarr[i] eq 'A' then begin
                    curr_col[w] = flds[w]
                endif
            endif

                table.(i) = curr_col
                
        endif else begin
                
   

           if typarr[i] eq 'I' then begin
                    table.(i) =  long(flds)
            endif else if typarr[i] eq 'E' || typarr[i] eq 'F' then begin
                    table.(i) = float(flds)
            endif else if typarr[i] eq 'D' then begin
                    table.(i) = double(flds)
             endif else if typarr[i] eq 'A' then begin
                    table.(i) = flds
            endif
        endelse
    endfor

end


; Define a structure to hold a FITS ASCII table.
pro mrd_ascii, header, structyp, use_colnum,   $
    range, table, $
    nbytes, nrows, nfld, typarr, posarr, lenarr, nullarr, $
    fnames, fvalues, scales, offsets, scaling, status, rows = rows, $
    silent=silent, columns=columns, alias=alias, outalias=outalias
compile_opt idl2, hidden
    ;
    ; Header                FITS header for table.
    ; Structyp              IDL structure type to be used for
    ;                       structure.
    ; Use_colnum            Use column numbers not names.
    ; Range                 Range of rows of interest
    ; Table                 Structure to be defined.
    ; Nbytes                Bytes per row
    ; Nrows                 Number of rows in table
    ; Nfld                  Number of fields
    ; Typarr                Array of field types
    ; Posarr                Array of field offsets
    ; Lenarr                Array of field lengths
    ; Nullarr               Array of field null values
    ; Fname                 Column names
    ; Fvalues               Formats for columns
    ; Scales/offsets        Scaling factors for columns
    ; Scaling               Do we need to scale?
    ; Status                Return status.

    table = 0

    types  = ['I', 'E', 'F', 'D', 'A']
; Set default 'null' values   
    sclstr = ['-2147483647L', '!VALUES.f_nan', '!VALUES.f_nan', '!VALUES.d_nan', '...']
    status = 0

    if strmid(fxpar(header, 'XTENSION'),0,8) ne 'TABLE   ' then begin
        message, 'ERROR - Header is not from ASCII table.',/CON
        status = -1;
        return
    endif

    nfld = fxpar(header, 'TFIELDS')
    nrows = long64( fxpar(header, 'NAXIS2'))
    nbytes = long64( fxpar(header, 'NAXIS1'))
 
    if range[0] ge 0 then begin
        range[0] = range[0] < (nrows-1)
        range[1] = range[1] < (nrows-1)
    endif else begin
        range[0] = 0
        range[1] = nrows-1
    endelse

    if N_elements(rows) EQ 0 then nrows = range[1] - range[0] + 1 else begin
          bad = where(rows GT nrows, Nbad)
          if Nbad GT 0  then begin 
             message,/CON,'ERROR: Row numbers must be between 0 and ' + $
                      strtrim(nrows-1,2)
             status = -1
             return
          endif      
          nrows = N_elements(rows)
     endelse

    if nrows le 0 then begin
        if ~keyword_set(silent) then begin
            print,'MRDFITS: ASCII table.  ',strcompress(string(nfld)),  $
                  ' columns, no rows'
        endif
        return
    endif

    ;
    ;  Loop over the columns

    typarr  = strarr(nfld)
    lenarr  = intarr(nfld)
    posarr  = intarr(nfld)
    nullarr = strarr(nfld)
    fnames  = strarr(nfld)
    fvalues = strarr(nfld)
    scales  = dblarr(nfld)
    offsets = dblarr(nfld)
    tname  =  strarr(nfld)

    for i=0, nfld-1 do begin
        suffix = strcompress(string(i+1), /remove_all)
        fname = fxpar(header, 'TTYPE' + suffix, count=cnt)
	tname[i] = fname
	if cnt eq 0 then xx = temporary(fname)
        fform = fxpar(header, 'TFORM' + suffix)
        fpos = fxpar(header, 'TBCOL' + suffix)
        fnull = fxpar(header, 'TNULL' + suffix, count=cnt)
	if cnt eq 0 then fnull = ''
        scales[i] = fxpar(header, 'TSCAL' + suffix)
        if scales[i] eq 0.0d0 then scales[i] = 1.0d0
        offsets[i] = fxpar(header, 'TZERO'+suffix)
        
        fname = strupcase( mrd_dofn(fname,i+1, use_colnum, alias=alias))

        if i GT 0 then fname = mrd_chkfn(fname, fnames, i) ;Check for duplicates
	fnames[i] = fname
        
        mrd_atype, fform, ftype, flen
        typarr[i] = ftype
        lenarr[i] = flen
        posarr[i] = fpos
        nullarr[i] = fnull
        
 
       j = where(types EQ ftype, Nj) 
       if Nj EQ 0 then begin 
                message, 'Invalid format code:'+ ftype + ' for column ' + $
		    strtrim(i+1,2),/CON
                status = -1
                return
       endif	       
       fvalues[i] = ftype NE 'A' ? sclstr[j] : $
	                  'string(replicate(32b,'+strtrim(flen,2)+'))'
                                               
         
    endfor
    
    if scaling then $
        scaling = ~array_equal(scales,1.0d0) || ~array_equal(offsets,0.0)
   
    if ~scaling && ~keyword_set(columns) then begin
        table = mrd_struct(fnames, fvalues, nrows, structyp=structyp, $
           silent=silent)
    endif else begin
        table = mrd_struct(fnames, fvalues, nrows, silent=silent)
    endelse

    if ~keyword_set(silent) then begin
        print,'MRDFITS: ASCII table.  ',strcompress(string(nfld)),  $
         ' columns by ',strcompress(string(nrows)), ' rows.'
    endif
    
    outalias = transpose([ [tag_names(table)],[tname] ] )
    status = 0
    return

end


; Eliminate columns from the table that do not match the
; user specification.
pro  mrd_columns, table, columns, fnames, fvalues, $
    vcls, vtpes, scales,  offsets, scaling,        $
    structyp=structyp, silent=silent
compile_opt idl2, hidden



    type = size(columns,/type)
    nele = N_elements(columns)
    if type eq 8 || type eq 6 || type eq 0 then return  ; Can't use structs
                                                    ; or complex.

    if type eq 4 || type eq 5 then tcols = fix(columns)
    if type eq 1 || type eq 2 || type eq 3 then tcols = columns

    ; Convert strings to uppercase and compare with column names.

    if type eq 7 then begin
       match, strupcase(columns), strupcase(fnames), tmp, tcols,count=nmatch 
       if Nmatch GT 0 then begin 
              s = sort(tmp)             ;Sort order of supplied column name
              tcols = tcols[s] + 1
       endif     
     endif

    ; Subtract one from column indices and check that all indices >= 0.
    if n_elements(tcols) gt 0 then begin
        tcols = tcols-1
        w = where(tcols ge 0, Nw)
        if Nw EQ 0 then dummy = temporary(tcols)
    endif

    if n_elements(tcols) le 0 then begin
        print, 'MRDFITS:  No columns match'
        
        ; Undefine variables.  First ensure they are defined, then
        ; use temporary() to undefine them.
        table = 0
        fnames = 0
        fvalues = 0
        vcls = 0
        vtpes = 0
        scales = 0
        offsets = 0
        dummy = temporary(fnames)
        dummy = temporary(fvalues)
        dummy = temporary(vcls)
        dummy = temporary(vtpes)
        dummy = temporary(scales)
        dummy = temporary(offsets)
        scaling = 0
        
    endif else begin

        ; Replace arrays with only desired columns.
        
        fnames = fnames[tcols]
        fvalues = fvalues[tcols]
        
        ; Check if there are still variable length columns.
        if n_elements(vcls) gt 0 then begin
            vcls = vcls[tcols]
            vtpes = vtpes[tcols]
            w = where(vcls eq 1, Nw)
            if Nw EQ 0 then begin
                dummy = temporary(vcls)
                dummy = temporary(vtpes)
            endif
        endif
        
        ; Check if there are still columns that need scaling.
        if n_elements(scales) gt 0 then begin
            scales = scales[tcols]
            offsets = offsets[tcols]
	    scaling = ~array_equal(scales,1.d0) || ~array_equal(offsets,0.0)
         endif
        

        ndim = n_elements(table)
        
        if scaling || n_elements(vcls) gt 0 then begin
            tabx = mrd_struct(fnames, fvalues, ndim, silent=silent )
        endif else begin
            tabx = mrd_struct(fnames, fvalues, ndim, structyp=structyp, silent=silent )
        endelse
        
        for i=0, n_elements(tcols)-1 do $
                tabx.(i) = table.(tcols[i]);
 
        table = temporary(tabx)
    endelse
    
end


; Read in the image information. 
pro mrd_read_image, unit, range, maxd, rsize, table, rows = rows,status=status, $
     unixpipe = unixpipe
 compile_opt idl2, hidden
    ; 
    ; Unit          Unit to read data from. 
    ; Table         Table/array to read information into. 
    ; 

    error=0
    catch,error
    if error ne 0 then begin
     catch,/cancel
     status=-2
     return
    endif

    ; If necessary skip to beginning of desired data. 

    if range[0] gt 0 then mrd_skip, unit, range[0]*rsize

    status=-2
    if rsize eq 0 then return 

    on_ioerror,done
    readu, unit, table

    if N_elements(rows) GT 0 then begin
    row1 = rows- range[0]
    case size(table,/n_dimen) of 
    1: table = table[row1]
    2: table = table[*,row1]
    3: table = table[*,*,row1]
    4: table = table[*,*,*,row1]
    5: table = table[*,*,*,*,row1]
    6: table = table[*,*,*,*,*,row1]
    7: table = table[*,*,*,*,*,*,row1]
    8: table = table[*,*,*,*,*,*,*,row1]
    else: begin 
          print,'MRDFITS: Subscripted image must be between 1 and 8 dimensions'
          status = -1
          return
          end
    endcase
    endif

    ; Skip to the end of the data

    skipB = 2880 - (maxd*rsize) mod 2880
    if skipB eq 2880 then skipB = 0

    if range[1] lt maxd-1 then $
        skipB = skipB + (maxd-range[1]-1)*rsize
 
    mrd_skip, unit, skipB
    if unixpipe then swap_endian_inplace, table,/swap_if_little

    ; Fix offset for unsigned data
    type = mrd_unsignedtype(table)
    if type gt 0 then $
	table = table - mrd_unsigned_offset(type)
    
    status=0
    done:

;-- probably an EOF 

    if status ne 0 then begin 
          message,!ERROR_STATE.MSG,/CON
         free_lun,unit
    endif

    return
end 

; Truncate superfluous axes.

pro mrd_axes_trunc,naxis, dims, silent
compile_opt idl2, hidden
    mysilent = silent
    for i=naxis-1,1,-1 do begin 

        if dims[i] eq 1 then begin
            if ~mysilent then begin
                print, 'MRDFITS: Truncating unused dimensions'
                mysilent = 1
            endif
            dims = dims[0:i-1] 
            naxis = naxis - 1 
        
        endif else return
     
    endfor 
 
    return
end

; Define structure/array to hold a FITS image. 
pro mrd_image, header, range, maxd, rsize, table, scales, offsets, scaling, $
  status, silent=silent, unsigned=unsigned, rows = rows
 compile_opt idl2, hidden
    ; 
    ; Header                FITS header for table. 
    ; Range                 Range of data to be retrieved. 
    ; Rsize                 Size of a row or group. 
    ; Table                 Structure to be defined. 
    ; Status                Return status
    ; Silent=silent         Suppress info messages?
 
    table = 0

    ; type    0         1           2         3         4         5  6  7  8  9 10 11        12         13          14          15
    lens =  [ 0,        1,          2,        4,        4,        8, 0, 0, 0, 0, 0, 0,        2,         4,          8,          8] 
    typstrs=['',   'Byte',    'Int*2',  'Int*4', 'Real*4', 'Real*8','','','','','','', 'UInt*2',  'Uint*4',    'Int*8',    'Uint*8']
    typarr= ['', 'bytarr',   'intarr', 'lonarr', 'fltarr', 'dblarr','','','','','','','uintarr', 'ulonarr', 'lon64arr', 'ulon64arr'] 
 
    status = 0 
 
 
    naxis = fxpar(header, 'NAXIS') 
    bitpix= fxpar(header, 'BITPIX') 
    if naxis gt 0 then begin 
          dims = long64(fxpar(header, 'NAXIS*', Count = N_axis)) 
          if N_axis GT naxis then begin
; Check if extra NAXISn keywords are present (though this is not legal FITS)
                   nextra = N_axis - naxis
                   dim_extra = dims[naxis:N_axis-1]
                   if total(dim_extra) EQ nextra then $
                        dims = dims[0:naxis-1] else $
                   message,'ERROR - NAXIS = ' + strtrim(naxis,2) +  $
                          ' but NAXIS' + strtrim(N_axis,2) + ' keyword present'
          endif
   endif else dims = 0
    
    gcount = fxpar(header, 'GCOUNT') 
    pcount = fxpar(header, 'PCOUNT')
    isgroup = fxpar(header, 'GROUPS')
    gcount = long(gcount)

    xscale = fxpar(header, 'BSCALE', count=cnt)
    if cnt eq 0 then xscale = 1      ;Corrected 06/29/06
    
    xunsigned = mrd_chkunsigned(bitpix,  xscale, $
				fxpar(header, 'BZERO'), unsigned=unsigned)
    ; Note that type is one less than the type signifier returned in the size call.
    type = -1
    
    if ~xunsigned then begin 
 
        if bitpix eq 8        then type = 1     $ 
        else if bitpix eq  16 then type = 2     $ 
        else if bitpix eq  32 then type = 3     $ 
        else if bitpix eq -32 then type = 4     $ 
        else if bitpix eq -64 then type = 5     $
        else if bitpix eq  64 then type = 14

    endif else begin

	if bitpix eq 16       then type = 12     $
	else if bitpix eq  32 then type = 13     $
	else if bitpix eq  64 then type = 15
	
    endelse

    if type eq -1 then begin
	print,'MRDFITS: Error: Invalid BITPIX: '+strtrim(bitpix)
	table = 0
	return
    endif

    ; Note that for random groups data we must ignore the first NAXISn keyword. 
    if isgroup GT 0  then begin 


        range[0] = range[0] > 0
        if (range[1] eq -1) then begin
            range[1] = gcount-1
        endif else begin
            range[1] = range[1] < gcount - 1
        endelse
	
	maxd = gcount
        
        if (n_elements(dims) gt 1) then begin
            dims = dims[1:*]
            naxis = naxis-1
        endif else begin
            print, 'MRDFITS: Warning: No data specified for group data.'
            dims = [0]
            naxis = 0
        endelse
        
        ; The last entry is the scaling for the sample data.
        
        if (pcount gt 0) then begin
            scales  = dblarr(pcount+1)
            offsets = dblarr(pcount+1)
        endif
        
        values = strarr(2)
        
        
        mrd_axes_trunc, naxis, dims, keyword_set(silent)
        
        values[0] = typarr[type] + "("+string(pcount)+")" 
        rsize = dims[0] 
        sarr = "(" + strcompress(string(dims[0]), /remo )
         
        for i=1, naxis-1 do begin
	    
            sarr = sarr + "," + strcompress(string(dims[i]),/remo)
            rsize = rsize*dims[i]
	    
        endfor 
         
        sarr = sarr + ")"

        if ~keyword_set(silent) then print,'MRDFITS--Image with groups:', $
          ' Ngroup=',strcompress(string(gcount)),' Npar=',                   $
          strcompress(string(pcount),/remo), ' Group=', sarr, '  Type=',typstrs[type]

        sarr = typarr[type] + sarr
        values[1] = sarr 
        rsize = (rsize + pcount)*lens[type] 
         
        table = mrd_struct(['params','array'], values, range[1]-range[0]+1, $
                           silent=silent)

	if xunsigned then begin
	    fxaddpar,header, 'BZERO', 0, 'Reset by MRDFITS v'+mrd_version()
	endif
           

        for i=0, pcount-1 do begin
	    
            istr = strcompress(string(i+1),/remo)
	    
            scales[i] = fxpar(header, 'PSCAL'+istr)
            if scales[i] eq 0.0d0 then scales[i] =1.0d0
	    
            offsets[i] = fxpar(header, 'PZERO'+istr)
	    
            scales[pcount] = fxpar(header, 'BSCALE')
            if scales[pcount] eq 0.0d0 then scales[pcount] = 1.0d0
            offsets[pcount] = fxpar(header, 'BZERO')
	    
        endfor
  
     if scaling then $
        scaling = ~array_equal(scales,1.0d0) || ~array_equal(offsets,0.0)
         
    endif else begin 
 
        if naxis eq 0 then begin
	
            rsize = 0 
            table = 0
            if ~keyword_set(silent) then $
                print, 'MRDFITS: Null image, NAXIS=0'
            return
	    
        endif 
         
        if gcount gt 1 then begin 
            dims = [dims, gcount] 
            naxis = naxis + 1 
        endif 
         
        mrd_axes_trunc, naxis, dims, keyword_set(silent)

                
        maxd = dims[naxis-1] 
         
        if range[0] ne -1 then begin 
            range[0] = range[0]<(maxd-1) 
            range[1] = range[1]<(maxd-1) 
        endif else begin 
            range[0] = 0 
            range[1] = maxd - 1 
        endelse 

        Nlast = dims[naxis-1]   
        dims[naxis-1] = range[1]-range[0]+1
        pdims = dims
        if N_elements(rows) GT 0 then begin
             if max(rows) GE Nlast then begin 
               print, 'MRDFITS: Row numbers must be between 0 and ' + $
                      strtrim(Nlast-1,2)
               status = -1 & rsize = 0
               return
             endif
             pdims[naxis-1] = N_elements(rows)
        endif 
 
        if ~keyword_set(silent) then begin
            str = '('
            for i=0, naxis-1 do begin
                if i ne 0 then str = str + ','
                str = str + strcompress(string(pdims[i]),/remo)
            endfor
            str = str+')'
            print, 'MRDFITS: Image array ',str, '  Type=', typstrs[type]
        endif
         
        rsize = 1
	
        if naxis gt 1 then for i=0, naxis - 2 do rsize=rsize*dims[i] 
        rsize = rsize*lens[type] 
        sz = lonarr(naxis+3) 
        sz[0] = naxis 
        sz[1:naxis] = dims 

	nele = product(dims,/integer)
         
        sz[naxis+1] = type   
        sz[naxis+2] = nele 
  
        table = nele GT 0 ? make_array(size=sz) : 0
	
        scales = dblarr(1)
        offsets = dblarr(1)

	if xunsigned then begin
	    fxaddpar,header, 'BZERO', 0, 'Updated by MRDFITS v'+mrd_version()
	endif
	
        scales[0] = fxpar(header, 'BSCALE')
        offsets[0] = fxpar(header, 'BZERO')
	
        if scales[0] eq 0.0d0 then scales[0] = 1.0d0
        if scaling && (scales[0] eq 1.0d0) && (offsets[0] eq 0.0d0) then  $
	          scaling = 0
    endelse 
         
    status = 0 
    return 
 
end

; Scale an array of pointers
pro mrd_ptrscale, array, scale, offset
compile_opt idl2, hidden
    for i=0, n_elements(array)-1 do begin
        if ptr_valid(array[i]) then begin
	    array[i] = ptr_new(*array[i] * scale + offset)
	endif
    endfor
end

; Scale a FITS array or table.
pro mrd_string, table, header, typarr, $
               fnames, fvalues, nrec, structyp=structyp, silent=silent
compile_opt idl2, hidden
    ;
    ; Type:         FITS file type, 0=image/primary array
    ;                               1=ASCII table
    ;                               2=Binary table
    ;
    ; scales:       An array of scaling info
    ; offsets:      An array of offset information
    ; table:        The FITS data.
    ; header:       The FITS header.
    ; dscale:       Should data be scaled to R*8?
    ; fnames:       Names of table columns.
    ; fvalues:      Values of table columns.
    ; nrec:         Number of records used.
    ; structyp:     Structure name.
 
    w = where( typarr EQ 'A', Nw, $
                complement=ww, Ncomplement = Nww)
		
    if Nw EQ 0 then return    ;No tags require string conversion? 

; First do ASCII and Binary tables.    We need to create a new structure 
; because scaling will change the tag data types.

          sclr = "' '"
          vc = 'strarr'
                
           for i=0, Nw-1 do begin
                col = w[i]
                sz = size(table[0].(col),/str)

		; Handle pointer columns
		if sz.type eq 10 then begin
		    fvalues[col] = 'ptr_new()'

		; Scalar columns
		endif else if sz.N_dimensions eq 0 then begin
                    fvalues[col] = sclr

		; Vectors
                endif else begin
		    dim = sz.dimensions[0:sz.N_dimensions-1]
                    fvalues[col] = vc + $
		      '(' + strjoin(strtrim(dim,2),',') + ')'
		    
                endelse
            endfor
        tabx = mrd_struct(fnames, fvalues, nrec, structyp=structyp, silent=silent )

; First copy the unscaled columns indexed by ww.     This is actually more 
; efficient than using STRUCT_ASSIGN since the tag names are all identical,
; so STRUCT_ASSIGN would copy everything (scaled and unscaled).
 	    
       for i=0, Nww - 1 do tabx.(ww[i]) = table.(ww[i])
       
; Now copy the string items indexed by w after converting the byte array        
       
        for i=0, Nw - 1 do begin	    
 		
		str = size(tabx.(w[i]),/str)
		dim = [1,str.dimensions[0:str.N_dimensions-1]]
                if str.n_dimensions GT 1 then $
                tabx.(w[i]) = string(reform(table.(w[i]),dim)) else $
		tabx.(w[i]) = string(table.(w[i]))
			    
        endfor

        table = temporary(tabx)   ;Remove original structure from memory
  
end


; Scale a FITS array or table.
pro mrd_scale, type, scales, offsets, table, header,  $
               fnames, fvalues, nrec, dscale = dscale, structyp=structyp, silent=silent
compile_opt idl2, hidden
    ;
    ; Type:         FITS file type, 0=image/primary array
    ;                               1=ASCII table
    ;                               2=Binary table
    ;
    ; scales:       An array of scaling info
    ; offsets:      An array of offset information
    ; table:        The FITS data.
    ; header:       The FITS header.
    ; dscale:       Should data be scaled to R*8?
    ; fnames:       Names of table columns.
    ; fvalues:      Values of table columns.
    ; nrec:         Number of records used.
    ; structyp:     Structure name.
 
    w = where( (scales ne 1.d0  || offsets ne 0.d0), Nw, $
                complement=ww, Ncomplement = Nww)
		
    if Nw EQ 0 then return    ;No tags require scaling? 

; First do ASCII and Binary tables.    We need to create a new structure 
; because scaling will change the tag data types.

    if type ne 0 then begin
        
        if type eq 1 then begin
	    fvalues[w] = keyword_set(dscale) ? '0.0d0' : '0.0 
        endif else if type eq 2 then begin

            if keyword_set(dscale) then begin
                sclr = '0.d0'
                vc = 'dblarr'
            endif else begin
                sclr = '0.0'
                vc = 'fltarr'
            endelse
                
           for i=0, Nw-1 do begin
                col = w[i]
                sz = size(table[0].(col),/str)

		; Handle pointer columns
		if sz.type eq 10 then begin
		    fvalues[col] = 'ptr_new()'

		; Scalar columns
		endif else if sz.N_dimensions eq 0 then begin
                    fvalues[col] = sclr

		; Vectors
                endif else begin
		    dim = sz.dimensions[0:sz.N_dimensions-1]
                    fvalues[col] = vc + $
		      '(' + strjoin(strtrim(dim,2),',') + ')'
		    
                endelse
            endfor
        endif

        tabx = mrd_struct(fnames, fvalues, nrec, structyp=structyp, silent=silent )

; First copy the unscaled columns indexed by ww.     This is actually more 
; efficient than using STRUCT_ASSIGN since the tag names are all identical,
; so STRUCT_ASSIGN would copy everything (scaled and unscaled).
 	    
       for i=0, Nww - 1 do tabx.(ww[i]) = table.(ww[i])
       
; Now copy the scaled items indexed by w after applying the scaling.        
       
        for i=0, Nw - 1 do begin	    
 		
		dtype = size(tabx.(w[i]),/type)
		if dtype eq 10 then $
		    mrd_ptrscale, table.(w[i]), scales[w[i]], offsets[w[i]]
		
                tabx.(w[i]) = table.(w[i])*scales[w[i]] + offsets[w[i]]
		
            istr = strtrim(w[i]+1,2)
            fxaddpar, header, 'TSCAL'+istr, 1.0, ' Set by MRD_SCALE'
            fxaddpar, header, 'TZERO'+istr, 0.0, ' Set by MRD_SCALE'
	    
        endfor

        table = temporary(tabx)   ;Remove original structure from memory
    endif else begin
    ; Now process images and random groups.

        sz = size(table[0])
        if sz[sz[0]+1] ne 8 then begin
            ; Not a structure so we just have an array of data.
            if keyword_set(dscale) then begin
                table = temporary(table)*scales[0]+offsets[0]
            endif else begin
                table = temporary(table)*float(scales[0]) + float(offsets[0])
            endelse
            fxaddpar, header, 'BSCALE', 1.0, 'Set by MRD_SCALE'
            fxaddpar, header, 'BZERO', 0.0, 'Set by MRD_SCALE'

        endif else begin
            ; Random groups.  Get the number of parameters by looking
            ; at the first element in the table.
            nparam = n_elements(table[0].(0))
            if keyword_set(dscale) then typ = 'dbl' else typ='flt'
            s1 = typ+'arr('+string(nparam)+')'
            ngr = n_elements(table)
            sz = size(table[0].(1))
            if sz[0] eq 0 then dims = [1] else dims=sz[1:sz[0]]
            s2 = typ + 'arr('
            for i=0, n_elements(dims)-1 do begin 
                if i ne 0 then s2 = s2+ ','
                s2 = s2+string(dims[i])
            endfor
            s2 = s2+')'
            tabx = mrd_struct(['params', 'array'],[s1,s2],ngr, silent=silent)

            for i=0, nparam-1 do begin
                istr = strcompress(string(i+1),/remo)
                fxaddpar, header, 'PSCAL'+istr, 1.0, 'Added by MRD_SCALE'
                fxaddpar, header, 'PZERO'+istr, 0.0, 'Added by MRD_SCALE'
                tabx.(0)[i] = table.(0)[i]*scales[i]+offsets[i]
            endfor
	    
            tabx.(1) = table.(1)*scales[nparam] + offsets[nparam]
            fxaddpar, header, 'BSCALE', 1.0, 'Added by MRD_SCALE'
            fxaddpar, header, 'BZERO', 0.0, 'Added by MRD_SCALE'
            table = temporary(tabx)
        endelse
    endelse

end

; Read a variable length column into a pointer array.
pro mrd_varcolumn, vtype, array, heap, off, siz
compile_opt idl2, hidden

    ; Guaranteed to have at least one non-zero length column
    w   = where(siz gt 0)
    nw  = n_elements(w)
    
    if vtype eq 'X' then siz = 1 + (siz-1)/8
    
    siz = siz[w]
    off = off[w]

    unsigned = 0
    if vtype eq '1' then begin
	unsigned = 12
    endif else if vtype eq '2' then begin
	unsigned = 13
    endif else if vtype eq '3' then begin
	unsigned = 15;
    endif
    unsigned = mrd_unsigned_offset(unsigned)
    

    for j=0, nw-1 do begin

        case vtype of

            'L': array[w[j]] = ptr_new(  byte(heap,off[j],siz[j]) )
            'X': array[w[j]] = ptr_new(  byte(heap,off[j],siz[j]) )
            'B': array[w[j]] = ptr_new(  byte(heap,off[j],siz[j]) )
	
            'I': array[w[j]] = ptr_new(  fix(heap, off[j], siz[j]) )
            'J': array[w[j]] = ptr_new(  long(heap, off[j], siz[j]) )
            'K': array[w[j]] = ptr_new(  long64(heap, off[j], siz[j]) )
			  
            'E': array[w[j]] = ptr_new(  float(heap, off[j], siz[j]) )
            'D': array[w[j]] = ptr_new(  double(heap, off[j], siz[j]) )
			  
            'C': array[w[j]] = ptr_new(  complex(heap, off[j], siz[j]) )
            'M': array[w[j]] = ptr_new(  dcomplex(heap, off[j], siz[j]) )
			   
            '1': array[w[j]] = ptr_new(  uint(heap, off[j], siz[j]) )
            '2': array[w[j]] = ptr_new(  ulong(heap, off[j], siz[j]) )
            '3': array[w[j]] = ptr_new(  ulong64(heap, off[j], siz[j]) )
      
        endcase

	; Fix endianness.
        if (vtype ne 'B') && (vtype ne 'X') && (vtype ne 'L') then begin
	    swap_endian_inplace, *array[w[j]],/swap_if_little
        endif

	; Scale unsigneds.
	if unsigned gt 0 then *array[w[j]] = *array[w[j]] - unsigned
	
    endfor
end

; Read a variable length column into a fixed length array.
pro mrd_fixcolumn, vtype, array, heap, off, siz
compile_opt idl2, hidden

    w   = where(siz gt 0, nw)
    if nw EQ 0 then return
    
    if vtype eq 'X' then siz = 1 + (siz-1)/8
    
    siz = siz[w]
    off = off[w]

    for j=0, nw-1 do begin
        case vtype of
            'L': array[0:siz[j]-1,w[j]] = byte(heap,off[j],siz[j])  
            'X': array[0:siz[j]-1,w[j]] = byte(heap,off[j],siz[j])
            'B': array[0:siz[j]-1,w[j]] = byte(heap,off[j],siz[j]) 
	
            'I': array[0:siz[j]-1,w[j]] = fix(heap, off[j], siz[j]) 
            'J': array[0:siz[j]-1,w[j]] = long(heap, off[j], siz[j]) 
            'K': array[0:siz[j]-1,w[j]] = long64(heap, off[j], siz[j]) 
			  
            'E': begin                  ;Delay conversion until after byteswapping to avoid possible math overflow   Feb 2005
	         temp = heap[off[j]: off[j] + 4*siz[j]-1 ]
		 byteorder, temp, /LSWAP, /SWAP_IF_LITTLE	 
	         array[0:siz[j]-1,w[j]] = float(temp,0,siz[j]) 
                 end			  
           'D': begin 
	         temp = heap[off[j]: off[j] + 8*siz[j]-1 ]
		 byteorder, temp, /L64SWAP, /SWAP_IF_LITTLE		 
	         array[0:siz[j]-1,w[j]] = double(temp,0,siz[j]) 
                 end			  
            'C': array[0:siz[j]-1,w[j]] = complex(heap, off[j], siz[j]) 
            'M': array[0:siz[j]-1,w[j]] = dcomplex(heap, off[j], siz[j]) 
			   
            'A': array[w[j]] = string(byte(heap,off[j],siz[j])) 

            '1': array[0:siz[j]-1,w[j]] = uint(heap, off[j], siz[j]) 
            '2': array[0:siz[j]-1,w[j]] = ulong(heap, off[j], siz[j])
            '3': array[0:siz[j]-1,w[j]] = ulong64(heap, off[j], siz[j])
      
        endcase

    endfor

    ; Fix endianness for datatypes with more than 1 byte
    if  ~stregex(vtype,'[^ABXLDE]') then $ 
	swap_endian_inplace, array, /swap_if_little
 
    ; Scale unsigned data
    case vtype of
    '1': unsigned = 12
    '2': unsigned = 13
    '3': unsigned = 15
    else: unsigned = 0
    endcase
   
    if unsigned gt 0 then $
        unsigned = mrd_unsigned_offset(unsigned)
   
    if unsigned gt 0 then begin
        for j=0, nw-1 do begin
            array[0:siz[j]-1,w[j]] = array[0:siz[j]-1,w[j]] - unsigned
	endfor
    endif


end
		
; Read the heap area to get the actual values of variable 
; length arrays. 
pro mrd_read_heap, unit, header, range, fnames, fvalues, vcls, vtpes, table, $ 
   structyp, scaling, scales, offsets, status, silent=silent,                $
   columns=columns, rows = rows, pointer_var=pointer_var, fixed_var=fixed_var
compile_opt idl2, hidden
    ; 
    ; Unit:         FITS unit number. 
    ; header:       FITS header. 
    ; fnames:       Column names. 
    ; fvalues:      Column values. 
    ; vcols:        Column numbers of variable length columns. 
    ; vtypes:       Actual types of variable length columns 
    ; table:        Table of data from standard data area, on output 
    ;               contains the variable length data. 
    ; structyp:     Structure name. 
    ; scaling:      Is there going to be scaling of the data?
    ; status:       Set to -1 if an error occurs.
    ;
    typstr = 'LXBIJKAEDCM123' 
    prefix = ['bytarr(', 'bytarr(', 'bytarr(', 'intarr(',     $ 
              'lonarr(', 'lon64arr(', 'string(bytarr(', 'fltarr(',         $ 
              'dblarr(', 'complexarr(', 'dcomplexarr(',            $
	      'uintarr(', 'ulonarr(', 'ulon64arr(']
    
    status = 0 

    ; Convert from a list of indicators of whether a column is variable
    ; length to pointers to only the variable columns.

    vcols = where(vcls eq 1)
    vtypes = vtpes[vcols]

    nv = n_elements(vcols) 
 
    ; Find the beginning of the heap area. 
 
    heapoff = long64(fxpar(header, 'THEAP')) 
    sz = fxpar(header, 'NAXIS1')*fxpar(header, 'NAXIS2')
    
    if (heapoff ne 0) && (heapoff lt sz) then begin 
        print, 'MRDFITS: ERROR Heap begins within data area' 
        status = -1 
        return 
    endif

    ; Skip to beginning.
    if (heapoff > sz) then begin
        mrd_skip, unit, heapoff-sz
    endif
 
    ; Get the size of the heap. 
    pc = long64(fxpar(header, 'PCOUNT')) 
    if heapoff eq 0 then heapoff = sz 
    hpsiz = pc - (heapoff-sz) 
 
    if (hpsiz gt 0) then heap = bytarr(hpsiz) 
 
 
    ; Read in the heap 
    readu, unit, heap

    ; Skip to the end of the data area.
    skipB = 2880 - (sz+pc) mod 2880
    if skipB ne 2880 then begin
        mrd_skip, unit, skipB
    endif
 
    ; Find the maximum dimensions of the arrays. 
    ; 
    ; Note that the variable length column currently has fields which 
    ; are I*4 2-element arrays where the first element is the 
    ; length of the field on the current row and the second is the 
    ; offset into the heap. 

    vdims = lonarr(nv)
    for i=0, nv-1 do begin 
        col = vcols[i]
        curr_col = table.(col)
        vdims[i] = max(curr_col[0,*])
        w = where(curr_col[0,*] ne vdims[i])
        if w[0] ne -1 then begin
            if n_elements(lencols) eq 0 then begin
                lencols = [col]
            endif else begin
                lencols=[lencols,col]
            endelse
        endif

        if vtypes[i] eq 'X' then vdims[i]=(vdims[i]+7)/8 
        ind = strpos(typstr, vtypes[i])
    
        ; Note in the following that we ensure that the array is
        ; at least one element long.

        fvalues[col] = prefix[ind] + string((vdims[i] > 1)) + ')'
        if vtypes[i] eq 'A' then fvalues[col] = fvalues[col] + ')' 
    
    endfor 
 
    nfld = n_elements(fnames) 

    ; Get rid of columns which have no actual data. 
    w= intarr(nfld) 
    w[*] = 1
    corres = indgen(nfld)

    
    ; Should we get rid of empty columns?
    delete = 1
    if keyword_set(pointer_var) then delete = pointer_var eq 1

    if delete then begin
	
        ww = where(vdims eq 0, N_ww) 
        if N_ww GT 0 then  begin
            w[vcols[ww]] = 0
            if ~keyword_set(silent) then $
                print, 'MRDFITS: ', strcompress(string(n_elements(ww))),  $
                  ' unused variable length columns deleted'
        endif

        ; Check if all columns have been deleted...
        wx = where(w gt 0, N_wx)
        if N_wx EQ 0 then begin
            if ~keyword_set(silent) then $
                print, 'MRDFITS: All columns have been deleted'
 	    table = 0
	    return
        endif
    

        ; Get rid of unused columns.
        corres = corres[wx]
        fnames = fnames[wx] 
        fvalues = fvalues[wx]
        scales = scales[wx]
        offsets = offsets[wx]

        wx = where(vdims gt 0)
    
        if (wx[0] eq -1) then begin
            vcols=[-9999]
	    x=temporary(vtypes)
	    x=temporary(vdims)
        endif else begin 
            vcols = vcols[wx]
            vtypes = vtypes[wx]
            vdims = vdims[wx]
        endelse
    endif

    if ~keyword_set(pointer_var) then begin
        ; Now add columns for lengths of truly variable length records.
        if n_elements(lencols) gt 0 then begin
            if ~keyword_set(silent) then $
                print, 'MRDFITS: ', strcompress(string(n_elements(lencols))), $
                  ' length column[s] added'
         

            for i=0, n_elements(lencols)-1 do begin
                col = lencols[i]
                w = where(col eq corres)
                ww = where(col eq vcols)
                w = w[0]
                ww = ww[0]
                fvstr = '0L' ; <-- Originally, '0l'; breaks under the virtual machine!
                fnstr = 'L'+strcompress(string(col),/remo)+'_'+fnames[w]
                nf = n_elements(fnames)
                
                ; Note that lencols and col refer to the index of the
                ; column before we started adding in the length
                ; columns.
                
                if w eq nf-1 then begin
                    ; Subtract -1 for the length columns so 0 -> -1 and
                    ; we can distinguish this column.

                    corres = [corres, -col-1 ]
                    fnames = [fnames, fnstr ]
                    fvalues = [fvalues, fvstr ]
                    scales = [scales, 1.0d0 ]
                    offsets = [offsets, 0.0d0 ]
		    
                endif else begin
		    
                    corres = [corres[0:w],-col-1,corres[w+1:nf-1] ]
                    fnames = [fnames[0:w],fnstr,fnames[w+1:nf-1] ]
                    fvalues = [fvalues[0:w],fvstr,fvalues[w+1:nf-1] ]
                    scales = [scales[0:w], 1.0d0, scales[w+1:nf-1] ]
                    offsets = [offsets[0:w],0.0d0, offsets[w+1:nf-1] ]
                endelse
            endfor
        endif
	
    endif else begin
	
        ; We'll just read data into pointer arrays.
	for i=0,n_elements(lencols)-1 do begin
	    col = lencols[i]
	    if vtpes[col] eq 'A' then begin
	        fvalues[col] = '" "'
	    endif else begin
	        fvalues[col] = 'ptr_new()'
	    endelse
	endfor
	
    endelse
	


    ; Generate a new table with the appropriate structure definitions 
    if ~scaling && ~keyword_set(columns) then begin
        tablex = mrd_struct(fnames, fvalues, n_elements(table), structyp=structyp, $
                            silent=silent)
    endif else begin
        tablex = mrd_struct(fnames, fvalues, n_elements(table), silent=silent)
    endelse


    if N_elements(rows) EQ 0 then nrow = range[1]-range[0]+1 $
                             else nrow = N_elements(rows)
    
    ; I loops over the new table columns, col loops over the old table.
    ; When col is negative, it is a length column.
    for i=0, n_elements(fnames)-1 do begin
        
        col = corres[i]
                
        if col ge 0 then begin
	    
            w = where(vcols eq col)
                
            ; First handle the case of a column that is not
            ; variable length -- just copy the column.
                
            if w[0] eq -1 then begin
		    
                     tablex.(i) = table.(col)
 		    
            endif else begin
		
                vc = w[0]
                ; Now handle the variable length columns
                        
                ; If only one row in table, then
                ; IDL will return curr_col as one-dimensional.
                ; Since this is a variable length pointer column we
                ; know that the dimension of the column is 2.
                    curr_col = table.(col)
		
                if (nrow eq 1) then curr_col = reform(curr_col,2,1)
                siz = curr_col[0,*] 
                off = curr_col[1,*] 
                    
                ; Now process each type.
                    curr_colx = tablex.(i)
                    sz = size(curr_colx)
                    if (sz[0] lt 2) then begin
                        curr_colx = reform(curr_colx, 1, n_elements(curr_colx), /overwrite)
                    endif
                           
                    
                ; As above we have to worry about IDL truncating
                ; dimensions.  This can happen if either
                ; nrow=1 or the max dimension of the column is 1.
                    

                    sz = size(tablex.(i))
 
                nel = sz[sz[0]+2]
                if (nrow eq 1) && (nel eq 1) then begin
                    curr_colx = make_array(1,1,value=curr_colx)
                endif else if nrow eq 1 then begin
                    curr_colx = reform(curr_colx,[nel, 1], /overwrite)
                endif else if nel eq 1 then begin
                    curr_colx = reform(curr_colx,[1, nrow], /overwrite)
                endif

		vtype = vtypes[vc]
		varying = 0
		if n_elements(lencols) gt 0 then begin
		    varying = where(lencols eq col)
		    if varying[0] eq -1 then varying=0 else varying=1
		endif
		
		if varying && keyword_set(pointer_var) && (vtype ne 'A') then begin
		    mrd_varcolumn, vtype, curr_colx, heap, off, siz
		endif else begin
		    mrd_fixcolumn, vtype, curr_colx, heap, off, siz
		endelse


                
                if nel eq 1 and nrow eq 1 then begin
                    curr_colx = curr_colx[0]
                endif else if nrow eq 1 then begin
                    curr_colx = reform(curr_colx, nel, /overwrite)
                endif else if nel eq 1 then begin
                    curr_colx = reform(curr_colx, nrow, /overwrite)
                endif

                    sz = size(curr_colx)
                    if sz[1] eq 1 then begin
                         sz_tablex = size(tablex.(i))
                         sdimen = sz_tablex[1:sz_tablex[0]]
                         tablex.(i) = reform(curr_colx,sdimen)
                    endif else begin
                        tablex.(i) = curr_colx
                    endelse
                 
            endelse
                
        endif else begin
            ; Now handle the added columns which hold the lengths
            ; of the variable length columns.
                
            ncol = -col - 1 ; Remember we subtracted an extra one.
                xx = table.(ncol)
                tablex.(i) = reform(xx[0,*])
       endelse
    endfor 
 
    ; Finally get rid of the initial table and return the table with the 
    ; variable arrays read in. 
    ; 
    table = temporary(tablex) 
    return 
end 

; Read in the binary table information. 
pro mrd_read_table, unit, range, rsize, structyp, nrows, nfld, typarr, table, rows = rows, $
     unixpipe = unixpipe
compile_opt idl2, hidden 
    ; 
    ; 
    ; Unit          Unit to read data from. 
    ; Range         Desired range 
    ; Rsize         Size of row. 
    ; structyp      Structure type. 
    ; Nfld          Number of fields in structure. 
    ; Typarr        Field types 
    ; Table         Table to read information into.
    ; 

    if range[0] gt 0 then mrd_skip, unit, rsize*range[0]
    readu,unit, table
    if N_elements(rows) GT 0 then table = table[rows- range[0]]

    ; Move to the beginning of the heap -- we may have only read some rows of
    ; the data.
    if range[1] lt nrows-1 then begin
        skip_dist = (nrows-range[1]-1)*rsize
        mrd_skip, unit, skip_dist
    endif

    

    ; If necessary then convert to native format.
    if unixpipe then swap_endian_inplace,table,/swap_if_little
	

    ; Handle unsigned fields.
    for i=0, nfld-1 do begin

	    type = mrd_unsignedtype(table.(i))

	    if type gt 0 then begin	    
	        table.(i) = table.(i) - mrd_unsigned_offset(type)
	    endif
	    
	
    endfor
 end


; Check the values of TDIM keywords to see that they have valid
; dimensionalities.  If the TDIM keyword is not present or valid
; then the a one-dimensional array with a size given in the TFORM
; keyword is used.

pro mrd_tdim, header, index, flen, arrstr, no_tdim=no_tdim
compile_opt idl2, hidden
    ; HEADER        Current header array.
    ; Index         Index of current parameter
    ; flen          Len given in TFORM keyword
    ; arrstr        String returned to be included within paren's in definition.
    ; no_tdim       Disable TDIM processing

    arrstr = strcompress(string(flen),/remo)

    if keyword_set(no_tdim) then return

    tdstr = fxpar(header, 'TDIM'+strcompress(string(index),/remo))
    if tdstr eq '' then return

    ;
    ; Parse the string.  It should be of the form '(n1,n2,...nx)' where
    ; all of the n's are positive integers and the product equals flen.
    ;
    tdstr = strcompress(tdstr,/remo)
    len = strlen(tdstr)
    if strmid(tdstr,0,1) ne '(' && strmid(tdstr,len-1,1) ne ')' || len lt 3 then begin
        print, 'MRDFITS: Error: invalid TDIM for column', index
        return
    endif

    ; Get rid of parens.
    tdstr = strmid(tdstr,1,len-2)
    len = len-2

    nind = 0
    cnum = 0

    for nchr=0, len-1 do begin
        c = strmid(tdstr,nchr, 1)
        
        if c ge '0' &&  c le '9' then begin
            cnum = 10*cnum + long(c)
                
        endif else if c eq ',' then begin
        
            if cnum le 0 then begin
                print,'MRDFITS: Error: invalid TDIM for column', index
                return
            endif
                
            if n_elements(numbs) eq 0 then  $
                 numbs = cnum $
            else    numbs = [numbs,cnum]
                
            cnum = 0
                
       endif else begin
       
            print,'MRDFITS: Error: invalid TDIM for column', index
            return
                
       endelse

    endfor

    ; Handle the last number.
    if cnum le 0 then begin
        print,'MRDFITS: Error: invalid TDIM for column', index
        return
    endif

    if n_elements(numbs) eq 0 then numbs = cnum else numbs = [numbs,cnum]

    prod = 1

    for i=0, n_elements(numbs)-1 do prod = prod*numbs[i]
 
    if prod ne flen then begin
        print,'MRDFITS: Error: TDIM/TFORM dimension mismatch'
        return
    endif

    arrstr = tdstr
end
 
; Define a structure to hold a FITS binary table. 
pro mrd_table, header, structyp, use_colnum,           $ 
    range, rsize, table, nrows, nfld, typarr, fnames, fvalues,   $ 
    vcls, vtpes, scales, offsets, scaling, status, rows = rows, $
    silent=silent, columns=columns, no_tdim=no_tdim, $
    alias=alias, unsigned=unsigned, outalias=outalias,emptystring=emptystring
 compile_opt idl2, hidden
    ; 
    ; Header                FITS header for table. 
    ; Structyp              IDL structure type to be used for 
    ;                       structure. 
    ; N_call                Number of times this routine has been called. 
    ; Table                 Structure to be defined. 
    ; Status                Return status.
    ; No_tdim               Disable TDIM processing.

    table = 0

    types =  ['L', 'X', 'B', 'I', 'J', 'K', 'A', 'E', 'D', 'C', 'M', 'P']
    arrstr = ['bytarr(', 'bytarr(', 'bytarr(', 'intarr(', 'lonarr(', 'lon64arr(',      $ 
              'string(replicate(32b,', 'fltarr(', 'dblarr(', 'complexarr(',            $ 
              'dcomplexarr(', 'lonarr(2*']
    bitpix = [  0,   0,   0,  16,  32,  64,   0,  0,   0,   0,   0,   0]

    sclstr = ["'T'", '0B', '0B', '0', '0L', '0LL', '" "', '0.', '0.d0', 'complex(0.,0.)', $ 
              'dcomplex(0.d0,0.d0)', 'lonarr(2)']
    if keyword_set(emptystring) then begin 
        sclstr[6] = '0B'
        arrstr[6] = 'bytarr(' 
    endif 	
    unsarr = ['', '', '', 'uintarr(', 'ulonarr(', 'ulon64arr('];
    unsscl = ['', '', '', '0US',        '0UL',      '0ULL']
 

    status = 0 

; NEW WAY: E.S.S.

    ;; get info from header. Using vectors is much faster
    ;; when there are many columns

    mrd_fxpar, header, xten, nfld, nrow, rsize, fnames, fforms, scales, offsets
    nnames = n_elements(fnames)

    tname = fnames
    ;; nrow will change later
    nrows = nrow

    ;; Use scale=1 if not found
    if nnames GT 0 then begin
      wsc=where(scales EQ 0.0d,nwsc)
      IF nwsc NE 0 THEN scales[wsc] = 1.0d
    endif

    xten = strtrim(xten,2)
    if xten ne 'BINTABLE' and xten ne 'A3DTABLE' then begin 
        print, 'MRDFITS: ERROR - Header is not from binary table.' 
        nfld = 0 & status = -1 
        return 
    endif 
 
    if range[0] ge 0 then begin 
        range[0] = range[0] < (nrow-1) 
        range[1] = range[1] < (nrow-1) 
    endif else begin 
        range[0] = 0 
        range[1] = nrow - 1 
    endelse
    
    nrow = range[1] - range[0] + 1 
    if nrow le 0 then begin
        if ~keyword_set(silent) then $
            print, 'MRDFITS: Binary table. ', $
             strcompress(string(nfld)), ' columns, no rows.'
        return
    endif

    if N_elements(rows) EQ 0 then nrowp  = nrow else begin 
          bad = where((rows LT range[0]) or (rows GT range[1]), Nbad)
          if Nbad GT 0 then begin 
             print,'MRDFITS: Row numbers must be between 0 and ' + $
                    strtrim(nrow-1,2)      
             status = -1
             return
           endif
           nrowp = N_elements(rows)
    endelse
;    rsize = fxpar(header, 'NAXIS1') 
 
    ; 
    ;  Loop over the columns           
 
    typarr   = strarr(nfld) 
    
    fvalues  = strarr(nfld) 
    dimfld   = strarr(nfld)
    
    vcls     = intarr(nfld)
    vtpes    = strarr(nfld)
 
    fnames2 = strarr(nfld)

    for i=0, nfld-1 do begin
	
        istr = strcompress(string(i+1), /remo)

        fname = fnames[i]

        ;; check for a name conflict
        fname = mrd_dofn(fname, i+1, use_colnum, alias=alias)
	
        ;; check for a name conflict
        fname = mrd_chkfn(fname, fnames2, i)

        ;; copy in the valid name
        fnames[i] = fname
        ;; for checking conflicts
        fnames2[i] = fname
	
        fform = fforms[i]

        mrd_doff, fform, dim, ftype
        
        ; Treat arrays of length 1 as scalars.
        if dim eq 1 then begin
            dim = 0
        endif else if dim EQ -1 then begin 
            dimfld[i] = -1
        endif else begin
            mrd_tdim, header, i+1, dim, str, no_tdim=no_tdim
            dimfld[i] = str
        endelse
                
        typarr[i] = ftype 
        
        
        ; Find the number of bytes in a bit array. 
 
        if ftype eq 'X' && (dim gt 0) then begin
            dim = (dim+7)/8 
            dimfld[i] = strtrim(string(dim),2)
        endif
         
        ; Add in the structure label. 
        ; 
         
        ; Handle variable length columns. 
        if ftype eq 'P' then begin 
 
            if (dim ne 0)  && (dim ne 1) then begin 
                print, 'MRDFITS: Invalid dimension for variable array column '+string(i+1) 
                status = -1 
                return 
            endif
	    
            ppos = strpos(fform, 'P') 
            vf = strmid(fform, ppos+1, 1); 
            if strpos('LXBIJKAEDCM', vf) eq -1 then begin 
                print, 'MRDFITS: Invalid type for variable array column '+string(i+1) 
                status = -1 
                return 
            endif 

            vcls[i] = 1
	    
	    
	    xunsigned = mrd_chkunsigned(bitpix[ppos], scales[i],       $
				       offsets[i], $
				       unsigned=unsigned)

	    if (xunsigned) then begin
		
		if      vf eq 'I' then vf = '1' $
		else if vf eq 'J' then vf = '2' $
		else if vf eq 'K' then vf = '3'
		
	    endif
							   
            vtpes[i] = vf
            dim = 0
                         
        endif 
         

        for j=0, n_elements(types) - 1 do begin
	    
            if ftype eq types[j] then begin

                xunsigned = mrd_chkunsigned(bitpix[j], scales[i], $
                                            offsets[i], $
                                            unsigned=unsigned)

		if xunsigned then begin		     
		    fxaddpar, header, 'TZERO'+istr, 0, 'Modified by MRDFITS V'+mrd_version()
                    offsets[i] = 0 ;; C. Markwardt Aug 2007 - reset to zero so offset is not applied twice'
	        endif
                if dim eq 0 then begin

                   fvalues[i] = xunsigned ? unsscl[j] : sclstr[j]
		    
                endif else begin

		    line = xunsigned ?  unsarr[j] : arrstr[j]
		    
                    line = line + dimfld[i] + ')'
                    if not keyword_set(emptystring) then $
		         if ftype eq 'A' then line = line + ')' 
                    fvalues[i] = line
		    
                endelse
		
                goto, next_col
		
            endif
	    
        endfor 
         
        print, 'MRDFITS: Invalid format code:',ftype, ' for column ', i+1 
        status = -1 
        return 
  next_col: 
    endfor 

    ; Check if there are any variable length columns.  If not then
    ; undefine vcls and vtpes
    w = where(vcls eq 1, N_w)
    if N_w eq 0 then begin
        dummy = temporary(vcls)
        dummy = temporary(vtpes)
        dummy = 0
    endif

    if scaling then begin 
        w = where( (scales ne 1.0d0) or (offsets ne 0.0d0), Nw)
        scaling = Nw GT 0
    endif

    zero = where(long(dimfld) LT 0L, N_zero)
    if N_zero GT 0 then begin
	
        if N_zero Eq nfld then begin
            print,'MRDFITS: Error - All fields have zero length'
            return
        endif
	
        for i=0, N_zero-1 do begin
	    print,'MRDFITS: Table column ' + fnames[zero[i]] + ' has zero length'
	endfor
	
        nfld    = nfld - N_zero
        good    = where(dimfld GE 0)
        fnames  = fnames[good]
        fvalues = fvalues[good]
        typarr = typarr[good]      ;Added 2005-1-6   (A.Csillaghy)
        tname = tname[good]        
	
    endif

    if n_elements(vcls) eq 0  &&  (~scaling) && ~keyword_set(columns) then begin
	
        table = mrd_struct(fnames, fvalues, nrow, structyp=structyp,  silent=silent )
	
    endif else begin
	
        table = mrd_struct(fnames, fvalues, nrow, silent=silent )
	
    endelse

    if ~keyword_set(silent) then begin
        print, 'MRDFITS: Binary table. ',strcompress(string(nfld)), ' columns by ',  $
          strcompress(string(nrowp)), ' rows.'
        if n_elements(vcls) gt 0 then begin
                print, 'MRDFITS: Uses variable length arrays'
        endif
    endif

    outalias = transpose([[tag_names(table)],[tname] ])
    status = 0 
    return 
 
end 

function mrdfits, file, extension, header,      $
        structyp = structyp,                    $
        use_colnum = use_colnum,                $
        range = range,                          $
        dscale = dscale, fscale=fscale,         $
        fpack = fpack, no_fpack = no_fpack,     $
        silent = silent,                        $
        columns = columns,                      $
        no_tdim = no_tdim,                      $
        error_action = error_action,            $
 	compress=compress,                      $
	alias=alias,                            $
        rows = rows,                        $
	unsigned=unsigned,                      $
	version=version,                        $
	pointer_var=pointer_var,                $
	fixed_var=fixed_var,                    $
	outalias = outalias,                     $
	emptystring = emptystring,               $
        status=status, extnum = extnum

    compile_opt idl2    
    ;   Let user know version if MRDFITS being used.
    if keyword_set(version) then $
        print,'MRDFITS: Version '+mrd_version() + 'April 29, 2011'
        
      
    if N_elements(error_action) EQ 0 then error_action = 2
    On_error, error_action
   
    ; Check positional arguments.

    if n_params() le 0  || n_params() gt 3 then begin
	if keyword_set(version) then return, 0
        print, 'MRDFITS: Usage'
        print, '   a=mrdfits(file/unit, [exten_no/exten_name, header], /version $'
        print, '       /fscale, /dscale, /unsigned, /use_colnum, /silent    $'
        print, '       range=, rows= , structyp=, columns=, $'
	print, '       /pointer_var, /fixed_var, error_action=, status= )'
        return, 0
    endif
   
    if n_params() eq 1 then extension = 0
   
    ; Check optional arguments.
    ;
    ;  *** Structure name ***

    if keyword_set(structyp) then begin
        sz = size(structyp)
        if sz[0] ne 0 then begin
            ; Use first element of array
            structyp = structyp[0]
            sz = size(structyp[0])
        endif
	
        if sz[1] ne 7 then begin
            print, 'MRDFITS: stucture type must be a string'
            return, 0
        endif
    endif

    ;  *** Use column numbers not names?
    use_colnum = keyword_set(use_colnum)

    ;  *** Get only a part of the FITS file.
    if N_elements(rows) GT 0 then begin
        range1 = min(rows,max=range2)
        range = [range1,range2]
    endif
    if keyword_set(range) then begin
        if n_elements(range) eq 2 then arange = range $
        else if n_elements(range) eq 1 then arange = [0,range[0]-1] $
        else if n_elements(range) gt 2 then arange = range[0:1] $
        else if n_elements(range) eq 0 then arange = [-1,-1]
	
    endif else begin
	arange = [-1,-1]
    endelse

    arange = long(arange)

    ; Open the file and position to the appropriate extension then read
    ; the header.

    if (N_elements(file) GT 1 ) then begin
        print, 'MRDFITS: Vector input not supported'
        return, 0
    endif

    inputUnit = 0
   
    dtype = size(file,/type)
    if (dtype gt 0) && (dtype lt 4) then begin    ;File unit number specified
	
        inputUnit = 1
        unit = file
        unixpipe =  (fstat(unit)).size EQ 0     ;Unix pipes have no files size    
        if fxmove(unit,extension) lt 0 then return, -1
    
    endif else begin                         ;File name specified
        unit = fxposit(file, extension, compress=compress, unixpipe=unixpipe, $
	               /readonly,extnum=extnum, errmsg= errmsg, fpack=fpack)

        if unit lt 0 then begin
            message, 'File access error',/CON
	    if errmsg NE '' then message,errmsg,/CON
            status = -1
            return, 0
        endif
    endelse

    if eof(unit) then begin
        message,'ERROR - Extension past EOF',/CON
	if inputUnit eq 0 then free_lun,unit 
	status = -2
	return, 0
    endif

    mrd_hread, unit, header, status, SILENT = silent, ERRMSG = errmsg
    
    if status lt 0 then begin
	message,'ERROR - ' +errmsg,/CON
        message, 'ERROR - FITS file may be invalid or corrupted',/CON
 	if inputUnit eq 0 then free_lun,unit
        return, 0
    endif

; If the ZIMAGE keyword is present in the header, then we must re-open the
; file using a pipe.

    if ~keyword_set(no_fpack) then $
         if (inputunit EQ 0) && (~unixpipe) then begin 
            if sxpar(header,'ZIMAGE') then begin 
	    free_lun,unit
            unit = fxposit(file, extension, compress=compress, /fpack, $ 
	           unixpipe=unixpipe,/readonly,extnum=extnum, errmsg= errmsg)
            mrd_hread, unit, header, status, SILENT = silent, ERRMSG = errmsg
      endif
    endif  	    
	     
    ; If this is primary array then XTENSION will have value
    ; 0 which will be converted by strtrim to '0'

    xten = strtrim( fxpar(header,'XTENSION'), 2)
    if xten eq '0' || xten eq 'IMAGE' then type = 0 $
    else if xten eq 'TABLE' then type = 1 $
    else if xten eq 'BINTABLE' || xten eq 'A3DTABLE' then type = 2 $
    else begin 
        message, 'Unable to process extension type:' + strtrim(xten,2),/CON
	if inputUnit eq 0 then free_lun,unit
	status = -1
        return, 0
    endelse

    scaling = keyword_set(fscale) or keyword_set(dscale)

    if type eq 0 then begin

        ;*** Images/arrays
        
        mrd_image, header, arange, maxd, rsize, table, scales, offsets, $
          scaling, status, silent=silent, unsigned=unsigned, $
           rows= rows
       if (status ge 0) && (rsize gt 0) then begin
           mrd_read_image, unit, arange, maxd, rsize, table, rows = rows,$
            status=status, unixpipe=unixpipe
        endif
       size = rsize
    endif else if type eq 1 then begin

        ;*** ASCII tables.
        
        mrd_ascii, header, structyp, use_colnum,                              $
            arange, table, nbytes, nrows, nfld, rows=rows,                    $
            typarr, posarr, lenarr, nullarr, fnames, fvalues,                 $
            scales, offsets, scaling, status, silent=silent,                  $
            columns=columns, alias=alias, outalias=outalias
        size = nbytes*nrows
        
        if (status ge 0)   &&  (size gt 0)  then begin
        
            ;*** Read data.
            mrd_read_ascii, unit,  arange, nbytes, nrows,   $
              nfld, typarr, posarr, lenarr, nullarr, table,  rows= rows
              
            ;*** Extract desired columns.
            if (status ge 0) && keyword_set(columns) then                  $
                mrd_columns, table, columns, fnames, fvalues, vcls, vtps, $
                  scales, offsets, scaling, structyp=structyp, silent=silent
        endif
        
    endif else begin

        ; *** Binary tables.

        mrd_table, header, structyp, use_colnum,                            $
          arange, rsize, table, nrows, nfld, typarr,                        $ 
          fnames, fvalues, vcls, vtpes, scales, offsets, scaling, status,   $
          silent=silent, columns=columns, no_tdim=no_tdim, $
          alias=alias, unsigned=unsigned, rows = rows, outalias = outalias, $
	  emptystring=emptystring

        size = nfld*(arange[1] - arange[0] + 1)
        if (status ge 0)  &&  (size gt 0)  then begin

            ;*** Read data.
            mrd_read_table, unit, arange, rsize,  rows = rows, $
              structyp, nrows, nfld, typarr, table, unixpipe=unixpipe

            if (status ge 0) && keyword_set(columns) then begin
        
                ;*** Extract desired columns.
                mrd_columns, table, columns, fnames, fvalues,                  $
                  vcls, vtpes, scales, offsets, scaling, structyp=structyp,    $
                  silent=silent
	    
	    endif
         
             if keyword_set(emptystring) then $
	      mrd_string, table, header, typarr, $
               fnames, fvalues,  1+arange[1]-arange[0], structyp=structyp, silent=silent

            if (status ge 0) && n_elements(vcls) gt 0 then begin 
          
                ;*** Get variable length columns
                mrd_read_heap, unit, header, arange, fnames, fvalues,             $
                  vcls, vtpes, table, structyp, scaling, scales, offsets, status, $
                  silent=silent, pointer_var=pointer_var, fixed_var=fixed_var, rows= rows
		
	    endif else begin

	        ; Skip remainder of last data block
	        sz = long64(fxpar(header, 'NAXIS1'))* $
                     long64(fxpar(header,'NAXIS2')) +  $
		       long64(fxpar(header, 'PCOUNT'))
	        skipB = 2880 - sz mod 2880
	        if (skipB ne 2880) then mrd_skip, unit, skipB
            endelse
		     
        endif

    endelse


    ; Don't tie up a unit number that we allocated in this routine.
    if (unit gt 0) && (inputUnit eq 0) then free_lun, unit

; If any of the scales are non-unity, or any of the offsets are nonzero then 
; apply scalings.

    if  (status ge 0)  &&  scaling  &&  (size gt 0)  then begin
	noscale = array_equal(scales,1.d0) &&  array_equal(offsets,0.0) 
        
        if ~noscale then mrd_scale, type, scales, offsets, table, header,  $
            fnames, fvalues, 1+arange[1]-arange[0], structyp=structyp,       $
            dscale=dscale, silent=silent
    endif

    ; All done. Check the status to see if we ran into problems on the way.
    
    if status ge 0 then return, table else return,0
 
end
function SXPAR, hdr, name, abort, COUNT=matches, COMMENT = comments, $
                                  NoContinue = NoContinue, SILENT = silent
;+
; NAME:
;      SXPAR
; PURPOSE:
;      Obtain the value of a parameter in a FITS header
;
; CALLING SEQUENCE:
;      result = SXPAR( Hdr, Name, [ Abort, COUNT=, COMMENT =, /NoCONTINUE, 
;                                           /SILENT  ])   
;
; INPUTS:
;      Hdr =  FITS header array, (e.g. as returned by READFITS) 
;             string array, each element should have a length of 80 characters      
;
;      Name = String name of the parameter to return.   If Name is of the
;             form 'keyword*' then an array is returned containing values of
;             keywordN where N is an integer.  The value of keywordN will be
;             placed in RESULT(N-1).  The data type of RESULT will be the
;             type of the first valid match of keywordN found.
;
; OPTIONAL INPUTS:
;       ABORT - string specifying that SXPAR should do a RETALL
;               if a parameter is not found.  ABORT should contain
;               a string to be printed if the keyword parameter is not found.
;               If not supplied, SXPAR will return quietly with COUNT = 0
;               (and !ERR = -1) if a keyword is not found.
;
; OPTIONAL INPUT KEYWORDS: 
;       /NOCONTINUE = If set, then continuation lines will not be read, even
;                 if present in the header
;       /SILENT - Set this keyword to suppress warning messages about duplicate
;                 keywords in the FITS header.
;
; OPTIONAL OUTPUT KEYWORDS:
;       COUNT - Optional keyword to return a value equal to the number of 
;               parameters found by SXPAR, integer scalar
;
;       COMMENT - Array of comments associated with the returned values
;
; OUTPUTS:
;       Function value = value of parameter in header.
;               If parameter is double precision, floating, long or string,
;               the result is of that type.  Apostrophes are stripped
;               from strings.  If the parameter is logical, 1b is
;               returned for T, and 0b is returned for F.
;               If Name was of form 'keyword*' then a vector of values
;               are returned.
;
; SIDE EFFECTS:
;       !ERR is set to -1 if parameter not found, 0 for a scalar
;       value returned.  If a vector is returned it is set to the
;       number of keyword matches found.    The use of !ERR is deprecated, and
;       instead the COUNT keyword is preferred
;
;       If a keyword (except HISTORY or COMMENT) occurs more than once in a 
;       header, a warning is given, and the *last* occurence is used.
;
; EXAMPLES:
;       Given a FITS header, h, return the values of all the NAXISi values
;       into a vector.    Then place the history records into a string vector.
;
;       IDL> naxisi = sxpar( h ,'NAXIS*')         ; Extract NAXISi value
;       IDL> history = sxpar( h, 'HISTORY' )      ; Extract HISTORY records
;
; PROCEDURE:
;       The first 8 chacters of each element of Hdr are searched for a 
;       match to Name.  The value from the last 20 characters is returned.  
;       An error occurs if there is no parameter with the given name.
;
;       If a numeric value has no decimal point it is returned as type
;       LONG.   If it contains more than 8 numerals, or contains the 
;       characters 'D' or 'E', then it is returned as type DOUBLE.  Otherwise
;       it is returned as type FLOAT.    Very large integer values, outside
;       the range of valid LONG, are returned as DOUBLE.
;
;       If the value is too long for one line, it may be continued on to the
;       the next input card, using the OGIP CONTINUE convention.  For more info,
;       see http://fits.gsfc.nasa.gov/registry/continue_keyword.html
;
;       Complex numbers are recognized as two numbers separated by one or more
;       space characters.
;
;       If a numeric value has no decimal point (or E or D) it is returned as
;       type LONG.  If it contains more than 8 numerals, or contains the
;       character 'D', then it is returned as type DOUBLE.  Otherwise it is
;       returned as type FLOAT.    If an integer is too large to be stored as
;       type LONG, then it is returned as DOUBLE.
;
; NOTES:
;       The functions SXPAR() and FXPAR() are nearly identical, although
;       FXPAR() has slightly more sophisticated parsing, and additional keywords
;       to specify positions in the header to search (for speed), and to force
;       the output to a specified data type..   There is no
;       particular reason for having two nearly identical procedures, but
;       both are too widely used to drop either one.
;
; PROCEDURES CALLED:
;       GETTOK(), VALID_NUM()
; MODIFICATION HISTORY:
;       DMS, May, 1983, STPAR Written.
;       D. Lindler Jan 90 added ABORT input parameter
;       J. Isensee Jul,90 added COUNT keyword
;       W. Thompson, Feb. 1992, added support for FITS complex values.
;       W. Thompson, May 1992, corrected problem with HISTORY/COMMENT/blank
;               keywords, and complex value error correction.
;       W. Landsman, November 1994, fix case where NAME is an empty string 
;       W. Landsman, March 1995,  Added COMMENT keyword, ability to read
;               values longer than 20 character
;       W. Landsman, July 1995, Removed /NOZERO from MAKE_ARRAY call
;       T. Beck May 1998, Return logical as type BYTE
;       W. Landsman May 1998, Make sure integer values are within range of LONG
;       W. Landsman Feb 1998, Recognize CONTINUE convention 
;       W. Landsman Oct 1999, Recognize numbers such as 1E-10 as floating point
;       W. Landsman Jan 2000, Only accept integer N values when name = keywordN
;       W. Landsman Dec 2001, Optional /SILENT keyword to suppress warnings
;       W. Landsman/D. Finkbeiner  Mar 2002  Make sure extracted vectors 
;             of mixed data type are returned with the highest type.
;       W.Landsman Aug 2008  Use vector form of VALID_NUM()
;       W. Landsman Jul 2009  Eliminate internal recursive call
;-
;----------------------------------------------------------------------
 On_error,2
 compile_opt idl2

 if N_params() LT 2 then begin
     print,'Syntax -  result =  sxpar( hdr, name, [abort])'
     print,'   Input Keywords:    /NOCONTINUE, /SILENT'
     print,'   Output Keywords:   COUNT=,  COMMENT= '
     return, -1
 endif 

 VALUE = 0
 if N_params() LE 2 then begin
      abort_return = 0
      abort = 'FITS Header'
 end else abort_return = 1
 if abort_return then On_error,1 else On_error,2

;       Check for valid header

;Check header for proper attributes.
  if ( size(hdr,/N_dimen) NE 1 ) or ( size(hdr,/type) NE 7 ) then $
           message,'FITS Header (first parameter) must be a string array'

  nam = strtrim( strupcase(name) )      ;Copy name, make upper case     


;  Determine if NAME is of form 'keyword*'.  If so, then strip off the '*', and
;  set the VECTOR flag.  One must consider the possibility that NAM is an empty
;  string.

   namelength1 = (strlen(nam) - 1 ) > 1         
   if strpos( nam, '*' ) EQ namelength1 then begin    
            nam = strmid( nam, 0, namelength1)  
            vector = 1                  ;Flag for vector output  
            name_length = strlen(nam)   ;Length of name 
            num_length = 8 - name_length        ;Max length of number portion  
            if num_length LE 0 then  $ 
                  message, 'Keyword length must be 8 characters or less'

;  Otherwise, extend NAME with blanks to eight characters.

    endif else begin  
                while strlen(nam) LT 8 do nam = nam + ' ' ;Make 8 chars long
                vector = 0      
    endelse


;  If of the form 'keyword*', then find all instances of 'keyword' followed by
;  a number.  Store the positions of the located keywords in NFOUND, and the
;  value of the number field in NUMBER.

        histnam = (nam eq 'HISTORY ') or (nam eq 'COMMENT ') or (nam eq '') 
        keyword = strmid( hdr, 0, 8)
 
        if vector then begin
            nfound = where(strpos(keyword,nam) GE 0, matches)
            if  matches GT 0  then begin
                numst= strmid( hdr[nfound], name_length, num_length)
		igood = where(VALID_NUM(numst,/INTEGER), matches)
		if matches GT 0 then begin 
		     nfound = nfound[igood]
                     number = long(numst[igood])
		endif 
           endif

;  Otherwise, find all the instances of the requested keyword.  If more than
;  one is found, and NAME is not one of the special cases, then print an error
;  message.

        endif else begin
            nfound = where(keyword EQ nam, matches)
             if (matches GT 1) and ~histnam then        $
                if ~keyword_set(silent) then $
                message,/informational, 'Warning - keyword ' +   $
                nam + ' located more than once in ' + abort
        endelse


; Process string parameter 

 if matches GT 0 then begin
  line = hdr[nfound]
  svalue = strtrim( strmid(line,9,71),2)
  if histnam then $
        value = strtrim(strmid(line,8,71),2) else for i = 0,matches-1 do begin
      if ( strmid(svalue[i],0,1) EQ "'" ) then begin   ;Is it a string?
                  test = strmid( svalue[i],1,strlen( svalue[i] )-1)
                  next_char = 0
                  off = 0
                  value = '' 
          NEXT_APOST:
                  endap = strpos(test, "'", next_char)      ;Ending apostrophe  
                  if endap LT 0 then $ 
                            MESSAGE,'Value of '+name+' invalid in '+abort
                  value = value + strmid( test, next_char, endap-next_char )  

;  Test to see if the next character is also an apostrophe.  If so, then the
;  string isn't completed yet.  Apostrophes in the text string are signalled as
;  two apostrophes in a row.

                 if strmid( test, endap+1, 1) EQ "'" then begin    
                    value = value + "'"
                    next_char = endap+2         
                    goto, NEXT_APOST
                 endif      

; Extract the comment, if any
                
                slash = strpos( test, "/", endap )
                if slash LT 0 then comment = '' else    $
                        comment = strmid( test, slash+1, strlen(test)-slash-1 )

; This is a string that could be continued on the next line.  Check this
; possibility with the following four criteria: *1) Ends with '&'
; (2) Next line is CONTINUE  (3) LONGSTRN keyword is present (recursive call to
; SXPAR) 4. /NOCONTINE is not set

    if ~keyword_set(nocontinue) then begin
                off = off + 1
                val = strtrim(value,2)

                if (strlen(val) gt 0) && $
                  (strmid(val, strlen(val)-1, 1) EQ '&') && $
                  (strmid(hdr[nfound[i]+off],0,8) EQ 'CONTINUE') then $
		      if ~array_equal(keyword EQ 'LONGSTRN',0b) then begin 
                  value = strmid(val, 0, strlen(val)-1)
                  test = hdr[nfound[i]+off]
                  test = strmid(test, 8, strlen(test)-8)
                  test = strtrim(test, 2)
                  if strmid(test, 0, 1) NE "'" then message, $
                    'ERROR: Invalidly CONTINUEd string in '+ abort
                  next_char = 1
                  GOTO, NEXT_APOST
                ENDIF
    ENDIF


; Process non-string value  

          endif else begin

                test = svalue[i]
                slash = strpos( test, "/" )
                if slash GT 0 then begin
                        comment = strmid( test, slash+1, strlen(test)-slash-1 )
                        test = strmid( test, 0, slash )
                end else comment = ''

; Find the first word in TEST.  Is it a logical value ('T' or 'F')

                test2 = test
                value = gettok(test2,' ')
               if ( value EQ 'T' ) then value = 1b else $
               if ( value EQ 'F' ) then value = 0b else begin

;  Test to see if a complex number.  It's  a complex number if the value and
;  the next word, if any, are both valid values.

                if strlen(test2) EQ 0 then goto, NOT_COMPLEX
                value2 = gettok( test2, ' ') 
                if value2 EQ '' then goto, NOT_COMPLEX
                On_ioerror, NOT_COMPLEX
                value2 = float(value2)
                value = complex(value,value2)
                goto, GOT_VALUE

;  Not a complex number.  Decide if it is a floating point, double precision,
;  or integer number.

NOT_COMPLEX:
                On_IOerror, GOT_VALUE
                  if (strpos(value,'.') GE 0) or (strpos(value,'E') GT 0) $
                  or (strpos(value,'D') GE 0) then begin  ;Floating or double?
                      if ( strpos(value,'D') GT 0 ) or $  ;Double?
                         ( strlen(value) GE 8 ) then value = double(value) $
                                                else value = float(value)
                       endif else begin                   ;Long integer
                            lmax = 2.0d^31 - 1.0d
                            lmin = -2.0d^31      ;Typo fixed Feb 2010
                            value = double(value)
                            if (value GE lmin) && (value LE lmax) then $
                                value = long(value)
                       endelse

GOT_VALUE:
                On_IOerror, NULL
                endelse
             endelse; if c eq apost

;  Add to vector if required

         if vector then begin
               if ( i EQ 0 ) then begin
                     maxnum = max(number)
                     dtype = size(value,/type)
                     result = make_array( maxnum, TYPE = dtype )
                     comments = strarr( maxnum )
               endif 
               if size(value,/type) GT dtype then begin   ;Do we need to recast?
                    result = result + 0*value
                    dtype = size(value,/type)
               endif
               result[ number[i]-1 ] =  value
               comments[ number[i]-1 ] = comment
          endif else $
                comments = comment
  endfor

  if vector then begin
         !ERR = matches     
         return, result
  endif else !ERR = 0

endif  else  begin    
     if abort_return then message,'Keyword '+nam+' not found in '+abort
     !ERR = -1
endelse     

return, value       

END                 
;+
; NAME: 
;       FXADDPAR
; Purpose     : 
;       Add or modify a parameter in a FITS header array.
; Explanation : 
;       This version of FXADDPAR will write string values longer than 68 
;       characters using the FITS continuation convention described at 
;       http://heasarc.gsfc.nasa.gov/docs/heasarc/ofwg/docs/ofwg_recomm/r13.html
; Use         : 
;       FXADDPAR, HEADER, NAME, VALUE, COMMENT
; Inputs      : 
;       HEADER  = String array containing FITS header.  The maximum string
;                 length must be equal to 80.  If not defined, then FXADDPAR
;                 will create an empty FITS header array.
;
;       NAME    = Name of parameter.  If NAME is already in the header the
;                 value and possibly comment fields are modified. Otherwise a
;                 new record is added to the header.  If NAME is equal to
;                 either "COMMENT" or "HISTORY" then the value will be added to
;                 the record without replacement.  In this case the comment
;                 parameter is ignored.
;
;       VALUE   = Value for parameter.  The value expression must be of the
;                 correct type, e.g. integer, floating or string.
;                 String values of 'T' or 'F' are considered logical
;                 values unless the /NOLOGICAL keyword is set.  If the value is
;                 a string and is "long" (more than 69 characters), then it 
;                 may be continued over more than one line using the OGIP 
;                 CONTINUE standard.
;
; Opt. Inputs : 
;       COMMENT = String field.  The '/' is added by this routine.  Added
;                 starting in position 31.  If not supplied, or set equal to ''
;                 (the null string), then any previous comment field in the
;                 header for that keyword is retained (when found).
; Outputs     : 
;       HEADER  = Updated header array.
; Opt. Outputs: 
;       None.
; Keywords    : 
;       BEFORE  = Keyword string name.  The parameter will be placed before the
;                 location of this keyword.  For example, if BEFORE='HISTORY'
;                 then the parameter will be placed before the first history
;                 location.  This applies only when adding a new keyword;
;                 keywords already in the header are kept in the same position.
;
;       AFTER   = Same as BEFORE, but the parameter will be placed after the
;                 location of this keyword.  This keyword takes precedence over
;                 BEFORE.
;
;       FORMAT  = Specifies FORTRAN-like format for parameter, e.g. "F7.3".  A
;                 scalar string should be used.  For complex numbers the format
;                 should be defined so that it can be applied separately to the
;                 real and imaginary parts.      If not supplied, then the IDL
;                 default formatting is used, except that double precision is
;                 given a format of G19.12.
;
;       /NOCONTINUE = By default, FXADDPAR will break strings longer than 68 
;                characters into multiple lines using the continuation
;                convention.    If this keyword is set, then the line will
;                instead be truncated to 68 characters.    This was the default
;                behaviour of FXADDPAR prior to December 1999.  
;
;      /NOLOGICAL = If set, then the values 'T' and 'F' are not interpreted as
;                logical values, and are simply added without interpretation.
;
;	ERRMSG	 = If defined and passed, then any error messages will be
;		   returned to the user in this parameter rather than
;		   depending on the MESSAGE routine in IDL, e.g.
;
;			ERRMSG = ''
;			FXADDPAR, ERRMSG=ERRMSG, ...
;			IF ERRMSG NE '' THEN ...
;
; Calls       : 
;       DETABIFY(), FXPAR(), FXPARPOS()
; Common      : 
;       None.
; Restrictions: 
;       Warning -- Parameters and names are not checked against valid FITS
;       parameter names, values and types.
;
;       The required FITS keywords SIMPLE (or XTENSION), BITPIX, NAXIS, NAXIS1,
;       NAXIS2, etc., must be entered in order.  The actual values of these
;       keywords are not checked for legality and consistency, however.
;
; Side effects: 
;       All HISTORY records are inserted in order at the end of the header.
;
;       All COMMENT records are also inserted in order at the end of the
;       header, but before the HISTORY records.  The BEFORE and AFTER keywords
;       can override this.
;
;       All records with no keyword (blank) are inserted in order at the end of
;       the header, but before the COMMENT and HISTORY records.  The BEFORE and
;       AFTER keywords can override this.
;
;       All other records are inserted before any of the HISTORY, COMMENT, or
;       "blank" records.  The BEFORE and AFTER keywords can override this.
;
;       String values longer than 68 characters will be split into multiple
;       lines using the OGIP CONTINUE convention, unless the /NOCONTINUE keyword
;       is set.    For a description of the CONTINUE convention see    
;       http://fits.gsfc.nasa.gov/registry/continue_keyword.html
; Category    : 
;       Data Handling, I/O, FITS, Generic.
; Prev. Hist. : 
;       William Thompson, Jan 1992, from SXADDPAR by D. Lindler and J. Isensee.
;       Differences include:
;
;               * LOCATION parameter replaced with keywords BEFORE and AFTER.
;               * Support for COMMENT and "blank" FITS keywords.
;               * Better support for standard FITS formatting of string and
;                 complex values.
;               * Built-in knowledge of the proper position of required
;                 keywords in FITS (although not necessarily SDAS/Geis) primary
;                 headers, and in TABLE and BINTABLE extension headers.
;
;       William Thompson, May 1992, fixed bug when extending length of header,
;       and new record is COMMENT, HISTORY, or blank.
; Written     : 
;       William Thompson, GSFC, January 1992.
; Modified    : 
;       Version 1, William Thompson, GSFC, 12 April 1993.
;               Incorporated into CDS library.
;       Version 2, William Thompson, GSFC, 5 September 1997
;               Fixed bug replacing strings that contain "/" character--it
;               interpreted the following characters as a comment.
;       Version 3, Craig Markwardt, GSFC,  December 1997
;               Allow long values to extend over multiple lines
;	Version 4, D. Lindler, March 2000, modified to use capital E instead
;		of a lower case e for exponential format.
;       Version 4.1 W. Landsman April 2000, make user-supplied format uppercase
;       Version 4.2 W. Landsman July 2002, positioning of EXTEND keyword
;       Version 5, 23-April-2007, William Thompson, GSFC
;       Version 6, 02-Aug-2007, WTT, bug fix for OGIP long lines
;       Version 6.1, 10-Feb-2009, W. Landsman, increase default format precision
;       Version 6.2  30-Sep-2009, W. Landsman, added /NOLOGICAL keyword
; Version     : 
;       Version 6.2, 30-Sep-2009
;-
;

; This is a utility routine, which splits a parameter into several
; continuation bits.
PRO FXADDPAR_CONTPAR, VALUE, CONTINUED
  
  APOST = "'"
  BLANK = STRING(REPLICATE(32B,80)) ;BLANK line

  ;; The value may not need to be CONTINUEd.  If it does, then split
  ;; out the first value now.  The first value does not have a
  ;; CONTINUE keyword, because it will be grafted onto the proper
  ;; keyword in the calling routine.

  IF (STRLEN(VALUE) GT 68) THEN BEGIN
      CONTINUED = [ STRMID(VALUE, 0, 67)+'&' ]
      VALUE = STRMID(VALUE, 67, STRLEN(VALUE)-67)
  ENDIF ELSE BEGIN
      CONTINUED = [ VALUE ]
      RETURN
  ENDELSE

  ;; Split out the remaining values.
  WHILE( STRLEN(VALUE) GT 0 ) DO BEGIN
      H = BLANK

      ;; Add CONTINUE keyword
      STRPUT, H, 'CONTINUE  '+APOST
      ;; Add the next split
      IF(STRLEN(VALUE) GT 68) THEN BEGIN
          STRPUT, H, STRMID(VALUE, 0, 67)+'&'+APOST, 11
          VALUE = STRMID(VALUE, 67, STRLEN(VALUE)-67)
      ENDIF ELSE BEGIN
          STRPUT, H, VALUE+APOST, 11
          VALUE = ''
      ENDELSE

      CONTINUED = [ CONTINUED, H ]
  ENDWHILE

  RETURN
END

; Utility routine to add a warning to the file.  The calling routine
; must ensure that the header is in a consistent state before calling
; FXADDPAR_CONTWARN because the header will be subsequently modified
; by calls to FXADDPAR.
PRO FXADDPAR_CONTWARN, HEADER, NAME

;  By OGIP convention, the keyword LONGSTRN is added to the header as
;  well.  It should appear before the first occurrence of a long
;  string encoded with the CONTINUE convention.

  CONTKEY = FXPAR(HEADER, 'LONGSTRN', COUNT = N_LONGSTRN)

;  Calling FXADDPAR here is okay since the state of the header is
;  clean now.
  IF N_LONGSTRN GT 0 THEN $
    RETURN

  FXADDPAR, HEADER, 'LONGSTRN', 'OGIP 1.0', $
    ' The OGIP long string convention may be used.', $
    BEFORE=NAME

  FXADDPAR, HEADER, 'COMMENT', $
    ' This FITS file may contain long string keyword values that are', $
    BEFORE=NAME

  FXADDPAR, HEADER, 'COMMENT', $
    " continued over multiple keywords.  This convention uses the  '&'", $
    BEFORE=NAME

  FXADDPAR, HEADER, 'COMMENT', $
    ' character at the end of a string which is then continued', $
    BEFORE=NAME

  FXADDPAR, HEADER, 'COMMENT', $
    " on subsequent keywords whose name = 'CONTINUE'.", $
    BEFORE=NAME

  RETURN
END


PRO FXADDPAR, HEADER, NAME, VALUE, COMMENT, BEFORE=BEFORE,      $
              AFTER=AFTER, FORMAT=FORMAT, NOCONTINUE = NOCONTINUE, $
              ERRMSG=ERRMSG, NOLOGICAL=NOLOGICAL

        ON_ERROR,2                              ;Return to caller
;
;  Check the number of parameters.
;
        IF N_PARAMS() LT 3 THEN BEGIN
            MESSAGE = 'Syntax:  FXADDPAR, HEADER, NAME, VALUE [, COMMENT ]'
            GOTO, HANDLE_ERROR
        ENDIF
;
; Define a blank line and the END line
;
        ENDLINE = 'END' + STRING(REPLICATE(32B,77))     ;END line
        BLANK = STRING(REPLICATE(32B,80))               ;BLANK line
;
;  If no comment was passed, then use a null string.
;
        IF N_PARAMS() LT 4 THEN COMMENT = ''
;
;  Check the HEADER array.
;
        N = N_ELEMENTS(HEADER)          ;# of lines in FITS header
        IF N EQ 0 THEN BEGIN            ;header defined?
                HEADER=STRARR(36)       ;no, make it.
                HEADER[0]=ENDLINE
                N=36
        ENDIF ELSE BEGIN
                S = SIZE(HEADER)        ;check for string type
                IF (S[0] NE 1) OR (S[2] NE 7) THEN BEGIN
                    MESSAGE = 'FITS Header (first parameter) must be a ' + $
                      'string array'
                    GOTO, HANDLE_ERROR
                ENDIF
        ENDELSE
;
;  Make sure NAME is 8 characters long
;
        NN = STRING(REPLICATE(32B,8))   ;8 char name
        STRPUT,NN,STRUPCASE(NAME)       ;Insert name
;
;  Check VALUE.
;
        S = SIZE(VALUE)         ;get type of value parameter
        STYPE = S[S[0]+1]
        IF S[0] NE 0 THEN BEGIN
                MESSAGE = 'Keyword Value (third parameter) must be scalar'
                GOTO, HANDLE_ERROR
        END ELSE IF STYPE EQ 0 THEN BEGIN
                MESSAGE = 'Keyword Value (third parameter) is not defined'
                GOTO, HANDLE_ERROR
        END ELSE IF STYPE EQ 8 THEN BEGIN
                MESSAGE = 'Keyword Value (third parameter) cannot be structure'
                GOTO, HANDLE_ERROR
        ENDIF
;
;  Extract first 8 characters of each line of header, and locate END line
;
        KEYWRD = STRMID(HEADER,0,8)                     ;Header keywords
        IEND = WHERE(KEYWRD EQ 'END     ',NFOUND)
;
;  If no END, then add it.  Either put it after the last non-null string, or
;  append it to the end.
;
        IF NFOUND EQ 0 THEN BEGIN
                II = WHERE(STRTRIM(HEADER) NE '',NFOUND)
                II = MAX(II) + 1
                IF (NFOUND EQ 0) OR (II EQ N_ELEMENTS(HEADER)) THEN     $
                        HEADER = [HEADER,ENDLINE] ELSE HEADER[II] = ENDLINE
                KEYWRD = STRMID(HEADER,0,8)
                IEND = WHERE(KEYWRD EQ 'END     ',NFOUND)
        ENDIF
;
        IEND = IEND[0] > 0                      ;Make scalar
;
;  History, comment and "blank" records are treated differently from the
;  others.  They are simply added to the header array whether there are any
;  already there or not.
;
        IF (NN EQ 'COMMENT ') OR (NN EQ 'HISTORY ') OR          $
                        (NN EQ '        ') THEN BEGIN
;
;  If the header array needs to grow, then expand it in increments of 36 lines.
;
                IF IEND GE (N-1) THEN BEGIN
                        HEADER = [HEADER,REPLICATE(BLANK,36)]
                        N = N_ELEMENTS(HEADER)
                ENDIF
;
;  Format the record.
;
                NEWLINE = BLANK
                STRPUT,NEWLINE,NN+STRING(VALUE),0
;
;  If a history record, then append to the record just before the end.
;
                IF NN EQ 'HISTORY ' THEN BEGIN
                        HEADER[IEND] = NEWLINE          ;add history rec.
                        HEADER[IEND+1]=ENDLINE          ;move end up
;
;  The comment record is placed immediately after the last previous comment
;  record, or immediately before the first history record, unless overridden by
;  either the BEFORE or AFTER keywords.
;
                END ELSE IF NN EQ 'COMMENT ' THEN BEGIN
                        I = FXPARPOS(KEYWRD,IEND,AFTER=AFTER,BEFORE=BEFORE)
                        IF I EQ IEND THEN I =   $
                            FXPARPOS(KEYWRD,IEND,AFTER='COMMENT',$
                                     BEFORE='HISTORY')
                        HEADER[I+1] = HEADER[I:N-2]     ;move rest up
                        HEADER[I] = NEWLINE             ;insert comment
;
;  The "blank" record is placed immediately after the last previous "blank"
;  record, or immediately before the first comment or history record, unless
;  overridden by either the BEFORE or AFTER keywords.
;
                END ELSE BEGIN
                        I = FXPARPOS(KEYWRD,IEND,AFTER=AFTER,BEFORE=BEFORE)
                        IF I EQ IEND THEN I =   $
                            FXPARPOS(KEYWRD,IEND,AFTER='',BEFORE='COMMENT')<$
                            FXPARPOS(KEYWRD,IEND,AFTER='',BEFORE='HISTORY')
                        HEADER[I+1] = HEADER[I:N-2]     ;move rest up
                        HEADER[I] = NEWLINE             ;insert "blank"
                ENDELSE
                RETURN
        ENDIF                           ;history/comment/blank
;
;  Find location to insert keyword.  If the keyword is already in the header,
;  then simply replace it.  If no new comment is passed, then retain the old
;  one.
;
        IPOS  = WHERE(KEYWRD EQ NN,NFOUND)
        IF NFOUND GT 0 THEN BEGIN
                I = IPOS[0]
                IF COMMENT EQ '' THEN BEGIN
                        SLASH = STRPOS(HEADER[I],'/')
                        QUOTE = STRPOS(HEADER[I],"'")
                        IF (QUOTE GT 0) AND (QUOTE LT SLASH) THEN BEGIN
                                QUOTE = STRPOS(HEADER[I],"'",QUOTE+1)
                                IF QUOTE LT 0 THEN SLASH = -1 ELSE      $
                                        SLASH = STRPOS(HEADER[I],'/',QUOTE+1)
                        ENDIF
                        IF SLASH NE -1 THEN     $
                                COMMENT = STRMID(HEADER[I],SLASH+1,80) ELSE $
                                COMMENT = STRING(REPLICATE(32B,80))
                ENDIF
                GOTO, REPLACE
        ENDIF
;
;  Start of section dealing with the positioning of required FITS keywords.  If
;  the keyword is SIMPLE, then it must be at the beginning.
;
        IF NN EQ 'SIMPLE  ' THEN BEGIN
                I = 0
                GOTO, INSERT
        ENDIF
;
;  In conforming extensions, if the keyword is XTENSION, then it must be at the
;  beginning. 
;
        IF NN EQ 'XTENSION' THEN BEGIN
                I = 0
                GOTO, INSERT
        ENDIF
;
;  If the keyword is BITPIX, then it must follow the either SIMPLE or XTENSION
;  keyword.
;
        IF NN EQ 'BITPIX  ' THEN BEGIN
                IF (KEYWRD[0] NE 'SIMPLE  ') AND                $
                        (KEYWRD[0] NE 'XTENSION') THEN BEGIN
                    MESSAGE = 'Header must start with either SIMPLE or XTENSION'
                    GOTO, HANDLE_ERROR
                ENDIF
                I = 1
                GOTO, INSERT
        ENDIF
;
;  If the keyword is NAXIS, then it must follow the BITPIX keyword.
;
        IF NN EQ 'NAXIS   ' THEN BEGIN
                IF KEYWRD[1] NE 'BITPIX  ' THEN BEGIN
                    MESSAGE = 'Required BITPIX keyword not found'
                    GOTO, HANDLE_ERROR
                ENDIF
                I = 2
                GOTO, INSERT
        ENDIF
;
;  If the keyword is NAXIS1, then it must follow the NAXIS keyword.
;
        IF NN EQ 'NAXIS1  ' THEN BEGIN
                IF KEYWRD[2] NE 'NAXIS   ' THEN BEGIN
                    MESSAGE = 'Required NAXIS keyword not found'
                    GOTO, HANDLE_ERROR
                ENDIF
                I = 3
                GOTO, INSERT
        ENDIF
;
;  If the keyword is NAXIS<n>, then it must follow the NAXIS<n-1> keyword.
;
        IF STRMID(NN,0,5) EQ 'NAXIS' THEN BEGIN
                NUM_AXIS = FIX(STRMID(NN,5,3))
                PREV = STRING(REPLICATE(32B,8))         ;Format NAXIS<n-1>
                STRPUT,PREV,'NAXIS',0                   ;Insert NAXIS
                STRPUT,PREV,STRTRIM(NUM_AXIS-1,2),5     ;Insert <n-1>
                IF KEYWRD[NUM_AXIS+1] NE PREV THEN BEGIN
                    MESSAGE = 'Required '+PREV+' keyword not found'
                    GOTO, HANDLE_ERROR
                ENDIF
                I = NUM_AXIS + 2
                GOTO, INSERT
        ENDIF

;
;  If the keyword is EXTEND, then it must follow the last NAXIS* keyword.
;

        IF NN EQ 'EXTEND  ' THEN BEGIN
                IF KEYWRD[2] NE 'NAXIS   ' THEN BEGIN
                    MESSAGE = 'Required NAXIS keyword not found'
                    GOTO, HANDLE_ERROR
                ENDIF
                FOR I = 3, N-2 DO $   
                    IF STRMID(KEYWRD[I],0,5) NE 'NAXIS' THEN GOTO, INSERT 
                   
         ENDIF
    
;
;  If the first keyword is XTENSION, and has the value of either 'TABLE' or
;  'BINTABLE', then there are some additional required keywords.
;
        IF KEYWRD[0] EQ 'XTENSION' THEN BEGIN
                XTEN = FXPAR(HEADER,'XTENSION')
                IF (XTEN EQ 'TABLE   ') OR (XTEN EQ 'BINTABLE') THEN BEGIN
;
;  If the keyword is PCOUNT, then it must follow the NAXIS2 keyword.
;
                        IF NN EQ 'PCOUNT  ' THEN BEGIN
                                IF KEYWRD[4] NE 'NAXIS2  ' THEN BEGIN
                                    MESSAGE = 'Required NAXIS2 keyword not found'
                                    GOTO, HANDLE_ERROR
                                ENDIF
                                I = 5
                                GOTO, INSERT
                        ENDIF
;
;  If the keyword is GCOUNT, then it must follow the PCOUNT keyword.
;
                        IF NN EQ 'GCOUNT  ' THEN BEGIN
                                IF KEYWRD[5] NE 'PCOUNT  ' THEN BEGIN
                                    MESSAGE = 'Required PCOUNT keyword not found'
                                    GOTO, HANDLE_ERROR
                                ENDIF
                                I = 6
                                GOTO, INSERT
                        ENDIF
;
;  If the keyword is TFIELDS, then it must follow the GCOUNT keyword.
;
                        IF NN EQ 'TFIELDS ' THEN BEGIN
                                IF KEYWRD[6] NE 'GCOUNT  ' THEN BEGIN
                                    MESSAGE = 'Required GCOUNT keyword not found'
                                    GOTO, HANDLE_ERROR
                                ENDIF
                                I = 7
                                GOTO, INSERT
                        ENDIF
                ENDIF
        ENDIF
;
;  At this point the location has not been determined, so a new line is added
;  at the end of the FITS header, but before any blank, COMMENT, or HISTORY
;  keywords, unless overridden by the BEFORE or AFTER keywords.
;
        I = FXPARPOS(KEYWRD,IEND,AFTER=AFTER,BEFORE=BEFORE)
        IF I EQ IEND THEN I =                                     $
            FXPARPOS(KEYWRD,IEND,AFTER=AFTER,BEFORE='')         < $
            FXPARPOS(KEYWRD,IEND,AFTER=AFTER,BEFORE='COMMENT')  < $
            FXPARPOS(KEYWRD,IEND,AFTER=AFTER,BEFORE='HISTORY')
;
;  A new line needs to be added.  First check to see if the length of the
;  header array needs to be extended.  Then insert a blank record at the proper
;  place.
;
INSERT:
        IF IEND EQ (N-1) THEN BEGIN
                HEADER = [HEADER,REPLICATE(BLANK,36)]
                N = N_ELEMENTS(HEADER)
        ENDIF
        HEADER[I+1] = HEADER[I:N-2]
        HEADER[I] = BLANK
        IEND = IEND + 1        ; CM 24 Sep 1997
;
;  Now put value into keyword at line I.
;
REPLACE: 
        H=BLANK                 ;80 blanks
        STRPUT,H,NN+'= '        ;insert name and =.
        APOST = "'"             ;quote (apostrophe) character
        TYPE = SIZE(VALUE)      ;get type of value parameter
;
;  Store the value depending on the data type.  If a character string, first
;  check to see if it is one of the logical values "T" (true) or "F" (false).
;

        IF TYPE[1] EQ 7 THEN BEGIN              ;which type?
                UPVAL = STRUPCASE(VALUE)        ;force upper case.
                IF ~KEYWORD_SET(NOLOGICAL)  $ 
		   &&  ((UPVAL EQ 'T') OR (UPVAL EQ 'F')) THEN BEGIN
                        STRPUT,H,UPVAL,29       ;insert logical value.
;
;  Otherwise, remove any tabs, and check for any apostrophes in the string.
;
                END ELSE BEGIN
                        VAL = DETABIFY(VALUE)
                        NEXT_CHAR = 0
                        REPEAT BEGIN
                                AP = STRPOS(VAL,"'",NEXT_CHAR)
                                IF AP GE 66 THEN BEGIN
                                        VAL = STRMID(VAL,0,66)
                                END ELSE IF AP GE 0 THEN BEGIN
                                        VAL = STRMID(VAL,0,AP+1) + APOST + $
                                          STRMID(VAL,AP+1,80)
                                        NEXT_CHAR = AP + 2
                                ENDIF
                        ENDREP UNTIL AP LT 0

;
;  If a long string, then add the comment as soon as possible.
;
; CM 24 Sep 1997
;  Separate parameter if it needs to be CONTINUEd.
;
                        IF NOT KEYWORD_SET(NOCONTINUE) THEN $
                             FXADDPAR_CONTPAR, VAL, CVAL  ELSE $
                             CVAL = STRMID(VAL,0,68)
                        K = I + 1
                        ;; See how many CONTINUE lines there already are
                        WHILE K LT IEND DO BEGIN
                            IF STRMID(HEADER[K],0,8) NE 'CONTINUE' THEN $
                              GOTO, DONE_CHECK_CONT
                            K = K + 1
                        ENDWHILE
                        
                        DONE_CHECK_CONT:
                        NOLDCONT = K - I - 1
                        NNEWCONT = N_ELEMENTS(CVAL) - 1

                        ;; Insert new lines if needed
                        IF NNEWCONT GT NOLDCONT THEN BEGIN
                            INS = NNEWCONT - NOLDCONT
                            WHILE IEND+INS GE N DO BEGIN
                                HEADER = [HEADER, REPLICATE(BLANK,36)]
                                N = N_ELEMENTS(HEADER)
                            ENDWHILE
                        ENDIF 

                        ;; Shift the old lines properly
                        IF NNEWCONT NE NOLDCONT THEN $
                          HEADER[I+NNEWCONT+1] = HEADER[I+NOLDCONT+1:IEND]
                        IEND = IEND + NNEWCONT - NOLDCONT

                        ;; Blank out any lines at the end if needed
                        IF NNEWCONT LT NOLDCONT THEN BEGIN
                            DEL = NOLDCONT - NNEWCONT
                            HEADER[IEND+1:IEND+DEL] = REPLICATE('', DEL)
                        ENDIF

                        IF STRLEN(CVAL[0]) GT 18 THEN BEGIN
                            STRPUT,H,APOST+STRMID(CVAL[0],0,68)+APOST+ $
                              ' /'+COMMENT,10
                            HEADER[I]=H
                                
;  There might be a continuation of this string.  CVAL would contain
;  more than one element if that is so.
                            
                            ;; Add new continuation lines
                            IF N_ELEMENTS(CVAL) GT 1 THEN BEGIN
                              HEADER[I+1] = CVAL[1:*]
                            
                            ;; Header state is now clean, so add
                            ;; warning to header

                               FXADDPAR_CONTWARN, HEADER, NAME
                            ENDIF
                            DONE_CONT:
                            RETURN
;
;  If a short string, then pad out to at least eight characters.
;
                        END ELSE BEGIN
                                STRPUT,H,APOST+CVAL[0],10
                                STRPUT,H,APOST,11+(STRLEN(CVAL[0])>8)
                        ENDELSE

                    ENDELSE
;
;  If complex, then format the real and imaginary parts, and add the comment
;  beginning in column 51.
;
        END ELSE IF TYPE[1] EQ 6 THEN BEGIN
                IF N_ELEMENTS(FORMAT) EQ 1 THEN BEGIN   ;use format keyword
                        VR = STRING(FLOAT(VALUE),    '('+STRUPCASE(FORMAT)+')')
                        VI = STRING(IMAGINARY(VALUE),'('+STRUPCASE(FORMAT)+')')
                 END ELSE BEGIN
                        VR = STRTRIM(FLOAT(VALUE),2)
                        VI = STRTRIM(IMAGINARY(VALUE),2)
                ENDELSE
                SR = STRLEN(VR)  &  STRPUT,H,VR,(30-SR)>10
                SI = STRLEN(VI)  &  STRPUT,H,VI,(50-SI)>30
                STRPUT,H,' /'+COMMENT,50
                HEADER[I] = H
                RETURN
;
;  If not complex or a string, then format according to either the FORMAT
;  keyword, or the default for that datatype.
;
        END ELSE BEGIN
                IF (N_ELEMENTS(FORMAT) EQ 1) THEN $ ;use format keyword
                        V = STRING(VALUE,'('+STRUPCASE(FORMAT)+')' ) ELSE BEGIN
			IF TYPE[1] EQ 5 THEN $
			V = STRING(VALUE,FORMAT='(G19.12)') ELSE $
                        V = STRTRIM(strupcase(VALUE),2)    ;default format
			ENDELSE
                S = STRLEN(V)                 ;right justify
                STRPUT,H,V,(30-S)>10          ;insert
        ENDELSE
;
;  Add the comment, and store the completed line in the header.
;
        STRPUT,H,' /',30        ;add ' /'
        STRPUT,H,COMMENT,32     ;add comment
        HEADER[I]=H             ;save line
;
        ERRMSG = ''
        RETURN
;
;  Error handling point.
;
HANDLE_ERROR:
	IF ARG_PRESENT(ERRMSG) THEN ERRMSG = 'FXADDPAR: ' + MESSAGE	$
		ELSE MESSAGE, MESSAGE
        RETURN
        END

        FUNCTION FXPAR, HDR, NAME, ABORT, COUNT=MATCHES, COMMENT=COMMENTS, $
                        START=START, PRECHECK=PRECHECK, POSTCHECK=POSTCHECK, $
                                          NOCONTINUE = NOCONTINUE, $
                        DATATYPE=DATATYPE
;+
; NAME: 
;        FXPAR()
; PURPOSE: 
;       Obtain the value of a parameter in a FITS header.
; EXPLANATION: 
;       The first 8 chacters of each element of HDR are searched for a match to
;       NAME.  If the keyword is one of those allowed to take multiple values
;       ("HISTORY", "COMMENT", or "        " (blank)), then the value is taken
;       as the next 72 characters.  Otherwise, it is assumed that the next
;       character is "=", and the value (and optional comment) is then parsed
;       from the last 71 characters.  An error occurs if there is no parameter
;       with the given name.
;      
;       If the value is too long for one line, it may be continued on to the
;       the next input card, using the CONTINUE Long String Keyword convention.
;       For more info, http://fits.gsfc.nasa.gov/registry/continue_keyword.html
;       
;
;       Complex numbers are recognized as two numbers separated by one or more
;       space characters.
;
;       If a numeric value has no decimal point (or E or D) it is returned as
;       type LONG.  If it contains more than 8 numerals, or contains the
;       character 'D', then it is returned as type DOUBLE.  Otherwise it is
;       returned as type FLOAT.    If an integer is too large to be stored as
;       type LONG, then it is returned as DOUBLE.
;
; CALLING SEQUENCE: 
;       Result = FXPAR( HDR, NAME  [, ABORT, COUNT=, COMMENT=, /NOCONTINUE ] )
;
;       Result = FXPAR(HEADER,'DATE')           ;Finds the value of DATE
;       Result = FXPAR(HEADER,'NAXIS*')         ;Returns array dimensions as
;                                               ;vector
; REQUIRED INPUTS: 
;       HDR     = FITS header string array (e.g. as returned by FXREAD).  Each
;                 element should have a length of 80 characters
;       NAME    = String name of the parameter to return.  If NAME is of the
;                 form 'keyword*' then an array is returned containing values
;                 of keywordN where N is an integer.  The value of keywordN
;                 will be placed in RESULT(N-1).  The data type of RESULT will
;                 be the type of the first valid match of keywordN
;                 found, unless DATATYPE is given.
; OPTIONAL INPUT: 
;       ABORT   = String specifying that FXPAR should do a RETALL if a
;                 parameter is not found.  ABORT should contain a string to be
;                 printed if the keyword parameter is not found.  If not
;                 supplied, FXPAR will return with a negative !err if a keyword
;                 is not found.
;       DATATYPE = A scalar value, indicating the type of vector
;                  data.  All keywords will be cast to this type.
;                  Default: based on first keyword.
;                  Example: DATATYPE=0.0D (cast data to double precision)
;       START   = A best-guess starting position of the sought-after
;                 keyword in the header.  If specified, then FXPAR
;                 first searches for scalar keywords in the header in
;                 the index range bounded by START-PRECHECK and
;                 START+POSTCHECK.  This can speed up keyword searches
;                 in large headers.  If the keyword is not found, then
;                 FXPAR searches the entire header.  
;
;                 If not specified then the entire header is searched.
;                 Searches of the form 'keyword*' also search the
;                 entire header and ignore START.
;
;                 Upon return START is changed to be the position of
;                 the newly found keyword.  Thus the best way to
;                 search for a series of keywords is to search for
;                 them in the order they appear in the header like
;                 this:
;
;                       START = 0L
;                       P1 = FXPAR('P1', START=START)
;                       P2 = FXPAR('P2', START=START)
;       PRECHECK = If START is specified, then PRECHECK is the number
;                  of keywords preceding START to be searched.
;                  Default: 5
;       POSTCHECK = If START is specified, then POSTCHECK is the number
;                   of keywords after START to be searched.
;                   Default: 20
; OUTPUT: 
;       The returned value of the function is the value(s) associated with the
;       requested keyword in the header array.
;
;       If the parameter is complex, double precision, floating point, long or
;       string, then the result is of that type.  Apostrophes are stripped from
;       strings.  If the parameter is logical, 1 is returned for T, and 0 is
;       returned for F.
;
;       If NAME was of form 'keyword*' then a vector of values are returned.
;
; OPTIONAL INPUT KEYWORDS: 
;       /NOCONTINUE = If set, then continuation lines will not be read, even
;                 if present in the header
; OPTIONAL OUTPUT KEYWORD:
;       COUNT   = Optional keyword to return a value equal to the number of
;                 parameters found by FXPAR.
;       COMMENTS= Array of comments associated with the returned values.
;
; PROCEDURE CALLS: 
;       GETTOK(), VALID_NUM
; SIDE EFFECTS: 
;
;       The system variable !err is set to -1 if parameter not found, 0 for a
;       scalar value returned.  If a vector is returned it is set to the number
;       of keyword matches found.
;
;       If a keyword occurs more than once in a header, a warning is given,
;       and the first occurence is used.  However, if the keyword is "HISTORY",
;       "COMMENT", or "        " (blank), then multiple values are returned.
;
; NOTES:
;	The functions SXPAR() and FXPAR() are nearly identical, although
;	FXPAR() has slightly more sophisticated parsing.   There is no
;	particular reason for having two nearly identical procedures, but
;	both are too widely used to drop either one.
;
; REVISION HISTORY: 
;       Version 1, William Thompson, GSFC, 12 April 1993.
;               Adapted from SXPAR
;       Version 2, William Thompson, GSFC, 14 October 1994
;               Modified to use VALID_NUM instead of STRNUMBER.  Inserted
;               additional call to VALID_NUM to trap cases where character
;               strings did not contain quotation marks.
;       Version 3, William Thompson, GSFC, 22 December 1994
;               Fixed bug with blank keywords, following suggestion by Wayne
;               Landsman.
;       Version 4, Mons Morrison, LMSAL, 9-Jan-98
;               Made non-trailing ' for string tag just be a warning (not
;               a fatal error).  It was needed because "sxaddpar" had an
;               error which did not write tags properly for long strings
;               (over 68 characters)
;       Version 5, Wayne Landsman GSFC, 29 May 1998
;               Fixed potential problem with overflow of LONG values
;       Version 6, Craig Markwardt, GSFC, 28 Jan 1998, 
;               Added CONTINUE parsing         
;       Version 7, Craig Markwardt, GSFC, 18 Nov 1999,
;               Added START, PRE/POSTCHECK keywords for better
;               performance
;       Version 8, Craig Markwardt, GSFC, 08 Oct 2003,
;               Added DATATYPE keyword to cast vector keywords type
;       Version 9, Paul Hick, 22 Oct 2003, Corrected bug (NHEADER-1)
;-
;------------------------------------------------------------------------------
;
;  Check the number of parameters.
;
        IF N_PARAMS() LT 2 THEN BEGIN
            PRINT,'Syntax:  result =  FXPAR( HDR, NAME  [, ABORT ])'
            RETURN, -1
        ENDIF
;
;  Determine the abort condition.
;
        VALUE = 0
        IF N_PARAMS() LE 2 THEN BEGIN
            ABORT_RETURN = 0
            ABORT = 'FITS Header'
        END ELSE ABORT_RETURN = 1
        IF ABORT_RETURN THEN ON_ERROR,1 ELSE ON_ERROR,2
;
;  Check for valid header.  Check header for proper attributes.
;
        S = SIZE(HDR)
        IF ( S[0] NE 1 ) OR ( S[2] NE 7 ) THEN $
            MESSAGE,'FITS Header (first parameter) must be a string array'
;
;  Convert the selected keyword NAME to uppercase.
;
        NAM = STRTRIM( STRUPCASE(NAME) )
;
;  Determine if NAME is of form 'keyword*'.  If so, then strip off the '*', and
;  set the VECTOR flag.  One must consider the possibility that NAM is an empty
;  string.
;
        NAMELENGTH1 = (STRLEN(NAM) - 1) > 1
        IF STRPOS( NAM, '*' ) EQ NAMELENGTH1 THEN BEGIN    
            NAM = STRMID( NAM, 0, NAMELENGTH1)  
            VECTOR = 1                          ;Flag for vector output  
            NAME_LENGTH = STRLEN(NAM)           ;Length of name 
            NUM_LENGTH = 8 - NAME_LENGTH        ;Max length of number portion  
            IF NUM_LENGTH LE 0 THEN MESSAGE,    $
                'Keyword length must be 8 characters or less'
;
;  Otherwise, extend NAME with blanks to eight characters.
;
        ENDIF ELSE BEGIN
            WHILE STRLEN(NAM) LT 8 DO NAM = NAM + ' '
            VECTOR = 0
        ENDELSE
;
;  If of the form 'keyword*', then find all instances of 'keyword' followed by
;  a number.  Store the positions of the located keywords in NFOUND, and the
;  value of the number field in NUMBER.
;
        IF N_ELEMENTS(START)     EQ 0 THEN START = -1L
        START = LONG(START[0])
        IF NOT VECTOR AND START GE 0 THEN BEGIN
            IF N_ELEMENTS(PRECHECK)  EQ 0 THEN PRECHECK = 5
            IF N_ELEMENTS(POSTCHECK) EQ 0 THEN POSTCHECK = 20
            NHEADER = N_ELEMENTS(HDR)
            MN = (START - PRECHECK)  > 0
            MX = (START + POSTCHECK) < (NHEADER-1)      ;Corrected bug
            KEYWORD = STRMID(HDR[MN:MX], 0, 8)
        ENDIF ELSE BEGIN
            RESTART:
            START   = -1L
            KEYWORD = STRMID( HDR, 0, 8)
        ENDELSE

        IF VECTOR THEN BEGIN
            NFOUND = WHERE(STRPOS(KEYWORD,NAM) GE 0, MATCHES)
            IF ( MATCHES GT 0 ) THEN BEGIN
                NUMST= STRMID(HDR[NFOUND], NAME_LENGTH, NUM_LENGTH)
                NUMBER = INTARR(MATCHES)-1
                FOR I = 0, MATCHES-1 DO         $
                    IF VALID_NUM( NUMST[I], NUM) THEN NUMBER[I] = NUM
                IGOOD = WHERE(NUMBER GE 0, MATCHES)
                IF MATCHES GT 0 THEN BEGIN
                    NFOUND = NFOUND[IGOOD]
                    NUMBER = NUMBER[IGOOD]
                ENDIF
            ENDIF
;
;  Otherwise, find all the instances of the requested keyword.  If more than
;  one is found, and NAME is not one of the special cases, then print an error
;  message.
;
        ENDIF ELSE BEGIN
            NFOUND = WHERE(KEYWORD EQ NAM, MATCHES)
            IF MATCHES EQ 0 AND START GE 0 THEN GOTO, RESTART
            IF START GE 0 THEN NFOUND = NFOUND + MN
            IF (MATCHES GT 1) AND (NAM NE 'HISTORY ') AND               $
                (NAM NE 'COMMENT ') AND (NAM NE '') THEN        $
                MESSAGE,/INFORMATIONAL, 'WARNING- Keyword ' +   $
                NAM + 'located more than once in ' + ABORT
            IF (MATCHES GT 0) THEN START = NFOUND[MATCHES-1]
        ENDELSE
;
;  Extract the parameter field from the specified header lines.  If one of the
;  special cases, then done.
;
        IF MATCHES GT 0 THEN BEGIN
            LINE = HDR[NFOUND]
            SVALUE = STRTRIM( STRMID(LINE,9,71),2)
            IF (NAM EQ 'HISTORY ') OR (NAM EQ 'COMMENT ') OR    $
                    (NAM EQ '        ') THEN BEGIN
                VALUE = STRTRIM( STRMID(LINE,8,72),2)
                COMMENTS = STRARR(N_ELEMENTS(VALUE))
;
;  Otherwise, test to see if the parameter contains a string, signalled by
;  beginning with a single quote character (') (apostrophe).
;
            END ELSE FOR I = 0,MATCHES-1 DO BEGIN
                IF ( STRMID(SVALUE[I],0,1) EQ "'" ) THEN BEGIN
                    TEST = STRMID( SVALUE[I],1,STRLEN( SVALUE[I] )-1)
                    NEXT_CHAR = 0
                    OFF = 0
                    VALUE = ''
;
;  Find the next apostrophe.
;
NEXT_APOST:
                    ENDAP = STRPOS(TEST, "'", NEXT_CHAR)
                    IF ENDAP LT 0 THEN MESSAGE,         $
                        'WARNING: Value of '+NAME+' invalid in '+ABORT+ " (no trailing ')", /info
                    VALUE = VALUE + STRMID( TEST, NEXT_CHAR, ENDAP-NEXT_CHAR )
;
;  Test to see if the next character is also an apostrophe.  If so, then the
;  string isn't completed yet.  Apostrophes in the text string are signalled as
;  two apostrophes in a row.
;
                    IF STRMID( TEST, ENDAP+1, 1) EQ "'" THEN BEGIN    
                        VALUE = VALUE + "'"
                        NEXT_CHAR = ENDAP+2      
                        GOTO, NEXT_APOST
                    ENDIF
;
;  Extract the comment, if any.
;
                    SLASH = STRPOS(TEST, "/", ENDAP)
                    IF SLASH LT 0 THEN COMMENT = '' ELSE        $
                        COMMENT = STRMID(TEST, SLASH+1, STRLEN(TEST)-SLASH-1)

;
; CM 19 Sep 1997
; This is a string that could be continued on the next line.  Check this
; possibility with the following four criteria: *1) Ends with '&'
; (2) Next line is CONTINUE  (3) LONGSTRN keyword is present (recursive call to
;  FXPAR) 4. /NOCONTINE is not set

    IF NOT KEYWORD_SET(NOCONTINUE) THEN BEGIN
                    OFF = OFF + 1
                    VAL = STRTRIM(VALUE,2)

                    IF (STRLEN(VAL) GT 0) AND $
                      (STRMID(VAL, STRLEN(VAL)-1, 1) EQ '&') AND $
                      (STRMID(HDR[NFOUND[I]+OFF],0,8) EQ 'CONTINUE') THEN BEGIN
                       IF (SIZE(FXPAR(HDR, 'LONGSTRN',/NOCONTINUE)))[1] EQ 7 THEN BEGIN                    
                      VALUE = STRMID(VAL, 0, STRLEN(VAL)-1)
                      TEST = HDR[NFOUND[I]+OFF]
                      TEST = STRMID(TEST, 8, STRLEN(TEST)-8)
                      TEST = STRTRIM(TEST, 2)
                      IF STRMID(TEST, 0, 1) NE "'" THEN MESSAGE, $
                        'ERROR: Invalidly CONTINUEd string in '+ABORT
                      NEXT_CHAR = 1
                      GOTO, NEXT_APOST
                    ENDIF
                   ENDIF
    ENDIF

;
;  If not a string, then separate the parameter field from the comment field.
;
                ENDIF ELSE BEGIN
                    TEST = SVALUE[I]
                    SLASH = STRPOS(TEST, "/")
                    IF SLASH GT 0 THEN BEGIN
                        COMMENT = STRMID(TEST, SLASH+1, STRLEN(TEST)-SLASH-1)
                        TEST = STRMID(TEST, 0, SLASH)
                    END ELSE COMMENT = ''
;
;  Find the first word in TEST.  Is it a logical value ('T' or 'F')?
;
                    TEST2 = TEST
                    VALUE = GETTOK(TEST2,' ')
                    TEST2 = STRTRIM(TEST2,2)
                    IF ( VALUE EQ 'T' ) THEN BEGIN
                        VALUE = 1
                    END ELSE IF ( VALUE EQ 'F' ) THEN BEGIN
                        VALUE = 0
                    END ELSE BEGIN
;
;  Test to see if a complex number.  It's a complex number if the value and the
;  next word, if any, both are valid numbers.
;
                        IF STRLEN(TEST2) EQ 0 THEN GOTO, NOT_COMPLEX
                        VALUE2 = GETTOK(TEST2,' ')
                        IF VALID_NUM(VALUE,VAL1) AND VALID_NUM(VALUE2,VAL2) $
                                THEN BEGIN
                            VALUE = COMPLEX(VAL1,VAL2)
                            GOTO, GOT_VALUE
                        ENDIF
;
;  Not a complex number.  Decide if it is a floating point, double precision,
;  or integer number.  If an error occurs, then a string value is returned.
;  If the integer is not within the range of a valid long value, then it will 
;  be converted to a double.  
;
NOT_COMPLEX:
                        ON_IOERROR, GOT_VALUE
                        VALUE = TEST
                        IF NOT VALID_NUM(VALUE) THEN GOTO, GOT_VALUE
                        IF (STRPOS(VALUE,'.') GE 0) OR (STRPOS(VALUE,'E') $
                                GE 0) OR (STRPOS(VALUE,'D') GE 0) THEN BEGIN
                            IF ( STRPOS(VALUE,'D') GT 0 ) OR $
                                    ( STRLEN(VALUE) GE 8 ) THEN BEGIN
                                VALUE = DOUBLE(VALUE)
                                END ELSE VALUE = FLOAT(VALUE)
                        ENDIF ELSE BEGIN
                            LMAX = 2.0D^31 - 1.0D
                            LMIN = -2.0D^31       ;Typo fixed Feb 2010
                            VALUE = DOUBLE(VALUE)
                            if (VALUE GE LMIN) and (VALUE LE LMAX) THEN $
                                VALUE = LONG(VALUE)
                        ENDELSE
                            
;
GOT_VALUE:
                        ON_IOERROR, NULL
                    ENDELSE
                ENDELSE         ; if string
;
;  Add to vector if required.
;
                IF VECTOR THEN BEGIN
                    MAXNUM = MAX(NUMBER)
                    IF ( I EQ 0 ) THEN BEGIN
                        IF N_ELEMENTS(DATATYPE) EQ 0 THEN BEGIN
                            ;; Data type determined from keyword
                            SZ_VALUE = SIZE(VALUE)
                        ENDIF ELSE BEGIN
                            ;; Data type requested by user
                            SZ_VALUE = SIZE(DATATYPE[0])
                        ENDELSE
                        RESULT = MAKE_ARRAY( MAXNUM, TYPE=SZ_VALUE[1])
                        COMMENTS = STRARR(MAXNUM)
                    ENDIF 
                    RESULT[   NUMBER[I]-1 ] =  VALUE
                    COMMENTS[ NUMBER[I]-1 ] =  COMMENT
                ENDIF ELSE BEGIN
                    COMMENTS = COMMENT
                ENDELSE
            ENDFOR
;
;  Set the value of !ERR for the number of matches for vectors, or simply 0
;  otherwise.
;
            IF VECTOR THEN BEGIN
                !ERR = MATCHES
                RETURN, RESULT
            ENDIF ELSE !ERR = 0
;
;  Error point for keyword not found.
;
        ENDIF ELSE BEGIN
            IF ABORT_RETURN THEN MESSAGE,'Keyword '+NAM+' not found in '+ABORT
            !ERR = -1
        ENDELSE
;
        RETURN, VALUE
        END
	FUNCTION FXPARPOS, KEYWRD, IEND, BEFORE=BEFORE, AFTER=AFTER
;+
; NAME: 
;	FXPARPOS()
; Purpose     : 
;	Finds position to insert record into FITS header.
; Explanation : 
;	Finds the position to insert a record into a FITS header.  Called from
;	FXADDPAR.
; Use         : 
;	Result = FXPARPOS(KEYWRD, IEND  [, BEFORE=BEFORE ]  [, AFTER=AFTER ])
; Inputs      : 
;	KEYWRD	= Array of eight-character keywords in header.
;	IEND	= Position of END keyword.
; Opt. Inputs : 
;	None.
; Outputs     : 
;	Result of function is position to insert record.
; Opt. Outputs: 
;	None.
; Keywords    : 
;	BEFORE	= Keyword string name.  The parameter will be placed before the
;		  location of this keyword.  For example, if BEFORE='HISTORY'
;		  then the parameter will be placed before the first history
;		  location.  This applies only when adding a new keyword;
;		  keywords already in the header are kept in the same position.
;
;	AFTER	= Same as BEFORE, but the parameter will be placed after the
;		  location of this keyword.  This keyword takes precedence over
;		  BEFORE.
;
;	If neither BEFORE or AFTER keywords are passed, then IEND is returned.
;
; Calls       : 
;	None.
; Common      : 
;	None.
; Restrictions: 
;	KEYWRD and IEND must be consistent with the relevant FITS header.
; Side effects: 
;	None.
; Category    : 
;	Data Handling, I/O, FITS, Generic.
; Prev. Hist. : 
;	William Thompson, Jan 1992.
; Written     : 
;	William Thompson, GSFC, January 1992.
; Modified    : 
;	Version 1, William Thompson, GSFC, 12 April 1993.
;		Incorporated into CDS library.
; Version     : 
;	Version 1, 12 April 1993.
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
;
	ON_ERROR,2				;Return to caller
;
;  Check the number of parameters.
;
	IF N_PARAMS() NE 2 THEN MESSAGE,	$
		'Required parameters are KEYWRD and IEND'
;
;  If the AFTER keyword has been entered, then find the location.
;
	IF N_ELEMENTS(AFTER) EQ 1 THEN BEGIN
		KEY_AFTER = STRING(REPLICATE(32B,8))
		STRPUT,KEY_AFTER,STRUPCASE(STRTRIM(AFTER,2)),0
		ILOC = WHERE(KEYWRD EQ KEY_AFTER,NLOC)
		IF NLOC GT 0 THEN RETURN, (MAX(ILOC)+1) < IEND
	ENDIF
;
;  If AFTER wasn't entered or found, and if the BEFORE keyword has been
;  entered, then find the location.
;
	IF N_ELEMENTS(BEFORE) EQ 1 THEN BEGIN
		KEY_BEFORE = STRING(REPLICATE(32B,8))
		STRPUT,KEY_BEFORE,STRUPCASE(STRTRIM(BEFORE,2)),0
		ILOC = WHERE(KEYWRD EQ KEY_BEFORE,NLOC)
		IF NLOC GT 0 THEN RETURN,ILOC[0]
	ENDIF
;
;  Otherwise, simply return IEND.
;
	RETURN,IEND
	END
	FUNCTION DETABIFY, CHAR_STR
;+
; NAME:
;	DETABIFY
; PURPOSE:
;	Replaces tabs in character strings with appropriate number of spaces
; EXPLANATION:
;	The number of space characters inserted is calculated to space
;	out to the next effective tab stop, each of which is eight characters
;	apart.
;
; CALLING SEQUENCE:
;	Result = DETABIFY( CHAR_STR )
;
; INPUT PARAMETERS:
;	CHAR_STR = Character string variable (or array) to remove tabs from.
;
; OUTPUT:
;	Result of function is CHAR_STR with tabs replaced by spaces.
;
; RESTRICTIONS:
;	CHAR_STR must be a character string variable.
;
; MODIFICATION HISTORY:
;	William Thompson, Feb. 1992.
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
;
	ON_ERROR, 2
;
;  Check the number of parameters.
;
	IF N_PARAMS() NE 1 THEN MESSAGE,'Syntax:  Result = DETABIFY(CHAR_STR)'
;
;  Make sure CHAR_STR is of type string.
;
	SZ = SIZE(CHAR_STR)
	IF SZ[SZ[0]+1] NE 7 THEN BEGIN
		MESSAGE,/INFORMATIONAL,'CHAR_STR must be of type string'
		RETURN, CHAR_STR
	ENDIF
;
;  Step through each element of CHAR_STR.
;
	STR = CHAR_STR
	FOR I = 0,N_ELEMENTS(STR)-1 DO BEGIN
;
;  Keep looking for tabs until there aren't any more.
;
		REPEAT BEGIN
			TAB = STRPOS(STR[I],STRING(9B))
			IF TAB GE 0 THEN BEGIN
				NBLANK = 8 - (TAB MOD 8)
				STR[I] = STRMID(STR[I],0,TAB) +		$
					STRING(REPLICATE(32B,NBLANK)) +	$
					STRMID(STR[I],TAB+1,STRLEN(STR[I])-TAB-1)
			ENDIF
		ENDREP UNTIL TAB LT 0
	ENDFOR
;
	RETURN, STR
	END
function gettok,st,char, exact=exact, notrim=notrim
;+
; NAME:
;	GETTOK                                    
; PURPOSE:
;	Retrieve the first part of a (vector) string up to a specified character
; EXPLANATION:
;	GET TOKen - Retrieve first part of string until the character char 
;	is encountered.   
;
; CALLING SEQUENCE:
;	token = gettok( st, char, [ /EXACT, /NOTRIM ] )
;
; INPUT:
;	char - character separating tokens, scalar string
;
; INPUT-OUTPUT:
;	st - string to get token from (on output token is removed unless
;            /NOTRIM is set), scalar or vector
;
; OUTPUT:
;	token - extracted string value is returned, same dimensions as st
; OPTIONAL INPUT KEYWORD:
;       /EXACT -  The default behaviour of GETTOK is to remove any leading 
;              blanks and (if the token is a blank) convert tabs to blanks.    
;              Set the /EXACT keyword to skip these steps and leave the 
;              input string unchanged before searching for the  character 
;              tokens. 
;
;      /NOTRIM - if set, then the input string is left unaltered 
; EXAMPLE:
;	If ST is ['abc=999','x=3.4234'] then gettok(ST,'=') would return
;	['abc','x'] and ST would be left as ['999','3.4234'] 
;
; PROCEDURE CALLS:
;       REPCHR()
; HISTORY
;	version 1  by D. Lindler APR,86
;	Remove leading blanks    W. Landsman (from JKF)    Aug. 1991
;       V5.3 version, accept vector input   W. Landsman February 2000
;       Slightly faster implementation  W. Landsman   February 2001
;       Added EXACT keyword  W. Landsman March 2004
;       Assume since V5.4, Use COMPLEMENT keyword to WHERE W. Landsman Apr 2006
;       Added NOTRIM keyword W. L. March 2011
;-
;----------------------------------------------------------------------
  On_error,2                           ;Return to caller
  compile_opt idl2

   if N_params() LT 2 then begin
       print,'Syntax - token = gettok( st, char, [ /EXACT, /NOTRIM] )'
       return,-1
   endif

; if char is a blank treat tabs as blanks

 if ~keyword_set(exact) then begin
    st = strtrim(st,1)              ;Remove leading blanks and tabs
    if char EQ ' ' then begin 
       tab = string(9b)                 
       if max(strpos(st,tab)) GE 0 then st = repchr(st,tab,' ')
    endif
  endif
  token = st

; find character in string

  pos = strpos(st,char)
  test = pos EQ -1
  bad = where(test, Nbad, Complement = good, Ncomplement=Ngood)
  if Nbad GT 0 && ~keyword_set(notrim) then st[bad] = ''
 
; extract token
 if Ngood GT 0 then begin
    stg = st[good]
    pos = reform( pos[good], 1, Ngood )
    token[good] = strmid(stg,0,pos)
    if ~keyword_set(notrim) then st[good] = strmid(stg,pos+1)
 endif

;  Return the result.

 return,token
 end
pro match, a, b, suba, subb, COUNT = count, SORT = sort, epsilon=epsilon
;+
; NAME:
;       MATCH
; PURPOSE:
;       Routine to match values in two vectors.
;
; CALLING SEQUENCE:
;       match, a, b, suba, subb, [ COUNT =, /SORT, EPSILON =  ]
;
; INPUTS:
;       a,b - two vectors to match elements, numeric or string data types
;
; OUTPUTS:
;       suba - subscripts of elements in vector a with a match
;               in vector b
;       subb - subscripts of the positions of the elements in
;               vector b with matchs in vector a.
;
;       suba and subb are ordered such that a[suba] equals b[subb]
;
; OPTIONAL INPUT KEYWORD:
;       /SORT - By default, MATCH uses two different algorithm: (1) the
;               /REVERSE_INDICES keyword to HISTOGRAM is used for integer data,
;               while (2) a sorting algorithm is used for non-integer data.  The
;               histogram algorithm is usually faster, except when the input
;               vectors are sparse and contain very large numbers, possibly
;               causing memory problems.   Use the /SORT keyword to always use
;               the sort algorithm.
;       epsilon - if values are within epsilon, they are considered equal. Used only
;               only for non-integer matching.  Note that input vectors should 
;               be unique to within epsilon to provide one-to-one mapping.. 
;               Default=0.
;
; OPTIONAL KEYWORD OUTPUT:
;       COUNT - set to the number of matches, integer scalar
;
; SIDE EFFECTS:
;       The obsolete system variable !ERR is set to the number of matches;
;       however, the use !ERR is deprecated in favor of the COUNT keyword
;
; RESTRICTIONS:
;       The vectors a and b should not have duplicate values within them.
;       You can use rem_dup function to remove duplicate values
;       in a vector
;
; EXAMPLE:
;       If a = [3,5,7,9,11]   & b = [5,6,7,8,9,10]
;       then
;               IDL> match, a, b, suba, subb, COUNT = count
;
;       will give suba = [1,2,3], subb = [0,2,4],  COUNT = 3
;       and       a[suba] = b[subb] = [5,7,9]
;
;
; METHOD:
;       For non-integer data types, the two input vectors are combined and
;       sorted and the consecutive equal elements are identified.   For integer
;       data types, the /REVERSE_INDICES keyword to HISTOGRAM of each array
;       is used to identify where the two arrays have elements in common.
; HISTORY:
;       D. Lindler  Mar. 1986.
;       Fixed "indgen" call for very large arrays   W. Landsman  Sep 1991
;       Added COUNT keyword    W. Landsman   Sep. 1992
;       Fixed case where single element array supplied   W. Landsman Aug 95
;       Use a HISTOGRAM algorithm for integer vector inputs for improved
;             performance                W. Landsman         March 2000
;       Work again for strings           W. Landsman         April 2000
;       Use size(/type)                  W. Landsman         December 2002
;       Work for scalar integer input    W. Landsman         June 2003
;       Assume since V5.4, use COMPLEMENT to WHERE() W. Landsman Apr 2006
;       Added epsilon keyword            Kim Tolbert         March 14, 2008
;-
;-------------------------------------------------------------------------
 On_error,2
 compile_opt idl2

 if N_elements(epsilon) EQ 0 then epsilon = 0

 if N_params() LT 3 then begin
     print,'Syntax - match, a, b, suba, subb, [ COUNT =, EPSILON=, /SORT]'
     print,'    a,b -- input vectors for which to match elements'
     print,'    suba,subb -- output subscript vectors of matched elements'
     return
 endif

 da = size(a,/type) & db =size(b,/type)
 if keyword_set(sort) then hist = 0b else $
 hist = (( da LE 3 ) or (da GE 12)) and  ((db LE 3) or (db GE 12 ))

 if not hist then begin           ;Non-integer calculation

 na = N_elements(a)              ;number of elements in a
 nb = N_elements(b)             ;number of elements in b

; Check for a single element array

 if (na EQ 1) or (nb EQ 1) then begin
        if (nb GT 1) then begin
                subb = where(b EQ a[0], nw)
                if (nw GT 0) then suba = replicate(0,nw) else suba = [-1]
        endif else begin
                suba = where(a EQ b[0], nw)
                if (nw GT 0) then subb = replicate(0,nw) else subb = [-1]
        endelse
        count = nw
        return
 endif

 c = [ a, b ]                   ;combined list of a and b
 ind = [ lindgen(na), lindgen(nb) ]       ;combined list of indices
 vec = [ bytarr(na), replicate(1b,nb) ]  ;flag of which vector in  combined
                                         ;list   0 - a   1 - b

; sort combined list

 sub = sort(c)
 c = c[sub]
 ind = ind[sub]
 vec = vec[sub]

; find duplicates in sorted combined list

 n = na + nb                            ;total elements in c
 if epsilon eq 0. then $
    firstdup = where( (c EQ shift(c,-1)) and (vec NE shift(vec,-1)), Count ) $
 else $
    firstdup = where( (abs(c - shift(c,-1)) lt epsilon) and (vec NE shift(vec,-1)), Count )

 if Count EQ 0 then begin               ;any found?
        suba = lonarr(1)-1
        subb = lonarr(1)-1
        return
 end

 dup = lonarr( Count*2 )                     ;both duplicate values
 even = lindgen( N_elements(firstdup))*2     ;Changed to LINDGEN 6-Sep-1991
 dup[even] = firstdup
 dup[even+1] = firstdup+1
 ind = ind[dup]                         ;indices of duplicates
 vec = vec[dup]                         ;vector id of duplicates
 subb = ind[ where( vec, complement = vzero) ]             ;b subscripts
 suba = ind[ vzero]

 endif else begin             ;Integer calculation using histogram.

 minab = min(a, MAX=maxa) > min(b, MAX=maxb) ;Only need intersection of ranges
 maxab = maxa < maxb

;If either set is empty, or their ranges don't intersect:
;  result = NULL (which is denoted by integer = -1)
  !ERR = -1
  suba = -1
  subb = -1
  COUNT = 0L
 if (maxab lt minab) or (maxab lt 0) then return

 ha = histogram([a], MIN=minab, MAX=maxab, reverse_indices=reva)
 hb = histogram([b], MIN=minab, MAX=maxab, reverse_indices=revb)

 r = where((ha ne 0) and (hb ne 0), count)
 if count gt 0 then begin
  suba = reva[reva[r]]
  subb = revb[revb[r]]
 endif
 endelse

 return

 end
pro mrd_skip, unit, nskip
;+
; NAME:
;       MRD_SKIP
; PURPOSE:
;       Skip a number of bytes from the current location in a file or a pipe
; EXPLANATION:
;       First tries using POINT_LUN and if this doesn't work, perhaps because
;       the unit is a pipe or a socket, MRD_SKIP will just read in the 
;       requisite number  of bytes.    
; CALLING SEQUENCE:
;       MRD_SKIP, Unit, Nskip
;
; INPUTS:
;       Unit - File unit for the file or pipe in question, integer scalar
;       Nskip - Number of bytes to be skipped, positive integer
; NOTES:
;       This routine should be used in place of POINT_LUN wherever a pipe
;       or socket may be the input unit (see the procedure FXPOSIT for an 
;       example).   Note that it assumes that it can only work with nskip >= 0 
;       so it doesn't even try for negative values.      
;
;       For reading a pipe, MRD_SKIP currently uses a maximum buffer size
;       of 8 MB.   This chunk value can be increased for improved efficiency
;       (or decreased if you really have little memory.)
; REVISION HISTORY:
;       Written, Thomas A. McGlynn    July 1995
;	Don't even try to skip bytes on a pipe with POINT_LUN, since this
;	might reset the current pointer     W. Landsman        April 1996
;       Increase buffer size, check fstat.compress W. Landsman  Jan 2001
;       Only a warning if trying read past EOF   W. Landsman   Sep 2001
;       Use 64bit longword for skipping in very large files W. Landsman Sep 2003
;       Assume since V5.4, fstat.compress available W. Landsman April 2006
;       POINT_LUN for compressed files is as fast as any W. Landsman Oct 2006
;       Don't try to use POINT_LUN on compressed files W. Landsman Dec. 2006
;       
;-
        On_error,2

	if nskip le 0 then return
        compress = (fstat(unit)).compress

; We try to use POINT_LUN but if an error ocurrs, we just read in the bytes 

          if not compress then begin
 	  On_IOerror, byte_read
	  point_lun, -unit, curr_pos
	  On_IOerror, null
          if curr_pos NE -1 then point_lun, unit, long64(curr_pos) + nskip
           return
	  endif 

; Otherwise, we have to explictly read the number of bytes to skip
; If the number is very large we don't want to create a array so skip
; in chunks of 8 Megabyte

byte_read:

        chunk = 8000000L
	buf = bytarr(nskip<chunk, /nozero)
	nleft = nskip
	on_ioerror, DONE
	while (nleft gt 0) do begin
		readu, unit, buf
		nleft = nleft - chunk
	        if (nleft gt 0 and nleft lt chunk) then buf = buf[0:nleft-1]	
	endwhile
	return
DONE:  message,'Warning - Byte padding in FITS file may not be correct',/CON
       return		
end

pro remchar,st,char	;Remove character
;+
; NAME:
;	REMCHAR
; PURPOSE:
;	Remove all appearances of character (char) from string (st)
;
; CALLING SEQUENCE:
;	REMCHAR, ST, CHAR
;
; INPUT-OUTPUT:
;	ST  - String from which character will be removed, scalar or vector  
; INPUT:
;	CHAR- Single character to be removed from string or all elements of a
;		string array 
;
; EXAMPLE:
;	If a = 'a,b,c,d,e,f,g' then 
;
;	IDL> remchar,a, ','
;
;      will give a = 'abcdefg'
;
; REVISIONS HISTORY
;	Written D. Lindler October 1986
;	Test if empty string needs to be returned   W. Landsman  Feb 1991
;	Work on string arrays    W. Landsman   August 1997
;	Avoid 32 bit integer overflow K. Tolbert/W. Landsman Feb 2007
;-
 compile_opt idl2                             
 if N_params() LT 2 then begin
     print,'Syntax - REMCHAR, string, character'
     return
 endif

 bchar = byte(char) & bchar = bchar[0]          ;Convert character to byte

 for i = 0L,N_elements(st)-1 do  begin

 bst = byte(st[i])
 good = where( bst NE bchar, Ngood)
 if Ngood GT 0 then st[i] = string(bst[good]) else st[i] = ''

 endfor
 return
 end
function strn, number, LENGTH = length, PADTYPE = padtype, PADCHAR = padchar, $
                       FORMAT = Format
;+
; NAME:
;	STRN
; PURPOSE:
;	Convert a number to a string and remove padded blanks.
; EXPLANATION:
;	The main and original purpose of this procedure is to convert a number
;	to an unpadded string (i.e. with no blanks around it.)  However, it 
;	has been expanded to be a multi-purpose formatting tool.  You may 
;	specify a length for the output string; the returned string is either 
;	set to that length or padded to be that length.  You may specify 
;	characters to be used in padding and which side to be padded.  Finally,
;	you may also specify a format for the number.  NOTE that the input 
;	"number" need not be a number; it may be a string, or anything.  It is
;	converted to string.
;
; CALLING SEQEUNCE:
;	tmp = STRN( number, [ LENGTH=, PADTYPE=, PADCHAR=, FORMAT = ] )
;
; INPUT:
;	NUMBER    This is the input variable to be operated on.  Traditionally,
;		 it was a number, but it may be any scalar type.
;
; OPTIONAL INPUT:
;	LENGTH    This KEYWORD specifies the length of the returned string.  
;		If the output would have been longer, it is truncated.  If 
;		the output would have been shorter, it is padded to the right 
;		length.
;	PADTYPE   This KEYWORD specifies the type of padding to be used, if any.
;		0=Padded at End, 1=Padded at front, 2=Centered (pad front/end)
;		IF not specified, PADTYPE=1
;	PADCHAR   This KEYWORD specifies the character to be used when padding.
;		The default is a space (' ').
;	FORMAT    This keyword allows the FORTRAN type formatting of the input
;		number (e.g. '(f6.2)')
;
; OUTPUT:
;	tmp       The formatted string
;
; USEFUL EXAMPLES:
;	print,'Used ',strn(stars),' stars.'  ==> 'Used 22 stars.'
;	print,'Attempted ',strn(ret,leng=6,padt=1,padch='0'),' retries.'
;		==> 'Attempted 000043 retries.'
;	print,strn('M81 Star List',length=80,padtype=2)
;		==> an 80 character line with 'M81 Star List' centered.
;	print,'Error: ',strn(err,format='(f15.2)')
;		==> 'Error: 3.24'     or ==> 'Error: 323535.22'
;
; HISTORY:
;	03-JUL-90 Version 1 written by Eric W. Deutsch
;	10-JUL-90 Trimming and padding options added         (E. Deutsch)
;	29-JUL-91 Changed to keywords and header spiffed up     (E. Deutsch)
;	Ma7 92 Work correctly for byte values (W. Landsman)
;	19-NOV-92 Added Patch to work around IDL 2.4.0 bug which caused an
;	error when STRN('(123)') was encountered.            (E. Deutsch)
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
 On_error,2
  if ( N_params() LT 1 ) then begin
    print,'Call: IDL> tmp=STRN(number,[length=,padtype=,padchar=,format=])'
    print,"e.g.: IDL> print,'Executed ',strn(ret,leng=6,padt=1,padch='0'),' retries.'"
    return,''
    endif
  if (N_elements(padtype) eq 0) then padtype=1
  if (N_elements(padchar) eq 0) then padchar=' '
  if (N_elements(Format) eq 0) then Format=''

  padc = byte(padchar)
  pad = string(replicate(padc[0],200))

  ss=size(number) & PRN=1 & if (ss[1] eq 7) then PRN=0
  if ( Format EQ '') then tmp = strtrim( string(number, PRINT=PRN),2) $
    else tmp = strtrim( string( number, FORMAT=Format, PRINT=PRN),2)

  if (N_elements(length) eq 0) then length=strlen(tmp)

  if (strlen(tmp) gt length) then tmp=strmid(tmp,0,length)

  if (strlen(tmp) lt length) and (padtype eq 0) then begin
    tmp = tmp+strmid(pad,0,length-strlen(tmp))
    endif

  if (strlen(tmp) lt length) and (padtype eq 1) then begin
    tmp = strmid(pad,0,length-strlen(tmp))+tmp
    endif

  if (strlen(tmp) lt length) and (padtype eq 2) then begin
    padln=length-strlen(tmp) & padfr=padln/2 & padend=padln-padfr
    tmp=strmid(pad,0,padfr)+tmp+strmid(pad,0,padend)
    endif

  return,tmp
end
pro textclose,textout=textout
;+
; NAME:
;	TEXTCLOSE                   
;
; PURPOSE:
;	Close a text outpu file previously opened with TEXTOPEN 
; EXPLANATION:
;	procedure to close file for text output as specifed
;	by the (non-standard) system variable !TEXTOUT. 
;
; CALLING SEQUENCE:
;	textclose, [ TEXTOUT = ]
;
; KEYWORDS:
;	textout - Indicates output device that was used by
;		TEXTOPEN
;
; SIDE EFFECTS:
;	if !textout is not equal to 5 and the textunit is
;	opened.   Then unit !textunit is closed and released
;
; HISTORY:
;	D. Lindler  Dec. 1986  (Replaces PRTOPEN)
;	Test if TEXTOUT is a scalar string   W. Landsman   August 1993
; Can't close unit -1 (Standard Output) I. Freedman  April  1994
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
;-----------------------------------------------------------
; CLOSE PROPER UNIT
;

 if N_elements( textout ) EQ 0 then textout = !textout ;use default

 ptype = size( textout )           ;Test if TEXTOUT is a scalar string
 if ptype[1] EQ 7  then text_out = 6 else text_out = textout

 if ( text_out NE 5 ) then begin
	if !textunit ne 0 AND !textunit ne -1 then begin
		free_lun, !TEXTUNIT  
                !textunit = 0
	end
 end

 return
 end
PRO TEXTOPEN,PROGRAM,TEXTOUT=TEXTOUT, STDOUT = STDOUT, MORE_SET = more_set, $
             SILENT = silent, WIDTH = width
;+
; NAME:
;       TEXTOPEN
; PURPOSE:
;       Open a device specified by TEXTOUT with unit !TEXTUNIT 
; EXPLANATION:
;       Procedure to open file for text output.   The type of output 
;       device (disk file or terminal screen) is specified by the 
;       TEXTOUT keyword or the (nonstandard) system variable !TEXTOUT.
;
; CALLING SEQUENCE:
;       textopen, program, [ TEXTOUT =, /STDOUT, /SILENT, MORE_SET=, WIDTH= ]
;
; INPUTS:
;       program - scalar string giving name of program calling textopen
;
; OPTIONAL INPUT KEYWORDS:
;       TEXTOUT - Integer scalar (0-7) specifying output file/device to be 
;               opened (see below) or scalar string giving name of output file.
;               If TEXTOUT is not supplied, then the (non-standard) system 
;               variable !TEXTOUT is used.
;       /SILENT - By default, TEXTOPEN prints an informational message when
;                opening a file for hardcopy output.   Set /SILENT (or !QUIET)
;                to suppress this message.
;       /STDOUT - if this keyword is set and non-zero, then the standard output
;               (unit = -1) is used for TEXTOUT=1 or TEXTOUT=2.   The use
;               of STDOUT has  2 possible advantages:
;               (1) the output will appear in a journal file
;               (2) Many Unix machines print spurious control characters when
;               printing to /dev/tty.   These characters are eliminated by 
;               setting /STDOUT
;
;               The disadvantage of /STDOUT is that the /MORE option is not
;               available.
;
;         WIDTH - Specify line width for hardcopy output line wrapping (passed onto OPENW).
;
; OPTIONAL OUTPUT KEYWORD:
;       MORE_SET - Returns 1 if the output unit was opened with /MORE.   This
;               occurs if (1) TEXTOUT = 1 and (2) the device is a tty, and 
;               (3) /STDOUT is not set.      User can use the returned value
;                of MORE_SET to determine whether to end output when user
;                presses 'Q'.
; SIDE EFFECTS:
;       The following dev/file is opened for output.    Different effects
;       occur depending whether the standard output is a GUI (Macintosh,
;       Windows, Unix/IDLTool) or a TTY
;
;               textout=0       Nowhere
;               textout=1       if a TTY then TERMINAL using /more option
;                                   otherwise standard (Unit=-1) output
;               textout=2       if a TTY then TERMINAL without /more option
;                                   otherwise standard (Unit=-1) output
;               textout=3       <program>.prt
;               textout=4       laser.tmp
;               textout=5      user must open file
;               textout=7      same as 3 but text is appended to <program>.prt
;                               file if it already exists.
;               textout = filename (default extension of .prt)
;
;       The unit to be opened is obtained with the procedure GET_LUN
;       unless !TEXTOUT=5.  The unit number is placed in system variable 
;       !TEXTUNIT.  For !TEXTOUT=5 the user must set !TEXTUNIT to the 
;       appropriate unit number.
;
; NOTES:
;       When printing to a TTY terminal, the output will *not* appear in an 
;       IDL JOURNAL session, unlike text printed with the PRINT command.
;
; NON-STANDARD SYSTEM VARIABLES:
;       TEXTOPEN will automatically define the following system variables if
;       they are not previously defined:
;
;       DEFSYSV,'!TEXTOUT',1
;       DEFSYSV,'!TEXTUNIT',0
; HISTORY:
;       D. Lindler  Dec. 1986  
;       Keyword textout added, J. Isensee, July, 1990
;       Made transportable, D. Neill, April, 1991
;       Trim input PROGRAM string W. Landsman  Feb 1993
;       Don't modify TEXTOUT value   W. Landsman   Aug 1993
;       Modified for MacOS  I. Freedman April 1994
;       Modified for output terminals without a TTY  W. Landsman  August 1995
;       Added /STDOUT keyword   W. Landsman    April 1996
;       added textout=7 option, D. Lindler, July, 1996
;       Exit with RETURN instead of RETALL  W. Landsman  June 1999
;       In IDL V5.4 filepath(/TERMINAL) not allowed in the IDLDE WL August 2001
;       Added MORE_SET output keyword   W.Landsman   January 2002
;       Added /SILENT keyword  W. Landsman  June 2002  
;	Define !TEXTOUT and !TEXTUNIT if needed.  R. Sterner, 2002 Aug 27
;       Return Calling Sequence if no parameters supplied W.Landsman Nov 2002
;       Remove VMS specific code  W. Landsman Sep 2006
;       Make sure MORE_SET is always defined   W. Landsman Jan 2007
;       Added WIDTH keyword   J. Bailin Nov 2010
;       Use V6.0 notation  W. Landsman April 2011
;-
;-----------------------------------------------------------
  On_Error,2
  compile_opt idl2

  if N_params() LT 1 then begin
      print,'Syntax - TEXTOPEN, program, [ TEXTOUT =, /STDOUT, /SILENT,' 
      print,'                              MORE_SET=, WIDTH= ]' 
      return
  endif

  defsysv,'!TEXTOUT',exists=ex			; Check if !TEXTOUT exists.
  if ex eq 0 then defsysv,'!TEXTOUT',1		; If not define it.
  defsysv,'!TEXTUNIT',exists=ex			; Check if !TEXTUNIT exists.
  if ex eq 0 then defsysv,'!TEXTUNIT',0		; If not define it.
  more_set = 0                                  
  ;
  ; Open proper unit.
  ;
  if N_elements( textout ) NE 1 then textout = !textout ;use default output dev.

  ; keywords for openw
  if n_elements(width) gt 0 then openw_keywords = {width: width}

  if size(textout,/tname) EQ 'STRING' then begin  ;test if filename entered
        filename = textout
        j = strpos(filename,'.')        ;test if file extension given
        if j lt 0 then filename = filename + ".prt"
        text_out = 6
  endif else text_out = textout     

  if TEXT_OUT eq 5 then begin
     if !TEXTUNIT eq 0 then begin
         print,' '
         print,' You must set !TEXTUNIT to the desired unit number...'
         print,'                    ...see following example'
         print,' '
         print,'                    OPENW, LUN, filename, /GET_LUN
         print,'                    !TEXTUNIT = LUN
         print,'                    DBPRINT...
         print,'
         print,' Action: returning'
         print,' '
         return
     end
     return
  end
   stndout = fstat(-1)
   isatty = (stndout.isatty) && (~stndout.isagui) && $
             (~keyword_set(STDOUT))

   if isatty || (text_out GT 2) then begin 

        if !TEXTUNIT GT 0 then free_lun,!TEXTUNIT 
        get_lun,unit
        !TEXTUNIT = unit

    endif else !TEXTUNIT = -1                     ;standard output

  more_set = (text_out EQ 1) && isatty
  
  case text_out of
     1: if isatty then openw, !TEXTUNIT, filepath(/TERMINAL), /MORE, _extra=openw_keywords

     2: if isatty then openw, !TEXTUNIT, filepath(/TERMINAL) , _extra=openw_keywords

     3: begin
        oname = strlowcase( strtrim( PROGRAM,2) +'.prt')
         openw, !TEXTUNIT, oname, _extra=openw_keywords
        if ~keyword_set(SILENT) then $
        message,'Output is being directed to a file ' + oname,/INFORM
        end

     4: openw, !TEXTUNIT, 'laser.tmp', _extra=openw_keywords

     6: begin
        openw,!TEXTUNIT,filename, _extra=openw_keywords
        if ~keyword_set(SILENT) then $
        message,'Output is being directed to a file ' + filename,/INFORM
        end

     7: begin
        oname = strlowcase(strtrim( PROGRAM,2) +'.prt')
        openw, !TEXTUNIT, oname, /append, _extra=openw_keywords
        if ~keyword_set(SILENT) then $
        message,'Output is being appended to file ' + oname,/INFORM
        for i=0,3 do printf,!textunit,' '       ;added a couple of blank lines
        end

     0: openw,!TEXTUNIT, strtrim(PROGRAM,2) + '.tmp',/DELETE, _extra=openw_keywords

     else: begin
        !textunit = 0
        print,' '
        print,' Invalid value for TEXTOUT =',TEXTOUT
        print,' '
        print,'                 ...the possibilities are:
        print,' '
        print,'                 textout=0      nowhere
        if isatty then begin
                print,'                 textout=1      terminal with /more 
                print,'                 textout=2      terminal without /more 
        endif else begin
                print,'                 textout=1      terminal
                print,'                 textout=2      terminal
        endelse
        print,'                 textout=3      file   <program>.prt
        print,'                 textout=4      file   laser.tmp
        print,'                 textout=5      User supplied file
        print,'                 textout = filename (default extension of .prt)
        print,'                 textout=7      Same as 3 but append the file
        print,' '
        print,' Action: returning
        print,' '
        return
    end
 endcase

 return
 end   ; textout
;+
; NAME: 
;     VALID_NUM()
; PURPOSE:               
;     Check if a string is a valid number representation.
; EXPLANATION:              
;     The input string is parsed for characters that may possibly
;     form a valid number.  It is more robust than simply checking
;     for an IDL conversion error because that allows strings such
;     as '22.3qwert' to be returned as the valid number 22.3
;
;     This function had a major rewrite in August 2008 to use STREGEX
;     and allow vector input.    It should be backwards compatible.
; CALLING SEQUENCE: 
;     IDL> status = valid_num(string  [,value]  [,/integer])
;    
; INPUTS:
;     string  -  the string to be tested, scalar or array
;               
; RETURNS
;     status - byte scalar or array, same size as the input string
;              set to 1 where the string is a  valid number, 0 for invalid
; OPTIONAL OUTPUT:               
;     value     - The value the string decodes to, same size as input string.
;           This will be returned as a double precision number unless 
;           /INTEGER is present, in which case a long integer is returned.
;           
; OPTIONAL INPUT KEYWORD:          
;    /INTEGER   -  if present code checks specifically for an integer.
; EXAMPLES:
;     (1) IDL> print,valid_num(3.2,/integer) 
;        --> 0     ;Since 3.2 is not an integer 
;     (2) IDL> str =['-0.03','2.3g', '3.2e12']
;         IDL> test = valid_num(str,val)
;              test = [1,0,1]    &  val =  [-0.030000000 ,NaN ,3.2000000e+12]
; REVISION HISTORY:
;          Version 1, C D Pike, RAL, 24-May-93
;          Version 2, William Thompson, GSFC, 14 October 1994
;                       Added optional output parameter VALUE to allow
;                       VALID_NUM to replace STRNUMBER in FITS routines.
;          Version 3 Wayne Landsman rewrite to use STREGEX, vectorize
;          Version 4 W.L. (fix from C. Markwardt) Better Stregex expression, 
;                    was missing numbers like '134.' before Jan 1 2010
;-            

FUNCTION valid_num, string, value, INTEGER=integer
 On_error,2
 compile_opt idl2 
 
; A derivation of the regular expressions below can be found on 
; http://wiki.tcl.tk/989

   if keyword_set(INTEGER) then $ 
    st = '^[-+]?[0-9][0-9]*$'  else $                    ;Integer
     st = '^[-+]?([0-9]+\.?[0-9]*|\.[0-9]+)([eEdD][-+]?[0-9]+)?$' ;F.P.
   
;Simple return if we just need a boolean test.
    if N_params() EQ 1 then return, stregex(strtrim(string,2),st,/boolean)

   
      vv = stregex(strtrim(string,2),st,/boolean)      
      if size(string,/N_dimen) EQ 0 then begin     ;Scalar
         if vv then $
            value= keyword_set(integer) ? long(string) : double(string) 
      endif else begin                             ;Array 
         
      g = where(vv,Ng)
      if Ng GT 0 then begin      ;Need to create output vector
        if keyword_set(integer) then begin 
              value = vv*0L 
              value[g] = long(string[g])
        endif else begin 
                value = replicate(!VALUES.D_NAN,N_elements(vv))
                value[g] = double(string[g])
        endelse 
        endif   
        endelse 
     
       return,vv
      end
;+
; NAME:
;       MRD_STRUCT
; PURPOSE:
;       Return a structure as defined in the names and values data.
; CALLING SEQUENCE:
;       struct = MRD_STRUCT(NAMES, VALUES, NROW, STRUCTYP='name' )
; INPUT PARAMETERS:
;       NAMES   = A string array of names of structure fields.
;       VALUES  = A string array giving the values of the structure
;                 fields.  See examples below.
;       NROW    = The number of elements in the structure array.
;       
; RETURNS:
;       A structure as described in the parameters or 0 if an error
;       is detected.
;
; OPTIONAL KEYWORD PARAMETERS:
;       /NO_EXECUTE - If set then the use of the EXECUTE() statement is avoided.
;                  By default, the NO_EXECUTE pathway is used if IDL is 
;                  running under the Virtual Machine.    Note if  /NO_EXECUTE
;                  is set, then the user cannot supply arbitary values, but
;                  all possible values used by MRDFITS will be allowed.
;       STRUCTYP = The structure type.  Since IDL does not allow the
;                  redefinition of a named structure it is an error
;                  to call MRD_STRUCT with different parameters but
;                  the same STRUCTYP in the same session.  If this
;                  keyword is not set an anonymous structure is created.
; COMMON BLOCKS:
;       MRD_COMMON
; SIDE EFFECTS:                                                            
;       May create a temporary file if the structure definition is too long 
;       for the EXECUTE function and using old style structures
;
; RESTRICTIONS:
;       By default, the program defines the structure in a long string
;       which is executed with CREATE_STRUCT within a single EXECUTE statement.
;
;       If program is being run in the IDL Virtual machine (EXECUTE statement
;       not allowed), then a separate CREATE_STRUCT statement is called
;       for each tag.   This mode does not have the full capabilities of the
;       normal mode, but should be sufficient for use with MRDFITS().
; PROCEDURE:
;       A structure definition is created using the parameter values.
;       MRD_NSTRUCT is called  and generates the structure in pieces using the
;       execute and create_struct keywords.
;
; EXAMPLES:
;       (1) str = mrd_struct(['fld1', 'fld2'], ['0','dblarr(10,10)'],3)
;           print, str(0).fld2(3,3)
;       Note that "0" is always considered short integer even if the default
;       integer is set to long.
;          
;
;       (2) str = mrd_struct(['a','b','c','d'],['1', '1.', '1.d0', "'1'"],1)
;               ; returns a structure with integer, float, double and string
;               ; fields.
; PROCEDURE CALLS:
;       GETTOK() - needed for virtual machine mode only
; MODIFICATION HISTORY:
;       Created by T. McGlynn October, 1994.
;       Modified by T. McGlynn September, 1995.
;          Added capability to create substructures so that structure
;          may contain up to 4096 distinct elements.  [This can be
;          increased by futher iteration of the process used if needed.]
;       Removed V4.0 reference to common block  October 1997
;       Allowed unlimited number of structure elements if the version
;       is greater than 5.0.  Put back in code to handle prior versions.
;       The [] will need to be translated back to () for this to
;       work.  T. McGlynn December 15 1998.
;       Add MRD_NSTRUCT since IDL has mysterious problems compiling
;       very large structures.
;       Removed TEMPDIR and OLD_STRUCT keywords  W. Landsman October 2003   
;       Alternate pathway without EXECUTE for V6.0 virtual machine, D. Lindler
;       Removed limit on EXECUTE statement.  W. Landsman  October 2003
;       Restore EXECUTE limit (sigh...), added NO_EXECUTE keyword
;                         W. Landsman July 2004
;       Fix use of STRUCTYP with /NO_EXECUTE  W. Landsman June 2005
;       Assume since V6.0 (lmgr function available), remove 131 string length
;             limit for execute    W. Landsman Jun 2009 
;      Restore EXECUTE limit (sigh...)   W. Landsman July 2009 
;      Make sure "0" is a short integer even with compile_opt idl2  July 2010
;-

; Check that the number of names is the same as the number of values.

function mrd_struct, names, values, nrow, no_execute = no_execute,  $
    structyp=structyp,  tempdir=tempdir, silent=silent, old_struct=old_struct

compile_opt idl2

; Keywords TEMPDIR, SILENT and OLD_STRUCT no longer do anything but are kept
; for backward compatibility.


 noexecute = keyword_set(no_execute) or lmgr(/vm) 

 if noexecute then begin

    ntags = n_elements(names)
    for i=0,ntags-1 do begin
;
; create variable with the specified data type
;
	case strlowcase(values[i]) of 
;
; scalar values
;
	    '0b': v = 0B
	    '0' : v = 0S
	    '0l': v = 0L
	    '0ll' : v = 0LL
	    '0.': v = 0.0
	    '0.0d0': v = 0.0d0
 	    '0.d0': v = 0.0d0
             '" "': v = " "          ;Added July 2004
	    'complex(0.,0.)': v = complex(0.,0.)
	    'dcomplex(0.d0,0.d0)': v = dcomplex(0.d0,0.d0)
;
; strings and arrays
;`
	    else: begin	     
	        value = values[i]
		remchar,value,"'"
		remchar,value,'"'   
		if strlen(value) EQ 1 then v= value else begin 
	        type = gettok(value,'(')
		if type eq 'string' then $
			junk = gettok(value,',')      ;remove "replicate(32b"
		dimen_string = gettok(value,')')	
		dimen = long(strsplit(dimen_string,',',/extract))
		case type of
		    'bytarr': v = make_array(dimen=dimen,/byte)
		    'intarr': v = make_array(dimen=dimen,/int)
		    'fltarr': v = make_array(dimen=dimen,/float)
		    'lonarr': v = make_array(dimen=dimen,/long)
		    'lon64arr': v = make_array(dimen=dimen,/long64)
		    'dblarr': v = make_array(dimen=dimen,/double)
		    'complexarr': v = make_array(dimen=dimen,/complex)
		    'dcomplexarr': v = make_array(dimen=dimen,/dcomplex)
                    'ptr_new': v = ptr_new()
		    'string': begin
		    		ndimen = n_elements(dimen)-1
				if ndimen gt 0 then begin
					v = make_array(dimen=dimen[1:*],/string)
					v[*] = string(replicate(32B,dimen[0]))
		    		end else v = string(replicate(32B,dimen[0]))
			      end
	            else: message,'ERROR - Invalid field value: ' + values[i]		      
		endcase
		        endelse 

	    end
	endcase     	
	if i eq 0 then struct = create_struct(names[i],v) $
		  else struct = create_struct(temporary(struct),names[i],v)
    end; for i    

endif else begin

; Build up the structure use a combination of execute and
; create_struct calls.  Basically we build as many rows as
; will fit in an execute call and create that structure.  Then
; we append that structure to whatever we've done before using
; create_struct

nel = N_elements(names)
strng = "a={"

comma = ' '
for i=0,nel-1 do  begin
    fval = values[i]
    if (fval eq '0') then fval = '0s'
  
   ; Now for each element put in a name/value pair.
    tstrng = strng + comma+names[i] + ':' + fval
    
    ; The nominal max length of the execute is 131
    ; We need one chacacter for the "}"
    if strlen(tstrng) gt 130 then begin
        strng = strng + "}"
        res = execute(strng)
	if  res eq 0 then return, 0
        struct = n_elements(struct) eq 0 ? a: $
	         create_struct(temporary(struct), a)
	strng = "a={" + names[i] + ":" + fval
	
    endif else strng = tstrng
    comma = ","

endfor
	

if strlen(strng) gt 3 then begin
    strng = strng + "}"
    res = execute(strng)
     if  res eq 0 then return, 0
     struct = n_elements(struct) eq 0 ? a : create_struct(temporary(struct), a)
 endif
 
endelse
if keyword_set(structyp) then $
     struct = create_struct(temporary(struct), name=structyp)


if nrow le 1 then return, struct $
             else return, replicate(struct, nrow)

end

;+
; NAME:
;  eve_read_whole_fits
;
; PURPOSE:
;  Read all of the HDUs in a FITS file, creating a merged set of
;  structures for the data, and string arrays for the keywords.
;
; CATEGORY:
;  SDO-EVE lib
;
; CALLING SEQUENCE:
;  IDL> data = eve_read_whole_fits( filename [, /verbose][,_extra=_extra][,/compress] )
;
; INPUTS:
;  filename : a scalar string for a FITS file
;             If comressed with gzip (.gz) MRDFITS is used in compress mode
;
; OPTIONAL INPUTS:
;  _extra : additional parameters to pass to mrdfits (refer to mrdfits.pro)
;
; KEYWORD PARAMETERS:
;  /verbose : report information on each HDU read (nothing is reported normally)
;
; OUTPUTS:
;  data : returned data is a structure that contains structures from
;         each HDU and an associated string array for the keywords
;
; OPTIONAL OUTPUTS:
;  none
;
; COMMON BLOCKS:
;  none
;
; SIDE EFFECTS:
;  Substructure names correspond to the keyword "EXTNAME", if absent,
;  then a default naming convention is used.
;
; RESTRICTIONS:
;  Requires fits_info.pro and mrdfits.pro (with it's own dependencies).
;
;  This should be compatible with all OSes, but we only test on Linux
;  and Mac OS X. However, if you encounter problems, try decompressing
;  the FITS files before you call this function.
;
; EXAMPLE:
;  IDL> data=eve_read_whole_fits('EVS_L2_2010120_00_002_01.fit.gz',/verbose)
;
; MODIFICATION HISTORY:
;  2/21/10 DLW Modified from CJs pre-release version.
;  8/3/2011 CDJ Use MRDFITS built-in compression handling, so it works in Windows also. 
;               Renamed to SSW standards
;  9/1/2011 CDJ added /swap_if_little to openr, to handle endian issues we've been having.
;
; $Id: eve_read_whole_fits.pro,v 3.4 2011/12/08 21:13:31 dlwoodra Exp $
;-
function eve_read_whole_fits,infname,verbose=verbose,swap=swap,_extra=extra
  compress=strlowcase((reverse(strsplit(infname,'.',/extract)))[0]) eq 'gz' 
  infn = infname
  ; determine the number of HDUs to read
  fits_info,silent=~keyword_set(verbose),infn,n_ext=n_hdus ; the only programmatic way to get number of HDUs?
  openr,inf,infn,/get_lun,compress=compress,/swap_if_little
  status=0
  hdu=0
  this_hdu=mrdfits(inf,0,this_header,/unsigned,silent=~keyword_set(verbose),compress=compress,_extra=extra,status=status)
  while status ge 0 and hdu le n_hdus do begin
    if hdu eq 0 then begin
      result=create_struct('primary',this_hdu,'primary_head',this_header)
    endif else begin
      w=where(strmid(this_header,0,8) eq 'EXTNAME ',count)
      if count eq 1 then begin
        this_hdu_name=strtrim((strsplit(this_header[w],"'",/extract))[1])
      endif else begin
        this_hdu_name=string(format='(%"HDR%03d")',hdu)
      endelse
      this_hdr_name=this_hdu_name+"_HEADER"
      result=create_struct(result,this_hdu_name,this_hdu,this_hdr_name,this_header)
    endelse
    hdu++
    ;0 means skip zero hdus
    if hdu le n_hdus then this_hdu=mrdfits(inf,0,this_header,/unsigned,silent=~keyword_set(verbose),_extra=extra,status=status)
  endwhile
  free_lun,inf
  if ~keyword_set(swap) then begin
    t=tag_names(result)
    w=where(strupcase(t) eq "SPECTRUM" or strupcase(t) eq "LINESDATA" or strupcase(t) eq "DATA", count)
    if count gt 0 then begin
      t=result.(w[0])
      tagnames = tag_names(t) ; string of tag names

      tmp = where(tagnames eq 'TAI',do_tai)
      if do_tai gt 0 then $
         if t[0].tai lt 1.4832d9 or t[0].tai gt 2.2406d9 then swap=1

      tmp = where(tagnames eq 'YYYYDOY',do_yyyydoy)
      if do_yyyydoy gt 0 then $
         if t[0].yyyydoy lt 2005001L or t[0].yyyydoy gt 2029001L then swap=1

    end
  end
  if keyword_set(swap) then result=swap_endian(temporary(result))

  return,result
end
