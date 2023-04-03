;;; [START] ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#include "usgard.h"

.org 0
	.db	"WISH 0.1", 0	; WWW's Incredible SHell

COUNT		= 6	; displayed entries
START_Y		= 9
STATUS_LINE	= 63-8

LINE_SIZE	= 7
CURS_SKIP	= LINE_SIZE * COUNT

;;; [CONFIGURATION] ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; This is user-configurable section. You may enable/disable features by
; (un)commenting lines with #define. Follow included instructions.
; You may comment-out features you don't need, thus reduce the size of
; shell.

;; define this to enable 'auto power down' ;;
#define CONFIG_APD

;; define this, if you want to be able to run TI-basic programs ;;
;; requires USGard standard ;;
#define CONFIG_RUN_BASIC

;; define this to enable top line with free mem, usgard and ti-os version ;;
#define CONFIG_TOPLINE

;; define this to enable 'DEL' feature ;;
;; requires USGard standard ;;
#define CONFIG_DELETE

;; define this to enable contrast changer (+/- buttons) ;;
#define CONFIG_CONTRAST

;; this is used to determine sequence of data on screen
; #define CONFIG_SIZE_AFTER_NAME

;; define this only if your IY points elsewhere than to the default location
; #define CONFIG_MAX_COMPAT

;; define this, when you are first installing shell. It will add code, needed
;; to set THIS shell as standard one for use with USGard.
;; after installing you may wish to compile shorter version without this code.
#define CONFIG_CAN_INSTALL

;; define this, if you don't want to see 'wish' in list of programs.
;; strongly recommended, if you've defined CONFIG_DELETE!
#define CONFIG_SELF_FILTER

;; if defined, text memory (where application variables often resides)
;; will be cleared. don't uncomment this, unless you are absolutly sure
;; what you are doing.
#define CONFIG_CLRTXTMEM

;;; [TODO] ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; cistenie pamati v uzivatelskych premennych pred spustenim softu
; optimalizacia na dlzku
; spravit LOCK tlacitko?
; opravit APD na konstantu, ak nie MAX_COMPAT
; spravit ifdefom prekreslujuci system - kratsi

;;; [CODE] ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;
; name:	Main - main routine

Main:
	ld hl, USG_BITS
	set 0, (hl)		; recalc checksum

	ld a, 4
	out (5), a		; bank 4

#ifdef CONFIG_APD
	ld a, (iy+8)
	ld (&APDorig), a
#endif

#ifdef CONFIG_RUN_BASIC
#ifdef CONFIG_MAX_COMPAT
	ld hl, &smp1 + 1
	push iy
	pop de
	ld (hl), e
	inc hl
	ld (hl), d
#endif

	ld hl, (PROGRAM_ADDR)
	call CHECK_APPEND
	jr nc, noneeduninst
	call INT_REMOVE
	call UNAPPEND
noneeduninst:
#endif

#ifdef CONFIG_CAN_INSTALL
	ld hl, &wishname
	ld de, USGSHELL
	ld bc, wishend - wishname
	ldir
#endif

	ld hl, &Cursor
	ld de, 1
	ld (hl), e
	inc hl
	ld (hl), d
	inc hl
	ld (hl), e
	inc hl
	ld (hl), d

inputs:
	call &DispAll
Key_OTH:

#ifdef CONFIG_APD
	call UPDATE_APD
	ld (iy+8), $8C
#endif

gkloop:
	halt
	in a, (3)
	and 8
	jr nz, gkloopcont
waitforrelease1:
	in a, (3)
	and 8
	jr z, waitforrelease1
	call OTH_SHUTDOWN
waitforrelease2:
	in a, (3)
	and 8
	jr z, waitforrelease2
gkloopcont:
	call GET_KEY
	and a
	jr z, gkloop

#ifdef CONFIG_APD
	push af
	ld a, (&APDorig)
	ld (iy+8), a
	pop af
#endif

	ld hl, &XLat3
	ld de, 2
	call &XLAT
	ld a, (hl)
	inc hl
	ld h, (hl)
	ld l, a
	ld de, (PROGRAM_ADDR)
	add hl, de

	jp (hl)



;;;;;;;
; name:	Key_CR - 'enter' handler

Key_CR:
	ld bc, (&Cursor)
	call &Find
	jr c, Key_OTH
	ld hl, (&VarStor + 2)
	ld a, h
	or l
	jr z, Key_OTH
	jp (hl)

;;;;;;;
; name:	Key_UP - UP key handler

Key_UP:
	ld hl, (&Cursor)
	dec hl
	ld a, h
	or l
	jr z, Key_OTH
	ld (&Cursor), hl
	ld de, (&Top)
	and a
	push hl
	sbc hl, de
	push de
	push af
	ld a, l
	inc a
	call &RemoveOldCursor
	pop af
	pop de
	pop hl
	jr nc, up_neposun
	ld (&Top), hl

	ld bc, 16 * LINE_SIZE * (COUNT-1)
	ld de, 16 * (START_Y + CURS_SKIP) + VIDEO_MEM - 1
	ld hl, 16 * (START_Y + CURS_SKIP - LINE_SIZE) + VIDEO_MEM - 1
	lddr
	push de
	pop hl
	ld (hl), 0
	dec de
	ld bc, 16 * LINE_SIZE - 1
	lddr
	jr dn_exitcode1

up_neposun:
	ld a, (CURSOR_Y)
	sub LINE_SIZE
	jr dn_exitcode2

;;;;;;;
; name:	Key_DN - DOWN key handler

Key_DN:
	ld bc, (&Cursor)
	inc bc
	push bc
	call &Find
	pop hl
	jp c, &Key_OTH

	ld (&Cursor), hl
	ld de, (&Top)
	and a
	sbc hl, de
			; remove old cursor
	ld a, l
	dec a
	call &RemoveOldCursor

	ld a, l
	cp COUNT

	jr c, dn_neposun
	ld hl, (&Top)
	inc hl
	ld (&Top), hl

	ld bc, 16 * LINE_SIZE * (COUNT-1)
	ld de, 16 * START_Y + VIDEO_MEM
	ld hl, 16 * (START_Y + LINE_SIZE) + VIDEO_MEM
	ldir
	push de
	pop hl
	ld (hl), 0
	inc de
	ld bc, 16 * LINE_SIZE - 1
	ldir
			; write last entry
dn_dispandexit:
dn_exitcode1:
	ld a, 1
	ld (CURSOR_X), a
	ld bc, (&Cursor)
	call &DispOneEntry
	jp &Key_OTH
dn_neposun:
	ld a, (CURSOR_Y)
	add a, LINE_SIZE
dn_exitcode2:
	ld (CURSOR_Y), a
	jr dn_dispandexit

;;;;;;;
; name:	RemoveOldCursor
; in:	a=screen offset
; out:  destroys AF, word at VarStor+6, DE=VIDEO_MEM, B=0

RemoveOldCursor:
	push hl
	ld l, a
	ld b, LINE_SIZE - 1
getcursy:
	add a, l		; a *= LINE_SIZE
	djnz getcursy
	add a, START_Y		; a += START_Y
	ld (CURSOR_Y), a
	call &xorcurs
	pop hl
	ret

;;;;;;;
; name:	Key_ON - ON key handler

Key_ON:
	call OTH_SHUTDOWN
	jp &Key_OTH

#ifdef CONFIG_DELETE

;;;;;;;
; name:	Key_DEL

Key_DEL:
	ld bc, (&Cursor)
	call &Find
	jp c, &Key_OTH
	set 3, (iy+5)
	ld hl, 256 * 7 + 3
	ld (CURSOR_ROW), hl
	ld hl, &DelQuestion
	call D_ZT_STR
	res 3, (iy+5)
#ifdef CONFIG_APD
	call UPDATE_APD
	ld (iy+8), $8C
#endif
keydelwait1:
	halt
	call GET_KEY
	and a
	jr z, keydelwait1

#ifdef CONFIG_APD
	push af
	ld a, (&APDorig)
	ld (iy+8), a
	pop af
#endif
	cp K_ENTER
	jr nz, apd_rettoinputs
	ld hl, VATName
	call VAR_DELETE
	jr c, apd_rettoinputs
	ld hl, (&Cursor)
	dec hl
	ld a, h
	or l
	jr z, apd_rettoinputs
	ld (&Cursor), hl
	ld de, (&Top)
	and a
	push hl
	sbc hl, de
	pop hl
	jr nc, apd_rettoinputs
	ld (&Top), hl
apd_rettoinputs:	
	jp &inputs
#endif

#ifdef CONFIG_CONTRAST

;;;;;;;
; name: Key_PLUS - decrease contrast

Key_PLUS:
	ld a, (CONTRAST)
	inc a
	and $1f
	jr z, k_plus_o
k_plus_do:
	ld (CONTRAST), a
	out (2), a
k_plus_o:
	jp &Key_OTH

;;;;;;;
; name:	Key_MINUS - increase contrast

Key_MINUS:
	ld a, (CONTRAST)
	and a
	jr z, k_plus_o
	dec a
	jr k_plus_do
#endif

;;;;;;;
; name:	Key_EXIT

Key_EXIT:
	jp CLEARLCD



;;;;;;;
; name: DispAll - initialises display and shows all entries

DispAll:
			; initialise display
	call CLEARLCD
	ld hl, 16 * LINE_SIZE + VIDEO_MEM
	ld de, 16 * LINE_SIZE + VIDEO_MEM + 1
	ld bc, 15
	ld (hl), 255
	push hl
	ldir
	pop hl
	ld de, 16 * STATUS_LINE + VIDEO_MEM - 16
	ld bc, 16
	ldir

	set 1, (iy+5)
	ld hl, 1
	ld (CURSOR_X), hl
	res 3, (iy+5)
#ifdef CONFIG_TOPLINE
			; print free mem
	ld hl, &Free
	call D_ZM_STR
	call FREEMEM
	call DM_HL_DECI
			; detect rom version
	ld hl, &Rom
	call D_ZM_STR
	ld de, 4
	ld a, (ROM_VERS)
	ld hl, &XLat1
	call &XLAT
	ld b, e
	call D_LM_STR
			; display USGard version
	ld hl, &USg
	call D_ZM_STR
	ld de, 7
	ld a, (USG_VER)
	ld hl, &XLat2
	call &XLAT
	ld b, e
	call D_LM_STR
#endif	; CONFIG_TOPLINE
	ld a, START_Y - LINE_SIZE
	ld (CURSOR_Y), a
			; display COUNT entries
	ld bc, (&Top)
	ld a, COUNT
luop:	push af
	ld a, 1
	ld (CURSOR_X), a
	ld a, (CURSOR_Y)
	add a, LINE_SIZE
	ld (CURSOR_Y), a
	call &DispOneEntry
	inc bc
	pop af
	dec a
	jr nz, luop
	ret


;;;;;;;
; name:	DispOneEntry - displays ONE variable entry
; in:	BC=index, CURSOR_X, CURSOR_Y
; out:  destroys all but BC, returns CARRY on error

DispOneEntry:
	push bc
	call &Find
	jr c, amen

#ifdef CONFIG_SIZE_AFTER_NAME
	ld hl, VATName
	call D_ZM_STR
	ld a, 64
	ld (CURSOR_X), a
#endif
	ld hl, (&VarStor + 8)
	dec hl
	ld a, (hl)
	dec hl
	ld l, (hl)
	ld h, a
	call DM_HL_DECI
#ifndef CONFIG_SIZE_AFTER_NAME
	ld a, 32
	ld (CURSOR_X), a
	ld hl, VATName
	call D_ZM_STR
#endif
	ld a, 92
	ld (CURSOR_X), a
	ld hl, (&VarStor)
	call D_ZM_STR

	pop bc

	ld de, (&Cursor)
	ld a, b
	cp d
	ret nz
	ld a, c
	cp e
	ret nz

	push bc
			; display the selected item and all info we know
	call &xorcurs

	ld hl, STATUS_LINE * 16 + VIDEO_MEM
	ld de, STATUS_LINE * 16 + VIDEO_MEM + 1
	ld bc, 16 * LINE_SIZE - 1
	ld (hl), 0
	ldir

	ld hl, STATUS_LINE * 256 + 1
	ld (CURSOR_X), hl
	ld hl, (&VarStor + 4)
	call D_ZM_STR
	ld hl, (&VarStor + 6)
	ld (CURSOR_X), hl
	pop bc
	and a
	ret
amen:
	pop bc
	scf
	ret


;;;;;;;
; name:	xorcurs, draws or deletes cursor rectangle
; in:	CURSOR_Y
; out:  destroys word at VarStor+6, A, HL; DE=VIDEO_MEM, B=0

xorcurs:
	ld hl, (CURSOR_X)
	ld (&VarStor + 6), hl
	ld l, h
	ld h, 0
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, hl
	ld de, VIDEO_MEM
	add hl, de
	ld b, 16*LINE_SIZE
xorcurl:ld a, (hl)
	xor 255
	ld (hl), a
	inc hl
	djnz xorcurl
	ret

;;;;;;;
; name:	XLAT, translation by the table
; in:	HL -> table, DE=lenght of data, A=parameter to translate
; out:	HL -> data, carry, destroys A, HL, D

XLAT:	ld d, a
	ld a, (hl)
	inc hl
	cp d
	ret z
	and a
	scf
	ret z
	ld a, d
	ld d, 0
	add hl, de
	jr XLAT


;;;;;;;
; name:	FIND, finds n-th variable
; in:	BC=variable index
; out:	VarStor, destroyed registers

Find:
FindString:
	ld hl, &RunString
	ld (&VarStor + 2), hl
	ld hl, VAT_START
FindNextString:
	ld a, $0c
	push bc
	ld (&VarStor + 6), hl	; VAT entry
	call SEARCH_VAT
	pop bc
#ifdef CONFIG_RUN_BASIC
	jr c, FindProg
#else
	ret c
#endif
	ld a, (de)
	and a
	jr nz, FindNextString

	ld (&VarStor + 8), de
	inc de

#ifdef CONFIG_SELF_FILTER
	push bc
	push de
	push hl
	ld de, VATName
	ld hl, &wishname
selfilter:
	ld a, (de)
	cp (hl)
	inc de
	inc hl
	jr nz, selfiltnot
	and a
	jr nz, selfilter
selfiltnot:
	pop hl
	pop de
	pop bc
	jr z, FindNextString
#endif
	ld a, (de)
	inc de
	inc de
	ld (&VarStor + 4), de

	cp $FD
	jr nz, oko1
	ld de, &TypeZS
	jr okoall
oko1:	cp PROG_HEADER
	jr nz, FindNextString
	ld de, &TypeUG
okoall:
	ld (&VarStor), de
	dec bc
	ld a, b
	or c
	ret z
	jr FindNextString

#ifdef CONFIG_RUN_BASIC
FindProg:
	ld hl, &VarStor
	ld de, &TypeBA
	ld (hl), e
	inc hl
	ld (hl), d
	inc hl
	ld de, &RunProg
	ld (hl), e
	inc hl
	ld (hl), d
	inc hl
	ld de, &FullBA
	ld (hl), e
	inc hl
	ld (hl), d

	ld hl, VAT_START
FindNextProg:
	ld a, $12
	push bc
	ld (&VarStor + 6), hl	; VAT entry
	call SEARCH_VAT
	pop bc
	ret c
	ld (&VarStor + 8), de
	ld de, VATName
	ld a, (de)
	cp '#'
	jr z, FindNextProg
	cp '!'
	jr z, FindNextProg
	dec bc
	ld a, b
	or c
	ret z
	jr FindNextProg
#endif

;;;;;;;
; name:	RunString - starts zshell and usgard programs
; in:	VarStor & VATName

RunString:
#ifdef CONFIG_CLRTXTMEM
	ld hl, TEXT_MEM
	ld de, TEXT_MEM + 1
	ld bc, 8 * 21 - 1
	ld (hl), 0
	ldir
	ld hl, TEXT_MEM2
	ld de, TEXT_MEM2 + 1
	ld bc, 8 * 21 - 1
	ld (hl), 0
	ldir
#endif
	ld hl, VATName
	call VAR_EXEC
	jr c, runstring_okolo
	ld a, 4
	out (5), a
	ld a, (USG_BITS)
	bit 1, a
	ret nz
runstring_okolo:
	jp &inputs

#ifdef CONFIG_RUN_BASIC

;;;;;;;
; name:	RunProg - starts TI Basic programs
; in:	VarStor & VATName

RunProg:
	ld hl, 0
	ld (CURSOR_ROW), hl
	ld hl, VATName
	ld bc, 65535
runprcnt:
	inc bc
	ld a, (hl)
	inc hl
	and a
	jr nz, runprcnt

	ld hl, &Vykricnik
	push bc
	call VAR_RESIZE
	pop bc
	ld ($8b17), bc
runb_errback:
	jp c, &inputs
	ld hl, &Vykricnik
	call VAR_GET
	jr c, runb_errback
	ld ($8b19), hl
	ld ($80c6), hl
	ex de, hl
	ld hl, VATName
	ldir
	ld hl, USG_BITS
	set 1, (hl)
runb_last:
	halt
	call GET_KEY
	and a
	jr nz, runb_last

	ld hl, (PROGRAM_ADDR)
	call CHECK_APPEND
	jr nz, installed

	ld hl, &tsr_basic
	ld de, (PROGRAM_ADDR)
	ld bc, tsrend - tsr_basic
	call APPEND
	jr c, runb_errback
	push hl
	call INT_INSTALL
	pop hl
	jr c, runb_errback

installed:
	ld de, smp2 + 1 - tsr_basic
	ex de, hl
	add hl, de
	ld (hl), e
	inc hl
	ld (hl), d
	ex de, hl
	ld (hl), 33
	ret

tsr_basic:
smp1:	.db 201			; will be rewritten to 33
	.dw _IY_TABLE
	bit 3, (hl)
	ret nz
	ld a, K_ENTER
	ld (KEY_0), a
	ld (LAST_KEY), a
	set 3, (hl)
smp2:	ld hl, 0		; will be rewritten to smp1
	ld (hl), 201
	ret
tsrend:
;;; [DATA] ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Vykricnik:
		.db	'!', 0

FullBA:		.db	"TI85 Basic "
TypeBA:		.db	"Program", 0
; CONFIG_RUN_BASIC section ends
#endif

TypeZS:		.db	"ZShell", 0
TypeUG:		.db	"USGard", 0

#ifdef CONFIG_TOPLINE
Free:		.db	"free:", 0

Rom:		.db	"  rom:", 0
XLat1:		.db	$2A, "2.0 "
		.db	$57, "3.0A"
		.db	$8E, "4.0 "
		.db	$5E, "5.0 "
		.db	$3D, "6.0 "
		.db	$97, "8.0 "
		.db	$91, "9.0 "
		.db	$35, "10.0"
		.db	$0,  "unkn"

USg:		.db	"  usg:", 0
XLat2:		.db	$24, "1.5lite"
		.db	$25, "1.5std "
		.db	$0,  "unknown"
#endif

XLat3:		.db	K_DOWN
		.dw	Key_DN
		.db	K_UP
		.dw	Key_UP
		.db	K_ENTER
		.dw	Key_CR
		.db	K_SECOND
		.dw	Key_CR
		.db	K_EXIT
		.dw	Key_EXIT
#ifdef CONFIG_DELETE
		.db	K_DEL
		.dw	Key_DEL
#endif
#ifdef CONFIG_CONTRAST
		.db	K_PLUS
		.dw	Key_PLUS
		.db	K_MINUS
		.dw	Key_MINUS
#endif
		.db	0
		.dw	Key_OTH


VarStor:	.dw	0	; [search -> TYPE]
		.dw	0	; [search -> RUN ]
		.dw	0	; [search -> FulN]
		.dw	0	; [search -> VAT ]
		.dw	0	; [search -> data]

Cursor:		.dw	0
Top:		.dw	0

#ifdef CONFIG_APD
APDorig:	.db	0
#endif

#ifdef CONFIG_DELETE
DelQuestion:	.db	"SURE?", 0
#endif

wishname:	.db	"wish", 0
wishend:

EndOfFile:
;;; [END] ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.end
