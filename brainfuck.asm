;---------------------------------------------------------------------
; Register Table:
; ~~~~~~~~~~~~~~~~
; R1 (UH) + R0 (LH):    Stack Pointer
;                       [During Parsing] (Volatile, Reserved)
;                       Null-Terminator Pointer For Brainfuck Code
;                       [After Parsing] (Permanent, Reserved)    
; R2:                   Backup for Symbol Validation (Temp)
; R5 (UH) + R4 (LH):    Table Pointer (Volatile, Reserved)
; R7 (UH) + R6 (LH):    DPTR Backup Registers (Volatile, Reserved)
; 
; ===================================================================
; Brainfuck Code with Newline Terminator
; ===================================================================
; Note: Since the brainfuck code definition is described here, it
;       can be found in the code memory starting at location 0x0006.
SYMBOLS: db '+-<>,.'
BRAINFUCK: db '[]>,>+>+>+<<<<-', 00h


; ===================================================================
; Flow Handling
; ===================================================================
;---------------------------------------------------------------------
; This function is the entry point to the program.
;
MAIN:
lcall PARSE              ; Parse


;---------------------------------------------------------------------
; This function is an endpoint that loops back to itself continuously
; and hence does never finish. Redirect here if your program effectively
; has come to an end
;
FINISH: sjmp FINISH


;---------------------------------------------------------------------
; This function is equivalent to FINISH above. However, it denotes
; that something has gone wrong.
;
ERROR: sjmp ERROR


; ===================================================================
; Parsing
; ===================================================================
;---------------------------------------------------------------------
; This function iterates over each symbol. It asserts the code's
; validity, tracks its length and constructs a bracket table used for 
; quickly executing loops.
;
; Overwrites: R1 (DPH) and R0 (DPL) as code length (permanent)
;
PARSE:
mov R0, #255d               ; Set XSTACK Pointer to end of external
mov R1, #255d               ; memory, so that it can grow downwards
mov DPTR, #0006h            ; Let DPTR point to start of code

_parse_read_next:
mov A,  #00h                ; Clear A, only DPTR should decide target
movc A, @A+DPTR             ; Load next symbol from code memory
jnz _parse_handle           ; Symbol is not terminator, so prepare
                            ; for next and read

; TODO Check for unbalanced brackets
mov R0, DPL                 ; Symbol is terminator, so backup DPTR
mov R1, DPH                 ; representing last symbol position into
ret                         ; R0 and R1

_parse_handle:
lcall IS_VALID_OPERATOR     ; Symbol not terminator, but is it valid?
jnz _parse_prepare_next     ; Validation successful, target next symbol

mov A, R2                   ; Restore A from R2 (backup done by validation
                            ; call but A not restored to old value because
                            ; used for output)
subb A, #5Bh                ; Subtract code for '[' from read symbol
clr C
jz _handle_open_bracket     ; Symbol is open bracket

mov A, R2                   ; Restore A from R2 (see above)
subb A, #5Dh                ; Subtract code for ']' from read symbol
clr C
jz _handle_closed_bracket   ; Symbol is closed bracket

lcall REPORT_INVALID_SYMBOL ; Symbol is invalid is gets reported

_handle_open_bracket:
lcall PUSH_OPEN_BRACKET     ; Push entry for open bracket
sjmp _parse_prepare_next    ; Read next symbol

_handle_closed_bracket:
lcall PUSH_CLOSED_BRACKET   ; Push entry for closed bracket

_parse_prepare_next:
lcall INC_DPTR              ; Let DPTR point to next symbol
sjmp _parse_read_next       ; Read next symbol


;---------------------------------------------------------------------
; This function "pushes" an entry for an opening bracket onto XSTACK. 
; The first two bytes are the symbol index (16-bit, DPTR with DPH and
; DPL) and the last two bytes encode the table entry pointer (16-bit, 
; R5 and R4).
;
PUSH_OPEN_BRACKET:
lcall PUSH_DPTR             ; Backup DPTR into R7 (UH) and R6 (LH)
mov DPL, R0                 ; Load lower half of XSTACK pointer into DPTR
mov DPH, R1                 ; Load upper half of XSTACK pointer into DPTR

mov A, R6                   ; Push lower half of backup DPTR onto XSTACK
movx @DPTR, A               
lcall INC_XSTACK            ; Grow XSTACK by one byte

mov A, R7                   ; Push upper half of backup DPTR onto XSTACK
movx @DPTR, A               
lcall INC_XSTACK            ; Grow XSTACK by one byte

mov A, R4                   ; Push lower half of table pointer
movx @DPTR, A               ; onto XSTACK
lcall INC_XSTACK            ; Grow XSTACK by one byte

mov A, R5                   ; Push upper half of table pointer
movx @DPTR, A               ; onto XSTACK
lcall INC_XSTACK            ; Grow XSTACK by one byte

lcall TABLE_NEXT_ENTRY      ; Move TPTR by one entry, so four byte
mov R0, DPL                 ; Load lower half of XSTACK pointer into R0
mov R1, DPH                 ; Load upper half of XSTACK pointer into R1
lcall POP_DPTR              ; Restore DPTR
ret


;---------------------------------------------------------------------
; This function resolves bracket pairs
;
PUSH_CLOSED_BRACKET:
; TODO: safe dptrbackup done by validation
                            ; call but A not restored to old value because
                            ; used for output)
; set dptr to xstack
; TODO: write r5 and r4 (this order) from xstack into r3 and r2
; set dptr to table ptr
; write r3 and r2 at table ptr
; table ptr += 2
; set dptr to xstack
; write r1 and r0 (this order) from xtsack into r3 and r2
; set dptr to table ptr
; write r3 and r2 at table ptr
; TODO: restore dptr
;


;---------------------------------------------------------------------
; This function checks that the byte in A is a valid brainfuck
; operator, so any of the symbols except for brackets.
;
; In:  A
; Out: A (0 = false, > 0 = true)
; 
; Overwrites: R2 as A backup (temporary),
;             R7 (DPH) and R6 (DPL) as DPTR backup (temporary)
;
IS_VALID_OPERATOR:
lcall PUSH_DPTR             ; Backup DPTR
mov R2, A                   ; Backup A into R2 for further use
                            ; Not restored into A, because it is used
                            ; for validation output flag
mov DPTR, #05h              ; Setup iterator index

_validate_symbol:
mov A, #00h                 ; Clear A, only DPTR should decide target
movc A, @A+DPTR             ; Load next symbol from code memory
mov B, A                    ; Load A into B
mov A, R2                   ; Restore A
subb A, B                   ; Subtract symbol from symbol under test
clr C                       
jz _is_valid                ; They are the same symbol (hence A valid)

lcall SQUASH_DPTR           ; Squash DPTR (A = 0 = 'false')
jz _validation_done         ; No symbol was matches yet and DPTR = 0
                            ; (no more left)

lcall DEC_DPTR              ; Decrement DPTR
sjmp _validate_symbol       ; There are some symbols left, so compare

_is_valid:
mov A, #01h                 ; Write 'true' into A

_validation_done:
lcall POP_DPTR              ; Restore DPTR
ret


; ===================================================================
; Error Handling
; ===================================================================
;---------------------------------------------------------------------
; This function reports an invalid symbol encountered during parsing.
; 
REPORT_INVALID_SYMBOL:
; TODO: Report in some way
lcall ERROR


;---------------------------------------------------------------------
; This function reports an unbalanced bracket encountered during parsing.
;
REPORT_UNBALANCED_BRACKET:
; TODO: Report in some way
lcall ERROR


; ===================================================================
; Table Pointer (TPTR) Handling
; ===================================================================
;---------------------------------------------------------------------
; This function moves TPTR by two bytes, so that it points
; to the following entry attribute.
; 
TABLE_NEXT_ATTRIBUTE:
lcall INC_TPTR              ; Increment TPTR by two byte
lcall INC_TPTR 
ret


;---------------------------------------------------------------------
; This function moves TPTR by four bytes, so that it points
; to the following table entry.
TABLE_NEXT_ENTRY:
lcall TABLE_NEXT_ATTRIBUTE  ; Increment TPTR by four byte
lcall TABLE_NEXT_ATTRIBUTE
ret


;---------------------------------------------------------------------
; This function increments TPTR by one. The pointer is 16-bit wide
; and is built using two 8-bit registers (R5 (UH) and R4 (LH)) that
; are not connected with each other.
;
; Note: Destroys C(arry) Flag (begin and end)
;
INC_TPTR:
clr C                       ; Clear carry for later changes
push A                      ; Backup A onto stack

mov A, R4                   ; Load lower half R4 into A
add A, #01h                 ; Increment A (carry might be set!)

jnc _store_lower_tptr       ; When carry not set, the upper half R5
                            ; will not be affected and the lower half
                            ; can be stored directly

inc R5                      ; Increment upper half (carry not affected)
clr C                       ; Clear C (might be set by increment of A)
                            ; so that is does not interfere with
                            ; following subb

_store_lower_tptr:
mov R4, A                   ; Load lower half from A
pop A                       ; Restore A from stack
ret


; ===================================================================
; XSTACK Pointer Handling
; ===================================================================
;---------------------------------------------------------------------
; This function "decrements" the XSTACK pointer. Actually, it
; increments the pointer, but the XSTACK grows downwards. Hence,
; that name is more fitting with what effectively happens.
; 
; Furthermore, it is assumed that the XSTACK pointer is currently
; active in the DPTR register(s).
; 
; Note: Destroys C(arry) Flag (begin and end)
;
DEC_XSTACK:
lcall INC_DPTR              ; Increment DPTR, so that the stack shrinks
ret


;---------------------------------------------------------------------
; This function "increments" the XSTACK pointer. Actually, it
; decrements the pointer, but the XSTACK grows downwards. Hence,
; that name is more fitting with what effectively happens.
; 
; Furthermore, it is assumed that the XSTACK pointer is currently
; active in the DPTR register(s).
; 
; Note: Destroys C(arry) Flag (begin and end)
;
INC_XSTACK:
lcall DEC_DPTR              ; Decrement DPTR, so that the stack grows
ret


; ===================================================================
; DPTR Handling
; ===================================================================
;---------------------------------------------------------------------
; This function "pushes" DPTR by storing it into R7 (DPH) and R6 (DPL)
; in order to avoid pushing onto the regular stack. Doing so makes it
; difficult to retrieve the two DPTR bytes on top of the stack using
; a function, as the return address is also pushed, burrying the
; wanted data.
;
PUSH_DPTR:
mov R6, DPL                 ; Store lower half (DPL) into R6
mov R7, DPH                 ; Store upper half (DPH) into R7
ret


;---------------------------------------------------------------------
; This function "pops" DPTR from R7 (DPH) and R6 (DPL). For further
; explanation, see PUSH_DPTR above.
; 
POP_DPTR:
mov DPL, R6                 ; Restore lower half (DPL) from R6
mov DPH, R7                 ; Restore upper half (DPH) from R7
ret


;---------------------------------------------------------------------
; This function squashes DPTR by applying a logical or to both its
; bytes. Can be used to subsequently check if DPTR is actually zero.
;
SQUASH_DPTR:
mov A, DPH                  ; Load upper half of DPTR into A
mov B, DPL                  ; Load lower half of DPTR into B
orl A, B                    ; Apply logical or to both halfs
ret


;---------------------------------------------------------------------
; This function decrements DPTR by one. Needed because DPTR
; is 16-bit wide and no decrementation is defined for it.
; 
; Note: Destroys C(arry) Flag (begin and end)
;
DEC_DPTR:
clr C                       ; Clear carry for later changes
push A                      ; Backup A onto stack

mov A, DPL                  ; Load lower half into A
subb A, #01h                ; Decrement A (carry might be set!)

jnc _store_lower_dptr       ; When carry not set, the upper half DPH
                            ; will not be affected and the lower half
                            ; can be stored directly

dec DPH                     ; Decrement upper half (carry not affected)
clr C                       ; Clear C (might be set by decrement of A)
                            ; so that it does not interfere with
                            ; following subb			

_store_lower_dptr:
mov DPL, A                  ; Load lower half from A
pop A                       ; Restore A from stack
ret


;---------------------------------------------------------------------
; This function increments DPTR by one. Needed because DPTR
; is 16-bit wide and no incrementation is defined for it.
;
; Note: Destroys C(arry) Flag (begin and end)
;
INC_DPTR:
clr C                       ; Clear carry for later changes
push A                      ; Backup A onto stack

mov A, DPL                  ; Load lower half into A
add A, #01h                 ; Increment A (carry might be set!)

jnc _store_lower_dptr       ; When carry not set, the upper half DPH
                            ; will not be affected and the lower half
                            ; can be stored directly

inc DPH                     ; Increment upper half (carry not affected)
clr C                       ; Clear C (might be set by increment of A)
                            ; so that is does not interfere with
                            ; following subb

sjmp _store_lower_dptr      ; Load lower half from A


; ===================================================================
; Data Area Clear
; ===================================================================
;---------------------------------------------------------------------
; This function clears up the bracket table (#0000 - #07FF)
; and the storage cells (#0800-#08FF).
;
CLEAR_DATA_AREA:
mov DPTR, #08FFh            ; Define loop counter (256 bracket pairs,
                            ; with 4 bytes each and 256 storage cells
                            ; -> size = 2304 byte)

_clear_next:
mov A, #00h                 ; Load overwriting value
movx @DPTR, A               ; Overwrite target byte @DPTR

lcall SQUASH_DPTR           ; Squash DPTR
jz _clear_done              ; If A = 0, then DPTR = 0 and the last
                            ; byte has been overwritten

lcall DEC_DPTR              ; Decrement DPTR
sjmp _clear_next            ; Overwrite next byte             

_clear_done:
ret
