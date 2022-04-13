;---------------------------------------------------------------------
; Register Table:
; ~~~~~~~~~~~~~~~~
; Memory Bank [0]:
; R7 (UH) + R6 (LH):    DPTR Backup (Volatile, Reserved)
; R5 (UH) + R4 (LH):    TPTR Backup (Volatile, Reserved)
; R1 (UH) + R0 (LH):    XSTACK Backup
;                       [During Parsing] (Volatile, Reserved)
;                       End Pointer of Code
;                       [After Parsing] (Permanent, Reserved)    
; 
; Memory Bank [3]:
; R7 (UH) + R6 (LH):    TPTR Before Close Bracket Entry
; R5 (UH) + R4 (LH):    TPTR After Close Bracket Entry
; R3 (UH) + R2 (LH):    TPTR Of Open Bracket
; R1 (UH) + R0 (LH):    Symbol Pointer
; ===================================================================
; Brainfuck Code with Newline Terminator
; ===================================================================
; Note: Since the brainfuck code definition is described here, it
;       can be found in the code memory starting at location 0x0000.
;
CODE:  DB '+[.[]]', 00h


; ===================================================================
; Flow Handling
; ===================================================================
;---------------------------------------------------------------------
; This function is the entry point to the program.
;
MAIN:
LCALL   USE_BANK0
;LCALL   CLEAR_DATA_AREA
LCALL   PARSE


;---------------------------------------------------------------------
; This function is an endpoint that loops back to itself continuously
; and hence does never finish. Redirect here if your program
; effectively has come to an end
;
FINISH: SJMP FINISH


;---------------------------------------------------------------------
; This function is equivalent to FINISH above. However, it denotes
; that something has gone wrong.
;
ERROR: SJMP ERROR


; ===================================================================
; Parsing
; ===================================================================
;---------------------------------------------------------------------
; This function iterates over the code and parses it.
;
; It checks for validity, tracks the code's length and constructs a
; bracket table used for quickly executing loops during interpretation.
;
; Overwrites: R1 (DPH) and R0 (DPL) as code length (permanent)
;
PARSE:
; ==- Prelude
LCALL   INIT_XSTACK
MOV     DPTR, #0000h   

_parse_next_symbol:
MOV     A, #00h
MOVC    A, @A+DPTR          ; Load symbol from code memory
JNZ     _parse_process      ; Read symbol is null-terminator

; ==- Parsing Exit
; TODO: Check for unbalanced brackets
MOV     R0, DPL             ; Backup terminator location from DPTR
MOV     R1, DPH
LCALL   FINISH

; ==- Process Symbol
_parse_process:
LCALL   IS_VALID_OPERATOR
JB      F0, _parse_prepare  ; Valid symbol, ready for next

_parse_opened_bracket:
CJNE    A, #5Bh, _parse_closed_bracket  ; Check if symbol is opened bracket 
LCALL   HANDLE_OPENED_BRACKET           ; Handle opened bracket
SJMP    _parse_prepare                  ; Ready for next symbol

_parse_closed_bracket:
CJNE    A, #5Dh, _parse_invalid         ; Check if symbol is closed bracket
LCALL   HANDLE_CLOSED_BRACKET           ; Handle closed bracket
SJMP    _parse_prepare                  ; Ready for next symbol

_parse_invalid:
LCALL   REPORT_INVALID_SYMBOL

_parse_prepare:
LCALL   INC_DPTR            ; Point to next symbol
SJMP    _parse_next_symbol


;---------------------------------------------------------------------
; This function checks whether the byte in register A is a valid
; brainfuck operator (exception bracket).
;
; In:  A
; Out: F0 flag (false = '0', true = '1')
;
IS_VALID_OPERATOR:
CLR     F0                  ; Output is false (symbol not validated)
_is_plus:
CJNE    A, #2Bh, _is_comma
SJMP    _is_valid

_is_comma:
CJNE    A, #2Ch, _is_minus
SJMP    _is_valid

_is_minus:
CJNE    A, #2Dh, _is_point
SJMP    _is_valid

_is_point:
CJNE    A, #2Eh, _is_left
SJMP    _is_valid

_is_left:
CJNE    A, #3Ch, _is_right
SJMP    _is_valid

_is_right:
CJNE    A, #3Eh, _exit_validation

_is_valid:
SETB    F0                  ; Output is true (symbol validated)

_exit_validation:
RET


;---------------------------------------------------------------------
; This function "pushes" a table entry encoding an opened bracket
; onto the XSTACK.
;
; The first two pushed bytes encode the symbol index, which is 
; described by DPTR (DPH and DPL). The last two encode the table entry
; pointer (R5 and R4):
;
;   +--------+--------+--------+--------+--------+--------+
;   |  BASE  |  ...   |  DPL   |  DPH   |   R4   |   R5   |
;   +--------+--------+--------+--------+--------+--------+
;   ^                                                     ^  
; START                                           XSTACK TOP POINTER
;
HANDLE_OPENED_BRACKET:
; ==- Prelude
LCALL PUSH_DPTR             ; Backup DPTR
LCALL POP_XSTACK            ; Restore XSTACK (into DPTR)

; ==- Write Table Entry For Open Bracket
MOV A, R6                   ; Push LH of backup DPTR
MOVX @DPTR, A               
LCALL INC_XSTACK            ; Move XSTACK pointer

MOV A, R7                   ; Push UH of backup DPTR
MOVX @DPTR, A               
LCALL INC_XSTACK

MOV A, R4                   ; Push LH of TPTR
MOVX @DPTR, A
LCALL INC_XSTACK

MOV A, R5                   ; Push UH of TPTR
MOVX @DPTR, A
LCALL INC_XSTACK

; ==- Move TPTR To End
LCALL PUSH_XSTACK           ; Backup XSTACK into R1 (UH) and R0 (LH)
LCALL POP_TPTR              ; Restore TPTR
LCALL TABLE_NEXT_ENTRY      ; Move TPTR by one entry
LCALL PUSH_TPTR             ; Backup TPTR

; ==- Clean-Up
LCALL POP_DPTR              ; Restore DPTR
RET


;---------------------------------------------------------------------
; This function "pops" the corresponding bracket from the XSTACK into
; some registers. Then, a new entry for the closed bracket is written
; into the table referencing the open bracket. Lastly, the entry for
; the open bracket is modified to point to the closed bracket.
;
HANDLE_CLOSED_BRACKET:
; ==- Prelude
LCALL PUSH_DPTR             ; Backup DPTR into R7 (UH) and R6 (LH)
LCALL POP_XSTACK            ; Restore XSTACK into DPTR
LCALL USE_BANK3             ; Select 1st memory bank

; ==- Read topmost table entry from XSTACK
LCALL DEC_XSTACK            ; Shrink XSTACK
MOVX A, @DPTR               ; Load topmost value into A
MOV R3, A                   ; Store topmost value into R5

LCALL DEC_XSTACK
MOVX A, @DPTR   
MOV R2, A

LCALL DEC_XSTACK
MOVX A, @DPTR
MOV R1, A

LCALL DEC_XSTACK
MOVX A, @DPTR
MOV R0, A

; ==- Prepare for write
LCALL USE_BANK0
LCALL PUSH_XSTACK
LCALL POP_TPTR
LCALL USE_BANK3

; ==- Store start of entry
MOV R6, DPL
MOV R7, DPH

; ==- Write closed bracket entry into table
MOV A, R3
MOVX @DPTR, A
LCALL INC_TPTR

MOV A, R2
MOVX @DPTR, A
LCALL INC_TPTR

MOV A, R1
MOVX @DPTR, A
LCALL INC_TPTR

MOV A, R0
MOVX @DPTR, A
LCALL INC_TPTR

; ==- Backup TPTR and move back to open bracket entry
LCALL PUSH_TPTR             ; Backup TPTR on 1st memory bank
MOV DPL, R2                 ; Load TPTR of open bracket
MOV DPH, R3

; ==- Overwrite entry of corresponding open bracket
MOV A, R7
MOVX @DPTR, A
LCALL INC_TPTR

MOV A, R6
MOVX @DPTR, A
LCALL INC_TPTR

LCALL USE_BANK0

MOV A, R7
MOVX @DPTR, A
LCALL INC_TPTR

MOV A, R6
MOVX @DPTR, A

LCALL USE_BANK3

; ==- Move TPTR back to end of table
LCALL POP_TPTR

; ==- Clean-Up
LCALL USE_BANK0             ; Select 0th memory bank
LCALL PUSH_TPTR
LCALL POP_DPTR              ; Restore DPTR
RET


; ===================================================================
; Table Pointer (TPTR) Handling
; ===================================================================
;---------------------------------------------------------------------
; This function "pushes" TPTR by storing it into R5 (UH) and R4 (LH)
; in order to avoid pushing onto the regular stack. Doing so makes it
; difficult to retrieve the two TPTR bytes on top of the stack using
; a function, as the return address is also pushed, burrying the
; wanted data.
;
PUSH_TPTR:
MOV     R4, DPL
MOV     R5, DPH
RET


;---------------------------------------------------------------------
; This function "pops" TPTR from R5 (UH) and R4 (LH). For further
; explanation, see PUSH_TPTR above.
; 
POP_TPTR:
MOV     DPL, R4
MOV     DPH, R5
RET


;---------------------------------------------------------------------
; This function moves TPTR by four bytes, so that it points to the
; next table entry.
;
TABLE_NEXT_ENTRY:
LCALL   INC_DPTR
LCALL   INC_DPTR
LCALL   INC_DPTR
LCALL   INC_DPTR
RET
 

;---------------------------------------------------------------------
; This function increments TPTR by one.
; There is no decrement defined for 16-bit values. 
;
INC_TPTR:
LCALL   INC_DPTR
RET


; ===================================================================
; XSTACK Pointer Handling
; ===================================================================
;---------------------------------------------------------------------
; This function initializes the XSTACK pointer to start at the end
; of the external memory, so that it can grow downwards.
;
INIT_XSTACK:
MOV     R0, #255d
MOV     R1, #255d
RET


;---------------------------------------------------------------------
; This function "pushes" XSTACK by storing it into R1 (UH) and R0 (LH)
; in order to avoid pushing onto the regular stack. Doing so makes it
; difficult to retrieve the two XSTACK bytes on top of the stack using
; a function, as the return address is also pushed, burrying the
; wanted data.
;
PUSH_XSTACK:
MOV     R0, DPL
MOV     R1, DPH
RET


;---------------------------------------------------------------------
; This function "pops" XSTACK from R1 (UH) and R0 (LH). For further
; explanation, see PUSH_XSTACK above.
; 
POP_XSTACK:
MOV     DPL, R0
MOV     DPH, R1
ret


;---------------------------------------------------------------------
; This function "decrements" the XSTACK pointer. Actually, the DPTR
; pointer register the XSTACK pointer is contained within while
; working with it, is incremented. That is because, the XSTACK here
; grows downwards, but it is intuitive and normally abstracted that
; it does the opposite way.
; 
; Note: Destroys C(arry) Flag (begin and end)
;
DEC_XSTACK:
LCALL   INC_DPTR            ; Incrementing the DPTR moves the XSTACK
RET                         ; pointer closer to the stack's base,
                            ; effectively shrinking it


;---------------------------------------------------------------------
; This function "increments" the XSTACK pointer. For further and
; analogous explanation, see DEC_XSTACK above.
; 
; Note: Destroys C(arry) Flag (begin and end)
;
INC_XSTACK:
LCALL   DEC_DPTR            ; Decrementing the DPTR moves the XSTACK
RET                         ; pointer farther from the stack's base,
                            ; effectively growing it


; ===================================================================
; DPTR Handling
; ===================================================================
;---------------------------------------------------------------------
; This function "pushes" DPTR by storing it into R7 (UH) and R6 (LH)
; in order to avoid pushing onto the regular stack. Doing so makes it
; difficult to retrieve the two DPTR bytes on top of the stack using
; a function, as the return address is also pushed, burrying the
; wanted data.
;
PUSH_DPTR:
MOV     R6, DPL
MOV     R7, DPH
RET


;---------------------------------------------------------------------
; This function "pops" DPTR from R7 (UH) and R6 (LH). For further
; explanation, see PUSH_DPTR above.
; 
POP_DPTR:
MOV     DPL, R6
MOV     DPH, R7
RET


;---------------------------------------------------------------------
; This function reduces DPTR to a bit mask by applying a logical or
; to its lower and upper half. That can be used to check if DPTR is
; zero.
;
OR_DPTR:
MOV     A, DPL
MOV     B, DPH
ORL     A, B
RET


;---------------------------------------------------------------------
; This function decrements DPTR by one.
; There is no decrement defined for 16-bit values.
; 
; Note: Destroys C(arry) Flag (begin and end)
;
DEC_DPTR:
CLR     C                   ; Remove unrelated carry
PUSH    A                   ; Backup A

MOV     A, DPL              ; Decrement lower half
SUBB    A, #01h

JNC     _store_lower_dptr   ; When no carry, the upper half
                            ; is unaffected

DEC     DPH                 ; Decrement upper half
CLR     C                   ; Clear carry of SUBB		

_store_lower_dptr:
MOV     DPL, A              ; Store lower half
POP     A                   ; Restore A
RET


;---------------------------------------------------------------------
; This function increments DPTR by one.
; There is no increment defined for 16-bit values.
;
; Note: Destroys C(arry) Flag (begin and end)
;
INC_DPTR:
CLR     C                   ; Remove unrelated carry           
PUSH    A                   ; Backup A

MOV     A, DPL              ; Increment lower half
ADD     A, #01h
JNC     _store_lower_dptr   ; When no carry, the upper half
                            ; is unaffected

INC     DPH                 ; Increment upper half
CLR     C                   ; Clear carry of ADD

SJMP    _store_lower_dptr   ; Store lower half


; ===================================================================
; Data Area Clear
; ===================================================================
;---------------------------------------------------------------------
; This function clears up the bracket table (#0000 - #07FF) and the
; storage cells (#0800-#08FF). The table contains 256 bracket pairs,
; two brackets each, summing to 512 entries. Each entry occupies four
; bytes. There are 256 storage cells and they are one byte wide each.
;
CLEAR_DATA_AREA:
MOV     DPTR, #08FFh

_clear_next_byte:
MOV     A, #00h             ; Zero out target byte
MOVX    @DPTR, A

LCALL   OR_DPTR             ; Or both bytes of DPTR and exit
JZ      _exit_clear         ; if DPTR has reached zero

LCALL   DEC_DPTR
SJMP    _clear_next_byte

_exit_clear:
RET


; ===================================================================
; Memory Bank Selection
; ===================================================================
;---------------------------------------------------------------------
; This function selects the 0th memory bank.
;
USE_BANK0:
CLR     RS0
CLR     RS1
RET

;---------------------------------------------------------------------
; This function selects the 3rd memory bank.
;
USE_BANK3:
SETB    RS0
SETB    RS1
RET


; ===================================================================
; Error Handling
; ===================================================================
;---------------------------------------------------------------------
; This function reports an invalid symbol encountered during parsing.
; 
REPORT_INVALID_SYMBOL:
; TODO: Report invalid symbol in some manner
LCALL   ERROR


;---------------------------------------------------------------------
; This function reports an unbalanced bracket encountered during parsing.
;
REPORT_UNBALANCED_BRACKET:
; TODO: Report unbalanced bracket in some way
LCALL   ERROR


;---------------------------------------------------------------------
; This function reports too many brackets encountered during parsing.
;
REPORT_TOO_MANY_BRACKETS:
; TODO: Report too many brackets in some way
LCALL   ERROR
