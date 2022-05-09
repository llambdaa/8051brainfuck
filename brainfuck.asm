;---------------------------------------------------------------------
; Register Table:
; ~~~~~~~~~~~~~~~~
; Memory Bank [0] (Parsing):
; R7 (UH) + R6 (LH):    DPTR Backup (Symbol Pointer)
; R5 (UH) + R4 (LH):    TPTR Backup
; R3 (UH) + R2 (LH):    Cell Pointer (CPTR) Backup
; R1 (UH) + R0 (LH):    XSTACK Backup
; 
; Memory Bank [0] (Error Report):
; R7 (UH) + R6 (LH):    DPTR Backup (Symbol Pointer)
; R4:                   Double Dabble Iterator
; R3:                   Digit Backup During Incrementation
; R2 (LH):              5. Digit
; R1 (UH) + R1 (LH):    4. Digit and 3. Digit
; R0 (UH) + R0 (LH):    2. Digit and 1. Digit
;
; Memory Bank [3]:
; R7 (UH) + R6 (LH):    TPTR Before Close Bracket Entry
; R5 (UH) + R4 (LH):    TPTR After Close Bracket Entry
; R3 (UH) + R2 (LH):    Symbol Pointer
; R1 (UH) + R0 (LH):    TPTR Of Open Bracket
;
;
; ===================================================================
; Brainfuck Code with Newline Terminator
; ===================================================================
ORG 0500h
DSTART  EQU 0500h
;---------------------------------------------------------------------
CODE:  DB '++[++', 00h


; ===================================================================
; Error Message Definitions
; ===================================================================
INVALID_SYMBOL:     DB 'Invalid symbol ', 00h
UNCLOSED_BRACKET:   DB 'Unclosed bracket', 00h
UNOPENED_BRACKET:   DB 'Unopened bracket', 00h
TOO_MANY_PAIRS:     DB 'Too many pairs', 00h
AT_INDEX:           DB 'at index ', 00h


; ===================================================================
; Constant Port Definitions
; ===================================================================
RS      EQU P0.0
RW      EQU P0.1
EN      EQU P0.2
PD      EQU P2


; ===================================================================
; Flow Handling 
; ===================================================================
;---------------------------------------------------------------------
; This function is the entry point to the program.
;
ORG 0000h
MAIN:

ACALL   LCD_INIT
ACALL   USE_BANK0
;ACALL   CLEAR_DATA_AREA
ACALL   PARSE
ACALL   INTERPRET


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
; Overwrites: A, F0
; Out: R0, R1, R4, R5, R6, R7
;
PARSE:
;-----------------------------
; Prelude
ACALL   INIT_XSTACK
MOV     DPTR, #DSTART
;-----------------------------
; Read symbol
_parse_next_symbol:
MOV     A, #00h
MOVC    A, @A+DPTR          ; Load symbol from code memory
JNZ     _parse_process      ; Read symbol is null-terminator

CJNE    R0, #0FFh, _parse_unbalanced_bracket    ; Check if XSTACK is empty
CJNE    R1, #0FFh, _parse_unbalanced_bracket    ; If empty, brackets are  
RET                                             ; balanced, everything is fine
;-----------------------------
; Error exit
_parse_unbalanced_bracket:
ACALL   REPORT_UNCLOSED_BRACKET
;-----------------------------
; Process symbol
_parse_process:
ACALL   IS_VALID_OPERATOR               ; Check if symbol is valid operator
JB      F0, _parse_prepare              ; Jump if it is and process next symbol

_parse_opened_bracket:
CJNE    A, #5Bh, _parse_closed_bracket  ; Check if symbol is opened bracket 
ACALL   HANDLE_OPENED_BRACKET           ; Handle opened bracket
SJMP    _parse_prepare                  ; Jump to read next symbol

_parse_closed_bracket:
CJNE    A, #5Dh, _parse_invalid_symbol  ; Check if symbol is closed bracket
ACALL   HANDLE_CLOSED_BRACKET           ; Handle closed bracket
SJMP    _parse_prepare                  ; Jump to read next symbol
;-----------------------------
; Error exit
_parse_invalid_symbol:
ACALL   REPORT_INVALID_SYMBOL
;-----------------------------
_parse_prepare:
ACALL   INC_DPTR                        ; Point to next symbol
SJMP    _parse_next_symbol              ; Jump to read next symbol


;---------------------------------------------------------------------
; This function checks whether the byte in the input register A is a
; valid brainfuck operator (except for bracket).
;
; In:  A
; Out: F0 (false = '0', true = '1')
;
IS_VALID_OPERATOR:
CLR     F0                  ; Output to be determined
_v_is_plus:
CJNE    A, #2Bh, _v_is_minus
SJMP    _is_valid

_v_is_minus:
CJNE    A, #2Dh, _v_is_right
SJMP    _is_valid

_v_is_right:
CJNE    A, #3Eh, _v_is_left
SJMP    _is_valid

_v_is_left:
CJNE    A, #3Ch, _v_is_point
SJMP    _is_valid

_v_is_point:
CJNE    A, #2Eh, _v_is_comma
SJMP    _is_valid

_v_is_comma:
CJNE    A, #2Ch, _exit_validation

_is_valid:
SETB    F0                  ; Content of A is any valid operator

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
;   |  BASE  |  ...   |   R4   |   R5   |  DPL   |  DPH   |
;   +--------+--------+--------+--------+--------+--------+
;   ^                                                     ^  
; START                                           XSTACK TOP POINTER
;
HANDLE_OPENED_BRACKET:
;-----------------------------
; Check bracket count
CJNE    R5, #08h, _handle_ob        ; Check upper half to TPTR to see
ACALL   REPORT_TOO_MANY_PAIRS       ; if bracket entry is beyond table
                                    ; boundary
;-----------------------------
; Prelude
_handle_ob:
ACALL   PUSH_DPTR           ; Backup DPTR
ACALL   POP_XSTACK          ; Restore XSTACK (into DPTR)
;-----------------------------
; Write opened bracket entry onto XSTACK
ACALL   WRITE_OPENED_BRACKET
;-----------------------------
; Move TPTR to end of table
ACALL   PUSH_XSTACK         ; Backup XSTACK
ACALL   POP_TPTR            ; Restore TPTR
ACALL   TABLE_NEXT_ENTRY    ; Move TPTR by one entry (write empty entry)
ACALL   PUSH_TPTR           ; Backup TPTR
;-----------------------------f
; Cleanup
ACALL   POP_DPTR            ; Restore DPTR
RET


;---------------------------------------------------------------------
; This function writes an entry for the currently read opened bracket.
;
WRITE_OPENED_BRACKET:
MOV     A, R4               ; Push LH of TPTR
MOVX    @DPTR, A
ACALL   INC_XSTACK

MOV     A, R5               ; Push UH of TPTR
MOVX    @DPTR, A
ACALL   INC_XSTACK

MOV     A, R6               ; Push LH of backup DPTR
MOVX    @DPTR, A               
ACALL   INC_XSTACK

MOV     A, R7               ; Push UH of backup DPTR
MOVX    @DPTR, A               
ACALL   INC_XSTACK
RET


;--------------------------------------------------------------------- 
; This function "pushes" a table entry encoding an closed bracket
; onto the XSTACK.
; 
; It "pops" the corresponding bracket from the XSTACK into several
; registers. Then, a new entry for the closed bracket is written into
; the table. That entry references the open bracket entry. Lastly, the
; entry for the open bracket is modified to point back to the closed
; bracket entry.
;
HANDLE_CLOSED_BRACKET:
;-----------------------------
; Check stack size
CJNE    R0, #0FFh, _handle_cb     ; Check if XSTACK is empty
CJNE    R1, #0FFh, _handle_cb     ; If it is, the closing bracket has
ACALL   REPORT_UNOPENED_BRACKET   ; no corresponding open bracket
;-----------------------------
; Prelude
_handle_cb:
ACALL   PUSH_DPTR           ; Backup DPTR
ACALL   POP_XSTACK          ; Restore XSTACK (into DPTR)
ACALL   USE_BANK3           ; Select 1st memory bank
;-----------------------------
; Read topmost stack entry
ACALL   READ_TOPMOST_ENTRY
;-----------------------------
; Prepare for table modifications
ACALL   PUSH_XSTACK         ; Backup XSTACK
ACALL   POP_TPTR            ; Restore TPTR                

ACALL   USE_BANK3           ; Select 3rd memory bank (bracket bank)
MOV     R6, DPL             ; Backup DPTR (currently pointing to start
MOV     R7, DPH             ; of closed bracket entry before actually
                            ; writing it
;-----------------------------
; Write closed bracket table entry
ACALL   WRITE_CLOSED_BRACKET
;-----------------------------
; Prepare for overwriting open bracket table
ACALL   PUSH_TPTR           ; Backup TPTR into 3rd memory bank
MOV     DPL, R0             ; Load TPTR of opened bracket 
MOV     DPH, R1
;-----------------------------
; Overwrite open bracket entry
ACALL   OVERWRITE_OPENED_BRACKET
;-----------------------------
; Move TPTR back to end of table
ACALL   POP_TPTR
;-----------------------------
; Cleanup
ACALL   USE_BANK0           ; Select 0th memory bank
ACALL   PUSH_TPTR           ; Backup TPTR (after writing entry)
ACALL   POP_DPTR            ; Restore DPTR
RET


;---------------------------------------------------------------------
; This function reads the symbol pointer from the XSTACK.
;
; In:           R0, R1
; Overwrite:    A
; Out:          R2, R3
;
READ_SYMBOL_POINTER:
;-----------------------------
; Prelude
ACALL   USE_BANK3           ; Select 3rd memory bank
;-----------------------------
; Read entry
ACALL   DEC_XSTACK          ; Shrink XSTACK
MOVX    A, @DPTR            ; Read symbol pointer (UH)
MOV     R3, A

ACALL   DEC_XSTACK          ; Shrink XSTACK
MOVX    A, @DPTR            ; Read symbol pointer (LH)
MOV     R2, A               
RET

;---------------------------------------------------------------------
; This function reads the topmost XSTACK table entry into the 3rd
; memory bank (R0-R3).
;
READ_TOPMOST_ENTRY:
ACALL   READ_SYMBOL_POINTER ; Read symbol pointer (bank 3 already set)
ACALL   DEC_XSTACK          ; Shrink XSTACK
MOVX    A, @DPTR            ; Read TPTR (UH)
MOV     R1, A

ACALL   DEC_XSTACK          ; Shrink XSTACK
MOVX    A, @DPTR            ; Read TPTR (LH)
MOV     R0, A
;-----------------------------
; Cleanup
ACALL   USE_BANK0
RET


;---------------------------------------------------------------------
; This function overwrites the entry for the corresponding opened
; bracket.
;
OVERWRITE_OPENED_BRACKET:
ACALL   USE_BANK0           ; Select 0th memory bank
MOV     A, R7               ; Load symbol pointer (UH) 
MOVX    @DPTR, A            ; Write to table
ACALL   INC_TPTR            ; Increment table pointer

MOV     A, R6               ; Load symbol pointer (LH)
MOVX    @DPTR, A            ; Write to table
ACALL   INC_TPTR            ; Increment table pointer

ACALL   USE_BANK3           ; Select 3rd memory bank
MOV     A, R7               ; Load TPTR (UH) before closed bracket
MOVX    @DPTR, A            ; Write to table
ACALL   INC_TPTR            ; Increment table pointer

MOV     A, R6               ; Load TPTR (LH) before closed bracket
MOVX    @DPTR, A            ; Write to table
RET


;---------------------------------------------------------------------
; This function writes an entry for the currently read closed bracket.
;
WRITE_CLOSED_BRACKET:
MOV     A, R3               ; Load values read from stack in step
MOVX    @DPTR, A            ; before into new table entry
ACALL   INC_TPTR

MOV     A, R2
MOVX    @DPTR, A
ACALL   INC_TPTR

MOV     A, R1
MOVX    @DPTR, A
ACALL   INC_TPTR

MOV     A, R0
MOVX    @DPTR, A
ACALL   INC_TPTR
RET


; ===================================================================
; Interpretation
; ===================================================================
;---------------------------------------------------------------------
; This function interprets the brainfuck code using the bracket table.
;
; Overwrites:   A
;
INTERPRET:
;-----------------------------
; Prelude
MOV     DPTR, #DSTART       ; Set DPTR to start of code
ACALL   PUSH_TPTR           ; Backup TPTR to point to table start
ACALL   INIT_CPTR           ; Initialize CPTR
;-----------------------------
; Read symbol
_interp_next_symbol:
MOV     A, #00h
MOVC    A, @A+DPTR          ; Load symbol from code memory
JNZ     _interp_symbol      ; Read symbol is null-terminator
;-----------------------------
; Interpretation exit
ACALL FINISH
;-----------------------------
; Interpret symbol
_interp_symbol:
_i_is_plus:
CJNE    A, #2Bh, _i_is_minus
ACALL   INC_CELL
JMP    _interp_prepare

_i_is_minus:
CJNE    A, #2Dh, _i_is_right
ACALL   DEC_CELL
SJMP    _interp_prepare

_i_is_right:
CJNE    A, #3Eh, _i_is_left
ACALL   MOVE_RIGHT
SJMP    _interp_prepare

_i_is_left:
CJNE    A, #3Ch, _i_is_opened_b
ACALL   MOVE_LEFT
SJMP    _interp_prepare

_i_is_opened_b:
CJNE    A, #5Bh, _i_is_closed_b
ACALL   INTERP_OPENED_BRACKET
SJMP    _interp_prepare

_i_is_closed_b:
CJNE    A, #5Dh, _i_is_point
ACALL   INTERP_CLOSED_BRACKET
SJMP    _interp_prepare

_i_is_point:
CJNE    A, #2Eh, _i_is_comma
ACALL   PRINT_CHAR
SJMP    _interp_prepare

_i_is_comma:
CJNE    A, #2Ch, _interp_error
NOP

_interp_prepare:
;-----------------------------
; Prepare for next symbol
ACALL   INC_DPTR            ; Increment symbol pointer
SJMP _interp_next_symbol    ; Jump to read next symbol
;-----------------------------
; Error exit
_interp_error:
LJMP    ERROR


;---------------------------------------------------------------------
; This function increments the cell the CPTR points onto.
;
; In:           R2, R3
; Overwrites:   A
; Out:          R2, R3
;
INC_CELL:
;-----------------------------
; Prelude
ACALL   PUSH_DPTR
ACALL   POP_CPTR
;-----------------------------
; Increment
MOVX    A, @DPTR            ; Load cell value into A
INC     A                   ; Increment value
MOVX    @DPTR, A            ; Load cell back to external memory
;-----------------------------
; Cleanup
ACALL   PUSH_CPTR
ACALL   POP_DPTR
RET


;---------------------------------------------------------------------
; This function decrements the cell the CPTR points onto.
;
; In:           R2, R3
; Overwrites:   A
; Out:          R2, R3
; 
DEC_CELL:
;-----------------------------
; Prelude
ACALL   PUSH_DPTR
ACALL   POP_CPTR
;-----------------------------
; Decrement
MOVX    A, @DPTR            ; Load cell value into A
DEC     A                   ; Decrement value
MOVX    @DPTR, A            ; Load cell back to external memory
;-----------------------------
; Clean-Up
ACALL   PUSH_CPTR
ACALL   POP_DPTR
RET


;---------------------------------------------------------------------
; This function moves the CPTR one cell to the right.
;
; In:           R2, R3
; Overwrites:   A
; Out:          R2, R3
;
MOVE_RIGHT:
;-----------------------------
; Prelude
ACALL   PUSH_DPTR
ACALL   POP_CPTR
;-----------------------------
; Increment
MOV     A, DPL
CJNE    A, #0FFh, _move_right ; Check if cell is last cell
MOV     DPTR, #0800h          ; If it is, go back to start of cells
SJMP    _move_right_exit

_move_right:
ACALL   INC_CPTR
;-----------------------------
; Cleanup
_move_right_exit:
ACALL   PUSH_CPTR
ACALL   POP_DPTR
RET


;---------------------------------------------------------------------
; This function moves the CPTR one cell to the left.
;
; In:           R2, R3
; Overwrites:   A
; Out:          R2, R3
; 
MOVE_LEFT:
;-----------------------------
; Prelude
ACALL   PUSH_DPTR
ACALL   POP_CPTR
;-----------------------------
; Decrement
MOV     A, DPL
CJNE    A, #00h, _move_left ; Check if cell is first cell
MOV     DPTR, #08FFh        ; If it is, go back to end of cells
SJMP    _move_left_exit

_move_left:
ACALL   DEC_CPTR
;-----------------------------
; Clean-Up
_move_left_exit:
ACALL   PUSH_CPTR
ACALL   POP_DPTR
RET


;---------------------------------------------------------------------
; This function interprets the current opened bracket. 
; 
; If the cell's value is zero, it jumps behind the matching closing
; bracket
;
; In:           R2, R3, R4, R5
;
INTERP_OPENED_BRACKET:
;-----------------------------
; Prelude
ACALL   PUSH_DPTR               ; Backup DPTR
ACALL   POP_CPTR                ; Load CPTR for loading the cell value
;-----------------------------
; Decide action
MOVX    A, @DPTR                ; Load cell value into A
CJNE    A, #00h, _iob_skip      ; Check if A is zero - if not, then
                                ; skip the bracket in the table to be
                                ; ready to read the next one
;-----------------------------
; Load table entry
ACALL   POP_TPTR                ; Load TPTR for reading table
ACALL   READ_TABLE_ENTRY        ; Load entry (symbol pointer is set
                                ; directly when reading; table pointer
                                ; is also backuped)
_iob_skip:
ACALL   POP_TPTR                ; Load TPTR for skipping to next entry
ACALL   TABLE_NEXT_ENTRY        ; Skip to next entry 
;-----------------------------
; Cleanup
_iob_exit:
ACALL   PUSH_TPTR               ; Backup TPTR
ACALL   POP_DPTR                ; Restore DPTR
RET


;---------------------------------------------------------------------
; This function read the table entry starting at TPTR.
;
; In:               R4, R5
;
READ_TABLE_ENTRY:
MOVX    A, @DPTR               ; Load UH of entry's symbol pointer
MOV     R7, A               
ACALL   INC_TPTR                

MOVX    A, @DPTR               ; Load LH of entry's symbol pointer
MOV     R6, A
ACALL   INC_TPTR

MOVX    A, @DPTR               ; Load UH of entry's TPTR
MOV     R5, A
ACALL   INC_TPTR

MOVX    A, @DPTR               ; Load LH of entry's TPTR
MOV     R4, A
RET


;---------------------------------------------------------------------
; This function interprets the close closed bracket. If the cell's
; value is not zero, it jumps back to the matching opened bracket.
;
; In:           R2, R3
; Overwrites:   A
;
INTERP_CLOSED_BRACKET:
;-----------------------------
; Prelude
ACALL   PUSH_DPTR               ; Backup DPTR
ACALL   POP_CPTR                ; Load CPTR for loading the cell value
;-----------------------------
; Decide action
MOVX    A, @DPTR                ; Load cell value into A
CJNE    A, #00h, _icb_back      ; Check if A is zero - if not, then
                                ; skip the bracket in the table to be
                                ; ready to read the next one)
;-----------------------------
; Skip bracket
ACALL   POP_TPTR                ; Load TPTR for skipping to next entry
ACALL   TABLE_NEXT_ENTRY        ; Skip to next entry 
ACALL   PUSH_TPTR
ACALL   POP_DPTR
RET
;-----------------------------
; Jump back to opened bracket
_icb_back:
ACALL   POP_TPTR                ; Load TPTR for reading table
ACALL   READ_TABLE_ENTRY        ; Load entry (symbol pointer is set
                                ; directly when reading; table pointer
                                ; is also backuped)
ACALL   POP_DPTR                ; Restore DPTR
ACALL   DEC_DPTR                ; Move behind bracket
RET


;---------------------------------------------------------------------
; This function prints the symbol behind CPTR.
;
; In:           R2, R3
; Overwrites:   A
;
PRINT_CHAR:
;-----------------------------
; Prelude
ACALL   PUSH_DPTR
ACALL   POP_CPTR
;-----------------------------
; Print
MOVX    A, @DPTR            ; Load cell value into A
ACALL   LCD_CHAR            ; Print symbol from A
;-----------------------------
; Cleanup
ACALL   PUSH_CPTR
ACALL   POP_DPTR
RET


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
; In:       DPL, DPH
; Out:      R6, R7
;
PUSH_DPTR:
MOV     R6, DPL
MOV     R7, DPH
RET


;---------------------------------------------------------------------
; This function "pops" DPTR from R7 (UH) and R6 (LH). For further
; explanation, see PUSH_DPTR above.
; 
; In:       R6, R7
; Out:      DPL, DPH

POP_DPTR:
MOV     DPL, R6
MOV     DPH, R7
RET


;---------------------------------------------------------------------
; This function reduces DPTR to a bit mask by applying a logical or
; to its lower and upper half. That can be used to check if DPTR is
; zero.
;
; In:           DPL, DPH
; Overwrites:   B
; Out:          A
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
; In:           DPL, DPH
; Overwrite:    C
; Out:          DPL, DPH
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
; In:           DPL, DPH
; Overwrite:    C
; Out:          DPL, DPH
;
INC_DPTR:
CLR     C                   ; Remove unrelated carry           
PUSH    A                   ; Backup A

MOV     A, DPL              ; Increment lower half
ADD     A, #01h
JNC     _store_lower_dptr   ; When no carry, the upper half
                            ; is unaffected

INC     DPH
CLR     C                   ; Clear carry of ADD
SJMP    _store_lower_dptr   ; Store lower half


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
; In:           DPL, DPH
; Out:          R4, R5
;
PUSH_TPTR:
MOV     R4, DPL
MOV     R5, DPH
RET


;---------------------------------------------------------------------
; This function "pops" TPTR from R5 (UH) and R4 (LH). For further
; explanation, see PUSH_TPTR above.
; 
; In:       R4, R5
; Out:      DPL, DPH
;
POP_TPTR:
MOV     DPL, R4
MOV     DPH, R5
RET


;---------------------------------------------------------------------
; This function moves TPTR by four bytes, so that it points to the
; next table entry.
;
; In:           DPL, DPH
; Overwrite:    C
; Out:          DPL, DPH
;
TABLE_NEXT_ENTRY:
ACALL   INC_DPTR
ACALL   INC_DPTR
ACALL   INC_DPTR
ACALL   INC_DPTR
RET
 

;---------------------------------------------------------------------
; This function increments TPTR by one.
; There is no decrement defined for 16-bit values. 
;
; In:           DPL, DPH
; Overwrite:    C
; Out:          DPL, DPH
;
INC_TPTR:
ACALL   INC_DPTR
RET


; ===================================================================
; Cell Pointer (CPTR) Handling
; ===================================================================
;---------------------------------------------------------------------
; This function initializes the CPTR to start at the end of the
; external memory, so that it can grow downwards.
;
; Out:          R2, R3
;
INIT_CPTR:
MOV     R2, #00h
MOV     R3, #08h
RET


;---------------------------------------------------------------------
; This function "pushes" CPTR by storing it into R3 (UH) and R2 (LH)
; in order to avoid pushing onto the regular stack. Doing so makes it
; difficult to retrieve the two CPTR bytes on top of the stack using
; a function, as the return address is also pushed, burrying the
; wanted data.
;
; In:           DPL, DPH
; Out:          R2, R3
;
PUSH_CPTR:
MOV     R2, DPL
MOV     R3, DPH
RET


;---------------------------------------------------------------------
; This function "pops" CPTR from R3 (UH) and R2 (LH). For further
; explanation, see PUSH_CPTR above.
;
; In:           R2, R3
; Out:          DPL, DPH
;
POP_CPTR:
MOV     DPL, R2
MOV     DPH, R3
RET
 

;---------------------------------------------------------------------
; This function increments TPTR by one.
; There is no increment defined for 16-bit values. 
;
; TODO: Wrap around when CPTR is 255
;
; In:           DPL, DPH
; Overwrite:    C
; Out:          DPL, DPH
;
INC_CPTR:
ACALL   INC_DPTR
RET


;---------------------------------------------------------------------
; This function decrements TPTR by one.
; There is no decrement defined for 16-bit values. 
;
; TODO: Wrap around when CPTR is 0
;
; In:           DPL, DPH
; Overwrite:    C
; Out:          DPL, DPH
;
DEC_CPTR:
ACALL   DEC_DPTR
RET


; ===================================================================
; XSTACK Pointer Handling
; ===================================================================
;---------------------------------------------------------------------
; This function initializes the XSTACK pointer to start at the end
; of the external memory, so that it can grow downwards.
;
; Out:          R0, R1
;
INIT_XSTACK:
MOV     R0, #0FFh
MOV     R1, #0FFh
RET


;---------------------------------------------------------------------
; This function "pushes" XSTACK by storing it into R1 (UH) and R0 (LH)
; in order to avoid pushing onto the regular stack. Doing so makes it
; difficult to retrieve the two XSTACK bytes on top of the stack using
; a function, as the return address is also pushed, burrying the
; wanted data.
;
; In:           DPL, DPH
; Out:          R0, R1
;
PUSH_XSTACK:
MOV     R0, DPL
MOV     R1, DPH
RET


;---------------------------------------------------------------------
; This function "pops" XSTACK from R1 (UH) and R0 (LH). For further
; explanation, see PUSH_XSTACK above.
;
; In:           R0, R1
; Out:          DPL, DPH
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
; In:           DPL, DPH
; Overwrite:    C
; Out:          DPL, DPH
;
DEC_XSTACK:
ACALL   INC_DPTR            ; Incrementing the DPTR moves the XSTACK
RET                         ; pointer closer to the stack's base,
                            ; effectively shrinking it


;---------------------------------------------------------------------
; This function "increments" the XSTACK pointer. For further and
; analogous explanation, see DEC_XSTACK above.
; 
; In:           DPL, DPH
; Overwrite:    C
; Out:          DPL, DPH
;
INC_XSTACK:
ACALL   DEC_DPTR            ; Decrementing the DPTR moves the XSTACK
RET                         ; pointer farther from the stack's base,
                            ; effectively growing it


; ===================================================================
; LCD Control
; ===================================================================
;---------------------------------------------------------------------
; This function initializes the LCD.
;
; Overwrite:    A
;
LCD_INIT:
MOV     A, #0Eh             ; Display on, Cursor on
ACALL   LCD_COMMAND

MOV     A, #06h             ; Auto-increment cursor
ACALL   LCD_COMMAND

MOV     A, #38h             ; Set char dimensions
ACALL   LCD_COMMAND

MOV     A, #80h             ; Set cursor to start of first line
ACALL   LCD_COMMAND
RET


;---------------------------------------------------------------------
; This function moves the LCD cursor to the second line.
;
; Overwrite: A
;
LCD_NEXT_LINE:
MOV     A, #0C0h             ; Select second line
ACALL   LCD_COMMAND
RET


;---------------------------------------------------------------------
; This function sends the string data behind the DPTR start until the
; string terminator is reached.
;
; Overwrite:    A
;
LCD_STR:
MOV     A, #00h             ; Reset A before reading, so that address
                            ; is only defined by DPTR
MOVC    A, @A+DPTR          ; Load character from code memory
JZ      _lcd_str_end   ; Return if terminator has been read

ACALL   LCD_CHAR            ; Send character data
ACALL   INC_DPTR            ; Increment DPTR
SJMP    LCD_STR        ; Send next character

_lcd_str_end:
RET


;---------------------------------------------------------------------
; This function sends the data in A with the modes described by RS and
; RW to tbe display and waits until the controller is no longer busy.
;
; Overwrite:    RS, RW, PD, EN
;
LCD_SEND:
SETB    EN
CLR     EN
ACALL   DELAY
RET


;---------------------------------------------------------------------
; This function sends a command to the LCD stored in register A.
;
; Overwrite:    PD, RS, RW
;
LCD_COMMAND:
MOV     PD, A               ; Load data to port
CLR     RS                  ; Set COMMAND mode
CLR     RW                  ; Set WRITE mode
ACALL   LCD_SEND
RET


;---------------------------------------------------------------------
; This function shows the character stored in register A.
;
; Overwrite:    PD, RS, RW
; 
LCD_CHAR:
MOV     PD, A               ; Load data to port
SETB    RS                  ; Set DATA mode
CLR     RW                  ; Set WRITE mode
ACALL   LCD_SEND
RET


;---------------------------------------------------------------------
; This function prints the value in the DPTR register to the LCD.
;
; In:           R0, R1, R2
; Overwrite:    A, F0
; 
LCD_DPTR:
;-----------------------------
; Prelude
CLR     F0                  ; Clear F0 (when set, all leading zeroes
                            ; have been skipped and everything
                            ; following must be printed)
;-----------------------------
; Handle R2 (lower)
_lcd_dptr_lower_r2:
MOV     A, R2               ; Load R2 into A
ACALL   LOWER_HALF
JZ     _lcd_dptr_upper_r1   ; Jump if lower half of R2 is zero 

_lcd_dptr_p_lower_r2:
SETB    F0                  ; No more leading zeroes
ADD     A, #30h             ; Adjust to ASCII table
ACALL   LCD_CHAR            ; Print digit
;-----------------------------
; Handle R1 (upper)
_lcd_dptr_upper_r1:
MOV     A, R1               ; Load R1 into A
ACALL   UPPER_HALF
JB      F0, _lcd_dptr_p_upper_r1    ; Jump to printing, when F0 set
JZ      _lcd_dptr_lower_r1  ; Jump if upper half of R1 is zero

_lcd_dptr_p_upper_r1:
SETB    F0                  ; No more leading zeroes
ADD     A, #30h             ; Adjust to ASCII table
ACALL   LCD_CHAR            ; Print digit
;-----------------------------
; Handle R1 (lower)
_lcd_dptr_lower_r1:
MOV     A, R1               ; Load R1 into A
ACALL   LOWER_HALF
JB      F0, _lcd_dptr_p_lower_r1    ; Jump to printing, when F0 set
JZ      _lcd_dptr_upper_r0  ; Jump if lower half of R1 is zero

_lcd_dptr_p_lower_r1:
SETB    F0                  ; No more leading zeroes
ADD     A, #30h             ; Adjust to ASCII table
ACALL   LCD_CHAR            ; Print digit
;-----------------------------
; Handle R0 (upper)
_lcd_dptr_upper_r0:
MOV     A, R0               ; Load R0 into A
ACALL   UPPER_HALF
JB      F0, _lcd_dptr_p_upper_r0    ; Jump to printing, when F0 set
JZ      _lcd_dptr_lower_r0  ; Jump if upper half of R0 is zero

_lcd_dptr_p_upper_r0:
ADD     A, #30h
ACALL   LCD_CHAR            ; Print digit
;-----------------------------
; Handle R0 (lower)
_lcd_dptr_lower_r0:
MOV     A, R0               ; Load R0 into A
ACALL   LOWER_HALF
ADD     A, #30h             ; Adjust to ASCII table
ACALL   LCD_CHAR            ; Print digit
RET


;---------------------------------------------------------------------
; This function delays execution until the display controller says,
; that it is no longer busy (using Busy Flag).
;
; Overwrite:    RS, RW, PD, EN
;
DELAY:
CLR     RS
SETB    RW                  
SETB	PD.7                ; Declare as input for busy flag poll
_delay_check:
SETB    EN                  ; Latch the data
CLR     EN
JB      PD.7, _delay_check  ; Check again, if busy flag still set
RET


; ===================================================================
; BCD Conversion
; ===================================================================
;---------------------------------------------------------------------
; This function shifts the DPTR by one and stores the leftover bit
; into the carry flag.
;
; In:           DPL, DPH
; Overwrite:    A, DPL, DPH
; Out:          C
;
DPTR_LSHIFT:
CLR     C                   ; Clear carry
MOV     A, DPL              ; Move lower half into A
RLC     A                   ; Left shift A
MOV     DPL, A              ; Transfer back into lower half

MOV     A, DPH              ; Move upper half into A
RLC     A                   ; Left shift A
MOV     DPH, A              ; Transfer back into upper half
RET


;---------------------------------------------------------------------
; This function converts the DPTR content into its BCD equivalent
; stored in the registers R2 to R0. For that purpose, the 'Double
; Dabble' algorithm is used.
;
; In:           DPTR
; Overwrites:   A, R4
; Out:          R2, R1, R0 (big endian, BCD registers)
;
DPTR_TO_BCD:
;-----------------------------
; Prelude
MOV     R4, #10h            ; Initialize iterator
MOV     R0, #00h            ; Clear BDC registers R0-R2
MOV     R1, #00h
MOV     R2, #00h
;-----------------------------
; Iterator check
_dptr_to_bcd:
MOV     A, R4               ; Load R4 into A
JZ      _exit_dptr_to_bcd   ; Exit early if DPTR is empty
DEC     A                   ; Decrement iterator
MOV     R4, A               ; Backup iterator into R4
;-----------------------------
; Double Dabble
ACALL   BCD_INCREMENT       ; Traverse nibbles and increment those
                            ; by three with values greater or equal five
ACALL   DPTR_LSHIFT         ; Left shift DPTR by one (setting C)
ACALL   BCD_LSHIFT          ; Left shift the BCD registers (using C)
SJMP _dptr_to_bcd           ; Inspect next bit
;-----------------------------
; Exit
_exit_dptr_to_bcd:
RET


;---------------------------------------------------------------------
; This function performs one left shift across the boundaries of the
; registers involved in the BCD transformation process. They, together,
; then behave like a 20 bit-integer.
;
; In:           R0, R1, R2
; Overwrite:    A
; Out:          R0, R1, R2
;
BCD_LSHIFT:
MOV     A, R0               ; Load R0 into A
RLC     A                   ; Left shift by one - carry comes from
                            ; integer to be converted into BCD
MOV     R0, A               ; Load R0 from A

MOV     A, R1               ; Load R1 into A
RLC     A                   ; Left shift by one - carry comes from R0
MOV     R1, A               ; Load R1 from A

MOV     A, R2               ; Load R2 into A
RLC     A                   ; Left shift by one - carry comes from R1
MOV     R2, A               ; Load R2 from A
RET


;---------------------------------------------------------------------
; This function searches for nibbles whose value is greater or equal
; to five from left to right. Each one is incremented by three.
;
; In:           R0, R1, R2
; Overwrites:   A, R3
; Out:          R0, R1, R2
; 
BCD_INCREMENT:
;-----------------------------
; Increment R1 (lower)
_bcd_increment_lower_r1:
MOV     A, R1               ; Load R1 into A
MOV     A, R1               ; Load R1 into A
ACALL   LOWER_HALF          ; Inspect lower half

ACALL   IS_GREATER_EQUAL_FIVE         ; Check qualification for addition
JNB     F0, _bcd_increment_upper_r1   ; Jump if smaller than five
ADD     A, #03h                       ; Increment lower half
;-----------------------------
; Increment R1 (upper)
_bcd_increment_upper_r1:
MOV     R3, A               ; Backup lower half into R3
MOV     A, R1               ; Load R1 into A
ACALL   UPPER_HALF          ; Inspect upper half

ACALL   IS_GREATER_EQUAL_FIVE         ; Check qualification for addition
JNB     F0, _bcd_increment_combine_r1 ; Jump if smaller than five
ADD     A, #03h                       ; Increment lower half
;-----------------------------
; Combine incremented nibbles
_bcd_increment_combine_r1:
ACALL   NIBBLE_LSHIFT       ; Shift theoretical upper half (currently
                            ; in lower half) into real upper half
ORL     A, R3               ; OR lower and upper half to full register
MOV     R1, A
;-----------------------------
; Increment R0 (lower)
_bcd_increment_lower_r0:
MOV     A, R0               ; Load R0 into A
ACALL   LOWER_HALF          ; Inspect lower half

ACALL   IS_GREATER_EQUAL_FIVE       ; Check qualification for addition
JNB     F0, _bcd_increment_upper_r0 ; Jump if smaller than five
ADD     A, #03h                     ; Increment lower half
;-----------------------------
; Increment R0 (upper)
_bcd_increment_upper_r0:
MOV     R3, A               ; Backup lower half into R3
MOV     A, R0               ; Load R0 into A
ACALL   UPPER_HALF          ; Inspect upper half

ACALL   IS_GREATER_EQUAL_FIVE         ; Check qualification for addition
JNB     F0, _bcd_increment_combine_r0 ; Jump if smaller than five
ADD     A, #03h                       ; Increment lower half
;-----------------------------
; Combine incremented nibbles
_bcd_increment_combine_r0:
ACALL   NIBBLE_LSHIFT       ; Shift theoretical upper half (currently
                            ; in lower half) into real upper half
ORL     A, R3               ; OR lower and upper half to full register
MOV     R0, A
RET


;---------------------------------------------------------------------
; This function finds the lower half of A.
; 
; In:           A
; Out:          A (lower half)
;
LOWER_HALF:
ANL     A, #0Fh             ; Mask b0-b3
RET


;---------------------------------------------------------------------
; This function finds the upper half of A.
; 
; In:           A
; Overwrites:   C
; Out:          A (upper half)
;
UPPER_HALF:
ANL     A, #0F0h            ; Mask b4-b7
ACALL   NIBBLE_RSHIFT
RET


;---------------------------------------------------------------------
; This function shifts the content of A into the right nibble, so
; performs four right shifts.
; 
; In:           A
; Out:          A (lower half)
;
NIBBLE_RSHIFT:
CLR     C                   ; Prevent wrap-around effect
RRC     A
RRC     A
RRC     A
RRC     A
RET


;---------------------------------------------------------------------
; This function shifts the content of A into the left nibble, so
; performs four left shifts.
; 
; In:           A
; Out:          A (upper half)
;
NIBBLE_LSHIFT:
CLR     C                   ; Prevent wrap-around effect
RLC     A
RLC     A
RLC     A
RLC     A
RET


;---------------------------------------------------------------------
; This function checks if A is greater or equal to 5, qualifying it
; for an addition in the 'Double Dabble' algorithm.
;
; In:           A
; Overwrites:   B
; Out:          F0  (false = '0', true = '1')
;
IS_GREATER_EQUAL_FIVE:
CLR     F0                  ; Result is to be determined
CLR     C                   ; Carry not of interest
MOV     B, A                ; Backup A into B

SUBB    A, #05                  ; Subtract 5 from A
JC      _exit_is_greater_four   ; Jump if C set, so A < 5

_is_greater_four:
SETB    F0                  ; A is greater four

_exit_is_greater_four:
MOV     A, B                ; Restore A from B
RET


; ===================================================================
; Data Area Clear
; ===================================================================
;---------------------------------------------------------------------
; This function clears up the bracket table (#0000 - #07FF) and the
; storage cells (#0800-#08FF). The table contains 256 bracket pairs,
; two brackets each, summing to 512 entries. Each entry occupies four
; bytes. There are 256 storage cells and they are one byte wide each.
;
; Overwrite:    A
;
CLEAR_DATA_AREA:
MOV     DPTR, #08FFh

_clear_next_byte:
MOV     A, #00h             ; Zero out target byte
MOVX    @DPTR, A

ACALL   OR_DPTR             ; Or both bytes of DPTR and exit
JZ      _exit_clear         ; Jump if DPTR has reached zero

ACALL   DEC_DPTR
SJMP    _clear_next_byte

_exit_clear:
RET


; ===================================================================
; Memory Bank Selection
; ===================================================================
;---------------------------------------------------------------------
; This function selects the 0th memory bank.
;
; Overwrite:    RS0, RS1
;
USE_BANK0:
CLR     RS0
CLR     RS1
RET

;---------------------------------------------------------------------
; This function selects the 3rd memory bank.
;
; Overwrite:    RS0, RS1
;
USE_BANK3:
SETB    RS0
SETB    RS1
RET


; ===================================================================
; Error Handling
; ===================================================================
;---------------------------------------------------------------------
; This function shifts the DPTR to revert the ORG directive, making
; symbol indices start at zero.
;
SHIFT_DPTR:
CLR     C                       ; Clear C to avoid interference with
                                ; subtraction
SUBB    A, #05h                 ; Revert ORG shift of code, so that
                                ; DPTR index starts at zero
MOV     DPH, A                  ; Load DPH from A
RET


;---------------------------------------------------------------------
; This function prints the index relevant for an error, when it can
; directly be deduced from the content in DPTR.
;
; In:           R4, R5
; Overwrite:    A
;
PRINT_DPTR_INDEX:
ACALL   POP_DPTR                ; Restore DPTR
MOV     A, DPH                  ; Load DPH into A
ACALL   SHIFT_DPTR              ; Revert ORG offset
ACALL   DPTR_TO_BCD             ; Convert DPTR to BCD
ACALL   LCD_DPTR                ; Print BCD representation
RET


;---------------------------------------------------------------------
; This function prints the "at index '<index>'" message fragment in
; the second line.
;
; In:           R4, R5
; Overwrite:    A
;
PRINT_AT_INDEX:
;-----------------------------
; Print
ACALL   LCD_NEXT_LINE           ; Move cursor to second line

MOV     A, #00h                 ; Print message fragment
MOV     DPTR, #AT_INDEX
ACALL   LCD_STR

MOV     A, #27h                 ; Print ' symbol
ACALL   LCD_CHAR
;-----------------------------
; Print index
ACALL   PRINT_DPTR_INDEX        ; Print index from DPTR content
;-----------------------------
; Print
MOV     A, #27h                 ; Print ' symbol
ACALL   LCD_CHAR

RET

;---------------------------------------------------------------------
; This function reports an invalid symbol encountered during parsing.
; 
; In:           R4, R5
; Overwrite:    A
;
REPORT_INVALID_SYMBOL:
;-----------------------------
; Backup invalid symbol and DPTR
MOV     A, #00h                 ; Backup invalid symbol into B
MOVC    A, @A+DPTR              ; Load byte at DPTR
MOV     B, A                    ; Transfer A into B
ACALL   PUSH_DPTR               ; Backup DPTR
;-----------------------------
; Print
MOV     A, #00h                 ; Print message fragment
MOV     DPTR, #INVALID_SYMBOL
ACALL   LCD_STR

MOV     A, #27h                 ; Print ' symbol
ACALL   LCD_CHAR                

MOV     A, B                    ; Print invalid symbol
ACALL   LCD_CHAR

MOV     A, #27h                 ; Print ' symbol
ACALL   LCD_CHAR

ACALL   PRINT_AT_INDEX          ; Print message fragment
;-----------------------------
; Error exit
ACALL   ERROR


;---------------------------------------------------------------------
; This function reports an unclosed bracket encountered during parsing.
;
; In:           R4, R5
; Overwrite:    A
;
REPORT_UNCLOSED_BRACKET:
;-----------------------------
; Print
MOV     A, #00h                 ; Print message fragment
MOV     DPTR, #UNCLOSED_BRACKET
ACALL   LCD_STR                
ACALL   LCD_NEXT_LINE           ; Move cursor to next line

MOV     A, #00h                 ; Print message fragment
MOV     DPTR, #AT_INDEX
ACALL   LCD_STR

MOV     A, #27h                 ; Print ' symbol
ACALL   LCD_CHAR
;-----------------------------
; Print index
ACALL   POP_XSTACK              ; Load XSTACK pointer to read symbol
                                ; pointer of topmost unclosed bracket
ACALL   READ_SYMBOL_POINTER
MOV     A, R3                   ; Transfer symbol pointer into DPTR
MOV     DPL, R2                 ; but put UH into A for reverting ORG
ACALL   SHIFT_DPTR              ; Revert ORG offset
ACALL   DPTR_TO_BCD             ; Convert DPTR to BCD
ACALL   LCD_DPTR                ; Print BCD representation
;-----------------------------
; Print
MOV     A, #27h                 ; Print ' symbol
ACALL   LCD_CHAR
;-----------------------------
; Error exit
ACALL   ERROR


;---------------------------------------------------------------------
; This function reports an unopened bracket encountered during parsing.
;
; In:           R4, R5
; Overwrite:    A
;
REPORT_UNOPENED_BRACKET:
;-----------------------------
; Backup DPTR
ACALL   PUSH_DPTR               ; Backup DPTR
;-----------------------------
; Print
MOV     A, #00h                 ; Print message fragment
MOV     DPTR, #UNOPENED_BRACKET
ACALL   LCD_STR                
ACALL   PRINT_AT_INDEX          ; Print message fragment
;-----------------------------
; Error exit
ACALL   ERROR
RET

;---------------------------------------------------------------------
; This function reports too many bracket pairs encountered during parsing.
; They are indicated by opening brackets.
;
; In:           R4, R5
; Overwrite:    A
;
REPORT_TOO_MANY_PAIRS:
;-----------------------------
; Backup DPTR
ACALL   PUSH_DPTR               ; Backup DPTR
;-----------------------------
; Print
MOV     A, #00h                 ; Print message fragment
MOV     DPTR, #TOO_MANY_PAIRS
ACALL   LCD_STR 
ACALL   PRINT_AT_INDEX          ; Print message fragment
;-----------------------------
; Error exit
ACALL   ERROR
END