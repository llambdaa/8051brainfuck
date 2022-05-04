;---------------------------------------------------------------------
; Register Table:
; ~~~~~~~~~~~~~~~~
; Memory Bank [0]:
; R7 (UH) + R6 (LH):    DPTR Backup (Symbol Pointer)
; R5 (UH) + R4 (LH):    TPTR Backup
; R3 (UH) + R2 (LH):    Cell Pointer (CPTR) Backup
; R1 (UH) + R0 (LH):    XSTACK Backup
; 
; Memory Bank [3]:
; R7 (UH) + R6 (LH):    TPTR Before Close Bracket Entry
; R5 (UH) + R4 (LH):    TPTR After Close Bracket Entry
; R3 (UH) + R2 (LH):    Symbol Pointer
; R1 (UH) + R0 (LH):    TPTR Of Open Bracket
; ===================================================================
; Brainfuck Code with Newline Terminator
; ===================================================================
; Note: Since the brainfuck code definition is described here, it
;       can be found in the code memory starting at location 0x0000.
;
CODE:  DB ']', 00h

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
; This function is the entry point to the program.C
;
MAIN:
ACALL   LCD_INIT

MOV     A, #'A'
ACALL   LCD_DATA

; ACALL   USE_BANK0
; ACALL   CLEAR_DATA_AREA
; ACALL   PARSE
;ACALL   INTERPRET

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
ACALL   INIT_XSTACK
MOV     DPTR, #0000h   

_parse_next_symbol:
MOV     A, #00h
MOVC    A, @A+DPTR          ; Load symbol from code memory
JNZ     _parse_process      ; Read symbol is null-terminator

; ==- Parsing Exit
CJNE    R0, #0FFh, _parse_unbalanced_bracket    ; Check if XSTACK is empty
CJNE    R1, #0FFh, _parse_unbalanced_bracket    ; Otherwise, an opened bracket  
RET                                             ; is not balanced

_parse_unbalanced_bracket:
ACALL   REPORT_UNBALANCED_BRACKET

; ==- Process Symbol
_parse_process:
ACALL   IS_VALID_OPERATOR
JB      F0, _parse_prepare  ; Valid symbol, ready for next

_parse_opened_bracket:
CJNE    A, #5Bh, _parse_closed_bracket  ; Check if symbol is opened bracket 
ACALL   HANDLE_OPENED_BRACKET           ; Handle opened bracket
SJMP    _parse_prepare                  ; Ready for next symbol

_parse_closed_bracket:
CJNE    A, #5Dh, _parse_invalid_symbol  ; Check if symbol is closed bracket
ACALL   HANDLE_CLOSED_BRACKET           ; Handle closed bracket
SJMP    _parse_prepare                  ; Ready for next symbol

_parse_invalid_symbol:
ACALL   REPORT_INVALID_SYMBOL

_parse_prepare:
ACALL   INC_DPTR            ; Point to next symbol
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
;   |  BASE  |  ...   |   R4   |   R5   |  DPL   |  DPH   |
;   +--------+--------+--------+--------+--------+--------+
;   ^                                                     ^  
; START                                           XSTACK TOP POINTER
;
HANDLE_OPENED_BRACKET:
; ==- Check Bracket Count
CJNE    R5, #08h, _handle_ob        ; Only need to check UH of TPTR
ACALL   REPORT_TOO_MANY_BRACKETS    ; If it went past #0800, then the
                                    ; bracket table is full

_handle_ob:
; ==- Prelude
ACALL   PUSH_DPTR           ; Backup DPTR
ACALL   POP_XSTACK          ; Restore XSTACK (into DPTR)

; ==- Write Opened Bracket Table Entry
ACALL   WRITE_OPENED_BRACKET

; ==- Move TPTR To End
ACALL   PUSH_XSTACK         ; Backup XSTACK
ACALL   POP_TPTR            ; Restore TPTR
ACALL   TABLE_NEXT_ENTRY    ; Move TPTR by one entry
ACALL   PUSH_TPTR           ; Backup TPTR

; ==- Clean-Up
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
ACALL   INC_XSTACK          ; Move XSTACK pointer

MOV     A, R7               ; Push UH of backup DPTR
MOVX    @DPTR, A               
ACALL   INC_XSTACK
RET

;---------------------------------------------------------------------
; This function "pops" the corresponding bracket from the XSTACK into
; some registers. Then, a new entry for the closed bracket is written
; into the table referencing the open bracket. Lastly, the entry for
; the open bracket is modified to point to the closed bracket.
;
HANDLE_CLOSED_BRACKET:
; ==- Check Stack Size
CJNE    R0, #0FFh, _handle_cb     ; Check if XSTACK is empty
CJNE    R1, #0FFh, _handle_cb     ; If it is, the closing bracket is
ACALL   REPORT_UNBALANCED_BRACKET ; not balanced

_handle_cb:
; ==- Prelude
ACALL   PUSH_DPTR           ; Backup DPTR
ACALL   POP_XSTACK          ; Restore XSTACK (into DPTR)
ACALL   USE_BANK3           ; Select 1st memory bank

; ==- Read Topmost Entry
ACALL   READ_TOPMOST_ENTRY

; ==- Prepare For Table Modification
ACALL   PUSH_XSTACK         ; Backup XSTACK
ACALL   POP_TPTR            ; Restore TPTR                

ACALL   USE_BANK3
MOV     R6, DPL             ; Backup DPTR (currently pointing to start
MOV     R7, DPH             ; of closed bracket entry before actually
                            ; writing it

; ==- Write Closed Bracket Table Entry
ACALL   WRITE_CLOSED_BRACKET

; ==- Prepare For Overwriting Opened Bracket Entry
ACALL   PUSH_TPTR           ; Backup TPTR into 3rd memory bank
MOV     DPL, R0             ; Load TPTR of opened bracket 
MOV     DPH, R1

; ==- Overwrite Entry Of Corresponding Opened Bracket
ACALL   OVERWRITE_OPENED_BRACKET

; ==- Move TPTR Back To End Of Table
ACALL   POP_TPTR

; ==- Clean-Up
ACALL   USE_BANK0           ; Select 0th memory bank
ACALL   PUSH_TPTR           ; Backup TPTR (after writing entry)
ACALL   POP_DPTR            ; Restore DPTR
RET


;---------------------------------------------------------------------
; This function reads the topmost XSTACK table entry into the 3rd
; memory bank (R0-R3)
;
READ_TOPMOST_ENTRY:
; ==- Prelude
ACALL   USE_BANK3           ; Select 3rd memory bank

; ==- Read Entry
ACALL   DEC_XSTACK          ; Shrink XSTACK
MOVX    A, @DPTR            ; Load topmost value into A
MOV     R3, A               ; Store topmost value

ACALL   DEC_XSTACK
MOVX    A, @DPTR   
MOV     R2, A

ACALL   DEC_XSTACK
MOVX    A, @DPTR
MOV     R1, A

ACALL   DEC_XSTACK
MOVX    A, @DPTR
MOV     R0, A

; ==- Clean-Up
ACALL   USE_BANK0
RET


;---------------------------------------------------------------------
; This function overwrites the entry for the corresponding opened
; bracket.
;
OVERWRITE_OPENED_BRACKET:
ACALL   USE_BANK0           ; Select 0th memory bank
MOV     A, R7               ; Write DPTR containing current symbol index 
MOVX    @DPTR, A            ; from 0th memory bank
ACALL   INC_TPTR

MOV     A, R6
MOVX    @DPTR, A
ACALL   INC_TPTR

ACALL   USE_BANK3           ; Select 3rd memory bank
MOV     A, R7               ; Write backup TPTR of closed bracket entry
MOVX    @DPTR, A            ; from 3rd memory bank
ACALL   INC_TPTR

MOV     A, R6               
MOVX    @DPTR, A
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
INTERPRET:
; ==- Prelude
MOV     DPTR, #0000h        ; Reset DPTR
ACALL   PUSH_TPTR           ; Backup TPTR to point to table start
ACALL   INIT_CPTR           ; Reset CPTR

_interp_next_symbol:
MOV     A, #00h
MOVC    A, @A+DPTR          ; Load symbol from code memory
JNZ     _interp_symbol      ; Read symbol is null-terminator

; ==- Interpretation Exit
ACALL FINISH

; ==- Interpret Symbol
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
NOP
SJMP    _interp_prepare

_i_is_comma:
CJNE    A, #2Ch, _interp_error
NOP

_interp_prepare:
ACALL   INC_DPTR            ; Increment symbol pointer
SJMP _interp_next_symbol

_interp_error:
LJMP    ERROR


;---------------------------------------------------------------------
; This function increments the cell the CPTR points onto.
;
INC_CELL:
; ==- Prelude
ACALL   PUSH_DPTR
ACALL   POP_CPTR

; ==- Increment
MOVX    A, @DPTR            ; Load cell value into A
INC     A                   ; Increment value
MOVX    @DPTR, A            ; Load cell back to external memory

; ==- Clean-Up
ACALL   PUSH_CPTR
ACALL   POP_DPTR
RET


;---------------------------------------------------------------------
; This function decrements the cell the CPTR points onto.
;
DEC_CELL:
; ==- Prelude
ACALL   PUSH_DPTR
ACALL   POP_CPTR

; ==- Decrement
MOVX    A, @DPTR            ; Load cell value into A
DEC     A                   ; Decrement value
MOVX    @DPTR, A            ; Load cell back to external memory

; ==- Clean-Up
ACALL   PUSH_CPTR
ACALL   POP_DPTR
RET


;---------------------------------------------------------------------
; This function moves the CPTR one cell to the right.
;
MOVE_RIGHT:
; ==- Prelude
ACALL   PUSH_DPTR
ACALL   POP_CPTR

; ==- Increment
MOV     A, DPL
CJNE    A, #0FFh, _move_right ; Check if cell is last cell
MOV     DPTR, #0800h          ; If it is, go back to start of cells
SJMP    _move_right_exit

_move_right:
ACALL   INC_CPTR

; ==- Clean-Up
_move_right_exit:
ACALL   PUSH_CPTR
ACALL   POP_DPTR
RET


;---------------------------------------------------------------------
; This function moves the CPTR one cell to the left.
;
MOVE_LEFT:
; ==- Prelude
ACALL   PUSH_DPTR
ACALL   POP_CPTR

; ==- Decrement
MOV     A, DPL
CJNE    A, #00h, _move_left ; Check if cell is first cell
MOV     DPTR, #08FFh        ; If it is, go back to end of cells
SJMP    _move_left_exit

_move_left:
ACALL   DEC_CPTR

; ==- Clean-Up
_move_left_exit:
ACALL   PUSH_CPTR
ACALL   POP_DPTR
RET


;---------------------------------------------------------------------
; This function interprets the current opened bracket. If the cell's
; value is zero, it jumps behind the matching closing bracket
INTERP_OPENED_BRACKET:
; ==- Prelude
ACALL   PUSH_DPTR               ; Backup DPTR
ACALL   POP_CPTR                ; Load CPTR for loading the cell value

; ==- Decide Action
MOVX    A, @DPTR                ; Load cell value into A
CJNE    A, #00h, _iob_skip      ; Check if A is zero - if not, then
                                ; skip the bracket in the table to be
                                ; ready to read the next one

; ==- Load Table Entry
ACALL   POP_TPTR                ; Load TPTR for reading table
ACALL   READ_TABLE_ENTRY        ; Load entry (symbol pointer is set
                                ; directly when reading; table pointer
                                ; is also backuped)

_iob_skip:
ACALL   POP_TPTR                ; Load TPTR for skipping to next entry
ACALL   TABLE_NEXT_ENTRY        ; Skip to next entry 

; ==- Clean-Up
_iob_exit:
ACALL   PUSH_TPTR               ; Backup TPTR
ACALL   POP_DPTR
RET


;---------------------------------------------------------------------
; This function read the table entry starting at TPTR.
;
READ_TABLE_ENTRY:
MOVX    A, @DPTR               ; Load UH of entry's symbol pointer
MOV     R7, A               
ACALL   INC_TPTR                

MOVX    A, @DPTR               ; Load LH of entry's symbol pointer
MOV     R6, A
ACALL   INC_TPTR

MOVX    A, @DPTR               ; Load UH of entry's table pointer
MOV     R5, A
ACALL   INC_TPTR

MOVX    A, @DPTR               ; Load LH of entry's table pointer
MOV     R4, A
RET


;---------------------------------------------------------------------
; This function interprets the close closed bracket. If the cell's
; value is not zero, it jumps back to the matching opened bracket.
INTERP_CLOSED_BRACKET:
; ==- Prelude
ACALL   PUSH_DPTR               ; Backup DPTR
ACALL   POP_CPTR                ; Load CPTR for loading the cell value

; ==- Decide Action
MOVX    A, @DPTR                ; Load cell value into A
CJNE    A, #00h, _icb_back      ; Check if A is zero - if not, then
                                ; skip the bracket in the table to be
                                ; ready to read the next one)

; ==- Skip Bracket
ACALL   POP_TPTR                ; Load TPTR for skipping to next entry
ACALL   TABLE_NEXT_ENTRY        ; Skip to next entry 
ACALL   PUSH_TPTR
ACALL   POP_DPTR
RET

; ==- Jump Back To Opened Bracket
_icb_back:
ACALL   POP_TPTR                ; Load TPTR for reading table
ACALL   READ_TABLE_ENTRY        ; Load entry (symbol pointer is set
                                ; directly when reading; table pointer
                                ; is also backuped)
ACALL   POP_DPTR                ; Restore DPTR
ACALL   DEC_DPTR                ; Move behind bracket
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
ACALL   INC_DPTR
ACALL   INC_DPTR
ACALL   INC_DPTR
ACALL   INC_DPTR
RET
 

;---------------------------------------------------------------------
; This function increments TPTR by one.
; There is no decrement defined for 16-bit values. 
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
PUSH_CPTR:
MOV     R2, DPL
MOV     R3, DPH
RET


;---------------------------------------------------------------------
; This function "pops" CPTR from R3 (UH) and R2 (LH). For further
; explanation, see PUSH_CPTR above.
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
INC_CPTR:
ACALL   INC_DPTR
RET


;---------------------------------------------------------------------
; This function decrements TPTR by one.
; There is no decrement defined for 16-bit values. 
;
; TODO: Wrap around when CPTR is 0
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
ACALL   INC_DPTR            ; Incrementing the DPTR moves the XSTACK
RET                         ; pointer closer to the stack's base,
                            ; effectively shrinking it


;---------------------------------------------------------------------
; This function "increments" the XSTACK pointer. For further and
; analogous explanation, see DEC_XSTACK above.
; 
; Note: Destroys C(arry) Flag (begin and end)
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
; This function sends the data in A with the modes described by RS and
; RW to tbe display and waits until the controller is no longer busy.
;
LCD_SEND:
SETB    EN
CLR     EN
ACALL   DELAY
RET

;---------------------------------------------------------------------
; This function sends a command to the LCD stored in register A.
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
LCD_DATA:
MOV     PD, A               ; Load data to port
SETB    RS                  ; Set DATA mode
CLR     RW                  ; Set WRITE mode
ACALL   LCD_SEND
RET

;---------------------------------------------------------------------
; This function delays execution until the display controller says,
; that it is no longer busy (using Busy Flag).
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

ACALL   OR_DPTR             ; Or both bytes of DPTR and exit
JZ      _exit_clear         ; if DPTR has reached zero

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
ACALL   ERROR


;---------------------------------------------------------------------
; This function reports an unbalanced bracket encountered during parsing.
;
REPORT_UNBALANCED_BRACKET:
; TODO: Report unbalanced bracket in some way
ACALL   ERROR


;---------------------------------------------------------------------
; This function reports too many brackets encountered during parsing.
;
REPORT_TOO_MANY_BRACKETS:
; TODO: Report too many brackets in some way
ACALL   ERROR
