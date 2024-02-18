; int64.asm - low level 64 bit signed integer math support
; Copyright (c) 2024 by David R. Van Wagner davevw.com
; MIT LICENSE

; github.com/davervw

*=$C000

ptr1=$fb
ptr2=$fd
ptr3=$22

int64_methods:
  jmp int64_zero
  jmp int64_arg1
  jmp int64_adc
  jmp int64_sbc
  jmp int64_compare
  jmp int64_negate
  jmp int64_mul
  jmp int128_div
  jmp int64_tostring
  jmp int64_fromstring
  jmp int64_outhex
  jmp int64_arg2
  jmp int64_arg3
  jmp + ; reserved
  jmp + ; reserved
  jmp + ; reserved
  jmp + ; reserved
  jmp + ; reserved
  jmp + ; reserved
  jmp + ; reserved
  jmp + ; reserved
  jmp + ; reserved
  jmp + ; reserved
  jmp + ; reserved
  jmp + ; reserved
  jmp + ; reserved
  jmp + ; reserved
  jmp + ; reserved
  jmp + ; reserved
+ brk

int64_zero: ; input: pointer to 8 byte memory in .A(low) and .X(high)
            ; output: none, affects all registers, flags
  jsr int64_arg1
  lda #0
  ldy #7
- sta (ptr1),y
  dey
  bpl -
  rts
  
int64_arg1: ; input: pointer to 8 byte memory in .A(low) and .X(high)
           ; output: saves pointers as argument for next operation
           ; dependency of calling binary operator routines
  sta ptr1
  stx ptr1+1
  rts

int64_arg2: ; input: pointer to 8 byte memory in .A(low) and .X(high)
            ; output: saves pointers as argument for next operation
  sta ptr2
  stx ptr2+1
  rts

int64_arg3: ; input: pointer to 8 byte memory in .A(low) and .X(high)
            ; output: saves pointers as argument for next operation
  sta ptr3
  stx ptr3+1
  rts

int64_adc: ; input: pointer to 8 byte memory in .A(low) and .X(high), and C flag
           ; output: add with carry result in arg1 and in .A(low) and .X(high), and C/V/N flags
  jsr int64_arg2
  ldy #0
- lda (ptr1), y
  adc (ptr2), y
  sta (ptr1), y
  iny
  tya
  and #7
  bne -
  lda ptr1
  ldx ptr1+1
  rts
  
int64_sbc: ; input: pointer to 8 byte memory in .A(low) and .X(high), and C flag
           ; output: subtract with carry result in arg1 and in .A(low) and .X(high), and C/V/N flags
  jsr int64_arg2
  ldy #0
- lda (ptr1), y
  sbc (ptr2), y
  sta (ptr1), y
  iny
  tya
  and #7
  bne -
  lda ptr1
  ldx ptr1+1
  rts  
  
int64_compare:   ; input: pointer to 8 byte memory in .A(low) and .X(high)
                 ; output: compares to arg1, result in C (greater than or equal) and Z flags (equal)
  jsr int64_arg2
  ldy #7
  sec
- lda (ptr1), y
  sbc (ptr2), y
  bcc +
  bne +
  dey
  bpl -
  iny ; set Z
+ rts  

int64_negate: ; input: pointer to 8 byte memory in .A(low) and .X(high)
              ; output: modifies contents of int64 via twos complement, either negating or making positive
  jsr int64_arg1
  jsr int64_negate_ptr1
  lda ptr1
  ldx ptr1+1
  rts

int64_negate_ptr1:
  ldy #0
  sec
- lda (ptr1),y
  eor #$FF ; invert all the bits to ones complement
  adc #0 ; increase by one to twos complement
  sta (ptr1),y
  iny
  tya
  and #7
  bne -
  rts

int64_negate_ptr2:
  ldy #0
  sec
- lda (ptr2),y
  eor #$FF ; invert all the bits to ones complement
  adc #0 ; increase by one to twos complement
  sta (ptr2),y
  iny
  tya
  and #7
  bne -
  rts

int128_negate_sum:
  ldy #0
  sec
- lda uint128_sum, y
  eor #$FF ; invert all the bits to ones complement
  adc #0 ; increase by one to twos complement
  sta uint128_sum, y
  iny
  tya
  and #$0F
  bne -
  rts

int64_mul: ; input: pointer to 8 byte memory in .A(low) and .X(high), should have called int64_arg1 first
           ; output: int128 result stored across arg1(low64) and arg2(high64), with .A(low), .X(high) pointing to arg1
  jsr int64_arg2
  ; check signs, convert to unsigned, save expected signed result
  ldx #0
  stx $ff
  ldy #7
  lda (ptr1), y
  bpl +
  inc $ff
  jsr int64_negate_ptr1
+ ldy #7
  lda (ptr2), y
  bpl +
  inc $ff
  jsr int64_negate_ptr2
  ; clear high 64-bits of adder, while copying low 64-bits(ptr1), loading and shifting arg2(ptr2) to right, and clearing entire result(uint128_sum)
+ ldy #7
  clc
- lda #0
  sta uint128_adder+8, y
  sta uint128_sum, y
  sta uint128_sum+8, y
  lda (ptr1), y
  sta uint128_adder, y
  lda (ptr2), y
  ror 
  sta (ptr2), y
  dey
  bpl -
--bcc + ; branch if bit is zero, to skip over add this time
  ; need to add
  clc
  ldy #0
- lda uint128_adder, y
  adc uint128_sum, y
  sta uint128_sum, y
  iny
  tya
  and #$0F
  bne -
  ; shift adder
+ clc
  ldx #0
- rol uint128_adder, x
  bne +
  inx
  txa
  and #$0F
  bne -
  beq ++ ; zero bits found in adder, so we're done
  brk ; shouldn't get here... loop seperated in clear bits found so far (above), and at least one set bit found (below)
- rol uint128_adder, x
+ inx
  txa
  and #$0F
  bne -
  ; shift arg2
  ldy #7
  clc
- lda (ptr2), y
  ror
  sta (ptr2), y
  dey
  bpl -
  bmi --
++ ; load result into arguments
  ror $ff
  bcc +
  jsr int128_negate_sum
+ ldy #7
- lda uint128_sum, y
  sta (ptr1), y
  lda uint128_sum+8, y
  sta (ptr2), y
  dey
  bpl -
  lda ptr1
  ldx ptr1+1
  rts

int128_div: ; input: divisor as pointer to 8 byte memory in .A(low) and .X(high), should have called int64_arg1(low) and int64_arg2(high) first to set int128
            ; output: contents of arg1(low), arg2(high) modified to be result, with .A(low) and .X(high) pointing to 128-bit remainder
  ;algorithm: 
  ; shift contains bit (starts right-most [1]) to add to answer in sum (starts at 0)
  ; arg3 saved to divisor, shifted in loop to match shift
  ; workarea(remainder) starts with arg1/arg2  subtracted by shift as appropriate, reduced to remainder
  ; shift advanced to left as high as possible at first, then shifted right as loops, until shifts out
; arguments to working area
; and initialize sum & shift to zeros
  jsr int64_arg3
  ldy #7
- lda (ptr3), y
  sta uint128_divisor, y
  lda #0
  sta uint128_divisor+8, y
  sta uint128_shift, y
  sta uint128_shift+8, y
  sta uint128_sum, y
  sta uint128_sum+8, y
  lda (ptr1), y
  sta uint128_remain, y
  lda (ptr2), y
  sta uint128_remain+8, y
  dey
  bpl -
  inc uint128_shift
; compare remain and divisor to see if already done
  ldy #15
- lda uint128_remain, y
  cmp uint128_divisor, y
  bcc ++ ; branch if less than
  bne + ; branch if greater than
  dey
  bpl -
  bcs +++ ; branch if values are equal, already know that shift is perfect
+ ; shift and divisor one bit left
- ldy #16
  ldx #0
  clc
--rol uint128_shift, x
  inx
  dey
  bne --
  bcs ++++ ; shifted too far, last bit fell out on shift
  ldy #16
  ldx #0
  clc
--rol uint128_divisor, x
  inx
  dey
  bne --
  bcs +++++ ; shifted too far, divisor shifted out
  ; compare uint128_remain and uint128_divisor
  ldy #15
--lda uint128_remain, y
  cmp uint128_divisor, y
  bcc +++++ ; number too small to divide, done
  bne - ; number is larger, try to keep shifting
  dey
  bpl --
  lda #0 ; if got here, values equal so set Z
  beq +++ ; will always branch
  brk ; should be impossible to get here
+++++ ; shifted too far
  ldx #15
  clc
--ror uint128_divisor, x
  dex
  bpl --
  bcc ++++ ; should always branch
  brk ; shouldn't happen
++++
  ldx #15
  clc
--ror uint128_shift, x
  dex
  bpl --
  bcs + ; lowest bit shifted out, nothing more to do
+++ ; shift is just right, add to sum, subtract from workarea 
---ldy #15
- lda uint128_shift, y
  ora uint128_sum, y
  sta uint128_sum, y
  dey
  bpl -
  sec
  ldx #16
  ldy #0
- lda uint128_remain, y
  sbc uint128_divisor, y
  sta uint128_remain, y
  iny
  dex
  bne -
++ 
--
  clc
  ldx #15
- ror uint128_divisor, x
  dex
  bpl -
  clc
  ldx #15
- ror uint128_shift, x
  dex
  bpl -
  bcs + ; done
  ; compare remain and divisor
  ldy #15
- lda uint128_remain, y
  cmp uint128_divisor, y
  bcc -- ; remain < divisor
  bne --- ; branch if remain > divisor
  ; equal, so far
  dey
  bpl -
  bcs --- ; branch if remain >= divisor
  brk ; not possible to get here
+ ; done, copy 128-bit result to arg1(low), arg2(high)
  ldy #7
- lda uint128_sum, y
  sta (ptr1), y
  lda uint128_sum+8, y
  sta (ptr2), y
  dey
  bpl -
  ; load pointer to remainder in .A(low), .X(high)
  lda #<uint128_remain
  ldx #>uint128_remain
  rts

int64_tostring: ; input: .A base to convert to (2, 8, 10, 16)
                ; output: pointer to a buffer containing string, .Y=length in bytes, nul terminated as well
  brk
  
int64_fromstring: ; input .A(low),.X(high) pointer to string nul terminated, .Y=base (2, 8, 10, 16), and should have called int64_arg1 first
                  ; output: modifies memory pointed to by arg1
  brk

int64_outhex ; input: pointer to 8 byte memory in .A(low) and .X(high)
             ; output: to output device (screen), 16 hex digits
  jsr int64_arg1
  ldy #7
- lda (ptr1),y
  jsr byte_outhex
  dey
  bpl -
  rts

byte_outhex: ; input: A byte value
             ; output: to output device (screen), 2 hex digits
  pha
  lsr
  lsr
  lsr
  lsr
  jsr nybble_outhex
  pla
  ; fall through to next routine
nybble_outhex: ; input: A nybble value (0..F)
               ; output: to output device (screen), 1 hex digit
  and #$0F
  cmp #10
  bcs + ; branch if not decimal
  ora #$30 ; convert to PETSCII '0' to '9'
  bne ++
+ clc
  adc #($41-$0A) ; convert to PETSCII 'A' to 'F'
++jmp $ffd2 ; output character

uint64_adder:
uint128_adder:
  !byte 0, 0, 0, 0, 0, 0, 0, 0
  !byte 0, 0, 0, 0, 0, 0, 0, 0

uint64_divisor:
uint128_divisor:
  !byte 0, 0, 0, 0, 0, 0, 0, 0
  !byte 0, 0, 0, 0, 0, 0, 0, 0

uint64_shift:
uint128_shift:
  !byte 0, 0, 0, 0, 0, 0, 0, 0
  !byte 0, 0, 0, 0, 0, 0, 0, 0

uint64_sum:
uint128_sum:
  !byte 0, 0, 0, 0, 0, 0, 0, 0
  !byte 0, 0, 0, 0, 0, 0, 0, 0

uint64_remain:
uint128_remain:
  !byte 0, 0, 0, 0, 0, 0, 0, 0
  !byte 0, 0, 0, 0, 0, 0, 0, 0
