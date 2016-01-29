.data

INPUT_BUFFER: .space 32
MSG:          .asciiz "\n\nString read from user: "      

RECEIVER_CONTROL: 		.word 0xffff0000
RECEIVER_DATA:			.word 0xffff0004

TRANSMITTER_CONTROL:		.word 0xffff0008
TRANSMITTER_DATA:		.word 0xffff000c

MASK_RECEIVER_INTERRUPT:	.word 0x00000002
MASK_ENABLE_ALL_INTERRUPTS:	.word 0x0000ff01 

  
        .text

main:
	
        # Enable interrupts ...
	lw $t0, RECEIVER_CONTROL
	lw $t1, MASK_RECEIVER_INTERRUPT
	sw $t1, 0($t0)
        # Other necessary initialization ...
    
 
program_0:
 
        # First program_0 executes. 

        # Prepare system call to read string. 

        # System call code 8 (read_string)
        li $v0, 8 

        # Address of buffer to store characters read from the keyboard. 
        la $a0, INPUT_BUFFER 

        # Size of buffer.
        li $a1, 32 

        # Initiate the read_string system call by causing a trap exception 
        # (exception code 13).
        teqi $zero, 0 

        # NOTE: program_0 should block until the string is ready.

        # NOTE: When the string is ready, program_0 should resume exception here. 

        # Print the string using the built in system call print_string. 

        li $v0, 4
        la $a0, MSG
        syscall 
 
        li $v0, 4
        la $a0, INPUT_BUFFER
        syscall

        # terminate normally.
 
        li $v0, 10 
        syscall 

 
program_1:
        
        # Code for program 1 goes here. 
         
        # Perform some work ...
        addi $t0, $t0, 1 

        # NOTE: You may try to make program_1 do something more interesting later.

        # For simplicity, make program_1 loop for ever. 
        j program_1
 

        .kdata

# What do you need to store here?

__a0:   .word 0
__a1:   .word 0
__v0:   .word 0
__at:   .word 0
__t0:   .word 0




# ---------------------------------------------------------------------------
# EXCEPTION HANDLER
#
# Kernel text segment, i.e., code for the exception/interrupt handler. 
# 
# The term exception is commonly used to refer to both exceptions and 
# interrupts. 
#
# Overall structure of the exception/interrupt handler:
#
# 1) Save contents in any registers (except $k0 and $k1) used by the
#    exception handler. 
#
# 2) Examine the cause register to find out if the reason for entering the
#    exception handler is an interrupt or and exception. If it is an 
#    exception, skip the offending instruction (EPC + 4) and go to step 6.
#
# 3) If it is an interrupt, find out if it is a keyboard interrupt
#    (hardware level 0). If it is not a keyboard interrupt, print a log 
#    message and go to step 6.
#
# 4) If it is a keyboard interrupt, read character from memory-mapped
#    receiver data register. 
#
# 5) Print the read character to the Run I/O console using the
#    MARS built in system call print_char.
#
# 6) Restore the contents the registers saved in step 1. 
#
# 7) Resume user level execution (eret instruction). 
# ---------------------------------------------------------------------------

        
        .ktext 0x80000180

__save_registers:

 	##################################
        ##### STEP 1: SAVE REGISTERS #####
	##################################
		
        # .set noat             # SPIM - Turn of warnings for using the $at register.
        move $k0, $at		# Copy value of $at to $k0.
       	sw $k0, __at		# Save value of $at to memory.
        # .set at               # SPIM - Turn on warnings for using the $at register.

	sw $v0, __v0
	sw $a0, __a0
	sw $a1, __a1
	sw $t0, __t0

	
	##############################################
	##### STEP 2: EXAMINE THE CAUSE REGISTER #####
	##############################################
	
	### TODO: Use the mfc0 instruction to move the value in the coprocessor 0 
	### Cause register ($13) to $k0. 
	
	mfc0 $k0, $13 	#move cause register ($13) to a usable register.
	
        
        ### TODO: Extract the exception code from the Cause register and store
        ### the exception code in $k1. 
        
        ### Extract the exception code (bit 2-5) to $k1.
        srl $k1, $k0, 2
        andi $k1, $k1, 31 	# This could be done with andi $k1, $k0, 124 / 0x7c / 1111100 in binary 
        
        ### TIP 2: Remember that other bits (for example pending interrupt bits) 
        ### may be set in the Cause register. You can use the andi (AND Immediate) 
        ### instruction to clear all but the 5 least significant bits. 


	
	##### IS IT AN EXCEPTION OR AN INTERRUPT? #####
	
        bne $k1, $zero, __unhandled_exception

__interrupt:
	
# Handle the trap exception and receiver ready interrupt.

# A read_string system call is initiated when a trap exception (exception code 13)
# occurs and $v0 == 8 (read_string system call code). 

# What information must be saved by the kernel when a read_string system call is initiated?

# How can the kernel detect if the read_string system call is complete?

# How can the kernel make program_1 execute until the read_string system call is complete?

# How can the kernel resume execution of program_0 when the read_string system call is
# complete? 

# After the read_string system call has been initiated, every time a receiver interrupt 
# occurs: 
#         - the kernel can store a character in the input buffer. 
#         - you may want to echo each character to the Run I/O display. 

# How can the kernel detect when the buffer is full?

# Should pressing on the enter key end the read_string system call even if the
# buffer is not full?