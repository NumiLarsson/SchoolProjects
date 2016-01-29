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
	
	mfc0 $k0, $13 	#move cause register ($13) to a usable register.
        
        ### Extract the exception code from the cause register (bit 2-5) to $k1.
        srl $k1, $k0, 2
        andi $k1, $k1, 31 	# This could be done with andi $k1, $k0, 124 / 0x7c / 1111100 in binary 

	
	##### IS IT AN EXCEPTION OR AN INTERRUPT? #####
	
        bne $k1, $zero, __unhandled_exception  #if bit 8 isn't set it's an exception.

__interrupt:
	#################################################
	#### STEP 3 - What kind of interrupt is it? #####
	#################################################
	        
        # If a receiver interrupt, the receiver interrupt pending bit (bit 8)
        # will be set to 1 in Cause register.  
	
	# $k0 - holds value of the cause register. 
	
	### Check if bit 8 is set to 1 in Cause register. 

	### TODO: Load the value at __MASK_STATUS_RECEIVER_INTERRUPT to $t0.
	lw $t0, __MASK_STATUS_RECEIVER_INTERRUPT
	 
       	and $k1, $t0, $k0 	#is bit 8 set? If so, it is a receiver interrupt.
       	
       	# If not a receiver interrupt, jump to __unhandled_interrupt.
       	
       	bne $k1, $t0, __unhandled_interrupt

__kbd_interrupt:
	
	nop # nop (NO Operation) used to make it possible to set breakpoint here. 
	
	# $t0 - __MASK_STATUS_RECEIVER_INTERRUPT
	
	# Reset Cause register, i.e., set bit 8 (receiver interrupt pending) to zero.
	
	not $t0, $t0
	and $k0, $k0, $t0
	mtc0 $k0, $13
	
	# step 4

        lw $k1, RECEIVER_DATA 	#load receiver adress
        
        # Load the ASCII value from the memory-mapped receiver data 
        # register to $a0.
        lw $a0, ($k1)		#Load the value from the memory-mapped receiver data register.

	####################################
	##### STEP 5 - Print character #####
	####################################
	
        li $v0, 11 # System call 11 (print_char)
        syscall
       
       	j __restore_registers

       	
__unhandled_exception:
	
	# $k1 - exception code.
	
	li $v0, 4
	la $a0, __unhandled_exception_msg_1
	syscall
	
	li $v0, 1
	move $a0, $k1
	syscall
	
	li $v0, 4
	la $a0, __unhandled_exception_msg_2
	syscall
	
	
__return_from_exception:

   	# Skip instruction causing the exception, otherwise the same exception
   	# will trigger again.

   	mfc0 $k0, $14    # Coprocessor 0 register $14 (EPC) has address of trapping instruction.
   	addi $k0, $k0, 4 # Add 4 to point to next instruction.
   	mtc0 $k0, $14    # Store new address back into $14 (EPC).
   
   	j __restore_registers
   
__unhandled_interrupt:

	li $v0, 4
	la $a0, __unhandled_interrupt_msg
	syscall


__restore_registers:

	######################################
   	##### STEP 6 - Restore registers #####
	######################################
	
	lw $v0, __v0
	lw $a0, __a0
	lw $a1, __a1
	lw $t0, __t0
	
  	# .set noat	# SPIM - Turn of warnings for using the $at register.
     	lw $at, __at
        # .set at       # SPIM - Turn on warnings for using the $at register.

__resume:
	# step 7
	eret


       	
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