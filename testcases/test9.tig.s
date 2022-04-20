L31: .asciiz " "
L36:
li $s7, 5
li $s6, 4
bgt $s7, $s6, L32 
L33:
la $s7, L31
move $s7, $s7
L34:
move $s7, $s7
j L35 
L32:
li $s7, 13
move $s7, $s7
j L34 
L35:
