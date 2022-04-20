L37: .asciiz "str2"
L36: .asciiz "str"
L39:
lw $s7 0($fp)
move $a0, $s7
move $a1, $s6
la $s7, L36
move $a2, $s7
jal L34
move $s7, $ra
j L38 
L38:
L41:
lw $s7 0($fp)
move $a0, $s7
addi $s7, $s6, 1
move $a1, $s7
jal L35
move $s7, $ra
j L40 
L40:
L43:
li $s7, 0
move $a0, $s7
li $s7, 0
move $a1, $s7
la $s7, L37
move $a2, $s7
jal L34
move $s7, $ra
j L42 
L42:
