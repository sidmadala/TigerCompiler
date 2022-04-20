L23: .asciiz "str2"
L22: .asciiz "str"
L25:
lw $s7 0($fp)
move $a0, $s7
move $a1, $s6
la $s7, L22
move $a2, $s7
jal L20
move $s7, $ra
j L24 
L24:
L27:
lw $s7 0($fp)
move $a0, $s7
addi $s7, $s6, 1
move $a1, $s7
jal L21
move $s7, $ra
j L26 
L26:
L29:
li $s7, 0
move $a0, $s7
li $s7, 0
move $a1, $s7
la $s7, L23
move $a2, $s7
jal L20
move $s7, $ra
j L28 
L28:
