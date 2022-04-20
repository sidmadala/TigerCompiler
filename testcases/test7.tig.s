L42: .asciiz "str2"
L41: .asciiz " "
L40: .asciiz "str"
L44:
lw $s7 0($fp)
move $a0, $s7
move $a1, $s6
la $s7, L40
move $a2, $s7
jal L38
la $s7, L41
move $s7, $s7
j L43 
L43:
L46:
lw $s7 0($fp)
move $a0, $s7
addi $s7, $s6, 1
move $a1, $s7
jal L39
li $s7, 0
move $s7, $s7
j L45 
L45:
L48:
li $s7, 0
move $a0, $s7
li $s7, 0
move $a1, $s7
la $s7, L42
move $a2, $s7
jal L38
move $s7, $ra
j L47 
L47:
