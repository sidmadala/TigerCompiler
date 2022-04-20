L60: .asciiz "asd"
L59: .asciiz "Name"
L62:
li $s7, 0
sw $s7, 4($s6) 
la $s7, L59
sw $s7, 0($s6) 
li $s7, 2
move $a0, $s7
jal allocRecord
move $s6, $ra
move $s7, $s6
la $s6, L60
sw $s6, 8($s7) 
li $s7, 0
move $s7, $s7
j L61 
L61:
