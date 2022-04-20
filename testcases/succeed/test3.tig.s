L18: .asciiz "Somebody"
L17: .asciiz "Nobody"
L20:
li $s7, 1000
sw $s7, 4($s6) 
la $s7, L17
sw $s7, 0($s6) 
li $s7, 8
move $a0, $s7
jal allocRecord
move $s6, $ra
move $s7, $s6
la $s6, L18
sw $s6, 0($s7) 
move $s7, $s7
j L19 
L19:
