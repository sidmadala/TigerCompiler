L57:
li $s7, 0
sw $s7, 4($s6) 
li $s7, 0
sw $s7, 0($s6) 
li $s7, 2
move $a0, $s7
jal allocRecord
move $s6, $ra
move $s7, $s6
move $s7, $s7
j L56 
L56:
