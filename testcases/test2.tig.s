L27:
li $s7, 10
move $a0, $s7
li $a0, 0
move $a1, $a0
jal initArray
move $s7, $ra
move $s7, $s7
j L26 
L26:
