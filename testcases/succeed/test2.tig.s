L15:
li $s7, 10
move $a0, $s7
li $s7, 0
move $a1, $s7
jal initArray
move $s7, $ra
move $s7, $s7
j L14 
L14:
