L15:
li $t9, 10
move $a0, $t9
li $t9, 0
move $a1, $t9
jal initArray
move $t9, $ra
move $t9, $t9
j L14 
L14: