L39:
li $s7, 10
move $a0, $s7
li $s7, 0
move $a1, $s7
jal initArray
move $s7, $ra
li $s6, 2
li $s5, 4
mul $s6, $s6, $s5
add $s7, $s7, $s6
move $s7, $s7
lw $s7 0($s7)
move $s7, $s7
j L38 
L38:
