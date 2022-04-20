L27:
li $s7, 0
beq $s6, $s7, L23 
L24:
move $s7, $s6
lw $s5 0($fp)
move $a0, $s5
li $s5, 1
sub $s6, $s6, $s5
move $a1, $s6
jal L22
move $s6, $ra
mul $s7, $s7, $s6
move $s7, $s7
L25:
move $s7, $s7
j L26 
L23:
li $s7, 1
move $s7, $s7
j L25 
L26:
L29:
li $s7, 0
move $a0, $s7
li $s7, 10
move $a1, $s7
jal L22
move $s7, $ra
j L28 
L28:
