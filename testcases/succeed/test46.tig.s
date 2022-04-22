PROCEDURE L194
L199:
li $s7, 0
move $s6, $s7
li $s7, 0
beq $s6, $s7, L195 
L201:
j L195 
L200:
li $t9, 1
move $t8, $t9
li $t9, 0
bne $s6, $t9, L196 
L197:
li $t9, 0
move $t8, $t9
L196:
move $t9, $t8
j L198 
L198:
END L194
