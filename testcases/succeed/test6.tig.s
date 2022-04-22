L179: .asciiz "str2"
L178: .asciiz "str"
PROCEDURE L177
L181:
lw $t9 0($fp)
move $a0, $t9
move $a1, $s7
la $t9, L178
move $a2, $t9
jal L176
move $t9, $ra
j L180 
L180:
END L177
PROCEDURE L176
L183:
lw $t9 0($fp)
move $a0, $t9
addi $t9, $s7, 1
move $a1, $t9
jal L177
move $t9, $ra
j L182 
L182:
END L176
PROCEDURE L175
L185:
li $t9, 0
move $a0, $t9
li $t9, 0
move $a1, $t9
la $t9, L179
move $a2, $t9
jal L176
move $t9, $ra
j L184 
L184:
END L175
