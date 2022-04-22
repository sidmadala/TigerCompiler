PROCEDURE L218
L220:
li $t9, 96
sub $t9, $fp, $t9
move $fp, $t9
li $t9, 96
sub $t9, $sp, $t9
move $sp, $t9
sw $s0, -4($fp) 
sw $s1, -8($fp) 
sw $s2, -12($fp) 
sw $s3, -16($fp) 
sw $s4, -20($fp) 
sw $s5, -24($fp) 
sw $s6, -28($fp) 
sw $s7, -32($fp) 
sw $ra, -36($fp) 
li $s7, 10
move $a0, $s7
li $s7, 0
move $a1, $s7
jal initArray
move $s7, $ra
lw $s7-4($fp)
move $s0, $s7
lw $s7-8($fp)
move $s1, $s7
lw $s7-12($fp)
move $s2, $s7
lw $s7-16($fp)
move $s3, $s7
lw $s7-20($fp)
move $s4, $s7
lw $s7-24($fp)
move $s5, $s7
lw $s7-28($fp)
move $s6, $s7
lw $s7-32($fp)
move $s7, $s7
lw $t9-36($fp)
move $ra, $t9
addi $t9, $fp, 96
move $fp, $t9
addi $t9, $sp, 96
move $sp, $t9
L219:
END L218
