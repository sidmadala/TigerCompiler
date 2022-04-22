L172: .asciiz "Somebody"
L171: .asciiz "Nobody"
PROCEDURE L170
L174:
li $t9, 1000
sw $t9, 4($t8) 
la $t9, L171
sw $t9, 0($t8) 
li $t9, 8
move $a0, $t9
jal allocRecord
move $t8, $ra
move $t8, $t8
la $t9, L172
sw $t9, 0($t8) 
move $t9, $t8
j L173 
L173:
END L170
