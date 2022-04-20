L139: .asciiz "sdf"
L138: .asciiz "sfd"
L137: .asciiz "kati"
L136: .asciiz "Allos"
L135: .asciiz "Kapou"
L134: .asciiz "Kapoios"
L133: .asciiz ""
L132: .asciiz "somewhere"
L131: .asciiz "aname"
L141:
li $t9, 10
move $a0, $t9
li $t9, 0
move $a1, $t9
jal initArray
move t665, $ra
li $t9, 0
sw $t9, 12(t670) 
li $t9, 0
sw $t9, 8(t670) 
la $t9, L132
sw $t9, 4(t670) 
la $t9, L131
sw $t9, 0(t670) 
li $t9, 16
move $a0, $t9
jal allocRecord
move t670, $ra
li $t9, 5
move $a0, $t9
move $a1, t670
jal initArray
move t668, $ra
li $t9, 100
move $a0, $t9
la $t9, L133
move $a1, $t9
jal initArray
move t671, $ra
li $t9, 44
sw $t9, 12(t674) 
li $t9, 2432
sw $t9, 8(t674) 
la $t9, L135
sw $t9, 4(t674) 
la $t9, L134
sw $t9, 0(t674) 
li $t9, 16
move $a0, $t9
jal allocRecord
move t674, $ra
move t673, t674
addi $t9, t677, 4
move t697, $t9
li $t9, 3
move $a0, $t9
li $t9, 1900
move $a1, $t9
jal initArray
move $t9, $ra
sw $t9, 0(t697) 
la $t9, L136
sw $t9, 0(t677) 
li $t9, 8
move $a0, $t9
jal allocRecord
move t677, $ra
move t676, t677
li $t9, 0
li $t8, 4
mul $t9, $t9, $t8
add $t9, t665, $t9
move $t9, $t9
li $t8, 1
sw $t8, 0($t9) 
li $t9, 9
li $t8, 4
mul $t9, $t9, $t8
add $t9, t665, $t9
move $t9, $t9
li $t8, 3
sw $t8, 0($t9) 
li $t9, 3
li $t8, 4
mul $t9, $t9, $t8
add $t9, t668, $t9
move $t9, $t9
la $t8, L137
lw $t9 0($t9)
sw $t8, 0($t9) 
li $t9, 1
li $t8, 4
mul $t9, $t9, $t8
add $t9, t668, $t9
move $t9, $t9
li $t8, 23
lw $t9 0($t9)
sw $t8, 12($t9) 
li $t9, 34
li $t8, 4
mul $t9, $t9, $t8
add $t9, t671, $t9
move $t9, $t9
la $t8, L138
sw $t8, 0($t9) 
la $t9, L139
sw $t9, 0(t673) 
lw $t94(t676)
li $t8, 0
li $t7, 4
mul $t8, $t8, $t7
add $t9, $t9, $t8
move $t9, $t9
li $t8, 2323
sw $t8, 0($t9) 
lw $t94(t676)
li $t8, 2
li $t7, 4
mul $t8, $t8, $t7
add $t9, $t9, $t8
move $t9, $t9
li $t8, 2323
sw $t8, 0($t9) 
li $t9, 0
move $t9, $t9
j L140 
L140:
