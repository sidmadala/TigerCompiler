L183: .asciiz "
"
L174: .asciiz " ."
L173: .asciiz " O"
L187:
add $s7, $s6, $s6
lw $s7 0($s7)
beq $s6, $s7, L168 
L169:
add $s7, $s6, $s6
lw $s7 0($s7)
li $s6, 1
sub $s7, $s7, $s6
move $s7, $s7
li $s6, 0
move $s6, $s6
li $s5, 0
ble $s7, $s5, L167 
L150:
li $s7, 0
move $s7, $s7
L170:
move $s7, $s7
j L186 
L168:
lw $s7 0($fp)
move $a0, $s7
jal L130
move $s7, $ra
j L170 
L166:
addi $s7, $s7, 1
move $s7, $s7
L167:
ble $s7, $s6, L166 
L188:
j L150 
L186:
L190:
add $s7, t0, $fp
lw $s7 0($s7)
add $s7, t~4, $s7
lw $s7 0($s7)
li $s6, 1
sub $s7, $s7, $s6
move t503, $s7
li $s7, 0
move t502, $s7
li $s7, 0
ble t503, $s7, L185 
L171:
li $s7, 0
move $a0, $s7
la $s7, L183
move $a1, $s7
jal L0
move $s7, $ra
j L189 
L184:
addi $s7, t503, 1
move t503, $s7
L185:
add $s7, t0, $fp
lw $s7 0($s7)
add $s7, t~4, $s7
lw $s7 0($s7)
li $s6, 1
sub $s7, $s7, $s6
move t501, $s7
li $s7, 0
move t500, $s7
li $s7, 0
ble t501, $s7, L182 
L172:
li $s7, 0
move $a0, $s7
la $s7, L183
move $a1, $s7
jal L0
ble t503, t502, L184 
L191:
j L171 
L181:
addi $s7, t501, 1
move t501, $s7
L182:
add $s7, t0, $fp
lw $s7 0($s7)
add $s7, t~12, $s7
lw $s7 0($s7)
li $s6, 4
mul $s6, t484, $s6
add $s7, $s7, $s6
move $s7, $s7
lw $s7 0($s7)
beq $s7, t485, L178 
L179:
la $s7, L174
move $s7, $s7
L180:
li $s6, 0
move $a0, $s6
move $a1, $s7
jal L0
ble t501, t500, L181 
L192:
j L172 
L178:
la $s7, L173
move $s7, $s7
j L180 
L189:
L194:
li $s7, 8
add $s6, t~4, $fp
sw $s7, 0($s6) 
add $s7, $s5, $fp
move t549, $s7
add $s7, t~4, $fp
lw $s7 0($s7)
move $a0, $s7
li $s7, 0
move $a1, $s7
jal initArray
move $s7, $ra
sw $s7, 0(t549) 
add $s7, t~12, $fp
move t551, $s7
add $s7, t~4, $fp
lw $s7 0($s7)
move $a0, $s7
li $s7, 0
move $a1, $s7
jal initArray
move $s7, $ra
sw $s7, 0(t551) 
add $s7, t~16, $fp
move t553, $s7
add $s7, t~4, $fp
lw $s7 0($s7)
add $s6, t~4, $fp
lw $s6 0($s6)
add $s7, $s7, $s6
li $s6, 1
sub $s7, $s7, $s6
move $a0, $s7
li $s7, 0
move $a1, $s7
jal initArray
move $s7, $ra
sw $s7, 0(t553) 
add $s7, t~20, $fp
move t555, $s7
add $s7, t~4, $fp
lw $s7 0($s7)
add $s6, t~4, $fp
lw $s6 0($s6)
add $s7, $s7, $s6
li $s6, 1
sub $s7, $s7, $s6
move $a0, $s7
li $s7, 0
move $a1, $s7
jal initArray
move $s7, $ra
sw $s7, 0(t555) 
li $s7, 0
move $a0, $s7
li $s7, 0
move $a1, $s7
jal L131
move $s7, $ra
j L193 
L193:
