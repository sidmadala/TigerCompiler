L152: .asciiz "
"
L143: .asciiz " ."
L142: .asciiz " O"
PROCEDURE L100
L156:
add $t9, $s7, $s7
lw $t9 0($t9)
beq $s7, $t9, L137 
L138:
add $t9, $s7, $s7
lw $t8 0($t9)
li $t9, 1
sub $t9, $t8, $t9
move $t7, $t9
li $t9, 0
move $t8, $t9
li $t9, 0
ble $t7, $t9, L136 
L119:
li $t9, 0
move $t9, $t9
L139:
move $t9, $t9
j L155 
L137:
lw $t9 0($fp)
move $a0, $t9
jal L99
move $t9, $ra
j L139 
L135:
addi $t9, $t7, 1
move $t7, $t9
L136:
ble $t7, $t8, L135 
L157:
j L119 
L155:
END L100
PROCEDURE L99
L159:
add $t9, t0, $fp
lw $t9 0($t9)
add $t9, t~4, $t9
lw $t8 0($t9)
li $t9, 1
sub $t9, $t8, $t9
move t465, $t9
li $t9, 0
move t464, $t9
li $t9, 0
ble t465, $t9, L154 
L140:
li $t9, 0
move $a0, $t9
la $t9, L152
move $a1, $t9
jal L0
move $t9, $ra
j L158 
L153:
addi $t9, t465, 1
move t465, $t9
L154:
add $t9, t0, $fp
lw $t9 0($t9)
add $t9, t~4, $t9
lw $t8 0($t9)
li $t9, 1
sub $t9, $t8, $t9
move t463, $t9
li $t9, 0
move t462, $t9
li t495, 0
ble t463, t495, L151 
L141:
li $t9, 0
move $a0, $t9
la $t9, L152
move $a1, $t9
jal L0
ble t465, t464, L153 
L160:
j L140 
L150:
addi $t9, t463, 1
move t463, $t9
L151:
add t503, t0, $fp
lw t502 0(t503)
add t501, t~12, t502
lw t500 0(t501)
li t505, 4
mul t504, t446, t505
add t499, t500, t504
move t459, t499
lw t506 0(t459)
beq t506, t447, L147 
L148:
la t507, L143
move t461, t507
L149:
li t508, 0
move $a0, t508
move $a1, t461
jal L0
ble t463, t462, L150 
L161:
j L141 
L147:
la t509, L142
move t461, t509
j L149 
L158:
END L99
PROCEDURE L98
L163:
li $t8, 8
add $t9, $s7, $fp
sw $t8, 0($t9) 
add $t9, $s7, $fp
move t511, $t9
add $t9, $s7, $fp
lw $t9 0($t9)
move $a0, $t9
li $t9, 0
move $a1, $t9
jal initArray
move $t9, $ra
sw $t9, 0(t511) 
add $t9, $s7, $fp
move t513, $t9
add $t9, $s7, $fp
lw $t9 0($t9)
move $a0, $t9
li $t9, 0
move $a1, $t9
jal initArray
move $t9, $ra
sw $t9, 0(t513) 
add $t9, $s7, $fp
move t515, $t9
add $t9, $s7, $fp
lw $t8 0($t9)
add $t9, $s7, $fp
lw $t9 0($t9)
add $t8, $t8, $t9
li $t9, 1
sub $t9, $t8, $t9
move $a0, $t9
li $t9, 0
move $a1, $t9
jal initArray
move $t9, $ra
sw $t9, 0(t515) 
add $t9, $s7, $fp
move t517, $t9
add $t9, $s7, $fp
lw $t8 0($t9)
add $t9, $s7, $fp
lw $t9 0($t9)
add $t8, $t8, $t9
li $t9, 1
sub $t9, $t8, $t9
move $a0, $t9
li $t9, 0
move $a1, $t9
jal initArray
move $t9, $ra
sw $t9, 0(t517) 
li $t9, 0
move $a0, $t9
li $t9, 0
move $a1, $t9
jal L100
move $t9, $ra
j L162 
L162:
END L98
