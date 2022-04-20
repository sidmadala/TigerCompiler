L147:
li $s7, 0
move $s7, $s7
li $s6, 100
move $s6, $s6
li $s5, 0
move $s5, $s5
li $s4, 0
ble $s6, $s4, L145 
L143:
li $s7, 0
move $s7, $s7
j L146 
L144:
addi $s6, $s6, 1
move $s6, $s6
L145:
addi $s7, $s7, 1
move $s7, $s7
ble $s6, $s5, L144 
L148:
j L143 
L146:
