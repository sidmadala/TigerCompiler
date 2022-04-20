L54:
li $s7, 10
li $s6, 20
bgt $s7, $s6, L50 
L51:
li $s7, 40
move $s7, $s7
L52:
move $s7, $s7
j L53 
L50:
li $s7, 30
move $s7, $s7
j L52 
L53:
