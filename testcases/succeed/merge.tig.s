L77: .asciiz "-"
L60: .asciiz "9"
L59: .asciiz "0"
L49: .asciiz "
"
L48: .asciiz " "
L112:
L57:
add $s7, t~4, t174
lw $s7 0($s7)
move $a0, $s7
la $s7, L48
move $a1, $s7
jal stringEqual
move $s7, $ra
li $s6, 1
beq $s6, $s7, L54 
L55:
add $s7, t~4, t174
lw $s7 0($s7)
move $a0, $s7
la $s7, L49
move $a1, $s7
jal stringEqual
move $s7, $ra
L56:
li $s6, 1
beq $s6, $s7, L58 
L53:
li $s7, 0
move $s7, $s7
j L111 
L58:
add $s7, t~4, t174
move $s7, $s7
li $s6, 0
move $a0, $s6
jal L2
move $s6, $ra
sw $s6, 0($s7) 
j L57 
L54:
li $s7, 1
move $s7, $s7
j L56 
L111:
L114:
li $s7, 0
move $a0, $s7
add $s7, t~4, t174
lw $s7 0($s7)
move $a1, $s7
jal L3
move $s7, $ra
move $s7, $s7
li $s6, 0
move $a0, $s6
la $s6, L59
move $a1, $s6
jal L3
move $s6, $ra
bge $s7, $s6, L63 
L64:
li $s7, 0
move $s7, $s7
L65:
move $s7, $s7
j L113 
L63:
li $s7, 1
move t179, $s7
li $s7, 0
move $a0, $s7
add $s7, t~4, t174
lw $s7 0($s7)
move $a1, $s7
jal L3
move $s7, $ra
move $s7, $s7
li $s6, 0
move $a0, $s6
la $s6, L60
move $a1, $s6
jal L3
move $s6, $ra
ble $s7, $s6, L61 
L62:
li $s7, 0
move t179, $s7
L61:
move $s7, t179
j L65 
L113:
L116:
li $s7, 0
move t175, $s7
lw $s7 0($fp)
lw $s7 0($s7)
move $a0, $s7
jal L47
addi $s7, t174, 0
move t262, $s7
lw $s7 0($fp)
lw $s7 0($s7)
move $a0, $s7
add $s7, t~4, t174
lw $s7 0($s7)
move $a1, $s7
jal L46
move $s7, $ra
sw $s7, 0(t262) 
L67:
lw $s7 0($fp)
lw $s7 0($s7)
move $a0, $s7
add $s7, t~4, t174
lw $s7 0($s7)
move $a1, $s7
jal L46
move $s7, $ra
li $s6, 1
beq $s6, $s7, L68 
L66:
move $s7, t175
j L115 
L68:
li $s7, 10
mul $s7, t175, $s7
move t264, $s7
li $s7, 0
move $a0, $s7
add $s7, t~4, t174
lw $s7 0($s7)
move $a1, $s7
jal L3
move $s7, $ra
add $s7, t264, $s7
move $s7, $s7
li $s6, 0
move $a0, $s6
la $s6, L59
move $a1, $s6
jal L3
move $s6, $ra
sub $s7, $s7, $s6
move t175, $s7
add $s7, t~4, t174
move $s7, $s7
li $s6, 0
move $a0, $s6
jal L2
move $s6, $ra
sw $s6, 0($s7) 
j L67 
L115:
L118:
li $s7, 0
beq $s6, $s7, L73 
L74:
lw $s7 0($fp)
move $a0, $s7
lw $s70($s6)
move $a1, $s7
jal L71
li $s7, 0
move $a0, $s7
la $s7, L48
move $a1, $s7
jal L0
lw $s7 0($fp)
move $a0, $s7
lw $s74($s6)
move $a1, $s7
jal L72
move $s7, $ra
L75:
move $s7, $s7
j L117 
L73:
li $s7, 0
move $a0, $s7
la $s7, L49
move $a1, $s7
jal L0
move $s7, $ra
j L75 
L117:
L120:
li $s7, 0
move $s7, $s7
j L119 
L119:
L122:
li $s7, 0
blt t183, $s7, L84 
L85:
li $s7, 0
bgt t183, $s7, L81 
L82:
li $s7, 0
move $a0, $s7
la $s7, L59
move $a1, $s7
jal L0
move $s7, $ra
L83:
move $s7, $s7
L86:
move $s7, $s7
j L121 
L84:
li $s7, 0
move $a0, $s7
la $s7, L77
move $a1, $s7
jal L0
lw $s7 0($fp)
lw $s7 0($s7)
move $a0, $s7
li $s7, 0
sub $s7, $s7, t183
move $a1, $s7
jal L76
move $s7, $ra
j L86 
L81:
lw $s7 0($fp)
lw $s7 0($s7)
move $a0, $s7
move $a1, t183
jal L76
move $s7, $ra
j L83 
L121:
L124:
li $s7, 0
beq t181, $s7, L105 
L106:
li $s7, 0
beq t182, $s7, L102 
L103:
lw $s70(t181)
lw $s60(t182)
blt $s7, $s6, L99 
L100:
addi $s7, t212, 4
move t319, $s7
lw $s7 0($fp)
move $a0, $s7
move $a1, t181
lw $s74(t182)
move $a2, $s7
jal L70
move $s7, $ra
sw $s7, 0(t319) 
lw $s70(t182)
sw $s7, 0(t212) 
li $s7, 8
move $a0, $s7
jal allocRecord
move t212, $ra
move $s7, t212
L101:
move $s7, $s7
L104:
move $s7, $s7
L107:
move $s7, $s7
j L123 
L105:
move $s7, t182
j L107 
L102:
move $s7, t181
j L104 
L99:
addi $s7, t211, 4
move t317, $s7
lw $s7 0($fp)
move $a0, $s7
lw $s74(t181)
move $a1, $s7
move $a2, t182
jal L70
move $s7, $ra
sw $s7, 0(t317) 
lw $s70(t181)
sw $s7, 0(t211) 
li $s7, 8
move $a0, $s7
jal allocRecord
move t211, $ra
move $s7, t211
j L101 
L123:
L126:
li $s7, 0
sw $s7, 0($s6) 
li $s7, 4
move $a0, $s7
jal allocRecord
move $s6, $ra
move $s7, $s6
lw $s6 0($fp)
move $a0, $s6
move $a1, $s7
jal L45
move t220, $ra
li $s6, 1
lw $s70($s7)
beq $s6, $s7, L108 
L109:
li $s7, 0
move $s7, $s7
L110:
move $s7, $s7
j L125 
L108:
addi $s7, t222, 4
move $s7, $s7
lw $s6 0($fp)
move $a0, $s6
jal L69
move $s6, $ra
sw $s6, 0($s7) 
sw t220, 0(t222) 
li $s7, 8
move $a0, $s7
jal allocRecord
move t222, $ra
move $s7, t222
j L110 
L125:
L128:
add $s7, $s6, $fp
move $s7, $s7
li $s5, 0
move $a0, $s5
jal L2
move $s5, $ra
sw $s5, 0($s7) 
li $s7, 0
move $a0, $s7
jal L69
move t225, $ra
add $s7, $s6, $fp
move $s7, $s7
li $s6, 0
move $a0, $s6
jal L2
move $s6, $ra
sw $s6, 0($s7) 
li $s7, 0
move $a0, $s7
jal L69
move $s7, $ra
li $s6, 0
move $a0, $s6
move $a1, t225
move $a2, $s7
jal L70
move $s7, $ra
li $s6, 0
move $a0, $s6
move $a1, $s7
jal L72
move $s7, $ra
j L127 
L127:
