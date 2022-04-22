L46: .asciiz "-"
L29: .asciiz "9"
L28: .asciiz "0"
L18: .asciiz "
"
L17: .asciiz " "
PROCEDURE L16
L81:
L26:
add $t9, $s7, $s7
lw $t9 0($t9)
move $a0, $t9
la $t9, L17
move $a1, $t9
jal stringEqual
move $t8, $ra
li $t9, 1
beq $t9, $t8, L23 
L24:
add $t9, $s7, $s7
lw $t9 0($t9)
move $a0, $t9
la $t9, L18
move $a1, $t9
jal stringEqual
move $t8, $ra
L25:
li $t9, 1
beq $t9, $t8, L27 
L22:
li $t9, 0
move $t9, $t9
j L80 
L27:
add $t9, $s7, $s7
move t190, $t9
li $t9, 0
move $a0, $t9
jal L2
move $t9, $ra
sw $t9, 0(t190) 
j L26 
L23:
li $t9, 1
move $t8, $t9
j L25 
L80:
END L16
PROCEDURE L15
L83:
li $t9, 0
move $a0, $t9
add $t9, $s7, $s7
lw $t9 0($t9)
move $a1, $t9
jal L3
move $t9, $ra
move t206, $t9
li $t9, 0
move $a0, $t9
la $t9, L28
move $a1, $t9
jal L3
move $t9, $ra
bge t206, $t9, L32 
L33:
li $t9, 0
move $t9, $t9
L34:
move $t9, $t9
j L82 
L32:
li $t9, 1
move t141, $t9
li $t9, 0
move $a0, $t9
add $t9, $s7, $s7
lw $t9 0($t9)
move $a1, $t9
jal L3
move $t9, $ra
move t209, $t9
li $t9, 0
move $a0, $t9
la $t9, L29
move $a1, $t9
jal L3
move $t9, $ra
ble t209, $t9, L30 
L31:
li $t9, 0
move t141, $t9
L30:
move $t9, t141
j L34 
L82:
END L15
PROCEDURE L14
L85:
li $t9, 0
move t137, $t9
lw $t9 0($fp)
lw $t9 0($t9)
move $a0, $t9
jal L16
addi $t9, $s7, 0
move t224, $t9
lw $t9 0($fp)
lw $t9 0($t9)
move $a0, $t9
add $t9, $s7, $s7
lw $t9 0($t9)
move $a1, $t9
jal L15
move $t9, $ra
sw $t9, 0(t224) 
L36:
lw $t9 0($fp)
lw $t9 0($t9)
move $a0, $t9
add $t9, $s7, $s7
lw $t9 0($t9)
move $a1, $t9
jal L15
move $t8, $ra
li $t9, 1
beq $t9, $t8, L37 
L35:
move $t9, t137
j L84 
L37:
li $t9, 10
mul $t9, t137, $t9
move t226, $t9
li $t9, 0
move $a0, $t9
add $t9, $s7, $s7
lw $t9 0($t9)
move $a1, $t9
jal L3
move $t9, $ra
add $t9, t226, $t9
move t228, $t9
li $t9, 0
move $a0, $t9
la $t9, L28
move $a1, $t9
jal L3
move $t9, $ra
sub $t9, t228, $t9
move t137, $t9
add $t9, $s7, $s7
move t230, $t9
li $t9, 0
move $a0, $t9
jal L2
move $t9, $ra
sw $t9, 0(t230) 
j L36 
L84:
END L14
PROCEDURE L41
L87:
li $t9, 0
beq $s7, $t9, L42 
L43:
lw $t9 0($fp)
move $a0, $t9
lw $t90($s7)
move $a1, $t9
jal L40
li $t9, 0
move $a0, $t9
la $t9, L17
move $a1, $t9
jal L0
lw $t9 0($fp)
move $a0, $t9
lw $t94($s7)
move $a1, $t9
jal L41
move $t9, $ra
L44:
move $t9, $t9
j L86 
L42:
li $t9, 0
move $a0, $t9
la $t9, L18
move $a1, $t9
jal L0
move $t9, $ra
j L44 
L86:
END L41
PROCEDURE L45
L89:
li $t9, 0
move $t9, $t9
j L88 
L88:
END L45
PROCEDURE L40
L91:
li $t9, 0
blt $s7, $t9, L53 
L54:
li $t9, 0
bgt $s7, $t9, L50 
L51:
li $t9, 0
move $a0, $t9
la $t9, L28
move $a1, $t9
jal L0
move $t9, $ra
L52:
move $t9, $t9
L55:
move $t9, $t9
j L90 
L53:
li $t9, 0
move $a0, $t9
la $t9, L46
move $a1, $t9
jal L0
lw $t9 0($fp)
lw $t9 0($t9)
move $a0, $t9
li $t9, 0
sub $t9, $t9, $s7
move $a1, $t9
jal L45
move $t9, $ra
j L55 
L50:
lw $t9 0($fp)
lw $t9 0($t9)
move $a0, $t9
move $a1, $s7
jal L45
move $t9, $ra
j L52 
L90:
END L40
PROCEDURE L39
L93:
li $t9, 0
beq $s7, $t9, L74 
L75:
li $t9, 0
beq $s7, $t9, L71 
L72:
lw $t80($s7)
lw $t90($s7)
blt $t8, $t9, L68 
L69:
addi $t9, t174, 4
move t281, $t9
lw $t9 0($fp)
move $a0, $t9
move $a1, $s7
lw $t94($s7)
move $a2, $t9
jal L39
move $t9, $ra
sw $t9, 0(t281) 
lw $t90($s7)
sw $t9, 0(t174) 
li $t9, 8
move $a0, $t9
jal allocRecord
move t174, $ra
move $t9, t174
L70:
move $t9, $t9
L73:
move $t9, $t9
L76:
move $t9, $t9
j L92 
L74:
move $t9, $s7
j L76 
L71:
move $t9, $s7
j L73 
L68:
addi $t9, t173, 4
move t279, $t9
lw $t9 0($fp)
move $a0, $t9
lw $t94($s7)
move $a1, $t9
move $a2, $s7
jal L39
move $t9, $ra
sw $t9, 0(t279) 
lw $t90($s7)
sw $t9, 0(t173) 
li $t9, 8
move $a0, $t9
jal allocRecord
move t173, $ra
move $t9, t173
j L70 
L92:
END L39
PROCEDURE L38
L95:
li $t8, 0
sw $t8, 0($t9) 
li $t9, 4
move $a0, $t9
jal allocRecord
move $t9, $ra
move t180, $t9
lw $t9 0($fp)
move $a0, $t9
move $a1, t180
jal L14
move t182, $ra
li $t8, 1
lw $t90(t180)
beq $t8, $t9, L77 
L78:
li $t9, 0
move $t9, $t9
L79:
move $t9, $t9
j L94 
L77:
addi $t9, t184, 4
move t297, $t9
lw $t9 0($fp)
move $a0, $t9
jal L38
move $t9, $ra
sw $t9, 0(t297) 
sw t182, 0(t184) 
li $t9, 8
move $a0, $t9
jal allocRecord
move t184, $ra
move $t9, t184
j L79 
L94:
END L38
PROCEDURE L13
L97:
add $t9, $s7, $fp
move t308, $t9
li $t9, 0
move $a0, $t9
jal L2
move $t9, $ra
sw $t9, 0(t308) 
li $t9, 0
move $a0, $t9
jal L38
move t187, $ra
add $t9, $s7, $fp
move t310, $t9
li $t9, 0
move $a0, $t9
jal L2
move $t9, $ra
sw $t9, 0(t310) 
li $t9, 0
move $a0, $t9
jal L38
move $t8, $ra
li $t9, 0
move $a0, $t9
move $a1, t187
move $a2, $t8
jal L39
move $t8, $ra
li $t9, 0
move $a0, $t9
move $a1, $t8
jal L41
move $t9, $ra
j L96 
L96:
END L13
