L15: .asciiz "Somebody"
L14: .asciiz "Nobody"
L17:
li t139, 1000
sw 's0, 4(t138) 
la t140, L14
sw 's0, 0(t138) 
li t141, 2
move t106, t141
jalallocRecord
move t138, t132
move t137, t138
la t142, L15
sw 's0, 0(t137) 
move t101, t137
j L16 
L16:
