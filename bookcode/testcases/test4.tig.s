L24:
li t147, 0
beq t143, t147, L20 
L21:
move t146, t143
lw t148 0('s0)
move t106, t148
li t150, 1
sub t149, t143, t150
move t107, t149
jalL19
move t145, t132
mul t151, t146, t145
move t144, t151
L22:
move t101, t144
j L23 
L20:
li t152, 1
move t144, t152
j L22 
L23:
L26:
li t153, 0
move t106, t153
li t154, 10
move t107, t154
jalL19
move t101, t132
j L25 
L25:
