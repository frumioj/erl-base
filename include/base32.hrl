% Z-base-32 decode mapping
-define(DECODE_MAP, {
    bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,  % 0-15
    bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,  % 16-31
    bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,  % 32-47
    bad,18,bad,25,26,27,30,29,7,31,bad,bad,bad,bad,bad,bad,           % 48-63
    bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,  % 64-79
    bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,  % 80-95
    bad,24,1,12,3,8,5,6,28,21,9,10,bad,11,2,16,                       % 96-111
    13,14,4,22,17,19,bad,20,15,0,23,bad,bad,bad,bad,bad,             % 112-127
    bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,  % 128-143
    bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,  % 144-159
    bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,  % 160-175
    bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,  % 176-191
    bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,  % 192-207
    bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,  % 208-223
    bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,  % 224-239
    bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad,bad   % 240-255
}).

% Z-base-32 encode mapping (ybndrfg8ejkmcpqxot1uwisza345h769)
-define(ENCODE_MAP, {
    $y, $b, $n, $d, $r, $f, $g, $8,
    $e, $j, $k, $m, $c, $p, $q, $x,
    $o, $t, $1, $u, $w, $i, $s, $z,
    $a, $3, $4, $5, $h, $7, $6, $9
}).
