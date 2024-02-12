
m:{[q] t {1=(x-y)*(x-y)}/: t: !q}
u:{[q; noleadingzeros] 1 - noleadingzeros , (q-1)#0}

matmul:{+/''(+x)*\:/:y}
vecmul:{+/',/x*\:/:+y}
matpow:{[mat; n] (=#mat) matmul/ n#,mat}

composite:{[q; r; noleadingzeros] vecmul[matpow[m[q]; r-1]; u[q; noleadingzeros]]}
n:{[q; r; noleadingzeros] +/composite[q; r; noleadingzeros]}

count:{[r] ((n[10; r; 1] - n[9; r; 1]) - n[9; r; 0]) + n[8; r; 0]}

rs:10+!31
+/count'rs
