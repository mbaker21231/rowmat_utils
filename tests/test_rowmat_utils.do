clear all

* Make the repository root visible to Stata/Mata
adopath ++ "."
mata: mata clear
mata: mata mlib index

mata:

X1 = 1,2,3 \ 4,5,6 \ 7,8,9
X2 = 2,3,4 \ 5,6,7 \ 8,9,10

Y1 = 3,4,5 \ 6,7,8 \ 9,10,11
Y2 = 4,5,6 \ 7,8,9 \ 10,11,12

X = rowshape(X1,1) \ rowshape(X2,1)
Y = rowshape(Y1,1) \ rowshape(Y2,1)

expected = rowshape(X1*Y1,1) \ rowshape(X2*Y2,1)
actual   = rm_matmult(X,Y)

assert(actual == expected)

printf("PASS: rm_matmult()\n")

/* ------------------------------------------------------------
   rm_transpose()
   ------------------------------------------------------------ */

expected = rowshape(X1',1) \ rowshape(X2',1)
actual   = rm_transpose(X)

assert(actual == expected)

printf("PASS: rm_transpose()\n")


/* ------------------------------------------------------------
   rm_matvecmult()
   ------------------------------------------------------------ */

b1 = 1 \ 2 \ 3
b2 = 4 \ 5 \ 6

B = b1' \ b2'

expected = (X1*b1)' \ (X2*b2)'
actual   = rm_matvecmult(X,B)

assert(actual == expected)

printf("PASS: rm_matvecmult()\n")


/* ------------------------------------------------------------
   rm_vecvecmult()
   ------------------------------------------------------------ */

u1 = 1 \ 2 \ 3
u2 = 4 \ 5 \ 6

v1 = 7 \ 8 \ 9
v2 = 2 \ 3 \ 4

U = u1' \ u2'
V = v1' \ v2'

expected = (u1'*v1) \ (u2'*v2)
actual   = rm_vecvecmult(U,V)

assert(actual == expected)

printf("PASS: rm_vecvecmult()\n")

/* Matrices used for row/column-sum and inversion tests */

A1 = 4,1,0 \ 1,3,1 \ 0,1,2
A2 = 3,.5,.2 \ .5,2,.4 \ .2,.4,1.5

A = rowshape(A1,1) \ rowshape(A2,1)


/* ------------------------------------------------------------
   rm_absrowsums()
   ------------------------------------------------------------ */

expected = rowsum(abs(A1))' \ rowsum(abs(A2))'
actual   = rm_absrowsums(A)

assert(actual == expected)

printf("PASS: rm_absrowsums()\n")


/* ------------------------------------------------------------
   rm_abscolsums()
   ------------------------------------------------------------ */

expected = colsum(abs(A1)) \ colsum(abs(A2))
actual   = rm_abscolsums(A)

assert(actual == expected)

printf("PASS: rm_abscolsums()\n")


/* ------------------------------------------------------------
   rm_alpha0()
   ------------------------------------------------------------ */

alpha1 = 1 / (max(rowsum(abs(A1))) * max(colsum(abs(A1))))
alpha2 = 1 / (max(rowsum(abs(A2))) * max(colsum(abs(A2))))

expected = alpha1 \ alpha2
actual   = rm_alpha0(A)

assert(max(abs(actual:-expected)) < 1e-12)

printf("PASS: rm_alpha0()\n")


/* ------------------------------------------------------------
   rm_newtinv()
   ------------------------------------------------------------ */

expected = rowshape(invsym(A1),1) \ rowshape(invsym(A2),1)
actual   = rm_newtinv(A,100,1e-12)

assert(max(abs(actual:-expected)) < 1e-8)

printf("PASS: rm_newtinv()\n")


printf("\nALL ROWMAT_UTILS TESTS PASSED\n")

end