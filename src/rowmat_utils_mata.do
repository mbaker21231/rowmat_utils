mata:
mata clear
mata set matastrict on
real matrix rm_matmult(real matrix A,real matrix B)
{
	real matrix C,Bt,Ap,Bp
	real scalar dim,i,j
	
	C=J(rows(A),0,.)
	dim=sqrt(cols(A))
	Bt=rm_transpose(B)
	for (i=1;i<=cols(A);i=i+dim) {
	for (j=1;j<=cols(A);j=j+dim) {
		Ap=A[,i::i+dim-1]
		Bp=Bt[,j::j+dim-1]
		C=C,rowsum(Ap:*Bp)
					 }
					 }
	return(C)
}
real matrix rm_vecvecmult(real matrix A,real matrix B) return(rowsum(A:*B))
real matrix rm_matvecmult(real matrix A,real matrix B)
{
	real matrix C
	real scalar dim,i
	
	C=J(rows(A),0,.)
	dim=sqrt(cols(A))
	for (i=1;i<=cols(A);i=i+dim) C=C,rm_vecvecmult(A[,i::i+dim-1],B)
	return(C)
}
real matrix rm_newtinv(real matrix A,real scalar maxiter,real scalar crit)
{
	real matrix alpha,Id,At,Xn,Xo
	real scalar dim,it,cha

	dim=sqrt(cols(A))
	alpha=rm_alpha0(A)
	Id=rowshape(I(dim),1)
	Id=J(rows(A),1,Id)

	At=rm_transpose(A)
	Xn=alpha:*At
	it=0
	do {
		Xo=Xn
		Xn=rm_matmult(Xo,2*Id-rm_matmult(A,Xn))
		cha=max(abs(Xo:-Xn))
		it=it+1
		} while (cha>crit & it<=maxiter)
	return(Xn)
}
real matrix rm_transpose(real matrix A)
{
	real matrix PV
	real scalar dim,i,j
	
	PV=J(1,0,.)
	dim=sqrt(cols(A))
	for (i=1;i<=dim;i++) {
	for (j=1;j<=dim;j++) {
		PV=PV,i+(j-1)*dim
		}
		}
	return(A[,PV])
}
real matrix rm_absrowsums(real matrix A)
{
	real matrix C
	real scalar dim,i
	
	dim=sqrt(cols(A))
	C=J(rows(A),0,.)
	for (i=1;i<=cols(A);i=i+dim) 	C=C,rowsum(abs(A[,i::i+dim-1]))
	return(C)
}

real matrix rm_abscolsums(real matrix A)
{
	real matrix At,C
	real scalar dim,i
	
	At=rm_transpose(A)
	dim=sqrt(cols(A))
	C=J(rows(A),0,.)
	for (i=1;i<=cols(A);i=i+dim) 	C=C,rowsum(abs(At[,i::i+dim-1]))
	return(C)	
}		
real matrix rm_alpha0(real matrix A) return(1:/(rowmax(rm_absrowsums(A)):*rowmax(rm_abscolsums(A))))
mata mlib create lrowmat_utils, dir(PERSONAL) replace
mata mlib add lrowmat_utils *()
mata mlib index
end
