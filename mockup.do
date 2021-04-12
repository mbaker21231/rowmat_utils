do C:\users\matth\documents\github\rowmat_utils\rowmat_utils_mata.do

mata

i = 1

A =  1,2,.5 \ 0, 1.5, .4 \ .1, .1, 1.1

XX = rowshape(A, 1)
XX = XX#J(4,1,1)

end

mata
    real matrix rm_sqrt(real matrix A, real scalar tol, real scalar its)
    {
        real matrix ZP, YP, Z, Y, K, Id, ZZ
        real scalar i
        
        i=1
        
        ZZ = rowshape(I(sqrt(cols(A))), 1)
        Z = ZZ#J(rows(A), 1, 1)
        Id = ZZ#J(rows(A), 1, 1)
        
        Y = A
        
        do {
            K  = (3:*Id :- rm_matmult(Z, Y))
            YP = .5:*rm_matmult(Y, K)
            ZP = .5:*rm_matmult(K, Z)
            Y = YP
            Z = ZP
            i = i + 1    
            } while ( i <its  )
        
        return(Y)
    }
end
