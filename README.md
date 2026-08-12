# rowmat_utils

`rowmat_utils` is a Mata library for performing batched operations on many small square matrices.

The package is designed for applications in which each of `N` observations is associated with an `n`-by-`n` matrix and the same matrix operation must be carried out for every observation. Each individual matrix is stored rowwise as a `1`-by-`n^2` vector, so that a collection of `N` matrices is represented by a single `N`-by-`n^2` Mata matrix.

This representation allows many matrix operations to be performed across all observations without explicitly looping over the `N` individual matrices.

## Installation

### Install directly from GitHub

To install the current version directly from GitHub, run the following command in Stata:

```stata
net install rowmat_utils, ///
    from("https://raw.githubusercontent.com/mbaker21231/rowmat_utils/main")
```

To reinstall or update an existing installation:

```stata
net install rowmat_utils, ///
    from("https://raw.githubusercontent.com/mbaker21231/rowmat_utils/main") replace
```

After installation, view the documentation with:

```stata
help rowmat_utils
```

## Basic idea

Suppose that for each of `N` observations we have a square matrix

```text
A_i
```

of dimension `n`-by-`n`.

Rather than storing these as `N` separate Mata matrices, `rowmat_utils` represents each matrix as a row vector:

```text
vec(A_i)'
```

and stacks the resulting rows into an `N`-by-`n^2` matrix.

For example, the two matrices

```text
A1 =  1  2
      3  4

A2 =  2  3
      4  5
```

are represented as

```text
A =  1  2  3  4
     2  3  4  5
```

Operations in `rowmat_utils` act on these matrices in batched form.

## Example

The following example multiplies two pairs of 2-by-2 matrices.

```stata
mata:

A1 = 1,2 \ 3,4
A2 = 2,3 \ 4,5

B1 = 5,6 \ 7,8
B2 = 6,7 \ 8,9

A = rowshape(A1,1) \ rowshape(A2,1)
B = rowshape(B1,1) \ rowshape(B2,1)

rm_matmult(A,B)

end
```

The first row of the result contains the rowwise representation of

```text
A1 * B1
```

and the second row contains the rowwise representation of

```text
A2 * B2
```

Thus many separate matrix operations can be carried out using a single Mata matrix.

## Functions

The package currently provides the following Mata functions.

### `rm_matmult()`

Batched matrix-matrix multiplication.

```text
rm_matmult(A,B)
```

If `A` and `B` are `N`-by-`n^2`, row `i` of the result contains the rowwise representation of the matrix product `A_i B_i`.

### `rm_matvecmult()`

Batched matrix-vector multiplication.

```text
rm_matvecmult(A,x)
```

Each row of `A` represents an `n`-by-`n` matrix and each corresponding row of `x` represents an `n`-element vector.

### `rm_vecvecmult()`

Batched vector dot products.

```text
rm_vecvecmult(x,y)
```

Computes the dot product of corresponding rows of `x` and `y`.

### `rm_newtinv()`

Batched matrix inversion using Newton iteration.

```text
rm_newtinv(A,maxiter,crit)
```

The routine applies Newton iteration to many relatively small matrices simultaneously.

### `rm_transpose()`

Batched matrix transposition.

```text
rm_transpose(A)
```

Each row of the returned matrix contains the rowwise representation of the transpose of the corresponding matrix in `A`.

### `rm_absrowsums()`

Computes the absolute row sums of each represented matrix.

```text
rm_absrowsums(A)
```

### `rm_abscolsums()`

Computes the absolute column sums of each represented matrix.

```text
rm_abscolsums(A)
```

### `rm_alpha0()`

Computes initial scaling values used by `rm_newtinv()`.

```text
rm_alpha0(A)
```

The scaling values are based on the matrix 1-norm and infinity norm and provide starting values for Newton inversion.

For complete syntax, conformability requirements, examples, and discussion, see:

```stata
help rowmat_utils
```

## When is this useful?

`rowmat_utils` is intended primarily for applications involving a large number of relatively small matrices.

A typical example is an estimation or simulation problem in which each observation has its own covariance matrix, Hessian, transition matrix, or other small matrix and the same operation must be performed for every observation.

Instead of writing code such as

```text
for i = 1,...,N
    operate on matrix i
end
```

the matrices can be stored rowwise and processed in batched form.

The routines therefore replace loops over matrices with operations that generally loop over the much smaller number of entries within each matrix.

The extent of any performance advantage depends on the dimensions of the matrices and the number of matrices being processed.

## Repository structure

The repository is organized as follows:

```text
rowmat_utils/
│
├── lrowmat_utils.mlib
├── rowmat_utils.sthlp
├── mf_rowmat_utils.sthlp
├── rowmat_utils.pkg
├── stata.toc
│
├── src/
│   └── rowmat_utils_mata.do
│
├── tests/
│   └── test_rowmat_utils.do
│
└── development/
    ├── MockUp.ipynb
    └── mockup.do
```

The main files are:

- `lrowmat_utils.mlib` — compiled Mata library distributed to users
- `rowmat_utils.sthlp` — main Stata help file
- `mf_rowmat_utils.sthlp` — Mata help entry
- `rowmat_utils.pkg` — Stata package manifest used by `net install`
- `stata.toc` — Stata package-site metadata
- `src/rowmat_utils_mata.do` — Mata source code and library build script
- `tests/test_rowmat_utils.do` — correctness tests
- `development/` — notebooks and experimental development material

## Development and testing

After cloning the repository, the test suite can be run from the repository root in Stata:

```stata
do tests/test_rowmat_utils.do
```

The tests compare the `rowmat_utils` routines with equivalent standard Mata operations.

Tests use `assert()` so that execution stops if a computed result differs from its expected value. A successful run ends with:

```text
ALL ROWMAT_UTILS TESTS PASSED
```

The compiled Mata library can be rebuilt from:

```text
src/rowmat_utils_mata.do
```

## Documentation

Full documentation is included with the package.

After installation:

```stata
help rowmat_utils
```

The help file contains detailed examples, conformability requirements, and discussion of each function.

## Author

Matthew J. Baker  
Hunter College and the Graduate Center, CUNY  
mjbaker@hunter.cuny.edu