# rowmat_utils

`rowmat_utils` is a Mata library for performing batched operations on many
small square matrices.

The package is intended for applications in which each of N observations is
associated with an n-by-n matrix and the same matrix operation must be carried
out for every observation. Each matrix is stored rowwise as a 1-by-n^2 vector,
allowing operations to be performed across many matrices without explicitly
looping over observations.

## Installation

### Install directly from GitHub

To install the current version directly from GitHub, run:

```stata
net install rowmat_utils, ///
    from("https://raw.githubusercontent.com/mbaker21231/rowmat_utils/main")