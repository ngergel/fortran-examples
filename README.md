## Fortran Examples

This repository contains multiple example programs written in Fortran exhibiting
different aspects of array and multidimensional array manipulation. 

### Contents

- [**Matrix Packing, COO Format**](matrix_coo.f90)</br>
  Example of the COO format for matrix packing. Demonstrates packing and unpacking
  on some example matrices.
- [**Matrix Packing, CRS Format**](matrix_crs.f90)</br>
  Example of the CRS format for matrix packing. Besides the differences in packing,
  same as COO.
- [**Matrix Packing, ELL Format**](matrix_ell.f90)</br>
  Example of the ELL format for matrix packing. Besides the differences in packing,
  same as COO.
- [**Anti-Transpose**](antitranspose.f90)</br>
  Program that impliments an anti-transpose function. An anti-tranpose is a transpose
  along the anti-diagonal. Demonstrates some funky array slicing.
- [**Cubestat**](cubestat.f90)</br>
  An implementation of the Cubestat lab for CMPUT 229, in Fortran.
- [**Non-Prime Factors**](nonprimefactors.f90)</br>
  For a given query, finds the number of non-prime factors that it has. While it doesn't
  do much in the way of multidimensional array manipulations, it does incorporate loops in
  an interesting way.
- [**Image Blurring**](image_blurring.f90)</br>
  Basic image blurring based on averaging the color of a pixel's neighbours. I wanted to
  avoid any messiness with any of the image formats, so it does the blurring on dummy image
  data.
