# In place addition

Warning: Typing x %+=% y/2 returns x \<- (x + y)/2. Adding parentheses,
i.e. x %+=% (y/2) solves the problem.

## Usage

``` r
e1 %+=% e2
```

## Arguments

- e1:

  first addend (and returned sum)

- e2:

  second addend

## Value

sum of the two addends replacing the values in the first addend

## Examples

``` r
if (FALSE) { # interactive()
add_QMOD.xts$VISUS_HI[pq(2022.3, 2023.4)] <- add_QMOD.xts$VISUS_HI[pq(2022.3, 2023.4)] +
  c(0.01, -0.04, rep(-0.025, 4))
add_QMOD.xts$VISUS_HI[pq(2022.3, 2023.4)] %+=% c(0.01, -0.04, rep(-0.025, 4)) # easier on the eye
}
```
