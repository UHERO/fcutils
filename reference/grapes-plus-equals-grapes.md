# In place addition

`x %+=% y` adds y to x and stores the sum in x. `%+=%` binds more
tightly than `*` and `/`, so `x %+=% y/2` sets x to x + y and halves
only the returned value. Write `x %+=% (y/2)` to add half of y.

## Usage

``` r
e1 %+=% e2
```

## Arguments

- e1:

  first addend, replaced by the sum

- e2:

  second addend

## Value

the new value of e1, invisibly

## Examples

``` r
if (FALSE) { # interactive()
add_QMOD.xts$VISUS_HI[pq(2022.3, 2023.4)] <- add_QMOD.xts$VISUS_HI[pq(2022.3, 2023.4)] +
  c(0.01, -0.04, rep(-0.025, 4))
add_QMOD.xts$VISUS_HI[pq(2022.3, 2023.4)] %+=% c(0.01, -0.04, rep(-0.025, 4)) # easier on the eye
}
```
