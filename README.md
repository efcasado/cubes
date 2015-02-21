cubes
=====

Find the smallest cube for which exactly `N` permutations of its digits are cube.

```erlang
cubes:smallest(1).
% => 1

cubes:smallest(2).
% => 125

cubes:smallest(3).
% => 41063625
```

> When computing the permutations, all permutations starting with a 0
> have been filtered out. This was not clear to me when reading the
> exercise. However, if permutations starting with a 0 are not
> filtered out, 1000000 would be the smallest cube which has exactly
> three permutations of its digits which are also cube (i.e.,
> 1000000, (000)1000, and (000000)1), and not 41063625.

### Results

| Test                              | Result                |  Time          |
|-----------------------------------|-----------------------|----------------|
| `cubes:smallest(1)` (memoisation) | 1                     | 3.7E-5 seconds |
| `cubes:smallest(2)` (memoisation) | 125                   | 5.0E-5 seconds |
| `cubes:smallest(3)` (memoisation) | 41063625              | 4.5 seconds    |
| `cubes:smallest(4)` (memoisation) | ERROR (out of memory) | NA             |
| `cubes:smallest(5)` (memoisation) | ERROR (out of memory) | NA             |
| `cubes:smallest(1)`               | 1                     | 1.8E-5 seconds |
| `cubes:smallest(2)`               | 125                   | 3.0E-5 seconds |
| `cubes:smallest(3)`               | 41063625              | 5.5 seconds    |
| `cubes:smallest(4)`               | ERROR (out of memory) | NA             |
| `cubes:smallest(5)`               | ERROR (out of memory) | NA             |

### How-To

##### Build the project

```
make
```

##### Run the tests

```
make test
```