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

### How-To

##### Build the project

```
make
```

##### Run the tests

```
make test
```