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

| Test                                 | Result                   |  Time           |
|--------------------------------------|-------------------------:|----------------:|
| `cubes:smallest(1)` #1               | 1                        | 1.8E-5 seconds  |
| `cubes:smallest(2)` #1               | 125                      | 3.0E-5 seconds  |
| `cubes:smallest(3)` #1               | 41063625                 | 5.5 seconds     |
| `cubes:smallest(4)` #1               | `ERROR (out of memory)`  | `NA`            |
| `cubes:smallest(5)` #1               | `ERROR (out of memory)`  | `NA`            |
| `cubes:smallest(1)` #1 (memoisation) | 1                        | 3.7E-5 seconds  |
| `cubes:smallest(2)` #1 (memoisation) | 125                      | 5.0E-5 seconds  |
| `cubes:smallest(3)` #1 (memoisation) | 41063625                 | 4.5 seconds     |
| `cubes:smallest(4)` #1 (memoisation) | `ERROR (out of memory)`  | `NA`            |
| `cubes:smallest(5)` #1 (memoisation) | `ERROR (out of memory)`  | `NA`            |
| `cubes:smallest(1)` #2               | 1                        | 6.0E-6 seconds  |
| `cubes:smallest(2)` #2               | 125                      | 3.3E-5 seconds  |
| `cubes:smallest(3)` #2               | 41063625                 | 0.01 seconds    |
| `cubes:smallest(4)` #2               | 1006012008               | 0.19 seconds    |
| `cubes:smallest(5)` #2               | 127035954683             | 4.39 seconds    |
| `cubes:smallest(1)` #2 (optimised)   | 1                        | 6.0E-6 seconds  |
| `cubes:smallest(2)` #2 (optimised)   | 125                      | 2.2E-5 seconds  |
| `cubes:smallest(3)` #2 (optimised)   | 41063625                 | 0.004 seconds   |
| `cubes:smallest(4)` #2 (optimised)   | 1006012008               | 0.05 seconds    |
| `cubes:smallest(5)` #2 (optimised)   | 127035954683             | 0.89 seconds    |



The table above illustrates the results I have obtained with the two solutions I
implemented for this exercise.

My first solution consisted in a brute-force approach. I would iterate over a
sequence of cubes (1, 8, 27, etc.), computing all the permutations and checking
whether that cube had exactly `N` permutations of its digits which were also
cubes. This solution seemed to work for the example provided with the exercise.
However, I soon realised that I was not going to be able to solve the exercise
for values of `N` larger than `3` using this approach. Before giving up on this
approach, I tried to optimise it (implementing a naive memoisation mechanism),
but even if the latency got reduced, I was not able to solve the exercise yet.
The problem was in the function calculating the permutations, which was not
tail-recursive.

I then attempted to solve the exercise without having to resort to computing
all the permutations of every cube. This led me to my second solution to this
exercise (identified by `#2` in the table above). This solution is, in fact,
more CPU and memory friendly than the first one. Most importantly, this second
approach can be used the solve the exercise when `N` equals to `5`.

An optimised version of the second approach is also available. The optimised
version (identified by `#2 (optimised)` in the table avoce) features a
`4.93`x speedup over the non-optimised version.

### How-To

##### Build the project

```
git clone https://github.com/efcasado/cubes efcasado-cubes
cd efcasado-cubes && make
```

##### Run the tests

The project ships with some unit tests, which are implemented in the `cubes.erl`
module itself.

These tests can be executed running `eunit:test(cubes)`. Alternatively, one can
trigger them using the provided `Makefile`.

```
make test
```