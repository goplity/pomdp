```sh
$ cat test.data
1 2 3
4 5 6
7 8 9

$ cat test.data | ./test.native

Result | Count
-------+------
     1 |     0
     4 |     0
     5 |     0
     3 |     0
     7 |     0
     8 |     0
     2 |     0
     6 |     0
     9 |  1000
==============
          1000

Iterations:
    min  418
    max  483
    mean 428
```
