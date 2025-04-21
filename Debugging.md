

To debug:

Canon expansion broke Last and LastTo.

```##timeknot

clock | x :|

a[last] <- clock[15>>] 75cpm*[1.1,0.85,1.21,0.61] | x[xx]ox ||
a.s = "grandpiano" .n = 8 .shurNot8 = 0 3 0 2 4 + [10,12,13,5];

b[last] <- clock[15>>] 90cpm*[0.7,0.78,1.4,1.5] | !xox[xx]#20 ||
b.s = "grandpiano" .n = 5 .shurNot8 = 2 5 4 3 0 1 0 + [0, 2, 11, 13];

c[last] <- clock[15>>] 140cpm | !(x[xx], o[ox], 3,7)#3 ||
c.s = "grandpiano" .n = 7 .n = 7 .shurNot8 = 14 15 14 14 15 14 16 17;

d[last] <- clock[15>>] 128cpm*[0.9,1,1.1,1.2] | ([xx],[ox],3,5) ||
d.s = "grandpiano" .n = 6 .shurNot8 = 0 1 0 1 2 + [0,2,4];```


This should not be constructors for ConvergeTo and ConvergeFrom. Rework it for it to translate last into a process at parsing entry point.

----

Spread is not working properly, fix it.

----

Ratio tempo mark is broken fix it!