---
title: Welcome
---

This is my personal site!

You can read more about what I do.

This is very much WIP, as I do not have a very active online presence.

``` Scheme
(define Y
  (lambda (h)
      ((lambda (x) (x x))
           (lambda (g)
                  (h (lambda args (apply (g g) args)))))))
```
