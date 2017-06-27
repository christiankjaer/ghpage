---
title: Welcome
---

This is my personal site!

You can read more about what I do.

Here is the Y combinator, it allows arbitrary recursion in the lambda calculus. Here expressed in Scheme syntax:

``` Scheme
(define Y
  (lambda (h)
      ((lambda (x) (x x))
           (lambda (g)
                  (h (lambda args (apply (g g) args)))))))
```
