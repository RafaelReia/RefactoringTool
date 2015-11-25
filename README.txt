Refactoring tool for Racket

At this moment this tool only transforms the following:
(if (?a) #f #t) -> (not (?a))

To use it, select the code to be transformed (e.g. the (if (< 1 2) #f #t)) and
hit the refactoring if button.

Still in production, any doubts contact me.
