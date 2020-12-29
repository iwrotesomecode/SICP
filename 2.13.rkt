#lang sicp

#| Exercise 2.13: Show that under the assumption of small percentage tolerances
there is a simple formula for the approximate percentage tolerance of the
product of two intervals in terms of the tolerances of the factors. You may
simplify the problem by assuming that all numbers are positive. |#

#|
CA = center A
CB = center B
PA = percent A
PB = percent B
A = (CA * (1-PA/2), CA * (1+PA/2))
B = (CB * (1-PB/2), CB * (1+PB/2))
AB = ((CA*CB-CA*CB(PA/2-PB/2)+ (CA*CB*PA*PB)/4),
      (CA*CB+CA*CB(PA/2-PB/2)+ (CA*CB*PA*PB)/4))

AB = ((CA*CB(1-(PA/2-PB/2)+ (PA*PB)/4)),
      (CA*CB(1+(PA/2-PB/2)+ (PA*PB)/4)))

One simplification is ignoring the final term.
(CA*CB*PA*PB/4). For small percentages, this term will be small.

|#
