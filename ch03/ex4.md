Is not feasible for function types to be instances of the `Eq` typeclass because
it would be necessary to map all the possible input values and their results
when applied to each function and them compared to see if all the results for
both functions match. This is something that can be extremely expensive.

When the input values have a finite and small number of possibilities then it
would be somewhat feasible to determine the equality for function types.
