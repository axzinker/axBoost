# axBoost 

This package contains various R functions for which I haven't found good and 
practical equivalents on CRAN or the internet. On how these functions work
(e.g., the generated graphics) please have a look at the function help and
the vignette. A short overview is given here:

Functions to graphically display data:

- df1: A constructed data set for the following functions
- axCorrgram(): Corrgram-like correlation table plot
- modRegGraph(): Plot a moderated regression interaction diagram and return 
  graphical coordinates to be re-used in other plots (e.g., in Excel)
- drawDD(): Draw a double dissociation based on regression coefficients

Functions to manipulate data:

- fillVec(): Add missing elements to a vector of consecutive numbers
- parcelMe(): Parcels a dataframe with item columns

Other functions:

- genSubjHconf(): Generate subject hour confirmation PDFs e.g., for psychology 
  students, digitally sign the PDFs and optionally send them via email
- whichKey(): Show the key filename used to encode the subject hour 
  confirmations based on the timestamp.
- axDecode(): Decrypt the code generated for the subject hour confirmations
