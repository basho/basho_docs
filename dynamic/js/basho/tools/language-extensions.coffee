###
Language Extensions
===================
Like it says on the tin, the below extend (hopefully in a small way) JS
###


#TODO: strict mode enables a pretty large number of runtime checks. We probably
#      want to turn it off when we're deploy to prod.
#      Perhaps we can figure out some way to enable it when building debug?
'use strict'



# ```padNumber :: (Num, Num, ?Char) -> Str```
# Convert any number to a string of at least length `len`, conditionally
# prefixed with `char` characters.
# This function was heavily inspired by
# http://stackoverflow.com/questions/1267283#answer-1268377
window.padNumber = (number, len, char = '0') ->
  n = Math.abs(number)
  zeros = Math.max(0, (len - n.toString().length))
  zeroString = Math.pow(10,zeros)
                   .toString()
                   .substr(1)
  zeroString.replace(/^0+/, (m) -> m.replace(/0/g, char)) if ( char != '0' )
  zeroString = '-' + zeroString                           if ( number < 0  )

  return (zeroString + n)
