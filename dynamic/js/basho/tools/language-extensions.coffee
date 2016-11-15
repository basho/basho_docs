###
Language Extensions
===================
Like it says on the tin, the below extend (hopefully in a small way) JS
###


#TODO: strict mode enables a pretty large number of runtime checks. We probably
#      want to turn it off when we're deploy to prod.
#      Perhaps we can figure out some way to enable it when building debug?
'use strict'



## padNumber :: (Num, Num, ?Char) -> Str
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



## rem :: (?Num) -> Num
# Return the current rem size in pixels, or the length of `n` rems in pixels.
#TODO: I should probably not be using JQuery here.
window.rem = (n = 1) ->
  n * parseInt($('html').css('font-size'))

## <Number>.rem :: () -> Num
# Convert Number rems into pixels.
Number::rem = () ->
  this * parseInt($('html').css('font-size'))



## <String>.toInt :: () -> Num
# Pass the given string through parseInt().
String::toInt = () ->
  parseInt(this)



## $.maxScrollLeft :: () -> Num
# Get the maximum value of .scrollLeft() for the given JQuery object.
#FIXME: These have not been tested for cross-browser compatibility.
$.fn.extend({
    maxScrollLeft: () -> this[0].scrollWidth - this[0].clientWidth
  })

## $.maxScrollTop :: () -> Num
# Get the maximum value of .scrollTop() for the given JQuery object.
#FIXME: These have not been tested for cross-browser compatibility.
$.fn.extend({
    maxScrollTop: () -> this[0].scrollHeight - this.outerHeight()
  })



# This function was taken from the Underscore.js 1.8.3 library.
# https://github.com/jashkenas/underscore/blob/1.8.3/underscore.js#L763-L770
#
# Delays a function for the given number of milliseconds, and then calls
# it with the arguments supplied.
window.delay = `function(func, wait) {
  var args = Array.prototype.slice.call(arguments, 2);
  return setTimeout(function(){
    return func.apply(null, args);
  }, wait);
};`



# This function was taken from the Underscore.js 1.8.3 library. It was modified
# to use `Date.now()` rather than `_.now()`, but is otherwise as  written by the
# Underscore team.
# https://github.com/jashkenas/underscore/blob/1.8.3/underscore.js#L776-L811
#
# Returns a function, that, when invoked, will only be triggered at most once
# during a given window of time. Normally, the throttled function will run
# as much as it can, without ever going more than once per `wait` duration;
# but if you'd like to disable the execution on the leading edge, pass
# `{leading: false}`. To disable execution on the trailing edge, ditto.
window.throttle = `function(func, wait, options) {
  var context, args, result;
  var timeout = null;
  var previous = 0;
  if (!options) options = {};
  var later = function() {
    previous = options.leading === false ? 0 : Date.now();
    timeout = null;
    result = func.apply(context, args);
    if (!timeout) context = args = null;
  };
  return function() {
    var now = Date.now();
    if (!previous && options.leading === false) previous = now;
    var remaining = wait - (now - previous);
    context = this;
    args = arguments;
    if (remaining <= 0 || remaining > wait) {
      if (timeout) {
        clearTimeout(timeout);
        timeout = null;
      }
      previous = now;
      result = func.apply(context, args);
      if (!timeout) context = args = null;
    } else if (!timeout && options.trailing !== false) {
      timeout = setTimeout(later, remaining);
    }
    return result;
  };
};`



# This function was taken from the Underscore.js 1.8.3 library. It was modified
# to use `Date.now()` rather than `_.now()`, but is otherwise as  written by the
# Underscore team.
# https://github.com/jashkenas/underscore/blob/1.8.3/underscore.js#L813-L847
#
# Returns a function, that, as long as it continues to be invoked, will not
# be triggered. The function will be called after it stops being called for
# N milliseconds. If `immediate` is passed, trigger the function on the
# leading edge, instead of the trailing.
window.debounce = `function(func, wait, immediate) {
  var timeout, args, context, timestamp, result;

  var later = function() {
    var last = Date.now() - timestamp;

    if (last < wait && last >= 0) {
      timeout = setTimeout(later, wait - last);
    } else {
      timeout = null;
      if (!immediate) {
        result = func.apply(context, args);
        if (!timeout) context = args = null;
      }
    }
  };

  return function() {
    context = this;
    args = arguments;
    timestamp = Date.now();
    var callNow = immediate && !timeout;
    if (!timeout) timeout = setTimeout(later, wait);
    if (callNow) {
      result = func.apply(context, args);
      context = args = null;
    }

    return result;
  };
};`
