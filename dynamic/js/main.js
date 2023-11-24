/* Primary JS
 * ==========
 * The primary set of JavaScipt that should be included _after_ the majority of
 * the DOM has been loaded (e.g. in a <script> tag at the bottom of the <body>).
 * This will include all libraries and custom code required to finish pre-
 * interaction DOM manipulation, and all code that drives dynamic interactions.
 * This file will (probably) include JS language modifications (think
 * `Object.new` before it was added to the standard), and Sprockets `require`
 * statements. Hopefully nothing else.
 */

//TODO: Make sure none of the code in here triggers a FOUC.



/* Vendor Library Includes
 * -----------------------
 * Included one at a time to ensure all requirements are met
 */

//= require ./vendor/modernizr-3.1.1.js
//= require ./vendor/jquery-2.2.4.js
//= require ./vendor/highlight-9.7.0.pack.js
//= require ./vendor/lunr-2.3.9.js



/* Basho Code Tools & Library Code
 * -------------------------------
 * Included one at a time to ensure all requirements are met
 */

//= require ./basho/tools/language-extensions.coffee
//= require ./basho/tools/sem-ver.js


/* Basho Code
 * ----------
 * Included one at a time to ensure all requirements are met
 */

//= require ./basho/anchorify-headings.js
//= require ./basho/table-of-contents.js

//= require ./basho/content-navigation.js
//= require ./basho/selectors.js
//= require ./basho/edge-fader.js
//= require ./basho/code-blocks.js

//= require ./basho/bitcask-calculator.js

//= require ./basho/lunr-search.js


/* Vendor Library Configuration and Initialization
 * -----------------------------------------------
 * Some vendor code requires some configuration and execution. This seems as
 * good a place as any to perform those configurations and execute.
 */


/** Attempted table-wrapping mitigation
 *  -----------------------------------
 * These two calls add non-rendered word-break opportunities after `.`s and
 * before `/` (that aren't the first characters on a line.) to all <code> text
 * that lives under <td> tags. The idea is to give tables full of paths and code
 * segments a chance to wrap s.t. they don't massively over-extend beyond the
 * edges of smaller screen widths.
 * TODO: THIS IS NOT A FULL SOLUTION, but it might help during the first push.
 */
$('td code').each(
    function() {
        $(this).html($(this).html().replace(/([^^])\./g, '$1.<wbr>'))
    });
$('td code').each(
    function() {
        $(this).html($(this).html().replace(/([^^])\//g, '$1<wbr>/'))
    });
