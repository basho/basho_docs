/*
 * Name: jQuery.localItem
 * Author: John Newman
 * Date: 07/11/2013
 * License: MIT
 * Version: 0.1
 * Description: Store with localStorage; fall back to cookies if necessary.
 *
 */

(function (global) {
  'use strict';

  var $        = global.jQuery,
      lsExists = (global.localStorage !== undefined),
      base     = global.encodeURIComponent(global.location.host);

  /**
   * A shortcut for global.encodeURIComponent
   */
  function encode(val) {
    return global.encodeURIComponent(val);
  }

  /**
   * A shortcut for global.decodeURIComponent
   */
  function decode(val) {
    return global.decodeURIComponent(val);
  }

  /**
   * Tries to call JSON.parse in the case of a string.
   */
  function maybeParse(val) {
    if (typeof val === 'string') {
      try {
        return JSON.parse(val);
      } catch (err) {
        return val;
      }
    } else {
      return val;
    }
  }

  /**
   * Calls JSON.stringify in the case of a non-string.
   */
  function maybeStringify(val) {
    return typeof val === 'string' ? val : JSON.stringify(val);
  }

  /**
   * Allows you to retrieve an item by name from localStorage.
   * Expects items to be prefixed with a base derived from
   * location.host. Automatically parses the value retrieved.
   */
  function getWithLocalStorage(name) {
    return maybeParse(decode(localStorage.getItem(base + ':' + encode(name))));
  }

  /**
   * Allows you to set an item by name in localStorage.
   * Prefixes items with a base derived from location.host.
   * Automatically stringifies values before storing.
   */
  function setWithLocalStorage(name, value) {
    var item = encode(maybeStringify(value));
    return localStorage.setItem(base + ':' + encode(name), item);
  }

  /**
   * Allows you to remove an item by name from localStorage.
   * Expects the item to be prefixed with a base derived from location.host.
   */
  function rmWithLocalStorage(name) {
    var itemName = base + ':' + encode(name);
    return localStorage.removeItem(itemName);
  }

  /**
   * Allows you to retrieve an item by name from the cookies.
   * Expects items to be prefixed with a base derived from
   * location.host. Automatically parses the value retrieved.
   */
  function getWithCookie(name) {
    var cookies = global.document.cookie.split(';'),
        toFind  = base + ':' + encode(name),
        len     = cookies.length,
        split,
        val,
        i;

    /*
     * Iterate over all cookies.
     */
    for (i = 0; i < len; i += 1) {
      split = cookie.split('=');

      /*
       * If the name of the cookie is a match,
       * decode and parse the value. Kill the loop.
       */
      if (split[0] === toFind) {
        val = maybeParse(decode(split[1]));
        break;
      }
    }
    return val;
  }

  /**
   * Allows you to set an item by name in cookies.
   * Prefixes items with a base derived from location.host.
   * Automatically stringifies values before storing.
   */
  function setWithCookie(name, value) {
    var item = encode(maybeStringify(value));
    return (document.cookie = (base + ':' + encode(name)) + '=' + item);
  }

  /**
   * Allows you to remove an item by name from cookies.
   * Expects the item to be prefixed with a base derived from location.host.
   */
  function rmWithCookie(name) {
    var itemName = base + ':' + encode(name),
        expiry   = 'Thu, 01 Jan 1970 00:00:00 GMT';

    /*
     * To remove a cookie, you have to overwrite it and set its
     * `expires` date to some time previous to now. The browser
     * will then delete it.
     */
    return (document.cookie = itemName + '=null; expires=' + expiry);
  }

  /**
   * Expose functionality.
   */
  $.localItem = {
    "set" : (lsExists ? setWithLocalStorage : setWithCookie),
    "get" : (lsExists ? getWithLocalStorage : getWithCookie),
    "rm"  : (lsExists ? rmWithLocalStorage  : rmWithCookie),

    /**
     * Allow the user to deliberately use cookies instead of localStorage.
     */
    "setCookie" : function (name, value) {
                    return setWithCookie('_$_cookie_' + name, value);
                  },
    "getCookie" : function (name) {
                    return getWithCookie('_$_cookie_' + name); 
                  },
    "rmCookie"  : function (name) {
                    return rmWithCookie('_$_cookie_' + name);
                  }
  };


}(this));

