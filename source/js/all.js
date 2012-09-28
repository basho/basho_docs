//= require_tree .

/*
 * Overy-paranoid jQuery docready call
 * Won't produce an error if jQuery doesn't load
 */
(typeof jQuery !== 'function') ? null : jQuery(function () {
  
  var $ = jQuery, contentMargin, i;
  
  
  
  /*
   * Global config options
   * Make a few recurring selections up front just to speed up the jQuery
   * process every time we select them
   */
  var options = {
    
    selectors : {
      navContainer : '#nav-container',
      navContent   : '#primary-nav',
      contentWell  : 'div[role=main]',
      navToggle    : '#nav-toggle',
    },
    
    params : {
      closedNavMargin : '12px',
      navSpeed        : 300
    },
    
    openMenus : ['all riak projects', 'start here', 'shortcuts']
  };
  
  for (i in options.selectors) {
    if (Object.prototype.hasOwnProperty.call(options.selectors, i)) {
      options.jq = options.jq || {};
      options.jq[i] = $(options.selectors[i]);
    }
  }



  
  /*----------------------------------------------------------*/
  // Code for opening and closing the sidebar
  /*----------------------------------------------------------*/
  
  /*
   * getContentMargin()
   * Since the screen size can change, we need a way of figuring out how wide
   * the nav should open up.
   */
  function getContentMargin() {
    var margin = options.jq.contentWell.css('margin-left');
    contentMargin = (margin === options.params.closedNavMargin) ? contentMargin : margin;
  }
  // Call this the first time on the docready to get the initial value
  getContentMargin();
  
  /*
   * reverseToggle()
   * Makes the arrows on the nav opener/closer flip back and forth
   */
  function reverseToggle() {
    options.jq.contentWell.toggleClass('closed');
    options.jq.navToggle.toggleClass('closed');
  }
  
  /*
   * animConfig()
   * Creates a jQuery.animate config object so we don't have to pass in
   * as many full blown objects every time we call .animate
   */
  function animConfig(queue, duration, complete) {
    var obj = {};
    if (arguments.length === 1 && typeof queue === 'function') {
      obj.queue    = false;
      obj.duration = options.params.navSpeed;
      obj.complete = queue;
    } else if (!arguments.length) {
      obj.queue    = false;
      obj.duration = options.params.navSpeed;
    } else {
      obj.queue    = queue;
      obj.duration = duration;
      obj.complete = complete;
    }
    return obj;
  }
  
  /*
   * closeNav()
   * Animates the sidebar nav into the closed position
   */
  function closeNav(callback) {
    options.jq.navContent.fadeOut(options.params.navSpeed / 2);
    options.jq.contentWell.animate({marginLeft: options.params.closedNavMargin}, animConfig(callback));
    options.jq.navContainer.animate({width: options.params.closedNavMargin}, animConfig());
  }
  
  /*
   * openNav()
   * Animates the sidebar nav into the open position
   */
  function openNav(callback) {
    var cm = contentMargin;
    options.jq.navContent.fadeIn(options.params.navSpeed / 2);
    options.jq.contentWell.animate({marginLeft: cm}, animConfig(callback));
    options.jq.navContainer.animate({width: cm}, animConfig());
  }
  
  /*
   * determineNavAction()
   * Determines whether the nav should be opened or closed
   * at any given time.
   */
  function determineNavAction() {
    if (options.jq.navContent.is(':hidden')) {
      openNav(reverseToggle);
    } else {
      closeNav(reverseToggle);
    }
  }
  
  /*
   * Any time the screen is resized, re-assess how wide
   * the nav should be able to open
   */
  $(window).on('resize', getContentMargin);

  /*
   * Any time the nav controller button gets clicked
   * open or close the nav as appropriate.
   */
  $(document).on('click', '#nav-toggle', determineNavAction);





  /*----------------------------------------------------------*/
  // Code for the animations in the left side bar
  /*----------------------------------------------------------*/
  
  /*
   * openMenu()
   * Animates the a nav menu into the open position
   */
  function openMenu(toggler, menu) {
    menu.slideDown('fast');
    toggler.toggleClass('open');
  }
  
  
  /*
   * closeMenu()
   * Animates the a nav menu into the closed position
   */
  function closeMenu(toggler, menu) {
    menu.slideUp('fast');
    toggler.toggleClass('open');
  }
  
  /*
   * determineMenuAction()
   * Determines whether a nav menu should be opened or closed
   * at any given time.
   */
  function determineMenuAction() {
    var that = $(this),
        correspondingUl = that.parent().next();
    if (correspondingUl.is(':hidden')) {
      openMenu(that, correspondingUl);
    } else {
      closeMenu(that, correspondingUl);
    }
  }
  
  
  /*
   * checkOpenMenu()
   * Checks menu titles to see if they should be open at page load
   * If so, returns true.
   */
  function checkOpenMenu(text) {
    var len = options.openMenus.length;
    for (i = 0; i < len; i += 1) {
      if (text === options.openMenus[i].toLowerCase()) {
        return true;
      }
    }
    return false;
  }
  
  /*
   * addNavMenuToggles()
   * For every h3 or h4, check if the corresponding nav menu has items in it.
   * If so, give that h tag a toggle button.
   */
  function addNavMenuToggles(index, item) {
    var that         = $(this),
        nextItem     = that.next(),
        nextItemIsUl = (nextItem[0].tagName.toLowerCase() === 'ul'),
        text;
    if (nextItemIsUl && nextItem.find('li').length) {
      text = that.text().toLowerCase();
      if (checkOpenMenu(text)) {
        nextItem.show();
        that.prepend('<span class="menu-toggle open"></span>');
      } else {
        that.prepend('<span class="menu-toggle"></span>');
      }
    }
  }
  // Call this on the docready to add nav menu toggle buttons where needed
  options.jq.navContent.find('h3, h4').each(addNavMenuToggles);
  
  
  /*
   * checkForToggler()
   * Determines whether a nav header is open-able or not.
   * If so, calls determineMenuAction() to open or close as needed.
   */
  function checkForToggler() {
    var toggler = $(this).find('.menu-toggle');
    if (toggler.length) {
      determineMenuAction.call(toggler[0]);
    }
  }
  
  /*
   * Any time a nav header or menu toggle button gets clicked
   * check to see if it is open-able.  Then open or close as needed.
   */
  $(document).on('click', (options.selectors.navContent + ' h3, ' + options.selectors.navContent + ' h4'), checkForToggler);
  
  
  

  /*----------------------------------------------------------*/
  // Extra helpers
  /*----------------------------------------------------------*/
  
  /*
   * Disable linking to the page you're already on.
   */
  $(document).on('click', '.current a', function () {return false;});
  
  /*
   * Don't let people jack up tables
   */
  $('table, tr, th, td, tbody, thead, tfoot').removeAttr('style');

});
