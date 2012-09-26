//= require_tree .

/*
 * Overy-paranoid jQuery docready call
 * Won't produce an error if jQuery doesn't load
 */
(typeof jQuery !== 'function') ? null : jQuery(function () {
  
  var $ = jQuery, contentMargin;
  
  /*
   * Global config options
   * Make a few recurring selections up front just to speed up the jQuery
   * process every time we select them
   */
  var options = {
    navContainer    : $('#nav-container'),
    navContent      : $('#primary-nav'),
    contentWell     : $('div[role=main]'),
    navToggle       : $('#nav-toggle'),
    closedNavMargin : '12px',
    navSpeed        : 300
  };
  
  /*
   * getContentMargin()
   * Since the screen size can change, we need a way of figuring out how wide
   * the nav should open up.
   */
  function getContentMargin() {
    var margin = options.contentWell.css('margin-left');
    contentMargin = (margin === options.closedNavMargin) ? contentMargin : margin;
  }
  // Call this the first time on the docready to get the initial value
  getContentMargin();
  
  /*
   * reverseToggle()
   * Makes the arrows on the nav opener/closer flip back and forth
   */
  function reverseToggle() {
    options.contentWell.toggleClass('closed');
    options.navToggle.toggleClass('closed');
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
      obj.duration = options.navSpeed;
      obj.complete = queue;
    } else if (!arguments.length) {
      obj.queue    = false;
      obj.duration = options.navSpeed;
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
    options.navContent.fadeOut(options.navSpeed / 2);
    options.contentWell.animate({marginLeft: options.closedNavMargin}, animConfig(callback));
    options.navContainer.animate({width: options.closedNavMargin}, animConfig());
  }
  
  /*
   * openNav()
   * Animates the sidebar nav into the open position
   */
  function openNav(callback) {
    var cm = contentMargin;
    options.navContent.fadeIn(options.navSpeed / 2);
    options.contentWell.animate({marginLeft: cm}, animConfig(callback));
    options.navContainer.animate({width: cm}, animConfig());
  }
  
  /*
   * determineNavAction()
   * Determines whether the nav should be opened or closed
   * at any given time.
   */
  function determineNavAction() {
    if (options.navContent.is(':hidden')) {
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
   * addNavMenuToggles()
   * For every h3 or h4, check if the corresponding nav menu has items in it.
   * If so, give that h tag a toggle button.
   */
  function addNavMenuToggles(index, item) {
    var that         = $(this),
        nextItem     = that.next(),
        nextItemIsUl = (nextItem[0].tagName.toLowerCase() === 'ul'),
        classes      = 'menu-toggle ';
    if (that.find('a').length) {
      console.log(that)
      classes += 'extra-margin';
    }
    if (nextItemIsUl && nextItem.find('li').length) {
      that.prepend('<span class="' + classes + '"></span>');
    }
  }
  // Call this on the docready to add nav menu toggle buttons where needed
  options.navContent.find('h3, h4').each(addNavMenuToggles);
  
  /*
   * Any time a nav menu toggle button gets clicked
   * open or close the nav menu as appropriate.
   */
  $(document).on('click', '.menu-toggle', determineMenuAction);
  

});
