//= require_tree ./libs
//= require_tree ./docs

/*
 * Overly-paranoid jQuery docready call
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
      navContainer     : '#nav-container',
      navContent       : '#primary-nav',
      contentWell      : 'div[role=main]',
      navToggle        : '#nav-toggle',
      responsiveToggle : '.responsive-toggle'
    },
    
    params : {
      closedNavMargin : '12px',
      navSpeed        : 175,
      responsiveWidth : 700
    }
  };
  
  
  /*
   * Create real selections from the items contained in options.selectors
   */
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
   * openNav_responsive()
   * Animates the sidebar nav into the open position in the responsive layout
   */
  function openNav_responsive() {
    options.jq.contentWell.css({
      position : 'absolute',
      top      : 0,
      left     : 0,
      height   : '100%',
      overflow : 'hidden'
    });
    options.jq.navContainer.animate({marginLeft: 0}, animConfig());
    options.jq.contentWell.animate({left: '100%'}, animConfig(function () {
      options.jq.contentWell.hide();
    }));
  }
  
  /*
   * closeNav_responsive()
   * Animates the sidebar nav into the closed position in the responsive layout
   */
  function closeNav_responsive() {
    options.jq.contentWell.show().animate({left: 0}, animConfig());
    options.jq.navContainer.animate({marginLeft: '-100%'}, animConfig(function () {
      options.jq.contentWell.removeAttr('style');
      options.jq.navContainer.removeAttr('style');
    }));
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
   * determineNavAction_responsive()
   * The same as devermineNavAction but for the responsive layout
   */
  function determineNavAction_responsive() {
    if (options.jq.contentWell.is(':hidden')) {
      closeNav_responsive();
    } else {
      openNav_responsive();
    }
  }
  
  /*
   * fixResponsiveArtifacts()
   * Animations in the resopnsive layout sometimes hide elements.
   * This will clear out any artifact style tag attributes when we
   * leave the responsive layout.
   */
  function fixResponsiveArtifacts(evnt) {
    if ($(this).width() > options.params.responsiveWidth) {
      options.jq.contentWell.removeAttr('style');
      options.jq.navContainer.removeAttr('style');
      options.jq.navContent.removeAttr('style');
      options.jq.navToggle.removeClass('open, closed');
    }
  }
  
  /*
   * Any time the screen is resized, re-assess how wide
   * the nav should be able to open
   */
  $(window).on('resize', getContentMargin);
  
  /*
   * Any time the screen is resized, check to see if there are
   * any responsiveness artifacts to be removed
   */
  $(window).on('resize', fixResponsiveArtifacts);

  /*
   * Any time the nav controller button gets clicked
   * open or close the nav as appropriate.
   */
  $(document).on('click', options.selectors.navToggle, determineNavAction);
  
  /*
   * We use .click instead of .on('click') here because mobile Safari
   * doesn't register live click handlers.
   */
  $(options.selectors.responsiveToggle).click(determineNavAction_responsive);





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
  function checkOpenMenu(text, correspondingUl) {
    if(correspondingUl.find('.current').length) {return true;}
    if(correspondingUl.prev('.active').length){return true;}
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
      if (checkOpenMenu(text, nextItem)) {
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
   * Use .click instead of .on('click') because mobile Safari doesn't register
   * live click handlers.
   */
  $(options.selectors.navContent + ' h3, ' + options.selectors.navContent + ' h4').click(checkForToggler);
  
  

  /*----------------------------------------------------------*/
  // Extra helpers
  /*----------------------------------------------------------*/
  
  /*
   * Disable linking to the page you're already on.
   */
  $(document).on('click', '.current > a', function () {return false;});
  
  /*
   * Don't let people jack up tables
   */
  $('table, tr, th, td, tbody, thead, tfoot').removeAttr('style');
  
  /*
   * Put the info icons inside the info boxes.
   * Doing this with JavaScript because the box requires two background images
   * and not enough browsers support that yet.  This way the user doesn't have
   * to build extra html just to get the icon in there.
   */
  $(options.selectors.contentWell + ' .info, ' + options.selectors.contentWell + ' .note').prepend('<span class="info-icon"></span>');
  
  // if this is a "dual style" screen, use as much as we can
  if($('body.dual').length > 0) {
    closeNav(function(){
      options.jq.contentWell.toggleClass('closed');
      options.jq.navToggle.toggleClass('closed');
      if(!!window.location.hash) {
        $(window.location.hash)[0].scrollIntoView();
      }
    });
  }

});
