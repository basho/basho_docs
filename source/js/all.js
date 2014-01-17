//= require_tree ./libs
//= require_tree ./docs

/*
 * Overly-paranoid jQuery docready call
 * Won't produce an error if jQuery doesn't load
 */
(typeof jQuery !== 'function') ? null : jQuery(function () {
  
  var $ = jQuery, contentMargin, defaultNavState = 1, i;

  /*
   * Polyfill Array.map if it doesn't exist.
   */
  Array.prototype.map = Array.prototype.map || function (fn) {
    var i, len = this.length, output = [];
    for (i = 0; i < len; i += 1) {
      output.push(fn.call(this, this[i], i));
    }
    return output;
  };
  
  /*
   * Global config options
   * Make a few recurring selections up front just to speed up the jQuery
   * process every time we select them
   */
  var options = {
    
    selectors : {
      navContainer     : '#nav-container',
      navContent       : '#primary-nav',
      navLinks         : '#fixed-nav',
      contentWell      : 'div[role=main]',
      navToggle        : '#nav-toggle',
      header           : '#header',
      watermark        : '.watermark-wrapper',
      responsiveToggle : '.responsive-toggle'
    },

    
    params : {
      closedNavMargin : '12px',
      navSpeed        : 174,
      responsiveWidth : 700
    }
  };
  
  /*
   * Default value for the version alert arrow
   */
  var version_box_scroll_off = false;
  
  
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
   /*
  function reverseToggle() {
    options.jq.contentWell.toggleClass('closed');
    options.jq.navToggle.toggleClass('closed');
  }
  */
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


  function setNavState(val) {
    return $.localItem.set('navstate', String(val));
  }

  function getNavState() {
    var item = String($.localItem.get('navstate'));
    return item ? parseInt(item) : defaultNavState;
  }
  
  /*
   * closeNav()
   * Animates the sidebar nav into the closed position.
   * If noFade is set to true, the animations will be immediate.
   */
  function closeNav(callback, noFade) {
    var config;
    if (noFade === true) {
      config = animConfig(false, 0, callback);
    }
    options.jq.navContent.fadeOut(noFade ? 0 : options.params.navSpeed);
    options.jq.navLinks.fadeOut(noFade ? 0 : options.params.navSpeed);
    options.jq.contentWell.animate({marginLeft: options.params.closedNavMargin}, config || animConfig(false, 200, callback));
    options.jq.navContainer.animate({width: options.params.closedNavMargin}, config || animConfig());
    options.jq.header.animate({marginLeft: options.params.closedNavMargin}, config || animConfig());
    options.jq.watermark.animate({marginLeft: options.params.closedNavMargin}, config || animConfig());
    setNavState(0);
  }
  
  /*
   * openNav()
   * Animates the sidebar nav into the open position
   * If noFade is set to true, the animations will be immediate.
   */
  function openNav(callback, noFade) {

    var cm = contentMargin, config;
    if (noFade === true) {
      config = animConfig(false, 0, callback);
    }
    options.jq.navContent.fadeIn(noFade ? 0 : options.params.navSpeed);
    options.jq.navLinks.fadeIn(noFade ? 0 : options.params.navSpeed);
    options.jq.contentWell.animate({marginLeft: cm}, config || animConfig(false, 200, callback));
    options.jq.navContainer.animate({width: cm}, config || animConfig());
    options.jq.header.animate({marginLeft: cm}, config || animConfig());
    options.jq.watermark.animate({marginLeft: cm}, config || animConfig());
    setNavState(1);
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
    setNavState(1);
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
    setNavState(0);
  }
  
  /*
   * determineNavAction()
   * Determines whether the nav should be opened or closed
   * at any given time.
   */
  function determineNavAction() {
    if (options.jq.navContent.is(':hidden')) {
      openNav();
    } else {
      closeNav();
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
      options.jq.header.removeAttr('style');
      options.jq.watermark.removeAttr('style');
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

  /*
   * When the page loads, determine whether the nav should be open or closed.
   */
  function assessNavState() {
    var userState    = getNavState(),
        isResponsive = options.jq.navToggle.is(':hidden');
    if (!isResponsive) {
      userState === 0 ? closeNav(undefined, true) : openNav(undefined, true);
    }
  }
  assessNavState();





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
  // $(options.selectors.contentWell + ' .info, ' + options.selectors.contentWell + ' .note').prepend('<span class="info-icon"></span>');
  
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

  /*
   * toggleMenu()
   * Toggle the menu display on/off when user
   * clicks the button and toggle the button class
   */
  function toggleMenu(buttonID, menuID) {
    var X=$(buttonID).attr('class');
    if(X=='selected') {
      $(menuID).hide();
      $(buttonID).attr('class', 'unselected');
    } else {
      $(menuID).show();
      $(buttonID).attr('class', 'selected');
    }
  }

  /*
   * documentClick()
   * Hide the dropdown menu when user clicks anywhere else
   * in the document and toggle the button class
   */
  function documentClick(buttonID, menuID) {
    $(menuID).hide();
    $(buttonID).attr('class', 'unselected');
  }

  /*
   * hideVersionAlert()
   * Hide or show the version alert box depending
   * on scrollTop
   */
  function hideVersionAlert() {

    if ($(window).scrollTop() > 0) {
        
      if (version_box_scroll_off) {
        return;
      }
      
      $('#alert-version').addClass('version_box_scroll_off');
      version_box_scroll_off = true;

    } else {
      $('#alert-version').removeClass('version_box_scroll_off');
      version_box_scroll_off = false;
    }
  }

  $(document).ready(function(){
    /*----------------------------------------------------------*/
    // Top Navigation Menu Interactions
    /*----------------------------------------------------------*/
    $('#nav-more').click(function() {
      toggleMenu('#nav-more', '#nav-menu');
    });

    //Mouse click on sub menu
    $('#nav-menu').mouseup(function() {
      return false
    });

    //Mouse click on my account link
    $('#nav-more').mouseup(function() {
      return false
    });

    // /*----------------------------------------------------------*/
    // // Version Menu Interactions
    // /*----------------------------------------------------------*/
    // $('#version-ddown-button').click(function() {
    //   toggleMenu('#version-ddown-button', '#version-list');
    //   //toggleSelected('#version-ddown-title');
    //   //toggleSelected('#version-ddown-arrow');
    // });

    // //Mouse click on sub menu
    // $('#version-list').mouseup(function() {
    //   return false
    // });

    // //Mouse click on my account link
    // $('#version-ddown-button').mouseup(function() {
    //   return false
    // });

    /*----------------------------------------------------------*/
    // Close all menu when the document is clicked
    /*----------------------------------------------------------*/
    $(document).mouseup(function() {
      documentClick('#nav-more', '#nav-menu');
      documentClick('#version-ddown-button', '#version-list');
    });

    /*----------------------------------------------------------*/
    // If the version alert box exists in the DOM then
    // hide it during scroll
    /*----------------------------------------------------------*/
    
    if ($('#alert-version').length) {
      $(window).scroll(function() {
        hideVersionAlert();
      });
    }

  });
});
