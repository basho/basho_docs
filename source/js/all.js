//= require_tree .

// Don't throw an error if jQuery doesn't load
(typeof jQuery !== 'function') ? null : jQuery(function () {
  
  var options = {
    closedNavMargin : '12px',
    navSpeed        : 300
  };

  var $ = jQuery,
      contentMargin;
      
  function getContentMargin() {
    var margin = $('div[role=main]').css('margin-left');
    contentMargin = (margin === options.closedNavMargin) ? contentMargin : margin;
  }
  getContentMargin();
  
  function reverseToggle() {
    $('#nav-toggle, div[role=main]').toggleClass('closed');
  }
  
  function closeNav(callback) {
    $('#primary-nav').fadeOut();
    $('div[role=main]').animate({
      marginLeft: options.closedNavMargin
    }, {
      queue: false, 
      duration: options.navSpeed, 
      complete: callback
    });
  }
  
  function openNav(callback) {
    var cm = contentMargin;
    $('#primary-nav').fadeIn();
    $('div[role=main]').animate({
      marginLeft: cm
    }, {
      queue: false,
      duration: options.navSpeed,
      complete: callback
    });
  }
  
  $(window).on('resize', getContentMargin);

  $(document).on('click', '#nav-toggle', function () {
    if ($('#primary-nav').is(':hidden')) {
      openNav(reverseToggle);
    } else {
      closeNav(reverseToggle);
    }
  });
  
  
  // Special display cases
  if ($('body').hasClass('index')) {
    $('.secondary').hide();
  }

});
