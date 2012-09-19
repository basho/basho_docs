//= require_tree .

// Don't throw an error if jQuery doesn't load
(typeof jQuery !== 'function') ? null : jQuery(function () {

  var $ = jQuery,
      contentMargin;
      
  function getContentMargin() {
    var margin = $('div[role=main]').css('margin-left');
    contentMargin = (margin === '24px') ? contentMargin : margin;
  }
  getContentMargin();
  
  function reverseToggle() {
    $('#nav-toggle, div[role=main]').toggleClass('closed');
  }
  
  function closeNav(callback) {
    $('#primary-nav').fadeOut();
    $('div[role=main]').animate({marginLeft: '24px'}, {queue: false, duration: 300, complete: callback});
  }
  
  function openNav(callback) {
    var cm = contentMargin;
    $('#primary-nav').fadeIn();
    $('div[role=main]').animate({marginLeft: cm}, {queue: false, duration: 300, complete: callback});
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
