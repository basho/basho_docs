//= require_tree .

// Don't throw an error if jQuery doesn't load
(typeof jQuery !== 'function') ? null : jQuery(function () {

  var $ = jQuery,
      contentMargin;
      
  function getContentMargin() {
    contentMargin = $('div[role=main]').css('margin-left');
  }
  getContentMargin();
  
  function closeNav() {
    $('#primary-nav').fadeOut();
    $('div[role=main]').animate({marginLeft: '24px'}, {queue: false, duration: 300});
  }
  
  function openNav() {
    var cm = contentMargin;
    $('#primary-nav').fadeIn();
    $('div[role=main]').animate({marginLeft: cm}, {queue: false, duration: 300});
  }
  
  $(window).on('resize', getContentMargin);
  $(document).on('click', '#nav-toggle', function () {
    if ($('#primary-nav').is(':hidden')) {
      openNav();
    } else {
      closeNav();
    }
  });

});
