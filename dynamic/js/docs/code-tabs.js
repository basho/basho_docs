// JavaScript / JQuery solution to wrap <pre><code> blocks in bootstrap-tabs.

$(function() {

  // To display on the code tabs title area
  var languages = {
    'language-advancedconfig': 'advanced.config',
    'language-appconfig': 'app.config',
    'language-bash': 'Shell',
    'language-csharp': 'C#',
    'language-curl': 'HTTP',
    'language-erlang': 'Erlang',
    'language-golang': 'Go',
    'language-java': 'Java',
    'language-javascript': 'Javascript',
    'language-json': 'JSON',
    'language-php': 'PHP',
    'language-protobuf': 'Protobuf',
    'language-python': 'Python',
    'language-riakconf': 'riak.conf',
    'language-riakcsconf': 'riak-cs.conf',
    'language-ruby': 'Ruby',
    'language-stanchionconf': 'stanchion.conf',
    'language-vmargs': 'vm.args',
  };

  // Classes to replace non-existent highlight.js languages
  var replace = {
    'language-advancedconfig': 'language-erlang',
    'language-appconfig': 'language-erlang',
    'language-curl': 'language-bash',
    'language-riakconf': 'language-matlab',
    'language-riakcsconf': 'language-matlab',
    'language-stanchionconf': 'language-matlab',
    'language-vmargs': 'language-ini',
  };

  // Wrap <pre> elements & groups of consecutive <pre>'s
  // with 'code-tabs' & 'tab-content' <div>'s.
  $('pre').each(function() {
    if ($(this.parentNode).hasClass('tab-content')) return;
    $(this).nextUntil(':not(pre)').andSelf().wrapAll(
      '<div class="code-tabs"><div class="tab-content">'
    );
  });

  // Add <ul> for the tab links
  $('div.code-tabs').prepend('<ul class="nav nav-tabs"></ul>');

  $('div.code-tabs').each(function(n) {
    // Count of code tab groups
    var count = "00" + n;
    count = count.substring(count.length - 3);

    // Tab links <ul> for the code tab group
    var linksList = $(this).children('ul.nav.nav-tabs');

    // Each <pre> element for the code tab group
    var preTabs = $(this).children('div.tab-content').children();

    preTabs.each(function() {
      var lang = $(this).children().attr('class');
      var langText = languages[lang];
      var suffix = lang + count;
      var href = '#' + suffix;
      var replaceClass = replace[lang];

      // Wrap each <pre> to enable bootstrap-tab functionality
      $(this).wrap('<div class="tab-pane" id="' + suffix + '">');

      // Add display text and tab link
      // TODO: For languages that don't match, strip the
      // language class and create display text from that
      if (langText !== undefined) {
        linksList.append('<li><a href="' + href + '" data-code="' +
        lang + '" data-toggle="tab">' + langText + '</a></li>');
      }

      // Replace <code> class if given language unsupported
      // by highlight.js
      if (replaceClass !== undefined) {
        $(this).children().attr('class', replaceClass);
      }
    });

    // Add active class to the first element of each
    // code tab group
    preTabs.first().parent().addClass('active');
    linksList.children().first().addClass('active');
  });
});
