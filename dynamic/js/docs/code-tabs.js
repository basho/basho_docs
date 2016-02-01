$(function() {

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
    'language-stanionconf': 'stanchion.conf',
    'language-vmargs': 'vm.args',
  };

  $('pre').each(function() {
    if ($(this.parentNode).hasClass('tab-content')) return;
    $(this).nextUntil(':not(pre)').andSelf().wrapAll(
      '<div class="code-tabs"><div class="tab-content">'
    );
  });

  $('div.code-tabs').prepend('<ul class="nav nav-tabs"></ul>');

  $('div.code-tabs').each(function(n) {
    var count = "00" + n;
    count = count.substring(count.length - 3);

    var linksList = $(this).children('ul.nav.nav-tabs');
    var preTabs = $(this).children('div.tab-content').children();

    preTabs.each(function() {
      var lang = $(this).children().attr('class');
      var langText = languages[lang];
      var suffix = lang + count;
      var href = '#' + suffix;

      $(this).wrap('<div class="tab-pane" id="' + suffix + '">')

      if (langText !== undefined) {
        linksList.append('<li><a href="' + href + '" data-code="' +
        lang + '" data-toggle="tab">' + langText + '</a></li>');
      };
    });

    preTabs.first().parent().addClass('active');
    linksList.children().first().addClass('active');
  });
});
