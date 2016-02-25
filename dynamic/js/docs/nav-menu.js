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
