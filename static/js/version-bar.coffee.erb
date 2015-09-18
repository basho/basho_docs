(function() {
  var $versions;

  $versions = {
    "riak": [["1.0.0"], ["1.1.0", "1.1.4"], ["1.2.0", "1.2.1"], ["1.3.0", "1.3.1", "1.3.2"], ["1.4.0", "1.4.1", "1.4.2", "1.4.6", "1.4.7", "1.4.8", "1.4.9", "1.4.10", "1.4.12"], ["2.0.0", "2.0.1", "2.0.2", "2.0.4", "2.0.5", "2.0.6"], ["2.1.1"]],
    "riakcs": [["1.2.0", "1.2.1"], ["1.3.0", "1.3.1"], ["1.4.0", "1.4.1", "1.4.2", "1.4.3", "1.4.4", "1.4.5"], ["1.5.0", "1.5.1", "1.5.2", "1.5.3", "1.5.4"], ["2.0.0", "2.0.1"]],
    "stanchion": [["1.3.0", "1.3.1"], ["1.4.0", "1.4.1", "1.4.2", "1.4.3"], ["1.5.0"], ["2.0.0"]],
    "riakcscontrol": [["1.0.0", "1.0.1", "1.0.2"]],
    "dataplatform": [["1.0.0"]],
    "currents": {
      "riak": "2.1.1",
      "riakcs": "2.0.1",
      "stanchion": "2.0.0",
      "riakcscontrol": "1.0.2",
      "dataplatform": "1.0.0",
      "riakee": "2.1.1"
    }
  };

  if (window.infinity_version == null) {
    window.infinity_version = {
      major: Infinity,
      minor: Infinity,
      patch: Infinity
    };
  }

  if (window.zeros_version == null) {
    window.zeros_version = {
      major: 0,
      minor: 0,
      patch: 0
    };
  }

  if (window.parseVersion == null) {
    window.parseVersion = function(str) {
      var v;
      v = /^[<>]?[=]?(\d+)\.?(\d+)?\.?(\d+)?/.exec(str);
      return {
        major: parseInt(v[1]) || 0,
        minor: parseInt(v[2]) || 0,
        patch: parseInt(v[3]) || 0
      };
    };
  }

  if (window.parseVersionRange == null) {
    window.parseVersionRange = function(str) {
      var version, vs;
      if (str.match(/\+$/) || str.match(/^\>\=/)) {
        return [window.parseVersion(str), window.infinity_version];
      }
      if (str.match(/^\>/)) {
        version = window.parseVersion(str);
        version.patch++;
        return [version, window.infinity_version];
      }
      if (str.match(/^\<\=/)) {
        return [window.zeros_version, window.parseVersion(str)];
      }
      if (str.match(/\-$/) || str.match(/^\</)) {
        version = window.parseVersion(str);
        version.patch--;
        return [window.zeros_version, version];
      }
      if (str.match(/.+?\-.+?/)) {
        vs = str.split('-');
        return [window.parseVersion(vs[0]), window.parseVersion(vs[1])];
      } else {
        version = window.parseVersion(str);
        return [version, version];
      }
    };
  }

  if (window.compareVersions == null) {
    window.compareVersions = function(v1, v2) {
      var v1num, v2num;
      v1num = (v1.major * 10000) + (v1.minor * 100) + v1.patch;
      v2num = (v2.major * 10000) + (v2.minor * 100) + v2.patch;
      return v2num - v1num;
    };
  }

  if (window.inVersionRange == null) {
    window.inVersionRange = function(version, range) {
      if (window.compareVersions(version, range[0]) > 0) {
        return false;
      }
      if (window.compareVersions(version, range[1]) < 0) {
        return false;
      }
      return true;
    };
  }

  if (window.pageInVersion == null) {
    window.pageInVersion = function(version) {
      var pageVersion, pageVersionStr, versionRange;
      pageVersionStr = $('meta[name=version]').attr('content');
      if (pageVersionStr == null) {
        return false;
      }
      pageVersion = window.parseVersion(pageVersionStr);
      versionRange = window.parseVersionRange(version);
      return window.inVersionRange(pageVersion, versionRange);
    };
  }

  $(function() {
    var base_url, blockCount, buildLists, currentClass, currentVersion, firstInBlock, halert, halert_header, i, latestUrl, latestVersion, latestVersionStr, len, liLink, link, lists, moved_ranges, moved_ranges_obj, newVersionsEl, output, pageToLatest, project, r, range, ref, ref1, toggleMenu, version, version_range, versionsEl, vmli;
    if (typeof toggleMenu === "undefined" || toggleMenu === null) {
      toggleMenu = function(buttonID, menuID) {
        var c;
        c = $(buttonID).attr('class');
        if (c === 'selected') {
          $(menuID).hide();
          return $(buttonID).attr('class', 'unselected');
        } else {
          $(menuID).show();
          return $(buttonID).attr('class', 'selected');
        }
      };
    }
    $('#version-ddown-button').live('click', function() {
      return toggleMenu('#version-ddown-button', '#version-list');
    });
    $('#version-list, #version-ddown-button').live('mouseup', function() {
      return false;
    });
    $(document).live('mouseup', function() {
      $('#version-list').hide();
      return $('#version-ddown-button').attr('class', 'unselected');
    });
    buildLists = function(project, base_url, moved_ranges_obj) {
      var current, firstInBlock, i, j, k, len, len1, len2, linkPath, linkProject, links, lists, ref, vData, vr_link, vs, vsi, vsp;
      lists = [];
      ref = $versions[project] || [];
      for (i = 0, len = ref.length; i < len; i++) {
        vs = ref[i];
        firstInBlock = true;
        for (j = 0, len1 = vs.length; j < len1; j++) {
          vsi = vs[j];
          vsp = window.parseVersion(vsi);
          if (window.inVersionRange(vsp, range)) {
            current = window.compareVersions(currentVersion, vsp) === 0;
            vsi = vsi.replace(/(\d+[.]\d+[.]\d+).*/, '$1');
            vData = {
              active: true,
              firstInBlock: firstInBlock,
              current: current,
              href: "/" + project + "/" + vsi + base_url,
              text: vsi
            };
            for (k = 0, len2 = moved_ranges_obj.length; k < len2; k++) {
              vr_link = moved_ranges_obj[k];
              if (window.inVersionRange(vsp, vr_link[0])) {
                linkProject = project;
                linkPath = vr_link[1];
                if (/\w+\:/.test(vr_link[1])) {
                  links = vr_link[1].split(/\:/, 2);
                  linkProject = links[0];
                  linkPath = links[1];
                }
                vData.href = "/" + linkProject + "/" + vsi + linkPath;
              }
            }
            lists.unshift(vData);
          } else {
            vsi = vsi.replace(/(\d+[.]\d+[.]\d+).*/, '$1');
            lists.unshift({
              active: false,
              firstInBlock: firstInBlock,
              current: false,
              href: "#",
              text: vsi
            });
          }
          firstInBlock = false;
        }
      }
      return lists;
    };
    version = $('meta[name=version]').attr('content');
    if (version == null) {
      return;
    }
    version = version.replace(/(\d+[.]\d+[.]\d+).*/, '$1');
    $("body").addClass("ver-" + (version.replace(/\./g, '-')));
    versionsEl = $('.mainarticle .versions');
    if (versionsEl.length === 0) {
      return;
    }
    if (!versionsEl.is('div')) {
      newVersionsEl = versionsEl.before("<div class=versions />");
      versionsEl.remove();
      versionsEl = newVersionsEl;
    }
    project = $('meta[name=project]').attr('content');
    base_url = $('meta[name=base-url]').attr('content');
    version_range = $('meta[name=version-range]').attr('content');
    moved_ranges = $('meta[name=moved-ranges]').attr('content');
    if (!((project != null) || (base_url != null) || (version_range != null))) {
      return;
    }
    if (((ref = $versions[project]) != null ? ref.length : void 0) <= 1) {
      return;
    }
    currentVersion = window.parseVersion(version);
    range = window.parseVersionRange(version_range);
    moved_ranges_obj = [];
    ref1 = $.parseJSON(moved_ranges);
    for (r in ref1) {
      link = ref1[r];
      moved_ranges_obj.push([window.parseVersionRange(r), link]);
    }
    lists = buildLists(project, base_url, moved_ranges_obj);
    blockCount = 0;
    firstInBlock = function(li) {
      if (li.firstInBlock) {
        blockCount++;
        return " first ";
      } else {
        return " ";
      }
    };
    currentClass = function(li) {
      var classStr;
      classStr = "";
      if (!li.active) {
        classStr = "inactive";
      } else if (li.current) {
        classStr = "current";
      }
      return " class=\"" + classStr + " versions-" + blockCount + (firstInBlock(li)) + "\"";
    };
    liLink = function(li) {
      return "<a class=\"versioned\" href=\"" + li.href + "\">" + li.text + "</a>";
    };
    output = "";
    output += "<div id=\"version-ddown-button\" class=\"unselected\">";
    output += "<div class=\"version-ddown-title\">Version</div>";
    output += "<div class=\"version-ddown-number\">";
    output += "" + version;
    output += "</div>";
    output += "<div class=\"version-ddown-arrow\"></div>";
    output += "</div>";
    output += "<ol id=\"version-list\">";
    for (i = 0, len = lists.length; i < len; i++) {
      vmli = lists[i];
      output += "<li" + (currentClass(vmli)) + ">";
      output += liLink(vmli);
      output += "</li>";
    }
    latestVersionStr = $versions.currents[project];
    latestVersion = window.parseVersion(latestVersionStr);
    pageToLatest = window.compareVersions(latestVersion, currentVersion);
    if (pageToLatest !== 0) {
      latestUrl = document.URL.replace("/" + version + "/", "/latest/");
      if (pageToLatest < 0) {
        halert_header = "This documentation is for an older version of Riak";
      } else if (pageToLatest > 0) {
        halert_header = "This documentation is for an unreleased version of Riak";
      }
      halert = "<div class=\"alert-version\" id=\"alert-version\">";
      halert += "<div class=\"arrow-left\"></div>";
      halert += "<div class=\"alert-version-message\">";
      halert += "<h4>" + halert_header + "</h4>";
      halert += "<p><a href=\"" + latestUrl + "\">";
      halert += "View the documentation for the latest version</a>";
      halert += "<span class=\"extra-alert-text\"> or select from the drop down on the left.</span>";
      halert += "</p></div><div class=\"version-icon-alert-version-num\">" + version + "</div>";
      halert += "<meta content=\"" + version + "\" itemprop=\"version\">";
      halert += "<div class=\"clear\"></div></div>";
      versionsEl.after(halert);
    }
    return versionsEl.html(output);
  });

}).call(this);
