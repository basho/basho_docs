window.addEventListener("DOMContentLoaded", function(event)
{
  var searchIndices  = null;
  var indexedItems = null;
  var queuedTerm = null;
  var queuedProject = null;
  var queuedVersion = null;
  var searchProjectDescriptions = null;

  var form = document.getElementById("lunrsearch");
  var metaDocsRootURL = $('meta[name=docs_root_url]').attr('content');
  
  // if a search form is defined, then set it up
  if (form)
  {
    // ensure project descriptions are available
    var pathProjectDescriptions = metaDocsRootURL + "data/project_descriptions.json";
    var requestProjectDescriptions = new XMLHttpRequest();
    requestProjectDescriptions.open("GET", pathProjectDescriptions);
    requestProjectDescriptions.responseType = "json";
    requestProjectDescriptions.addEventListener("load", function(event)
    {
      searchProjectDescriptions = requestProjectDescriptions.response;
      searchIndices  = {};
      indexedItems = {};

    
      // Enable search
      addSearchSubmitEventListener(form);
    }, false);
    requestProjectDescriptions.send(null);
  }

  function addSearchSubmitEventListener(formElement) {
    // Enable search
    formElement.addEventListener("submit", function(event)
    {
        event.preventDefault();

        var searchTermElement = formElement.querySelector(".lunrsearch-term");
        var searchProjectElement = formElement.querySelector(".lunrsearch-project");
        var searchVersionElement = formElement.querySelector(".lunrsearch-version");

        var searchTerm = searchTermElement.value.trim();
        var searchProject = searchProjectElement.value.trim();
        var searchVersion = searchVersionElement.value.trim();

        /*
        console.log("Search term: " + searchTerm);
        console.log("Search project: " + searchProject);
        console.log("Search version: " + searchVersion);
        */

        if (!searchTerm)
          return;

        // if the nav is open then close it
        if (document.querySelector(".content-nav.content-nav--fullscreen")) {
          document.querySelector(".menu-bars").click();
        }
        startSearch(searchTerm, searchProject, searchVersion);
    }, false);    
  }

  function startSearch(searchTerm, searchProject, searchVersion)
  {
    // Start icon animation.
    form.setAttribute("data-running", "true");

    if (searchIndices[searchProject] && searchIndices[searchProject][searchVersion])
    {
      // Index already present, search directly.
      search(searchTerm, searchProject, searchVersion);
    }
    else if (queuedTerm && (searchProject === queuedProject) && (searchVersion === queuedVersion))
    {
      // Same index is being loaded, replace the term we want to search for.
      queuedTerm = searchTerm;
      queuedProject = searchProject;
      queuedVersion = searchVersion;
    }
    else
    {
      // Start loading new index, perform the search when done.
      queuedTerm = searchTerm;
      queuedProject = searchProject;
      queuedVersion = searchVersion;
      initIndex(searchProject, searchVersion);
    }
  }

  function searchDone()
  {
    // Stop icon animation.
    form.removeAttribute("data-running");

    queuedTerm = null;
    queuedProject = null;
    queuedVersion = null;
}
  function initIndex(searchProject, searchVersion)
  {
    if (!searchIndices[searchProject]) {
        searchIndices[searchProject] = {};
    }
    if (!searchIndices[searchProject][searchVersion]) {
        searchIndices[searchProject][searchVersion] = null;
    }

    var projectVersionPath = "";
    if (searchProject && searchProject != '')
    {
        projectVersionPath += searchProject.replace('_','/') + '/';
        if (searchVersion && searchVersion != '')
        {
            projectVersionPath += searchVersion + '/';
        }
    }
    var path = metaDocsRootURL + projectVersionPath + "search-index.json";

    var request = new XMLHttpRequest();
    request.open("GET", path);
    request.responseType = "json";
    request.addEventListener("load", function(event)
    {
      searchIndices[searchProject][searchVersion] = lunr(function(){
        this.ref("uri");
        // If you added more searchable fields to the search index, list them here.
        this.field("title", { boost: 5 });
        this.field("title_supertext");
        this.field("content");
        this.field("description", { boost: 2 });
        this.field("project");
        this.field("version");
        this.field("keywords", { boost: 20 });
        var response = request.response;
        for (var i = 0; i < response.length; i++)
        {
          var doc = response[i];
          if (doc.search_boost) {
            this.add(doc, { boost: doc.search_boost });
          } else {
            this.add(doc);
          }
          // store the indexed item if not already stored
          if (!indexedItems[doc.uri])
            indexedItems[doc.uri] = doc;
        }

      });

      // Search index is ready, perform the search now if still the right search index
      if ((searchProject === queuedProject) && (searchVersion === queuedVersion))
        search(queuedTerm, queuedProject, queuedVersion);
    }, false);
    request.addEventListener("error", searchDone, false);
    request.send(null);
}
  function search(searchTerm, searchProject, searchVersion)
  {
    var results = searchIndices[searchProject][searchVersion].search(searchTerm);

    // The element where search results should be displayed, adjust as needed.
    var titleContainer = document.querySelector('.front-matter');
    var resultsContainer = document.querySelector('.main-article main');

    while (titleContainer.firstChild)
        titleContainer.removeChild(titleContainer.firstChild);

    while (resultsContainer.firstChild)
        resultsContainer.removeChild(resultsContainer.firstChild);

    var titleTemplate = document.getElementById("search-result-title");
    var titleElement = titleTemplate.content.cloneNode(true);
    if (searchProject) {
        titleElement.querySelector(".title-project-name").innerHTML = searchProjectDescriptions[searchProject].project_name_html;
        if (searchVersion) {
            titleElement.querySelector(".title-project-version").textContent = searchVersion;
        }
    }

    var titleText = "";

    if (results.length === 0)
        titleText = 'No results found for “' + searchTerm + '”';
    else if (results.length == 1)
        titleText = 'Found only one result for “' + searchTerm + '”';
    else
        titleText = 'Found ' + results.length + ' results for “' + searchTerm + '”';

    titleElement.querySelector(".title-text").textContent = titleText;
    // also update the search term in the nav menu
    titleElement.querySelector(".lunrsearch-term").value = form.querySelector(".lunrsearch-term").value = searchTerm;
    // do not update the verison or project search in the nav menu
    titleElement.querySelector(".lunrsearch-project").value = searchProject;
    titleElement.querySelector(".lunrsearch-version").value = searchVersion;
    
    addSearchSubmitEventListener(titleElement.querySelector(".search-redo-form"));

    titleContainer.appendChild(titleElement);
    document.title = titleText;

    var resultItemTemplate = document.getElementById("search-result-item");
    for (var i = 0; i < results.length; i++)
    {
      var result = results[i];
      var doc = indexedItems[result.ref];

      // Fill out search result template, adjust as needed.
      var element = resultItemTemplate.content.cloneNode(true);
      element.querySelector(".result-title-link").href = element.querySelector(".result-read-more-link").href = doc.uri;
      if (doc.title_supertext) {
        element.querySelector(".result-title-link").textContent = doc.title_supertext + " " + doc.title;
      } else {
        element.querySelector(".result-title-link").textContent = doc.title;
      }
      if (doc.project) {
        element.querySelector(".result-project-name").innerHTML = searchProjectDescriptions[doc.project].project_name_html;
        if (doc.version) {
            element.querySelector(".result-project-version").textContent = doc.version;
        }
      }
      element.querySelector(".result-summary").textContent = truncate(doc.content, 30);
      element.querySelector(".result-score").textContent = "(score:" + result.score + ")"
      resultsContainer.appendChild(element);
    }
    //titleContainer.querySelector(".front-matter__title--supertext").scrollIntoView({ behavior: "smooth" });
    window.scrollTo(0,0);

    searchDone();
}

  // This matches Hugo's own summary logic:
  // https://github.com/gohugoio/hugo/blob/b5f39d23b8/helpers/content.go#L543
  function truncate(text, minWords)
  {
    var match;
    var result = "";
    var wordCount = 0;
    var regexp = /(\S+)(\s*)/g;
    while (match = regexp.exec(text))
    {
      wordCount++;
      if (wordCount <= minWords)
        result += match[0];
      else
      {
        var char1 = match[1][match[1].length - 1];
        var char2 = match[2][0];
        if (/[.?!"]/.test(char1) || char2 == "\n")
        {
          result += match[1];
          break;
        }
        else
          result += match[0];
      }
    }
    return result;
  }
}, false);
