var filter = function(stanza) {
  var oldBody = stanza['body'];
  var newBody = runFilters(oldBody);
  stanza['body'] = newBody;
  ejsLog("/tmp/mod_js_filter.log", oldBody + " -> " + newBody);
  return stanza;
};

var runFilters = function(text) {
  return "[filtered] " + text;
};
