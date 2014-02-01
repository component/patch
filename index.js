var diff_match_patch = require('./diff_match_patch');

/**
 * Creates a patch.
 * @param {String} old (optional)
 * @param {Array} diff
 * @returns {Array}
 * @api public
 */

function patch() {
  var dmp = new diff_match_patch();
  if (arguments.length == 1) {
    // single argument, diff
    return dmp.patch_make(arguments[0]);
  } else {
    if (typeof arguments[1] == 'string') {
      // two arguments, old and delta
      var d = dmp.diff_fromDelta(arguments[0], arguments[1]);
      return dmp.patch_make(arguments[0], d);
    } else {
      // two arguments, old and diff
      return dmp.patch_make(arguments[0], arguments[1]);
    }
  }
}

/**
 * Applies a patch to a piece of text, and optionally reports what pieces
 * of the patch were succesfully applied.
 * @param {Array} patch
 * @param {String} text
 * @param {Boolean} report (optional)
 * @returns {String, Array}
 * @api public
 */

patch.apply = function(patch, text, report) {
  var dmp = new diff_match_patch();
  if (report) {
    return dmp.patch_apply(patch, text);
  } else {
    return dmp.patch_apply(patch, text)[0];
  }
}

/**
 * Converts a patch to a string representation
 * @param {Array} patch
 * @returns {String}
 * @api public 
 */

patch.toText = function(patch) {
  var dmp = new diff_match_patch();
  return dmp.patch_toText(patch);
}

/**
 * Converts a string representation of a patch back into a patch
 * @param {String} text
 * @returns {Array}
 * @api public 
 */

patch.fromText = function(text) {
  var dmp = new diff_match_patch();
  return dmp.patch_fromText(text);
}

module.exports = patch;
