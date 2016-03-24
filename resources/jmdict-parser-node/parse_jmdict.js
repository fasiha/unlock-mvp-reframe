"use strict";

/*
 * This script converts the [`JMdict_e` XML
 * file](ftp://ftp.monash.edu.au/pub/nihongo/JMdict_e.gz) from the [JMdict
 * Project](http://www.edrdg.org/jmdict/j_jmdict.html) into a line-delimited
 * JSON file containing the exact same data, in `JMdict-full.ldjson`.
 *
 * The line-delimited JSON format involves valid JSON strings separated by
 * newlines.
 *
 * The XML to JSON conversion is performed by `xml2js`, whose raw output is
 * cleaned up slightly by our custom `xmlCleanup` function below, e.g.,
 * converting numeric strings back to numbers, and converting arrays known to
 * have a single element to scalars (only `reb` and `keb`, as gleaned from the
 * XML file's DTD).
 *
 * The whole process takes <30 seconds on my high-end 2015 Mac Book Pro.
 */

var assert = require('assert');
var utils = require('../nodeUtilities.js');
var parseString = require('xml2js').parseString;
var Promise = require('bluebird');
var parseStringAsync = Promise.promisify(parseString);

var entries = utils.read('JMdict_e')   // Slurp file into giant string
                  .replace(/\n/g, '')  // remove all newlines
                  .replace(/&([a-zA-Z0-9-]+);/g,
                           '$1')      // replace "&X;" with "X"
                                      // xml2js hates "&XYZ;" entities: FIXME?
                  .split('</entry>')  // string to array at </entry>
                  .slice(0, -1);      // discard last element: </JMdict>

function xmlCleanUp(obj) {
  var entry = obj.entry;
  entry.ent_seq = +entry.ent_seq;
  if (entry.k_ele) {  // 0 or more k_ele allowed so check for existence
    entry.k_ele.forEach(o => {
      assert(o.keb.length == 1);  // for each k_ele, only one keb allowed
      o.keb = o.keb[0];
    });
  }
  entry.r_ele.forEach(o => {    // at least one r_ele guaranteed
    assert(o.reb.length == 1);  // for each r_ele, only one reb allowed
    o.reb = o.reb[0];
  });
  return entry;
}

var headwords;  // global variable in case I want to mess with it in REPL

// Send all elements of `entries` array to xml2js, clean up resulting objects,
// and write one object to line as line-delimited JSON
Promise.all(entries.map(s => parseStringAsync('<entry>' +
                                              s.split('<entry>').slice(-1)[0] +
                                              '</entry>')
                                 .then(o => xmlCleanUp(o))))
    .then(objs => {
      headwords = objs;
      // Write everything
      utils.writeLineDelimitedJSON('JMdict-full.ldjson', headwords);
    });

