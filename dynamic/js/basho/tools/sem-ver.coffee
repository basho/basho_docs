###
Semantic Version Helper Functions
=================================

This file defines the `window.SemVer` object that allows for quick and easy
manipulations of and comparisons between typical X.Y.Z semantic version numbers.
###


#TODO: strict mode enables a pretty large number of runtime checks. We probably
#      want to turn it off when we're deploy to prod.
#      Perhaps we can figure out some way to enable it when building debug?
'use strict'

# Global Library object.
SemVer = window.SemVer = {}

# Min possible version (0.0.0) and max possible versions (∞.∞.∞)
version_min = { major: 0;        minor: 0;        patch: 0        }
version_max = { major: Infinity; minor: Infinity; patch: Infinity }


# ```parse :: (Str) -> {Num, Num, Num}```
# Convert a semantic version string (ex. `"2.1.4"`) to a version object with
# explicit `major`, `minor`, and `patch` data members.
SemVer.parse = (str) ->
  v = /^[<>]?[=]?(\d+)\.?(\d+)?\.?(\d+)?/.exec(str)
  return {
    major: parseInt(v[1]) || 0
    minor: parseInt(v[2]) || 0
    patch: parseInt(v[3]) || 0
  }


# ```parseRange :: (Str) -> [{Num, Num, Num}, {Num, Num, Num}]```
# Convert a semantic version range string (ex. '">=2.1.4"') into a tuple (list
# of length 2, really) containing the minimum and maximum version in the
# given range.
#
# The range string must be in one of the following forms;
#   * Greater than             -- `">X.Y.Z"`
#   * Greater than or equal    -- `">-X.Y.Z"`, `"X.Y.Z+"`
#   * Less than                -- `"<X.Y.Z"`
#   * Less than or equal       -- `"<-X.Y.Z"`, `"X.Y.Z-"`
#   * Explicit range           -- `"A.B.C-X.Y.Z"`
#   * Single version           -- `"X.Y.Z"`
SemVer.parseRange = (str) ->
  if str.match(/\+$/) or str.match(/^\>\=/)    # Greater than or equal to
    return [SemVer.parse(str), version_max]
  else if str.match(/^\>/)                     # Greater than
    # This is explicitly greater than, so only include from the next version
    version = SemVer.parse(str)
    version.patch++
    return [version, version_max]
  else if str.match(/^\<\=/)                   # Less than or equal to
    return [version_min, SemVer.parse(str)]
  else if str.match(/\-$/) or str.match(/^\</) # Less than
    # This is explicitly less than, so only include up to the previous version
    version = SemVer.parse(str)
    version.patch--
    return [version_min, version]
  else if str.match(/.+?\-.+?/)                # Explicit range
    vs = str.split('-')
    return [SemVer.parse(vs[0]), SemVer.parse(vs[1])]
  else                                         # Single version
    version = SemVer.parse(str)
    return [version, version]


# ```compare :: ({Num, Num, Num}, {Num, Num, Num}) -> Num```
# Compare two semantic versions using typical comparator rules;
#   <0 == `v1` is newer than `v2`
#   0  == `v1` and `v2` are the same version
#   >0 == `v2` is newer than `v1`
SemVer.compare = (v1, v2) ->
  if      (v1.major != v2.major) then return (v2.major - v1.major)
  else if (v1.minor != v2.minor) then return (v2.minor - v1.minor)
  else if (v1.patch != v2.patch) then return (v2.patch - v1.patch)
  else                                return 0


# ```isInRange :: ({Num, Num, Num},
#                  [{Num, Num, Num}, {Num, Num, Num}]) -> Bool```
# Checks to see if the given version is within the given range. It's assumed
# that the range is ordered oldest to newest.
SemVer.isInRange = (version, range) ->
  [oldest, newest] = range
  if SemVer.compare(version, oldest) > 0 or
     SemVer.compare(version, newest) < 0
    return false
  else
    return true
