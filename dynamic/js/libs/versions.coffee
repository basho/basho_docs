window.infinity_version = {
  major: Infinity
  minor: Infinity
  patch: Infinity
}

window.zeros_version = {
  major: 0
  minor: 0
  patch: 0
}

window.parseVersion = (str)->
  v = /^[<>]?[=]?(\d+)\.?(\d+)?\.?(\d+)?/.exec(str)
  {
    major: parseInt(v[1]) || 0
    minor: parseInt(v[2]) || 0
    patch: parseInt(v[3]) || 0
  }

# X+, >=X, >X
# X-, <=X, <X
# X-Y
window.parseVersionRange = (str)->
  # greater than or equal to
  if str.match(/\+$/) or str.match(/^\>\=/)
    return [window.parseVersion(str), window.infinity_version]
  # greater than
  if str.match(/^\>/)
    version = window.parseVersion(str)
    # don't include this version, so bump it up
    version.patch++
    return [version, window.infinity_version]
  # less than or equal to
  if str.match(/^\<\=/)
    return [window.zeros_version, window.parseVersion(str)]
  # less than
  if str.match(/\-$/) or str.match(/^\</)
    version = window.parseVersion(str)
    # don't include this version, so drop it down
    version.patch--
    return [window.zeros_version, version]
  if str.match(/.+?\-.+?/)
    vs = str.split('-')
    return [window.parseVersion(vs[0]), window.parseVersion(vs[1])]
  else
    version = window.parseVersion(str)
    return [version, version]

window.compareVersions = (v1, v2)->
  v1num = (v1.major * 10000) + (v1.minor * 100) + v1.patch
  v2num = (v2.major * 10000) + (v2.minor * 100) + v2.patch
  v2num - v1num

# all comparisons are inclusive
window.inVersionRange = (version, range)->
  # false if less than 0 or greater than 1
  return false if window.compareVersions(version, range[0]) > 0
  return false if window.compareVersions(version, range[1]) < 0
  true

window.pageInVersion = (version)->
  pageVersionStr = $('meta[name=version]').attr('content')
  return false unless pageVersionStr?
  pageVersion = window.parseVersion(pageVersionStr)
  versionRange = window.parseVersionRange(version)
  window.inVersionRange(pageVersion, versionRange)
