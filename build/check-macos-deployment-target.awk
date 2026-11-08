function exceeds(version, limit,    v, l, i, n) {
  n = split(version, v, ".")
  split(limit, l, ".")
  for (i = 1; i <= n; i++) {
    if (v[i] + 0 != l[i] + 0) {
      return v[i] + 0 > l[i] + 0
    }
  }
  return 0
}

function check(version) {
  found = 1
  if (exceeds(version, target)) {
    print file " requires macOS " version > "/dev/stderr"
    status = 1
  }
}

/^[^ \t].*:$/ {
  file = substr($0, 1, length($0) - 1)
}

/cmd LC_BUILD_VERSION$/ {
  build_version = 1
}

build_version && $1 == "minos" {
  build_version = 0
  check($2)
}

/cmd LC_VERSION_MIN_MACOSX$/ {
  version_min = 1
}

version_min && $1 == "version" {
  version_min = 0
  check($2)
}

END {
  if (!found) {
    print "found no Mach-O deployment target in the checked paths" > "/dev/stderr"
    status = 1
  }
  exit status
}
