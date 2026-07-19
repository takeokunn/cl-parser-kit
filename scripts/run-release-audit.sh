#!/bin/sh

set -u

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
project_root=$(CDPATH= cd -- "$script_dir/.." && pwd)
failures=0

pass() {
  printf 'PASS %s\n' "$1"
}

fail() {
  printf 'FAIL %s\n' "$1" >&2
  failures=$((failures + 1))
}

run_with_timeout() {
  timeout_seconds=$1
  shift

  if command -v timeout >/dev/null 2>&1; then
    timeout "$timeout_seconds" "$@"
    return
  fi

  if command -v gtimeout >/dev/null 2>&1; then
    gtimeout "$timeout_seconds" "$@"
    return
  fi

  nix shell nixpkgs#coreutils -c timeout "$timeout_seconds" "$@"
}

check_file() {
  path=$1
  label=$2

  if [ -f "$project_root/$path" ]; then
    pass "$label"
  else
    fail "$label"
  fi
}

check_contains() {
  path=$1
  needle=$2
  label=$3

  if NEEDLE=$needle perl -0ne 'exit(index($_, $ENV{NEEDLE}) >= 0 ? 0 : 1)' \
      "$project_root/$path"; then
    pass "$label"
  else
    fail "$label"
  fi
}

check_matching_quick_start_surface() {
  label=$1

  if PROJECT_ROOT=$project_root perl -e '
    use strict;
    use warnings;

    sub read_file {
      my ($path) = @_;
      open my $fh, q{<}, $path or die "open $path: $!";
      local $/;
      return <$fh>;
    }

    sub quick_start_items {
      my ($contents, $heading) = @_;
      my @items;
      my $in_section = 0;

      for my $line (split /\n/, $contents) {
        if ($line eq $heading) {
          $in_section = 1;
          next;
        }

        if ($in_section && $line =~ /^###{0,1} /) {
          last;
        }

        next unless $in_section;

        if ($line =~ /^- `([^`]+)`/) {
          push @items, $1;
        }
      }

      return \@items;
    }

    sub duplicate_items {
      my ($items) = @_;
      my %count;
      my @duplicates;

      for my $item (@{$items}) {
        $count{$item}++;
      }

      for my $item (sort keys %count) {
        push @duplicates, $item if $count{$item} > 1;
      }

      return \@duplicates;
    }

    my $project_root = $ENV{PROJECT_ROOT};
    my $api_items = quick_start_items(
      read_file("$project_root/API.md"),
      q{## Quick Start Surface},
    );
    my $readme_items = quick_start_items(
      read_file("$project_root/README.md"),
      q{### Quick Start Surface},
    );

    my @duplicate_api = @{duplicate_items($api_items)};
    my @duplicate_readme = @{duplicate_items($readme_items)};

    print "duplicate in API: " . join(", ", @duplicate_api) . "\n"
      if @duplicate_api;
    print "duplicate in README: " . join(", ", @duplicate_readme) . "\n"
      if @duplicate_readme;
    exit 1 if @duplicate_api || @duplicate_readme;

    if ("@{$api_items}" eq "@{$readme_items}") {
      exit 0;
    }

    my %readme = map { $_ => 1 } @{$readme_items};
    my %api = map { $_ => 1 } @{$api_items};
    my @missing_from_readme = grep { !$readme{$_} } @{$api_items};
    my @extra_in_readme = grep { !$api{$_} } @{$readme_items};

    print "missing from README: " . join(", ", @missing_from_readme) . "\n"
      if @missing_from_readme;
    print "extra in README: " . join(", ", @extra_in_readme) . "\n"
      if @extra_in_readme;
    exit 1;
  '; then
    pass "$label"
  else
    fail "$label"
  fi
}

run_step_with_timeout() {
  timeout_seconds=$1
  label=$2
  shift 2

  printf 'RUN  %s\n' "$label"
  if run_with_timeout "$timeout_seconds" "$@"; then
    pass "$label"
  else
    fail "$label"
  fi
}

for required in \
  LICENSE \
  README.md \
  API.md \
  ARCHITECTURE.md \
  PARSING_PATTERNS.md \
  EXAMPLES.md \
  CONTRIBUTING.md \
  CODE_OF_CONDUCT.md \
  SUPPORT.md \
  SECURITY.md \
  VERSIONING.md \
  RELEASING.md \
  GOVERNANCE.md \
  MAINTAINERS.md \
  ROADMAP.md \
  CHANGELOG.md \
  scripts/run-compile-check.lisp \
  scripts/run-tests.lisp \
  scripts/run-examples.lisp \
  scripts/run-implementation-smoke.sh
do
  check_file "$required" "required artifact: $required"
done

check_contains README.md "scripts/run-release-audit.sh" \
  "README documents the release audit entry point"
check_contains README.md "scripts/run-tests.lisp" \
  "README documents the baseline verification entry point"
check_contains README.md "scripts/run-compile-check.lisp" \
  "README documents the compile verification entry point"
check_contains README.md "scripts/run-examples.lisp" \
  "README documents the examples verification entry point"
check_contains README.md "scripts/run-implementation-smoke.sh" \
  "README documents the smoke verification entry point"
check_contains README.md "CONTRIBUTING.md" \
  "README points contributors at the contributing guide"
check_contains README.md "CODE_OF_CONDUCT.md" \
  "README points collaborators at the conduct policy"
check_contains SUPPORT.md "scripts/run-release-audit.sh" \
  "SUPPORT documents release-audit usage"
check_contains SUPPORT.md "scripts/run-compile-check.lisp" \
  "SUPPORT documents compile verification usage"
check_contains SUPPORT.md "scripts/run-examples.lisp" \
  "SUPPORT documents example verification usage"
check_contains SUPPORT.md "VERSIONING.md" \
  "SUPPORT references versioning policy"
check_contains SUPPORT.md "SECURITY.md" \
  "SUPPORT references the security reporting path"
check_contains SUPPORT.md "RELEASING.md" \
  "SUPPORT references the release policy"
check_contains CONTRIBUTING.md "CODE_OF_CONDUCT.md" \
  "CONTRIBUTING references collaboration policy"
check_contains CONTRIBUTING.md "SECURITY.md" \
  "CONTRIBUTING references security policy"
check_contains CONTRIBUTING.md "scripts/run-tests.lisp" \
  "CONTRIBUTING documents the raw-checkout verification entry point"
check_contains CONTRIBUTING.md "scripts/run-compile-check.lisp" \
  "CONTRIBUTING documents the compile verification entry point"
check_contains CONTRIBUTING.md "scripts/run-examples.lisp" \
  "CONTRIBUTING documents the examples verification entry point"
check_contains SECURITY.md "SUPPORT.md" \
  "SECURITY references the verified support boundary"
check_contains CODE_OF_CONDUCT.md "SECURITY.md" \
  "CODE_OF_CONDUCT references the reporting path"
check_contains RELEASING.md "scripts/run-release-audit.sh" \
  "RELEASING documents the release audit entry point"
check_contains RELEASING.md "scripts/run-compile-check.lisp" \
  "RELEASING documents the compile verification entry point"
check_contains RELEASING.md "scripts/run-examples.lisp" \
  "RELEASING documents the examples verification entry point"
check_contains RELEASING.md "scripts/run-implementation-smoke.sh" \
  "RELEASING documents the smoke verification entry point"
check_contains RELEASING.md "repeatable CI path" \
  "RELEASING keeps CI as the remaining first-release gate"
check_contains RELEASING.md "CONTRIBUTING.md" \
  "RELEASING includes contributor-facing policy docs in the gate"
check_contains RELEASING.md "SECURITY.md" \
  "RELEASING includes the security policy in the gate"
check_contains RELEASING.md "CODE_OF_CONDUCT.md" \
  "RELEASING includes the conduct policy in the gate"
check_contains RELEASING.md "GOVERNANCE.md" \
  "RELEASING includes the governance policy in the gate"
check_contains RELEASING.md "MAINTAINERS.md" \
  "RELEASING includes the maintainer policy in the gate"
check_contains RELEASING.md "VERSIONING.md" \
  "RELEASING includes the versioning policy in the gate"
check_matching_quick_start_surface \
  "README quick-start API bullets mirror API.md"
check_contains VERSIONING.md "pin the exact commit" \
  "VERSIONING requires commit pinning before formal releases"
check_contains VERSIONING.md "semantic versioning" \
  "VERSIONING documents the intended post-release versioning model"
check_contains GOVERNANCE.md "behavioral claims are expected to be backed by executable tests" \
  "GOVERNANCE requires executable evidence for behavioral claims"
check_contains GOVERNANCE.md "keep the public surface small and intentional" \
  "GOVERNANCE keeps the public surface conservative"
check_contains MAINTAINERS.md "sbcl --script scripts/run-tests.lisp" \
  "MAINTAINERS preserves the raw-checkout verification baseline"
check_contains MAINTAINERS.md "SECURITY.md" \
  "MAINTAINERS points urgent incidents at the security policy"
check_contains ROADMAP.md "nix flake check" \
  "ROADMAP records the reproducible CI verification path"
check_contains CHANGELOG.md "## Unreleased" \
  "CHANGELOG keeps an Unreleased section"

run_step_with_timeout 300 "sbcl verification" \
  sbcl --script "$project_root/scripts/run-tests.lisp"
run_step_with_timeout 180 "compile verification" \
  sbcl --script "$project_root/scripts/run-compile-check.lisp"
run_step_with_timeout 180 "examples verification" \
  sbcl --script "$project_root/scripts/run-examples.lisp"
run_step_with_timeout 180 "implementation smoke" \
  "$project_root/scripts/run-implementation-smoke.sh" "$@"

if [ "$failures" -ne 0 ]; then
  printf 'FAIL release readiness audit (%s failing checks)\n' "$failures" >&2
  exit 1
fi

printf 'PASS release readiness audit\n'
