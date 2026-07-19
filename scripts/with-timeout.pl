#!/usr/bin/env perl

use strict;
use warnings;
use POSIX qw(:sys_wait_h);

my ($timeout, @command) = @ARGV;
die "usage: $0 TIMEOUT COMMAND [ARG ...]\n"
  unless defined $timeout && @command;

my $child = fork;
die "fork failed: $!\n" unless defined $child;

if ($child == 0) {
  exec @command or do {
    warn "exec failed for $command[0]: $!\n";
    exit 127;
  };
}

local $SIG{ALRM} = sub {
  kill 'TERM', $child;
  sleep 1;
  kill 'KILL', $child;
  die "timed out after $timeout seconds\n";
};

alarm $timeout;
my $waited = waitpid $child, 0;
alarm 0;

die "waitpid failed\n" if $waited < 0;

if (WIFEXITED($?)) {
  exit WEXITSTATUS($?);
}

if (WIFSIGNALED($?)) {
  my $signal = WTERMSIG($?);
  die "command terminated by signal $signal\n";
}

die "command terminated abnormally\n";
