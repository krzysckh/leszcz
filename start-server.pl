#!/usr/bin/perl

use strict;
use warnings;

use Privileges::Drop;

drop_privileges('nobody');

while (1) {
  if (fork == 0) {
    exec("/usr/bin/ol", "-r", "server/leszcz-server.scm");
  } else {
    wait
  }
}
