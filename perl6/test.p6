#!/usr/bin/env perl6

use v6;
use Test;
use lib 'lib';

use GildedRose;
use Item;

my $item = Item.new(:name<foo>);
my $app = GildedRose.new(items => ($item));

$app.update_quality();
is $app.items[0].name, 'fixme', "first day pass";

done-testing;
