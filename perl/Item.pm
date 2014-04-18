package Item;

use strict;
use warnings;

sub new {
    my ( $class, %attrs ) = @_;
    return bless \%attrs, $class;
}

sub _data_printer {    ## no critic (ProhibitUnusedPrivateSubroutines)
    my ( $self, $properties ) = @_;
    return $self->{name} . ', ' . $self->{sell_in} . ', ' . $self->{quality};
}

1;
