package Item;

use strict;
use warnings;

sub new {
    my ( $class, %attrs ) = @_;
    return bless \%attrs, $class;
}

sub to_string {
    my ($self) = @_;
    return $self->{name} . ', ' . $self->{sell_in} . ', ' . $self->{quality};
}

1;
