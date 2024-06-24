package GildedRose;

use strict;
use warnings;

sub new {
    my ( $class, %attrs ) = @_;
    return bless \%attrs, $class;
}

sub update_quality {
    my $self = shift;
    for my $item ( @{ $self->{items} } ) {
        if (   $item->{name} ne 'Aged Brie'
            && $item->{name} ne 'Backstage passes to a TAFKAL80ETC concert' )
        {
            if ( $item->{quality} > 0 ) {
                if ( $item->{name} ne 'Sulfuras, Hand of Ragnaros' ) {
                    $item->{quality} = $item->{quality} - 1;
                }
            }
        }
        else {
            if ( $item->{quality} < 50 ) {
                $item->{quality} = $item->{quality} + 1;

                if ( $item->{name} eq
                    'Backstage passes to a TAFKAL80ETC concert' )
                {
                    if ( $item->{sell_in} < 11 ) {
                        if ( $item->{quality} < 50 ) {
                            $item->{quality} = $item->{quality} + 1;
                        }
                    }

                    if ( $item->{sell_in} < 6 ) {
                        if ( $item->{quality} < 50 ) {
                            $item->{quality} = $item->{quality} + 1;
                        }
                    }
                }
            }
        }

        if ( $item->{name} ne 'Sulfuras, Hand of Ragnaros' ) {
            $item->{sell_in} = $item->{sell_in} - 1;
        }

        if ( $item->{sell_in} < 0 ) {
            if ( $item->{name} ne 'Aged Brie' ) {
                if ( $item->{name} ne
                    'Backstage passes to a TAFKAL80ETC concert' )
                {
                    if ( $item->{quality} > 0 ) {
                        if ( $item->{name} ne 'Sulfuras, Hand of Ragnaros' ) {
                            $item->{quality} = $item->{quality} - 1;
                        }
                    }
                }
                else {
                    $item->{quality} = $item->{quality} - $item->{quality};
                }
            }
            else {
                if ( $item->{quality} < 50 ) {
                    $item->{quality} = $item->{quality} + 1;
                }
            }
        }
    }
    return;
}

1;
