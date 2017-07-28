use v6;

class Item {
    has Str $.name;
    has Int $.sell_in is rw = 0;
    has Int $.quality is rw = 0;

    method Str {
	"{$!name}, {$!sell_in}, {$!quality}"
    }
};
