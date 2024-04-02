use Physics::Unit::Directory;

class Unit::Txxx {
    has $.dx = Directory.instance;

    method load( @a ) {
        for @a -> %h {
            $.dx.types.to-name{%h.keys} = %h.values;
        }
    }
}
