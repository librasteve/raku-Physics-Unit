use Physics::Unit::Directory;

class Unit::Types {
    has $.dx = Directory.instance;

    method load( @a ) {
        for @a -> %h {
            $.dx.type.to-name{%h.keys} = %h.values;
        }
    }
}
