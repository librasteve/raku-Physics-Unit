use Physics::Unit::Directory;

class Unit::Dims {
    has $.dx = Directory.instance;

    method load( @a ) {
        for @a -> %h {
            $.dx.types.to-dims{%h.keys} = %h.values;
        }
    }
}