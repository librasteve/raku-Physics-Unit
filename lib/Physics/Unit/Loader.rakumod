use v6.d;

class Physics::Unit::Loader {
    has @.loadees = 'en_SI';
    has @.loaders;
    has @.requests;


    submethod TWEAK {
        # get loaders from file system
        # get requests from .new method
        # populate loadees or error
    }
}
