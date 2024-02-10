class Physics::Unit::Loader is export {
    has $.session;
    has @.loadees = 'en_SI';
    has @.loaders;
    has @.requests;

    submethod TWEAK {
        say 'loading';
        dd $!session.dictionary;
        # get loaders from file system
        # get requests from .new method
        # populate loadees or error
    }
}
