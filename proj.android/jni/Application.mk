APP_STL := gnustl_static
APP_CPPFLAGS := -frtti
APP_MODULES = cocoscheme hellocpp_shared
# chicken dependencies (will not be included in build if not put here)
APP_MODULES += chicken.import files.import foreign.import extras.import \
               lolevel.import data-structures.import\
               srfi-1.import srfi-4.import srfi-69.import srfi-13.import \
               coops coops.import \
               cplusplus-object cplusplus-object.import \
               bind bind.import \
               matchable matchable.import \
               record-variants record-variants.import \
               chickmunk chickmunk.import
