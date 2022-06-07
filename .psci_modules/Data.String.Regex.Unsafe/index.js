import * as Control_Category from "../Control.Category/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_String_Regex from "../Data.String.Regex/index.js";
import * as Partial_Unsafe from "../Partial.Unsafe/index.js";

// | Constructs a `Regex` from a pattern string and flags. Fails with
// | an exception if the pattern contains a syntax error.
var unsafeRegex = function (s) {
    return function (f) {
        return Data_Either.either(Partial_Unsafe.unsafeCrashWith)(Control_Category.identity(Control_Category.categoryFn))(Data_String_Regex.regex(s)(f));
    };
};
export {
    unsafeRegex
};
