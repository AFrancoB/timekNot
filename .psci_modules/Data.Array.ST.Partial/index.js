// | Partial functions for working with mutable arrays using the `ST` effect.
// |
// | This module is particularly helpful when performance is very important.
import * as $foreign from "./foreign.js";

// | Change the value at the specified index in a mutable array.
var poke = function () {
    return $foreign.pokeImpl;
};

// | Read the value at the specified index in a mutable array.
var peek = function () {
    return $foreign.peekImpl;
};
export {
    peek,
    poke
};
