import * as T from "./index.js";

window.timekNot = {};

window.timekNot.launch = function() {
    return T.launch();
}

window.timekNot.evaluate = function(timekNot,txt) {
    return T.evaluate(timekNot)(txt)();
}

window.timekNot.setTempo = function(timekNot,foreignTempo) {
    T.setTempo(timekNot,foreignTempo);
}

window.timekNot.scheduleNoteEvents = function(timekNot,wStartPosix,wEndPosix) {
    return T.scheduleNoteEvents(timekNot)(wStartPosix)(wEndPosix)();
}
