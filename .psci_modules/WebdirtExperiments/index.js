import * as WebDirt from "../WebDirt/index.js";
var makeSound = function (wd) {
    return WebDirt.playSample(wd)({
        s: "gtr",
        n: "0"
    });
};
var launch = function __do() {
    var wd = WebDirt.newWebDirt({
        sampleMapUrl: "samples/sampleMap.json",
        sampleFolder: "samples"
    })();
    WebDirt.initializeWebAudio(wd)();
    return wd;
};
export {
    launch,
    makeSound
};
