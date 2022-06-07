import * as WebDirt from "../WebDirt/index.js";
var makeSound = function (wd) {
    return WebDirt.playSample(wd)({
        s: "808Kicks",
        n: "0"
    });
};
var launch = function __do() {
    var wd = WebDirt.newWebDirt({
        sampleMapUrl: "./src/samples/sampleMap.json",
        sampleFolder: "./src/samples"
    })();
    WebDirt.initializeWebAudio(wd)();
    return wd;
};
export {
    launch,
    makeSound
};
