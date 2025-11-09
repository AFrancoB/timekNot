"use strict";

export const objectWithWhenSN = when => s => n => () => { return { whenPosix: when, s: s, n: n }; }

export const addGain = o => gain => () => { o.gain = gain; return o; }

export const addPan = o => pan => () => { o.pan = pan; return o; }

export const addSpeed = o => speed => () => { o.speed = speed; return o; }

export const addBegin = o => begin => () => { o.begin = begin; return o; }

export const addEnd = o => end => () => { o.end = end; return o; }

export const addVowel = o => vowel => () => { o.vowel = vowel; return o; }

export const addCutOff = o => cutoff => () => { o.cutoff = cutoff; return o; }

export const addCutOffH = o => cutoffh => () => { o.hcutoff = cutoffh; return o; }

export const addMaxW = o => maxw => () => { o.maxw = maxw; return o; }

export const addMinW = o => minw => () => { o.minw = minw; return o; }

export const addInter = o => inter => () => { o.inter = inter; return o; }

export const addLegato = o => legato => () => { o.legato = legato; return o; }

export const addOrbit = o => orbit => () => { o.orbit = orbit; return o; }

export const addNote = o => note => () => { o.note = note; return o; }

// export const addVal =  valId => val => o => () => { o[valId] = val; return o; };

export const addVal = o => valId => val => () => { o[valId] = val; return o; };

// export const pianola = canvasId => obj => () => {  
//     const canvas = document.getElementById(canvasId);
//     const ctx = canvas.getContext("2d");
//     ctx.font = "bold italic 10px Arial";
//     let previousTime = 0;
//     let position = 0;
    

//     function step(currentTime) {
        
//         const delta = currentTime - previousTime;
//         previousTime = currentTime;
//         position += delta * 0.01; // Move at 0.1px per ms
//         ctx.clearRect(0, 0, canvas.width, canvas.height);
//         ctx.strokeText(obj.s,  obj.note*2,  position);
        

//         if (position < 300) {
//             requestAnimationFrame(step);
//         }
//         }
// requestAnimationFrame(step);

// };

