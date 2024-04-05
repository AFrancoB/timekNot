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

export const addNote = o => note => () => { o.note = note; return o; }