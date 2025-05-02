import * as Erv from "@diegovdc/erv";

export const ratioToCents = (num) => Erv.default.utils.ratioToCents(num);

export const ratiosToScale = (ratios) => Erv.default.utils.ratiosToScale(ratios);

export const makeCPSScale = (size) => (factor) => Erv.default.cps.make(size,factor).scale;

export const makeMOSScale = (period) => (generator) => Erv.default.mos.make(period,generator).scale;

export const degToFreq = (scale) => (rootFreq) => (degree) => Erv.default.scale.degToFreq(scale,rootFreq,degree); 
