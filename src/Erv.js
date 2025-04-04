import * as Erv from "@diegovdc/erv";

export const ratioToCents = Erv.default.utils.ratioToCents;

export const makeCPSScale = (size) => (factor) => Erv.default.cps.make(size,factor).scale;

export const makeMOSScale = (period) => (generator) => Erv.default.mos.make(period,generator).scale;
