import * as Erv from "@diegovdc/erv";

export const ratioToCents = Erv.default.utils.ratioToCents;

export const makeCPSScale = (size) => (factor) => Erv.default.cps.make(size,factor).scale;

