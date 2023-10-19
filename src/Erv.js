import * as ERV from 'erv';

const ratioToCents = ERV.utils.ratioToCents;

// export const _ratioToCents = ratioToCents;

export const _ratioToCents = x => { console.log("_ratioToCents" + str(x)); return ERV.utils.ratioToCents(x); }