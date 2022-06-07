import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Data_Typelevel_Num_Sets from "../Data.Typelevel.Num.Sets/index.js";
import * as Data_Typelevel_Undefined from "../Data.Typelevel.Undefined/index.js";
var typelevelSucc = function (dictPos) {
    return function () {
        return function (dictDivMod10) {
            return function () {
                return function (dictDivMod101) {
                    return {
                        Nat0: dictDivMod10.Nat1,
                        Pos1: function () {
                            return dictPos;
                        }
                    };
                };
            };
        };
    };
};
var trichLtEq = function (dictSucc) {
    return function (dictTrich) {
        return {};
    };
};
var trichLt = function (dictTrich) {
    return {};
};
var trichGtEq = function (dictSucc) {
    return function (dictTrich) {
        return {};
    };
};
var trichGt = function (dictTrich) {
    return {};
};
var trichEq = function (dictTrich) {
    return {};
};
var trichDxxDxx = function (dictPos) {
    return function (dictPos1) {
        return function (dictTrich) {
            return function (dictTrich1) {
                return function () {
                    return {
                        Nat0: dictPos.Nat0,
                        Nat1: dictPos1.Nat0
                    };
                };
            };
        };
    };
};
var trichDxxD9 = function (dictPos) {
    return {
        Nat0: dictPos.Nat0,
        Nat1: function () {
            return Data_Typelevel_Num_Sets.natD9;
        }
    };
};
var trichDxxD8 = function (dictPos) {
    return {
        Nat0: dictPos.Nat0,
        Nat1: function () {
            return Data_Typelevel_Num_Sets.natD8;
        }
    };
};
var trichDxxD7 = function (dictPos) {
    return {
        Nat0: dictPos.Nat0,
        Nat1: function () {
            return Data_Typelevel_Num_Sets.natD7;
        }
    };
};
var trichDxxD6 = function (dictPos) {
    return {
        Nat0: dictPos.Nat0,
        Nat1: function () {
            return Data_Typelevel_Num_Sets.natD6;
        }
    };
};
var trichDxxD5 = function (dictPos) {
    return {
        Nat0: dictPos.Nat0,
        Nat1: function () {
            return Data_Typelevel_Num_Sets.natD5;
        }
    };
};
var trichDxxD4 = function (dictPos) {
    return {
        Nat0: dictPos.Nat0,
        Nat1: function () {
            return Data_Typelevel_Num_Sets.natD4;
        }
    };
};
var trichDxxD3 = function (dictPos) {
    return {
        Nat0: dictPos.Nat0,
        Nat1: function () {
            return Data_Typelevel_Num_Sets.natD3;
        }
    };
};
var trichDxxD2 = function (dictPos) {
    return {
        Nat0: dictPos.Nat0,
        Nat1: function () {
            return Data_Typelevel_Num_Sets.natD2;
        }
    };
};
var trichDxxD1 = function (dictPos) {
    return {
        Nat0: dictPos.Nat0,
        Nat1: function () {
            return Data_Typelevel_Num_Sets.natD1;
        }
    };
};
var trichDxxD0 = function (dictPos) {
    return {
        Nat0: dictPos.Nat0,
        Nat1: function () {
            return Data_Typelevel_Num_Sets.natD0;
        }
    };
};
var trichD9Dxx = function (dictPos) {
    return {
        Nat0: function () {
            return Data_Typelevel_Num_Sets.natD9;
        },
        Nat1: dictPos.Nat0
    };
};
var trichD9D9 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD9;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD9;
    }
};
var trichD9D8 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD9;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD8;
    }
};
var trichD9D7 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD9;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD7;
    }
};
var trichD9D6 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD9;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD6;
    }
};
var trichD9D5 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD9;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD5;
    }
};
var trichD9D4 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD9;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD4;
    }
};
var trichD9D3 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD9;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD3;
    }
};
var trichD9D2 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD9;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD2;
    }
};
var trichD9D1 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD9;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD1;
    }
};
var trichD9D0 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD9;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD0;
    }
};
var trichD8Dxx = function (dictPos) {
    return {
        Nat0: function () {
            return Data_Typelevel_Num_Sets.natD8;
        },
        Nat1: dictPos.Nat0
    };
};
var trichD8D9 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD8;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD9;
    }
};
var trichD8D8 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD8;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD8;
    }
};
var trichD8D7 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD8;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD7;
    }
};
var trichD8D6 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD8;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD6;
    }
};
var trichD8D5 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD8;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD5;
    }
};
var trichD8D4 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD8;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD4;
    }
};
var trichD8D3 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD8;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD3;
    }
};
var trichD8D2 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD8;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD2;
    }
};
var trichD8D1 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD8;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD1;
    }
};
var trichD8D0 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD8;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD0;
    }
};
var trichD7Dxx = function (dictPos) {
    return {
        Nat0: function () {
            return Data_Typelevel_Num_Sets.natD7;
        },
        Nat1: dictPos.Nat0
    };
};
var trichD7D9 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD7;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD9;
    }
};
var trichD7D8 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD7;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD8;
    }
};
var trichD7D7 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD7;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD7;
    }
};
var trichD7D6 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD7;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD6;
    }
};
var trichD7D5 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD7;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD5;
    }
};
var trichD7D4 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD7;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD4;
    }
};
var trichD7D3 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD7;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD3;
    }
};
var trichD7D2 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD7;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD2;
    }
};
var trichD7D1 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD7;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD1;
    }
};
var trichD7D0 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD7;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD0;
    }
};
var trichD6Dxx = function (dictPos) {
    return {
        Nat0: function () {
            return Data_Typelevel_Num_Sets.natD6;
        },
        Nat1: dictPos.Nat0
    };
};
var trichD6D9 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD6;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD9;
    }
};
var trichD6D8 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD6;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD8;
    }
};
var trichD6D7 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD6;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD7;
    }
};
var trichD6D6 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD6;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD6;
    }
};
var trichD6D5 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD6;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD5;
    }
};
var trichD6D4 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD6;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD4;
    }
};
var trichD6D3 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD6;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD3;
    }
};
var trichD6D2 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD6;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD2;
    }
};
var trichD6D1 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD6;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD1;
    }
};
var trichD6D0 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD6;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD0;
    }
};
var trichD5Dxx = function (dictPos) {
    return {
        Nat0: function () {
            return Data_Typelevel_Num_Sets.natD5;
        },
        Nat1: dictPos.Nat0
    };
};
var trichD5D9 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD5;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD9;
    }
};
var trichD5D8 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD5;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD8;
    }
};
var trichD5D7 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD5;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD7;
    }
};
var trichD5D6 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD5;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD6;
    }
};
var trichD5D5 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD5;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD5;
    }
};
var trichD5D4 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD5;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD4;
    }
};
var trichD5D3 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD5;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD3;
    }
};
var trichD5D2 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD5;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD2;
    }
};
var trichD5D1 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD5;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD1;
    }
};
var trichD5D0 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD5;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD0;
    }
};
var trichD4Dxx = function (dictPos) {
    return {
        Nat0: function () {
            return Data_Typelevel_Num_Sets.natD4;
        },
        Nat1: dictPos.Nat0
    };
};
var trichD4D9 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD4;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD9;
    }
};
var trichD4D8 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD4;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD8;
    }
};
var trichD4D7 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD4;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD7;
    }
};
var trichD4D6 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD4;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD6;
    }
};
var trichD4D5 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD4;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD5;
    }
};
var trichD4D4 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD4;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD4;
    }
};
var trichD4D3 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD4;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD3;
    }
};
var trichD4D2 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD4;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD2;
    }
};
var trichD4D1 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD4;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD1;
    }
};
var trichD4D0 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD4;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD0;
    }
};
var trichD3Dxx = function (dictPos) {
    return {
        Nat0: function () {
            return Data_Typelevel_Num_Sets.natD3;
        },
        Nat1: dictPos.Nat0
    };
};
var trichD3D9 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD3;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD9;
    }
};
var trichD3D8 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD3;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD8;
    }
};
var trichD3D7 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD3;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD7;
    }
};
var trichD3D6 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD3;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD6;
    }
};
var trichD3D5 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD3;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD5;
    }
};
var trichD3D4 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD3;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD4;
    }
};
var trichD3D3 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD3;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD3;
    }
};
var trichD3D2 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD3;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD2;
    }
};
var trichD3D1 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD3;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD1;
    }
};
var trichD3D0 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD3;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD0;
    }
};
var trichD2Dxx = function (dictPos) {
    return {
        Nat0: function () {
            return Data_Typelevel_Num_Sets.natD2;
        },
        Nat1: dictPos.Nat0
    };
};
var trichD2D9 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD2;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD9;
    }
};
var trichD2D8 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD2;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD8;
    }
};
var trichD2D7 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD2;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD7;
    }
};
var trichD2D6 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD2;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD6;
    }
};
var trichD2D5 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD2;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD5;
    }
};
var trichD2D4 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD2;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD4;
    }
};
var trichD2D3 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD2;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD3;
    }
};
var trichD2D2 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD2;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD2;
    }
};
var trichD2D1 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD2;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD1;
    }
};
var trichD2D0 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD2;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD0;
    }
};
var trichD1Dxx = function (dictPos) {
    return {
        Nat0: function () {
            return Data_Typelevel_Num_Sets.natD1;
        },
        Nat1: dictPos.Nat0
    };
};
var trichD1D9 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD1;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD9;
    }
};
var trichD1D8 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD1;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD8;
    }
};
var trichD1D7 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD1;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD7;
    }
};
var trichD1D6 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD1;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD6;
    }
};
var trichD1D5 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD1;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD5;
    }
};
var trichD1D4 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD1;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD4;
    }
};
var trichD1D3 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD1;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD3;
    }
};
var trichD1D2 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD1;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD2;
    }
};
var trichD1D1 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD1;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD1;
    }
};
var trichD1D0 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD1;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD0;
    }
};
var trichD0Dxx = function (dictPos) {
    return {
        Nat0: function () {
            return Data_Typelevel_Num_Sets.natD0;
        },
        Nat1: dictPos.Nat0
    };
};
var trichD0D9 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD0;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD9;
    }
};
var trichD0D8 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD0;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD8;
    }
};
var trichD0D7 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD0;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD7;
    }
};
var trichD0D6 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD0;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD6;
    }
};
var trichD0D5 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD0;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD5;
    }
};
var trichD0D4 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD0;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD4;
    }
};
var trichD0D3 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD0;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD3;
    }
};
var trichD0D2 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD0;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD2;
    }
};
var trichD0D1 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD0;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD1;
    }
};
var trichD0D0 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD0;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD0;
    }
};
var trich = function (dictTrich) {
    return function (v) {
        return function (v1) {
            return Data_Typelevel_Undefined["undefined"];
        };
    };
};
var succPxiD9iyD0 = function (dictSucc) {
    return {};
};
var succPxiD8xiD9 = {};
var succPxiD7xiD8 = {};
var succPxiD6xiD7 = {};
var succPxiD5xiD6 = {};
var succPxiD4xiD5 = {};
var succPxiD3xiD4 = {};
var succPxiD2xiD3 = {};
var succPxiD1xiD2 = {};
var succPxiD0xiD1 = {};
var succPred = function (dictSucc) {
    return {
        Pos0: dictSucc.Pos1
    };
};
var succ = function (dictSucc) {
    return function (v) {
        return Data_Typelevel_Undefined["undefined"];
    };
};
var subtractTypeLevelRelation = function (dictAdd) {
    return {};
};
var sub = function () {
    return function (v) {
        return function (v1) {
            return Data_Typelevel_Undefined["undefined"];
        };
    };
};
var showLT = {
    show: function (v) {
        return "LT";
    }
};
var showGT = {
    show: function (v) {
        return "GT";
    }
};
var showEQ = {
    show: function (v) {
        return "EQ";
    }
};
var pred = function (dictPred) {
    return function (v) {
        return Data_Typelevel_Undefined["undefined"];
    };
};
var mulMultidigits = function (dictPos) {
    return function (dictNat) {
        return function (dictMul) {
            return function (dictMul10) {
                return function (dictMul1) {
                    return function (dictAdd) {
                        return {
                            Nat0: dictPos.Nat0,
                            Nat1: dictMul.Nat1
                        };
                    };
                };
            };
        };
    };
};
var mulD9Nat = function (dictAdd) {
    return function (dictMul) {
        return {
            Nat0: function () {
                return Data_Typelevel_Num_Sets.natD9;
            },
            Nat1: (dictAdd.AddP1()).Nat0
        };
    };
};
var mulD8Nat = function (dictAdd) {
    return function (dictMul) {
        return {
            Nat0: function () {
                return Data_Typelevel_Num_Sets.natD8;
            },
            Nat1: (dictAdd.AddP1()).Nat0
        };
    };
};
var mulD7Nat = function (dictAdd) {
    return function (dictMul) {
        return {
            Nat0: function () {
                return Data_Typelevel_Num_Sets.natD7;
            },
            Nat1: (dictAdd.AddP1()).Nat0
        };
    };
};
var mulD6Nat = function (dictAdd) {
    return function (dictMul) {
        return {
            Nat0: function () {
                return Data_Typelevel_Num_Sets.natD6;
            },
            Nat1: (dictAdd.AddP1()).Nat0
        };
    };
};
var mulD5Nat = function (dictAdd) {
    return function (dictMul) {
        return {
            Nat0: function () {
                return Data_Typelevel_Num_Sets.natD5;
            },
            Nat1: (dictAdd.AddP1()).Nat0
        };
    };
};
var mulD4Nat = function (dictAdd) {
    return function (dictMul) {
        return {
            Nat0: function () {
                return Data_Typelevel_Num_Sets.natD4;
            },
            Nat1: (dictAdd.AddP1()).Nat0
        };
    };
};
var mulD3Nat = function (dictAdd) {
    return function (dictMul) {
        return {
            Nat0: function () {
                return Data_Typelevel_Num_Sets.natD3;
            },
            Nat1: (dictAdd.AddP1()).Nat0
        };
    };
};
var mulD2Nat = function (dictAdd) {
    return {
        Nat0: function () {
            return Data_Typelevel_Num_Sets.natD2;
        },
        Nat1: (dictAdd.AddP1()).Nat0
    };
};
var mulD1Nat = function (dictNat) {
    return {
        Nat0: function () {
            return Data_Typelevel_Num_Sets.natD1;
        },
        Nat1: function () {
            return dictNat;
        }
    };
};
var mulD0Nat = function (dictNat) {
    return {
        Nat0: function () {
            return Data_Typelevel_Num_Sets.natD0;
        },
        Nat1: function () {
            return dictNat;
        }
    };
};
var mul10Nat = function (dictDivMod10) {
    return {
        Nat0: dictDivMod10.Nat1
    };
};
var mul10 = function (dictMul10) {
    return function (v) {
        return Data_Typelevel_Undefined["undefined"];
    };
};
var mul = function (dictMul) {
    return function (v) {
        return function (v1) {
            return Data_Typelevel_Undefined["undefined"];
        };
    };
};
var modNatPos = function (dictDivMod) {
    return {};
};
var mod = function () {
    return function (v) {
        return function (v1) {
            return Data_Typelevel_Undefined["undefined"];
        };
    };
};
var minRelation = function () {
    return function (dictTrich) {
        return {};
    };
};
var min = function () {
    return function (v) {
        return function (v1) {
            return Data_Typelevel_Undefined["undefined"];
        };
    };
};
var maxRelation = function () {
    return function (dictTrich) {
        return {};
    };
};
var maxPLT = {};
var maxPGT = {};
var maxPEQ = {};
var max = function () {
    return function (v) {
        return function (v1) {
            return Data_Typelevel_Undefined["undefined"];
        };
    };
};
var lteq = function () {
    return function (v) {
        return function (v1) {
            return Data_Typelevel_Undefined["undefined"];
        };
    };
};
var lt = function () {
    return function (v) {
        return function (v1) {
            return Data_Typelevel_Undefined["undefined"];
        };
    };
};
var isZeroPos = function (dictPos) {
    return {};
};
var isZeroD9 = {};
var isZeroD8 = {};
var isZeroD7 = {};
var isZeroD6 = {};
var isZeroD5 = {};
var isZeroD4 = {};
var isZeroD3 = {};
var isZeroD2 = {};
var isZeroD1 = {};
var isZeroD0 = {};
var isDivByPosNat = function (dictDivMod) {
    return {
        Pos0: dictDivMod.Pos1
    };
};
var isDivBy = function (dictIsDivBy) {
    return function (v) {
        return Data_Typelevel_Undefined["undefined"];
    };
};
var gteq = function () {
    return function (v) {
        return function (v1) {
            return Data_Typelevel_Undefined["undefined"];
        };
    };
};
var gt = function () {
    return function (v) {
        return function (v1) {
            return Data_Typelevel_Undefined["undefined"];
        };
    };
};
var gcdpXYLT = function (dictNat) {
    return function (dictNat1) {
        return function (dictGCD) {
            return {
                Nat0: dictGCD.Nat1,
                Nat1: dictGCD.Nat0,
                Nat2: dictGCD.Nat2
            };
        };
    };
};
var gcdpXYGT = function (dictNat) {
    return function (dictNat1) {
        return function () {
            return function (dictGCD) {
                return {
                    Nat0: function () {
                        return dictNat;
                    },
                    Nat1: dictGCD.Nat1,
                    Nat2: dictGCD.Nat2
                };
            };
        };
    };
};
var gcdpXX = function (dictNat) {
    return {
        Nat0: function () {
            return dictNat;
        },
        Nat1: function () {
            return dictNat;
        },
        Nat2: function () {
            return dictNat;
        }
    };
};
var gcdpD0 = function (dictNat) {
    return {
        Nat0: function () {
            return dictNat;
        },
        Nat1: function () {
            return Data_Typelevel_Num_Sets.natD0;
        },
        Nat2: function () {
            return Data_Typelevel_Num_Sets.natD0;
        }
    };
};
var gcdRelation = function (dictNat) {
    return function (dictNat1) {
        return function (dictTrich) {
            return function () {
                return function (dictGCDP) {
                    return {
                        Nat0: dictGCDP.Nat0,
                        Nat1: dictGCDP.Nat1,
                        Nat2: dictGCDP.Nat2
                    };
                };
            };
        };
    };
};
var gcd = function (dictGCD) {
    return function (v) {
        return function (v1) {
            return Data_Typelevel_Undefined["undefined"];
        };
    };
};
var failurePredOfZeroError = function () {
    return {};
};
var eq = function () {
    return function (v) {
        return function (v1) {
            return Data_Typelevel_Undefined["undefined"];
        };
    };
};
var divNatPos = function (dictDivMod) {
    return {};
};
var divModPQR = function (dictNat) {
    return function (dictPos) {
        return function () {
            return function (dictPred) {
                return function (dictDivMod) {
                    return {
                        Nat0: function () {
                            return dictNat;
                        },
                        Pos1: dictDivMod.Pos1
                    };
                };
            };
        };
    };
};
var divModPD0Nat = function (dictNat) {
    return function (dictPos) {
        return {
            Nat0: function () {
                return dictNat;
            },
            Pos1: function () {
                return dictPos;
            }
        };
    };
};
var divModPD0D1 = function (dictNat) {
    return function (dictPos) {
        return {
            Nat0: function () {
                return dictNat;
            },
            Pos1: function () {
                return dictPos;
            }
        };
    };
};
var divModNatPos = function (dictNat) {
    return function (dictPos) {
        return function (dictTrich) {
            return function (dictDivModP) {
                return {
                    Nat0: dictDivModP.Nat0,
                    Pos1: dictDivModP.Pos1
                };
            };
        };
    };
};
var divModIDontEvenAnymore = function (dictNat) {
    return function (dictNat1) {
        return {
            Nat0: function () {
                return dictNat;
            },
            Nat1: function () {
                return dictNat1;
            }
        };
    };
};
var divMod10D9x = function (dictNat) {
    return {
        Nat0: function () {
            return Data_Typelevel_Num_Sets.natD9;
        },
        Nat1: function () {
            return dictNat;
        }
    };
};
var divMod10D9D0 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD0;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD9;
    }
};
var divMod10D8x = function (dictNat) {
    return {
        Nat0: function () {
            return Data_Typelevel_Num_Sets.natD8;
        },
        Nat1: function () {
            return dictNat;
        }
    };
};
var divMod10D8D0 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD0;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD8;
    }
};
var divMod10D7x = function (dictNat) {
    return {
        Nat0: function () {
            return Data_Typelevel_Num_Sets.natD7;
        },
        Nat1: function () {
            return dictNat;
        }
    };
};
var divMod10D7D0 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD0;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD7;
    }
};
var divMod10D6x = function (dictNat) {
    return {
        Nat0: function () {
            return Data_Typelevel_Num_Sets.natD6;
        },
        Nat1: function () {
            return dictNat;
        }
    };
};
var divMod10D6D0 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD0;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD6;
    }
};
var divMod10D5x = function (dictNat) {
    return {
        Nat0: function () {
            return Data_Typelevel_Num_Sets.natD5;
        },
        Nat1: function () {
            return dictNat;
        }
    };
};
var divMod10D5D0 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD0;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD5;
    }
};
var divMod10D4x = function (dictNat) {
    return {
        Nat0: function () {
            return Data_Typelevel_Num_Sets.natD4;
        },
        Nat1: function () {
            return dictNat;
        }
    };
};
var divMod10D4D0 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD0;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD4;
    }
};
var divMod10D3x = function (dictNat) {
    return {
        Nat0: function () {
            return Data_Typelevel_Num_Sets.natD3;
        },
        Nat1: function () {
            return dictNat;
        }
    };
};
var divMod10D3D0 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD0;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD3;
    }
};
var divMod10D2x = function (dictNat) {
    return {
        Nat0: function () {
            return Data_Typelevel_Num_Sets.natD2;
        },
        Nat1: function () {
            return dictNat;
        }
    };
};
var divMod10D2D0 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD0;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD2;
    }
};
var divMod10D1x = function (dictNat) {
    return {
        Nat0: function () {
            return Data_Typelevel_Num_Sets.natD1;
        },
        Nat1: function () {
            return dictNat;
        }
    };
};
var divMod10D1D0 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD0;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD1;
    }
};
var divMod10D0D0 = {
    Nat0: function () {
        return Data_Typelevel_Num_Sets.natD0;
    },
    Nat1: function () {
        return Data_Typelevel_Num_Sets.natD0;
    }
};
var divMod10 = function (dictDivMod10) {
    return function (v) {
        return new Data_Tuple.Tuple(Data_Typelevel_Undefined["undefined"], Data_Typelevel_Undefined["undefined"]);
    };
};
var divMod = function (dictDivMod) {
    return function (v) {
        return function (v1) {
            return new Data_Tuple.Tuple(Data_Typelevel_Undefined["undefined"], Data_Typelevel_Undefined["undefined"]);
        };
    };
};
var div10Nat = function (dictDivMod10) {
    return {
        Nat0: dictDivMod10.Nat1
    };
};
var div10 = function (dictDiv10) {
    return function (v) {
        return Data_Typelevel_Undefined["undefined"];
    };
};
var div = function () {
    return function (v) {
        return function (v1) {
            return Data_Typelevel_Undefined["undefined"];
        };
    };
};
var csLTrLT = {};
var csGTrGT = {};
var csEQrr = {};
var addTypeLevelRelation = function (dictAddP) {
    return function (dictAddP1) {
        return {
            AddP0: function () {
                return dictAddP;
            },
            AddP1: function () {
                return dictAddP1;
            }
        };
    };
};
var addPMultiDigits = function (dictPos) {
    return function (dictNat) {
        return function (dictAddP) {
            return function (dictDivMod10) {
                return function (dictAddP1) {
                    return {
                        Nat0: dictPos.Nat0
                    };
                };
            };
        };
    };
};
var addPD9ToSucc = function (dictSucc) {
    return function (dictAddP) {
        return {
            Nat0: function () {
                return Data_Typelevel_Num_Sets.natD9;
            }
        };
    };
};
var addPD8ToSucc = function (dictSucc) {
    return function (dictAddP) {
        return {
            Nat0: function () {
                return Data_Typelevel_Num_Sets.natD8;
            }
        };
    };
};
var addPD7ToSucc = function (dictSucc) {
    return function (dictAddP) {
        return {
            Nat0: function () {
                return Data_Typelevel_Num_Sets.natD7;
            }
        };
    };
};
var addPD6ToSucc = function (dictSucc) {
    return function (dictAddP) {
        return {
            Nat0: function () {
                return Data_Typelevel_Num_Sets.natD6;
            }
        };
    };
};
var addPD5ToSucc = function (dictSucc) {
    return function (dictAddP) {
        return {
            Nat0: function () {
                return Data_Typelevel_Num_Sets.natD5;
            }
        };
    };
};
var addPD4ToSucc = function (dictSucc) {
    return function (dictAddP) {
        return {
            Nat0: function () {
                return Data_Typelevel_Num_Sets.natD4;
            }
        };
    };
};
var addPD3ToSucc = function (dictSucc) {
    return function (dictAddP) {
        return {
            Nat0: function () {
                return Data_Typelevel_Num_Sets.natD3;
            }
        };
    };
};
var addPD2ToSucc = function (dictSucc) {
    return function (dictAddP) {
        return {
            Nat0: function () {
                return Data_Typelevel_Num_Sets.natD2;
            }
        };
    };
};
var addPD1ToSucc = function (dictSucc) {
    return {
        Nat0: function () {
            return Data_Typelevel_Num_Sets.natD1;
        }
    };
};
var addPD0ToNat = function (dictNat) {
    return {
        Nat0: function () {
            return Data_Typelevel_Num_Sets.natD0;
        }
    };
};
var add = function (dictAdd) {
    return function (v) {
        return function (v1) {
            return Data_Typelevel_Undefined["undefined"];
        };
    };
};
export {
    succ,
    pred,
    add,
    sub,
    mul,
    divMod,
    div,
    mod,
    mul10,
    divMod10,
    div10,
    isDivBy,
    trich,
    eq,
    gt,
    lt,
    gteq,
    lteq,
    max,
    min,
    gcd,
    typelevelSucc,
    succPred,
    failurePredOfZeroError,
    succPxiD0xiD1,
    succPxiD1xiD2,
    succPxiD2xiD3,
    succPxiD3xiD4,
    succPxiD4xiD5,
    succPxiD5xiD6,
    succPxiD6xiD7,
    succPxiD7xiD8,
    succPxiD8xiD9,
    succPxiD9iyD0,
    addPD0ToNat,
    addPD1ToSucc,
    addPD2ToSucc,
    addPD3ToSucc,
    addPD4ToSucc,
    addPD5ToSucc,
    addPD6ToSucc,
    addPD7ToSucc,
    addPD8ToSucc,
    addPD9ToSucc,
    addPMultiDigits,
    addTypeLevelRelation,
    subtractTypeLevelRelation,
    mulD0Nat,
    mulD1Nat,
    mulD2Nat,
    mulD3Nat,
    mulD4Nat,
    mulD5Nat,
    mulD6Nat,
    mulD7Nat,
    mulD8Nat,
    mulD9Nat,
    mulMultidigits,
    divModNatPos,
    divModPD0Nat,
    divModPD0D1,
    divModPQR,
    divNatPos,
    modNatPos,
    mul10Nat,
    divMod10D0D0,
    divMod10D1D0,
    divMod10D2D0,
    divMod10D3D0,
    divMod10D4D0,
    divMod10D5D0,
    divMod10D6D0,
    divMod10D7D0,
    divMod10D8D0,
    divMod10D9D0,
    divMod10D1x,
    divMod10D2x,
    divMod10D3x,
    divMod10D4x,
    divMod10D5x,
    divMod10D6x,
    divMod10D7x,
    divMod10D8x,
    divMod10D9x,
    divModIDontEvenAnymore,
    div10Nat,
    isDivByPosNat,
    showLT,
    showEQ,
    showGT,
    trichD0D0,
    trichD0D1,
    trichD0D2,
    trichD0D3,
    trichD0D4,
    trichD0D5,
    trichD0D6,
    trichD0D7,
    trichD0D8,
    trichD0D9,
    trichD0Dxx,
    trichDxxD0,
    trichD1D0,
    trichD1D1,
    trichD1D2,
    trichD1D3,
    trichD1D4,
    trichD1D5,
    trichD1D6,
    trichD1D7,
    trichD1D8,
    trichD1D9,
    trichD1Dxx,
    trichDxxD1,
    trichD2D0,
    trichD2D1,
    trichD2D2,
    trichD2D3,
    trichD2D4,
    trichD2D5,
    trichD2D6,
    trichD2D7,
    trichD2D8,
    trichD2D9,
    trichD2Dxx,
    trichDxxD2,
    trichD3D0,
    trichD3D1,
    trichD3D2,
    trichD3D3,
    trichD3D4,
    trichD3D5,
    trichD3D6,
    trichD3D7,
    trichD3D8,
    trichD3D9,
    trichD3Dxx,
    trichDxxD3,
    trichD4D0,
    trichD4D1,
    trichD4D2,
    trichD4D3,
    trichD4D4,
    trichD4D5,
    trichD4D6,
    trichD4D7,
    trichD4D8,
    trichD4D9,
    trichD4Dxx,
    trichDxxD4,
    trichD5D0,
    trichD5D1,
    trichD5D2,
    trichD5D3,
    trichD5D4,
    trichD5D5,
    trichD5D6,
    trichD5D7,
    trichD5D8,
    trichD5D9,
    trichD5Dxx,
    trichDxxD5,
    trichD6D0,
    trichD6D1,
    trichD6D2,
    trichD6D3,
    trichD6D4,
    trichD6D5,
    trichD6D6,
    trichD6D7,
    trichD6D8,
    trichD6D9,
    trichD6Dxx,
    trichDxxD6,
    trichD7D0,
    trichD7D1,
    trichD7D2,
    trichD7D3,
    trichD7D4,
    trichD7D5,
    trichD7D6,
    trichD7D7,
    trichD7D8,
    trichD7D9,
    trichD7Dxx,
    trichDxxD7,
    trichD8D0,
    trichD8D1,
    trichD8D2,
    trichD8D3,
    trichD8D4,
    trichD8D5,
    trichD8D6,
    trichD8D7,
    trichD8D8,
    trichD8D9,
    trichD8Dxx,
    trichDxxD8,
    trichD9D0,
    trichD9D1,
    trichD9D2,
    trichD9D3,
    trichD9D4,
    trichD9D5,
    trichD9D6,
    trichD9D7,
    trichD9D8,
    trichD9D9,
    trichD9Dxx,
    trichDxxD9,
    trichDxxDxx,
    csEQrr,
    csGTrGT,
    csLTrLT,
    trichEq,
    trichGt,
    trichLt,
    trichGtEq,
    trichLtEq,
    maxPLT,
    maxPEQ,
    maxPGT,
    maxRelation,
    minRelation,
    gcdRelation,
    gcdpD0,
    gcdpXYLT,
    gcdpXX,
    gcdpXYGT,
    isZeroD0,
    isZeroD1,
    isZeroD2,
    isZeroD3,
    isZeroD4,
    isZeroD5,
    isZeroD6,
    isZeroD7,
    isZeroD8,
    isZeroD9,
    isZeroPos
};
