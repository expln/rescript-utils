type jsonAny

let asBoolOpt: jsonAny => option<bool>
let asBool: jsonAny => bool
let boolOpt: (jsonAny, string) => option<bool>
let bool: (jsonAny, string) => bool

// let boolOptWithDefault: (jsonAny, string, unit=>option<bool>) => option<bool>
// let asBoolOptWithDefault: (jsonAny, unit=>option<bool>) => option<bool>
// let boolWithDefault: (jsonAny, string, unit=>bool) => bool
// let asBoolWithDefault: (jsonAny, unit=>bool) => bool

let asNumOpt: (jsonAny) => option<float>
let asNum: (jsonAny) => float
let numOpt: (jsonAny, string) => option<float>
let num: (jsonAny, string) => float

let asIntOpt: (jsonAny) => option<int>
let asInt: (jsonAny) => int
let intOpt: (jsonAny, string) => option<int>
let int: (jsonAny, string) => int

let asStrOpt: (jsonAny) => option<string>
let asStr: (jsonAny) => string
let strOpt: (jsonAny, string) => option<string>
let str: (jsonAny, string) => string

let asArrOpt: (jsonAny, jsonAny => 'a) => option<array<'a>>
let asArr: (jsonAny, jsonAny => 'a) => array<'a>
let arrOpt: (jsonAny, string, jsonAny => 'a) => option<array<'a>>
let arr: (jsonAny, string, jsonAny => 'a) => array<'a>

let objOpt: (jsonAny, string, jsonAny => 'a) => option<'a>
let obj: (jsonAny, string, jsonAny => 'a) => 'a

let parseObjOpt: (string, jsonAny=>'a) => result<option<'a>,string>
let parseObjOptWithDefault: (string, jsonAny=>'a, unit=>option<'a>) => option<'a>
let parseObj: (string, jsonAny=>'a) => result<'a,string>
let parseObjWithDefault: (string, jsonAny=>'a, unit=>'a) => 'a


// TEST ----------------------------------------------------------------------------------
let test_pathToStr: list<string> => string