type jsonAny

let parseObjOpt: (string, jsonAny=>'a) => result<option<'a>,string>
let parseObj: (string, jsonAny=>'a) => result<'a,string>

let objOpt: (jsonAny, string, jsonAny => 'a) => option<'a>
let obj: (jsonAny, string, jsonAny => 'a) => 'a

let arrOpt: (jsonAny, string, jsonAny => 'a) => option<array<'a>>
let arr: (jsonAny, string, jsonAny => 'a) => array<'a>

let strOpt: (jsonAny, string) => option<string>
let asStrOpt: (jsonAny) => option<string>
let str: (jsonAny, string) => string
let asStr: (jsonAny) => string

let numOpt: (jsonAny, string) => option<float>
let asNumOpt: (jsonAny) => option<float>
let num: (jsonAny, string) => float
let asNum: (jsonAny) => float

let intOpt: (jsonAny, string) => option<int>
let asIntOpt: (jsonAny) => option<int>
let int: (jsonAny, string) => int
let asInt: (jsonAny) => int

let boolOpt: (jsonAny, string) => option<bool>
let asBoolOpt: (jsonAny) => option<bool>
let bool: (jsonAny, string) => bool
let asBool: (jsonAny) => bool

let runTests___: unit => unit