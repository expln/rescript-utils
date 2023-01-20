let {exn} = module(Expln_utils_common)

type path = list<string>

type json = Js.Json.t

type jsonAny = 
    | JsonNull(path)
    | JsonBool(bool, path)
    | JsonNum(float, path)
    | JsonStr(string, path)
    | JsonArr(array<Js_json.t>, path)
    | JsonObj(Js_dict.t<Js_json.t>, path)

let rootPath = list{}

let pathToStr = path => {
    switch path {
        | list{} => "/"
        | _ => path->Belt_List.reduceReverse("", (a,e) => a ++ "/" ++ e)
    }
}

let jsonToAny = (json,path):jsonAny => {
    switch json->Js.Json.classify {
        | JSONNull => JsonNull(path)
        | JSONFalse => JsonBool(false,path)
        | JSONTrue => JsonBool(true,path)
        | JSONNumber(num) => JsonNum(num,path)
        | JSONString(str) => JsonStr(str,path)
        | JSONArray(arr) => JsonArr(arr,path)
        | JSONObject(dict) => JsonObj(dict,path)
    }
}
    

let getPath = jsonAny =>
    switch jsonAny {
        | JsonNull(path) | JsonBool(_,path) | JsonNum(_,path) | JsonStr(_,path) | JsonArr(_,path) | JsonObj(_,path) => path
    }

let anyToBool = (jsonAny):result<option<bool>,string> => {
    switch jsonAny {
        | JsonNull(_) => Ok(None)
        | JsonBool(val,_) => Ok(Some(val))
        | _ => Error(`a boolean value was expected at '${jsonAny->getPath->pathToStr}'.`)
    }
}

let anyToNum = (jsonAny):result<option<float>,string> => {
    switch jsonAny {
        | JsonNull(_) => Ok(None)
        | JsonNum(val,_) => Ok(Some(val))
        | _ => Error(`a number value was expected at '${jsonAny->getPath->pathToStr}'.`)
    }
}

let anyToInt = (jsonAny):result<option<int>,string> => {
    anyToNum(jsonAny)->Belt.Result.map(numOpt => numOpt->Belt_Option.map(num => num->Belt_Float.toInt))
}

let anyToStr = (jsonAny):result<option<string>,string> => {
    switch jsonAny {
        | JsonNull(_) => Ok(None)
        | JsonStr(val,_) => Ok(Some(val))
        | _ => Error(`a number value was expected at '${jsonAny->getPath->pathToStr}'.`)
    }
}

let anyToArr = (jsonAny, mapper: jsonAny=>'a):result<option<array<'a>>,string> => {
    switch jsonAny {
        | JsonNull(_) => Ok(None)
        | JsonArr(val,path) => {
            Ok(Some(
                val->Js_array2.mapi((json,i) => mapper(jsonToAny(json,list{i->Belt.Int.toString, ...path})))
            ))
        }
        | _ => Error(`an array was expected at '${jsonAny->getPath->pathToStr}'.`)
    }
}

let getByPath = (obj:jsonAny, attrName: string):result<option<jsonAny>,string> => {
    switch obj {
        | JsonObj(dict,path) => {
            switch dict->Js_dict.get(attrName) {
                | Some(json) => Ok(Some(jsonToAny(json,list{attrName, ...path})))
                | None => Ok(None)
            }
        }
        | _ => Error(`an object was expected at '${obj->getPath->pathToStr}'.`)
    }
}

let asValOpt = (
    jsonAny, 
    anyToVal:jsonAny=>result<option<'v>,string>, 
    // validator:option<'v>=>result<option<'v>,string>,
    // default:unit=>option<'v>
): option<'v> => {
    switch anyToVal(jsonAny) {
        | Ok(valOpt) => valOpt
        | Error(msg) => exn(msg)
    }
}

let makeAsValOpt = (anyToVal:jsonAny=>result<option<'v>,string>):(jsonAny=>option<'v>) => 
    jsonAny => asValOpt(jsonAny, anyToVal)

let asVal = (jsonAny, anyToVal:jsonAny=>result<option<'v>,string>, descrOfExpectedValue:string): 'v => {
    switch asValOpt(jsonAny, anyToVal) {
        | Some(val) => val
        | None => exn(`${descrOfExpectedValue} was expected at '${jsonAny->getPath->pathToStr}'.`)
    }
}

let makeAsVal = (anyToVal:jsonAny=>result<option<'v>,string>, descrOfExpectedValue:string):(jsonAny=>'v) => 
    jsonAny => asVal(jsonAny, anyToVal, descrOfExpectedValue)

let valOpt = (jsonAny:jsonAny, attrName:string, anyToVal:jsonAny=>result<option<'v>,string>): option<'v> => {
    switch getByPath(jsonAny, attrName) {
        | Ok(None) => None
        | Ok(Some(attrVal)) => asValOpt(attrVal, anyToVal)
        | Error(msg) => exn(msg)
    }
}

let makeValOpt = (anyToVal:jsonAny=>result<option<'v>,string>):((jsonAny,string)=>option<'v>) => 
    (jsonAny, attrName) => valOpt(jsonAny, attrName, anyToVal)

let val = (jsonAny, attrName, anyToVal:jsonAny=>result<option<'v>,string>, descrOfExpectedValue:string): 'v => {
    switch valOpt(jsonAny, attrName, anyToVal) {
        | Some(val) => val
        | None => exn(`${descrOfExpectedValue} was expected at '${pathToStr(list{attrName, ...(jsonAny->getPath)})}'.`)
    }
}

let makeVal = (anyToVal:jsonAny=>result<option<'v>,string>, descrOfExpectedValue:string):((jsonAny,string)=>'v) => 
    (jsonAny, attrName) => val(jsonAny, attrName, anyToVal, descrOfExpectedValue)

let asBoolOpt = makeAsValOpt(anyToBool)
let asBool = makeAsVal(anyToBool, "a boolean")
let boolOpt = makeValOpt(anyToBool)
let bool = makeVal(anyToBool, "a boolean")

let asNumOpt = makeAsValOpt(anyToNum)
let asNum = makeAsVal(anyToNum, "a number")
let numOpt = makeValOpt(anyToNum)
let num = makeVal(anyToNum, "a number")

let asIntOpt = makeAsValOpt(anyToInt)
let asInt = makeAsVal(anyToInt, "an integer")
let intOpt = makeValOpt(anyToInt)
let int = makeVal(anyToInt, "an integer")

let asStrOpt = makeAsValOpt(anyToStr)
let asStr = makeAsVal(anyToStr, "a string")
let strOpt = makeValOpt(anyToStr)
let str = makeVal(anyToStr, "a string")

let asArrOpt = (arr:jsonAny, mapper:jsonAny=>'a):option<array<'a>> => asValOpt(arr, anyToArr(_,mapper))
let asArr = (arr:jsonAny, mapper:jsonAny=>'a):array<'a> => asVal(arr, anyToArr(_,mapper), "an array")
let arrOpt = (obj:jsonAny, attrName:string, mapper:jsonAny => 'a):option<array<'a>> => valOpt(obj, attrName, anyToArr(_,mapper))
let arr = (obj:jsonAny, attrName:string, mapper:jsonAny => 'a):array<'a> => val(obj, attrName, anyToArr(_,mapper), "an array")

let objOpt = (obj:jsonAny, attrName:string, mapper:jsonAny => 'a):option<'a> => {
    valOpt(
        obj, 
        attrName, 
        attrVal => Ok(Some(mapper(attrVal)))
    )
}

let obj = (obj:jsonAny, attrName:string, mapper:jsonAny => 'a):'a => {
    val(
        obj, 
        attrName, 
        attrVal => Ok(Some(mapper(attrVal))),
        "an object"
    )
}

let parseObjOpt = (jsonStr:string, mapper:jsonAny=>'a):result<option<'a>,string> => {
    try {
        switch jsonStr -> Js.Json.parseExn -> Js.Json.classify {
            | Js_json.JSONNull => Ok(None)
            | Js_json.JSONObject(dict) => Ok(Some(JsonObj(dict, rootPath) -> mapper))
            | _ => exn(`an object was expected at '/'.`)
        }
    } catch {
        | ex =>
            let msg = ex 
                -> Js.Exn.asJsExn
                -> Belt.Option.flatMap(Js.Exn.message)
                -> Belt.Option.getWithDefault("no message was provided.")
            Error("Parse error: " ++ msg)
    }
}

let parseObjOptWithDefault = (jsonStr:string, mapper:jsonAny=>'a, default:unit=>option<'a>) => {
    switch parseObjOpt(jsonStr, mapper) {
        | Ok(res) => res
        | Error(_) => default()
    }
}

let parseObj = (jsonStr:string, mapper:jsonAny=>'a):result<'a,string> => 
    switch parseObjOpt(jsonStr, mapper) {
        | Ok(Some(obj)) => Ok(obj)
        | Error(str) => Error(str)
        | _ => Error(`Parse error: an object was expected at '/'.`)
    }

let parseObjWithDefault = (jsonStr:string, mapper:jsonAny=>'a, default:unit=>'a) => {
    switch parseObj(jsonStr, mapper) {
        | Ok(res) => res
        | Error(_) => default()
    }
}

let test_pathToStr = (path:list<string>):string => pathToStr(path)