let exn = str => Js.Exn.raiseError(str)

@new external createArray: int => array<'a> = "Array"
let clearArray = %raw(`arr => arr.length = 0`)
let arrFlatMap = (arr,func) => arr -> Belt.Array.map(func)->Belt.Array.concatMany
let arrStrDistinct = arr => arr->Belt_Set.String.fromArray->Belt_Set.String.toArray
let arrIntDistinct = arr => arr->Belt_Set.Int.fromArray->Belt_Set.Int.toArray
let arrForEach = (arr: array<'a>, consumer: 'a => option<'b>):option<'b> => {
    let len = arr->Js_array2.length
    let i = ref(0)
    let res = ref(None)
    while (i.contents < len && res.contents->Belt_Option.isNone) {
        res.contents = consumer(arr[i.contents])
        i.contents = i.contents + 1
    }
    res.contents
}

let copySubArray = (~src:array<'t>, ~srcFromIdx:int, ~dst:array<'t>, ~dstFromIdx:int, ~len:int): unit => {
    let s = ref(srcFromIdx)
    let d = ref(dstFromIdx)
    let srcLen = src->Js_array2.length
    let dstLen = dst->Js_array2.length
    let sMax = Js_math.min_int(srcLen - 1, srcFromIdx + len - 1)
    while (s.contents <= sMax && d.contents < dstLen) {
        dst[d.contents] = src[s.contents]
        d.contents = d.contents + 1
        s.contents = s.contents + 1
    }
}

let toIntCmp: (('a,'a)=>float) => (('a,'a)=>int) = cmp => (a,b) => cmp(a,b)
    ->Js_math.sign_float
    ->Js_math.floor_int
let intCmp = (a:int, b:int) => if a < b {-1} else if a == b {0} else {1}
let floatCmp = (a:float ,b:float) => if a < b {-1} else if a == b {0} else {1}
let strCmp = Js.String2.localeCompare->toIntCmp
let strCmpI = (s1,s2) => strCmp(s1->Js_string2.toLocaleUpperCase ,s2->Js_string2.toLocaleUpperCase)
let cmpRev = cmp => (a,b) => -cmp(a,b)

let stringify: 'a => string = a => switch Js.Json.stringifyAny(a) {
    | Some(str) => str
    | None => exn(`Could not stringify '${Js.String2.make(a)}'`)
}

type explnUtilsException = {
    msg:string,
}
exception ExplnUtilsException(explnUtilsException)