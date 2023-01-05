open Expln_utils_common

type point = {x: float, y: float}
type vector = {begin: point, end: point}
type angle = float
type boundaries = {minX: float, minY: float, maxX: float, maxY: float}

let ex = {begin:{x:0., y:0.}, end:{x:1., y:0.}}
let ey = {begin:{x:0., y:0.}, end:{x:0., y:-1.}}

let deg: float => angle = d => d /. 180. *. Js.Math._PI
let rad: float => angle = r => r
let toDeg: angle => float = a => a /. Js.Math._PI *. 180.
let toRad: angle => float = a => a

let pntX: point => float = p => p.x
let pntY: point => float = p => p.y
let pntLen: point => float = p => Js.Math.sqrt(p.x *. p.x +. p.y *. p.y)
let pntSub: (point,point) => point = (a,b) => {x: a.x -. b.x, y: a.y -. b.y}
let pntAdd: (point,point) => point = (a,b) => {x: a.x +. b.x, y: a.y +. b.y}
let pntTrDelta: (point,float,float) => point = (p,dx,dy) => {x: p.x +. dx, y: p.y +. dy}
let pntTr: (point, vector) => point = (p,v) => p->pntTrDelta(v.end.x -. v.begin.x, v.end.y -. v.begin.y)
let pntMult: (point, float) => point = (p, x) => {x: p.x *. x, y: p.y *. x}
let pntDiv: (point, float) => point = (p, x) => {x: p.x /. x, y: p.y /. x}
let pntVec: (point,point) => vector = (b,e) => {begin:b, end:e}
let pntRot: (point, angle) => point = (p,a) => {
    x: p.x *. Js.Math.cos(-.a) -. p.y *. Js.Math.sin(-.a),
    y: p.x *. Js.Math.sin(-.a) +. p.y *. Js.Math.cos(-.a),
}


let vecBegin: vector => point = v => v.begin
let vecEnd: vector => point = v => v.end
let vecLen: vector => float = v => v.end -> pntSub(v.begin) -> pntLen
let vecMult: (vector, float) => vector = (v,x) => {begin: v.begin -> pntMult(x), end: v.end -> pntMult(x)}
let vecMultVec: (vector, vector) => float = (v1, v2) => {
    let a = v1.end -> pntSub(v1.begin)
    let b = v2.end -> pntSub(v2.begin)
    a.x *. b.x +. a.y *. b.y
}
let vecDiv: (vector, float) => vector = (v,x) => {begin: v.begin -> pntDiv(x), end: v.end -> pntDiv(x)}
let vecAdd: (vector, vector) => vector = (a,b) => {begin: a.begin -> pntAdd(b.begin), end: a.end -> pntAdd(b.end)}
let vecRot: (vector, angle) => vector = (v,a) => {
    begin: v.begin,
    end: v.begin -> pntAdd(v.end -> pntSub(v.begin) -> pntRot(a))
}
let vecNorm: vector => vector = v => v -> vecDiv(v -> vecLen)
let vecSwapEnds: vector => vector = v => {begin: v.end, end:v.begin}
let vecBeginAt: (vector, point) => vector = (v,p) => {begin: p, end: p -> pntTr(v)}
let vecEndAt: (vector, point) => vector = (v,p) => v -> vecSwapEnds -> vecBeginAt(p) -> vecSwapEnds
let vecTrDelta: (vector, float, float) => vector = (v,dx,dy) => {
    begin: v.begin->pntTrDelta(dx,dy),
    end: v.end->pntTrDelta(dx,dy)
}
let vecTr: (vector, vector) => vector = (v,t) => {
    let dx = t.end.x -. t.begin.x
    let dy = t.end.y -. t.begin.y
    v -> vecTrDelta(dx,dy)
}
let vecTrDir: (vector, vector, float) => vector = (v,dir,x) => v -> vecTr(dir -> vecNorm -> vecMult(x))

let pntTrDir: (point, vector, float) => point = (p, dir, dist) => p -> pntTr(dir -> vecNorm -> vecMult(dist))
let vecRev: vector => vector = vecRot(_, deg(180.))

let bndFromPoints: array<point> => boundaries = ps => {
    if (ps->Js.Array2.length == 0) {
        exn("Cannot create boudaries from an empty array of points.")
    }
    let minX = ref(ps[0].x)
    let minY = ref(ps[0].y)
    let maxX = ref(minX.contents)
    let maxY = ref(minY.contents)
    for i in 1 to ps->Js.Array2.length - 1 {
        let p = ps[i]
        minX := Js.Math.min_float(minX.contents, p.x)
        minY := Js.Math.min_float(minY.contents, p.y)
        maxX := Js.Math.max_float(maxX.contents, p.x)
        maxY := Js.Math.max_float(maxY.contents, p.y)
    }
    {minX:minX.contents, minY:minY.contents, maxX:maxX.contents, maxY:maxY.contents}
}
let bndAddPoint: (boundaries,point) => boundaries = (b,p) => {
    minX:Js.Math.min_float(b.minX,p.x),
    minY:Js.Math.min_float(b.minY,p.y),
    maxX:Js.Math.max_float(b.maxX,p.x),
    maxY:Js.Math.max_float(b.maxY,p.y),
}
let bndMerge: (boundaries,boundaries) => boundaries = (b1,b2) => {
    minX:Js.Math.min_float(b1.minX,b2.minX),
    minY:Js.Math.min_float(b1.minY,b2.minY),
    maxX:Js.Math.max_float(b1.maxX,b2.maxX),
    maxY:Js.Math.max_float(b1.maxY,b2.maxY),
}
let bndAddPoints: (boundaries,array<point>) => boundaries = (b,ps) => {
    if (ps->Js.Array2.length == 0) {
        b
    } else {
        b->bndMerge(bndFromPoints(ps))
    }
}
let bndMergeAll: array<boundaries> => boundaries = bs => {
    if (bs->Js.Array2.length == 0) {
        exn("Cannot merge empty array of boundaries.")
    } else {
        let b = ref(bs[0])
        for i in 1 to bs->Js.Array2.length - 1 {
            b := b.contents->bndMerge(bs[i])
        }
        b.contents
    }
}
let bndMinX: boundaries => float = b => b.minX
let bndMinY: boundaries => float = b => b.minY
let bndMaxX: boundaries => float = b => b.maxX
let bndMaxY: boundaries => float = b => b.maxY
let bndIncludes: (boundaries, point) => bool = (b,p) =>
    b.minX <= p.x && p.x < b.maxX && b.minY <= p.y && p.y < b.maxY
let bndWidth: boundaries => float = b => b.maxX -. b.minX
let bndHeight: boundaries => float = b => b.maxY -. b.minY