open Expln_utils_common

type point = {x: float, y: float}
type vector = {begin: point, end: point}
type angle = float
type boundaries = {minX: float, minY: float, maxX: float, maxY: float}

let ex = {begin:{x:0., y:0.}, end:{x:1., y:0.}}
let ey = {begin:{x:0., y:0.}, end:{x:0., y:1.}}

let deg = (d:float):angle => d /. 180. *. Js.Math._PI
let rad = (r:float):angle => r
let toDeg = (a:angle):float => a /. Js.Math._PI *. 180.
let toRad = (a:angle):float => a

let pntX = (p:point):float => p.x
let pntY = (p:point):float => p.y
let pntLen = (p:point):float => Js.Math.sqrt(p.x *. p.x +. p.y *. p.y)
let pntSub = (a:point,b:point):point => {x: a.x -. b.x, y: a.y -. b.y}
let pntAdd = (a:point,b:point):point => {x: a.x +. b.x, y: a.y +. b.y}
let pntTrDelta = (p:point,dx:float,dy:float):point => {x: p.x +. dx, y: p.y +. dy}
let pntTr = (p:point, v:vector):point => p->pntTrDelta(v.end.x -. v.begin.x, v.end.y -. v.begin.y)
let pntMult = (p:point, x:float):point => {x: p.x *. x, y: p.y *. x}
let pntDiv = (p:point, x:float):point => {x: p.x /. x, y: p.y /. x}
let pntVec = (b:point,e:point):vector => {begin:b, end:e}
let pntRot = (p:point, a:angle):point => {
    x: p.x *. Js.Math.cos(a) -. p.y *. Js.Math.sin(a),
    y: p.x *. Js.Math.sin(a) +. p.y *. Js.Math.cos(a),
}


let vecBegin = (v:vector):point => v.begin
let vecEnd = (v:vector):point => v.end
let vecLen = (v:vector):float => v.end->pntSub(v.begin)->pntLen
let vecMult = (v:vector, x:float):vector => {begin: v.begin, end: v.end->pntSub(v.begin)->pntMult(x)->pntAdd(v.begin)}
let vecMultVec = (v1:vector, v2:vector):float => {
    let a = v1.end->pntSub(v1.begin)
    let b = v2.end->pntSub(v2.begin)
    a.x *. b.x +. a.y *. b.y
}
let vecDiv = (v:vector, x:float):vector => {begin: v.begin, end: v.end->pntSub(v.begin)->pntDiv(x)->pntAdd(v.begin)}
let vecAdd = (a:vector, b:vector):vector => {begin: a.begin, end: a.end->pntAdd(b.end->pntSub(b.begin))}
let vecRot = (v:vector, a:angle):vector => {
    begin: v.begin,
    end: v.end->pntSub(v.begin)->pntRot(a)->pntAdd(v.begin)
}
let vecNorm = (v:vector):vector => v->vecDiv(v->vecLen)
let vecSwapEnds = (v:vector):vector => {begin: v.end, end:v.begin}
let vecBeginAt = (v:vector, p:point):vector => {begin: p, end: p -> pntTr(v)}
let vecEndAt = (v:vector, p:point):vector => v->vecSwapEnds->vecBeginAt(p)->vecSwapEnds
let vecTrDelta = (v:vector, dx:float, dy:float):vector => {
    begin: v.begin->pntTrDelta(dx,dy),
    end: v.end->pntTrDelta(dx,dy)
}
let vecTr = (v:vector, t:vector):vector => {
    let dx = t.end.x -. t.begin.x
    let dy = t.end.y -. t.begin.y
    v->vecTrDelta(dx,dy)
}
let vecTrDir = (v:vector, dir:vector, dist:float):vector => v->vecTr(dir->vecNorm->vecMult(dist))
let pntTrDir = (p:point, dir:vector, dist:float):point => p->pntTr(dir->vecNorm->vecMult(dist))
let vecRev = (v:vector):vector => v->vecRot(rad(Js_math._PI))

let bndMinX = (b:boundaries):float => b.minX
let bndMinY = (b:boundaries):float => b.minY
let bndMaxX = (b:boundaries):float => b.maxX
let bndMaxY = (b:boundaries):float => b.maxY
let bndIncludes = (b:boundaries, p:point):bool => b.minX <= p.x && p.x < b.maxX && b.minY <= p.y && p.y < b.maxY
let bndWidth = (b:boundaries):float => b.maxX -. b.minX
let bndHeight = (b:boundaries):float => b.maxY -. b.minY
let bndFromPoints = (ps:array<point>):boundaries => {
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
let bndFromVectors = (vs:array<vector>): boundaries => {
    if (vs->Js.Array2.length == 0) {
        exn("Cannot create boudaries from an empty array of vectors.")
    }
    let p1 = vs[0]->vecBegin
    let p2 = vs[0]->vecEnd
    let minX = ref(Js.Math.min_float(p1.x, p2.x))
    let minY = ref(Js.Math.min_float(p1.y, p2.y))
    let maxX = ref(Js.Math.max_float(p1.x, p2.x))
    let maxY = ref(Js.Math.max_float(p1.y, p2.y))
    for i in 1 to vs->Js.Array2.length - 1 {
        let p1 = vs[i]->vecBegin
        let p2 = vs[i]->vecEnd
        minX := Js.Math.min_float(minX.contents, Js.Math.min_float(p1.x, p2.x))
        minY := Js.Math.min_float(minY.contents, Js.Math.min_float(p1.y, p2.y))
        maxX := Js.Math.max_float(maxX.contents, Js.Math.max_float(p1.x, p2.x))
        maxY := Js.Math.max_float(maxY.contents, Js.Math.max_float(p1.y, p2.y))
    }
    {minX:minX.contents, minY:minY.contents, maxX:maxX.contents, maxY:maxY.contents}
}
let bndAddPoint = (b:boundaries, p:point):boundaries => {
    minX:Js.Math.min_float(b.minX,p.x),
    minY:Js.Math.min_float(b.minY,p.y),
    maxX:Js.Math.max_float(b.maxX,p.x),
    maxY:Js.Math.max_float(b.maxY,p.y),
}
let bndMerge = (b1:boundaries, b2:boundaries):boundaries => {
    minX:Js.Math.min_float(b1.minX,b2.minX),
    minY:Js.Math.min_float(b1.minY,b2.minY),
    maxX:Js.Math.max_float(b1.maxX,b2.maxX),
    maxY:Js.Math.max_float(b1.maxY,b2.maxY),
}
let bndAddPoints = (b:boundaries, ps:array<point>):boundaries => {
    if (ps->Js.Array2.length == 0) {
        b
    } else {
        b->bndMerge(bndFromPoints(ps))
    }
}
let bndAddMarginPct = (b:boundaries,pct:float):boundaries => {
    let size = Js_math.max_float(b->bndWidth, b->bndHeight)
    let margin = size *. pct
    {minX: b.minX -. margin, minY: b.minY -. margin, maxX: b.maxX +. margin, maxY: b.maxY +. margin}
}
let bndMergeAll = (bs:array<boundaries>):boundaries => {
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
