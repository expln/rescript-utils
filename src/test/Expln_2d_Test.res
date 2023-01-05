let {describe, it, assertEq, assertEqNum} = module(Expln_test)
open Expln_2d

type pnt = {x: float, y: float}
let mkp = p => ex->vecMult(p.x)->vecAdd(ey->vecMult(p.y))->vecEnd
type vec = {begin:pnt, end:pnt}
let mkv = v => pntVec(v.begin->mkp, v.end->mkp)

let precision = 0.000001

let assertEqNum = (a, b) => assertEqNum(a, b, precision)

let assertEqPnt = (p1:point,p2:point) => {
    assertEqNum(p1->pntX, p2->pntX)
    assertEqNum(p1->pntY, p2->pntY)
}

let assertEqVec = (v1:vector, v2:vector) => {
    assertEqPnt(v1->vecBegin, v2->vecBegin)
    assertEqPnt(v1->vecEnd, v2->vecEnd)
}

describe("utility test functions", _ => {
    it("work", _ => {
        let p = {x:39.62, y:-71.03}->mkp
        assertEqNum(p->pntX, 39.62)
        assertEqNum(p->pntY, 71.03)
        let v = {begin:{x:-7., y: 11.}, end:{x:23., y: -1.}}->mkv
        assertEqNum(v->vecBegin->pntX, -7.)
        assertEqNum(v->vecBegin->pntY, -11.)
        assertEqNum(v->vecEnd->pntX, 23.)
        assertEqNum(v->vecEnd->pntY, 1.)
    })
})

describe("Expln_2d", _ => {
    it("test all", _ => {
        //let ex: vector
        //let ey: vector
        assertEqVec(ex->vecRot(rad(Js.Math._PI /. 2.)), ey)

        //let deg: float => angle
        //let rad: float => angle
        //let toDeg: angle => float
        //let toRad: angle => float
        assertEqNum(deg(45.) -> toRad, 0.785398)
        assertEqNum(rad(2.14675) -> toDeg, 122.9997147)
        
        //let pntLen: point => float
        assertEqNum({x:3., y:4.}->mkp -> pntLen, 5.)

        //let pntSub: (point,point) => point
        assertEqPnt({x:1., y:7.}->mkp->pntSub({x:6.,y:-2.}->mkp), {x:-5., y:9.}->mkp)

        //let pntAdd: (point,point) => point
        assertEqPnt({x:1., y:7.}->mkp->pntAdd({x:6.,y:-2.}->mkp), {x:7., y:5.}->mkp)

        //let pntTr: (point, vector) => point
        assertEqPnt(
            {x:1., y:7.}->mkp->pntTr({begin:{x:-3., y:7.}, end:{x:4., y:-1.}}->mkv),
            {x:8., y:-1.}->mkp
        )

        //let pntTrDir: (point, vector, float) => point
        assertEqPnt(
            {x:3., y:2.}->mkp -> pntTrDir({begin:{x:100., y:-50.}, end:{x:104., y:-53.}}->mkv, 5.),
            {x:7., y:-1.}->mkp
        )

        //let pntMult: (point, float) => point
        assertEqPnt({x:-6., y:9.}->mkp -> pntMult(2.), {x:-12., y:18.}->mkp)

        //let pntDiv: (point, float) => point
        assertEqPnt({x:-12., y:18.}->mkp -> pntDiv(2.), {x:-6., y:9.}->mkp)

        //let pntVec: (point,point) => vector
        assertEqVec(
            {x:7., y:10.}->mkp -> pntVec({x:-7., y:100.}->mkp),
            {begin:{x:7., y:10.}, end:{x:-7., y:100.}}->mkv
        )

        //let pntRot: (point, angle) => point
        assertEqPnt(
            {x:Js.Math.sqrt(3.) /. 2., y: 0.5}->mkp -> pntRot(deg(-150.)),
            {x:-0.5, y:-.Js.Math.sqrt(3.) /. 2.}->mkp
        )
        
       let testVec = {begin:{x:3., y:4.}, end:{x:6., y:8.}}->mkv

        //let vecLen: vector => float
        assertEq(testVec -> vecLen, 5.)

        //let vecRev: vector => vector
        assertEqVec(testVec->vecRev, {begin:{x:3., y:4.}, end:{x:0., y:0.}}->mkv)

        //let vecMult: (vector, float) => vector
        assertEqVec(testVec->vecMult(3.), {begin:{x:9., y:12.}, end:{x:18., y:24.}}->mkv)

        //let vecMultVec: (vector, vector) => float
        assertEqNum(testVec->vecRot(deg(60.))->vecMultVec(testVec), 12.5)

        //let vecDiv: (vector, float) => vector
        assertEqVec(testVec->vecDiv(2.), {begin:{x:1.5, y:2.}, end:{x:3., y:4.}}->mkv)

        //let vecAdd: (vector, vector) => vector
        assertEqVec(
            testVec->vecAdd({begin:{x:3., y:4.}, end:{x:6., y:8.}}->mkv),
            {begin:{x:6., y:8.}, end:{x:12., y:16.}}->mkv
        )

        //let vecRot: (vector, angle) => vector
        assertEqVec(testVec->vecRot(deg(-90.)), {begin:{x:3., y:4.}, end:{x:7., y:1.}}->mkv)

        //let vecNorm: vector => vector
        assertEqVec(testVec->vecNorm, {begin:{x:3. /. 5., y:4. /. 5.}, end:{x:6. /. 5., y:8. /. 5.}}->mkv)

        //let vecSwapEnds: vector => vector
        assertEqVec(testVec->vecSwapEnds, {begin:{x:6., y:8.}, end:{x:3., y:4.}}->mkv)

        //let vecBeginAt: (vector, point) => vector
        assertEqVec(
            testVec->vecBeginAt({x:100., y: -30.}->mkp),
            {begin:{x:100., y: -30.}, end:{x:103., y:-26.}}->mkv
        )

        //let vecEndAt: (vector, point) => vector
        assertEqVec(testVec->vecEndAt({x:100., y: -30.}->mkp), {begin:{x:97., y: -34.}, end:{x:100., y: -30.}}->mkv)

        //let vecTr: (vector, vector) => vector
        assertEqVec(
            testVec->vecTr({begin:{x:-7., y: 11.}, end:{x:23., y: -1.}}->mkv),
            {begin:{x:33., y:-8.}, end:{x:36., y:-4.}}->mkv
        )

        //let vecTrDir: (vector, vector, float) => vector
        assertEqVec(testVec->vecTrDir(testVec, 5.), {begin:{x:6., y:8.}, end:{x:9., y:12.}}->mkv)
    })
})

describe("boundaries", _ => {
    it("works", _ => {
        //let bndFromPoints: array<point> => boundaries
        let b = bndFromPoints([mkp({x:4., y:-11.})])
        assertEq(b->bndMinX, 4.)
        assertEq(b->bndMaxX, 4.)
        assertEq(b->bndMinY, 11.)
        assertEq(b->bndMaxY, 11.)

        let b = bndFromPoints([mkp({x:4., y:-11.}), mkp({x:-14., y:110.})])
        assertEq(b->bndMinX, -14.)
        assertEq(b->bndMaxX, 4.)
        assertEq(b->bndMinY, -110.)
        assertEq(b->bndMaxY, 11.)

        //let bndAddPoint: (boundaries,point) => boundaries
        let b = bndFromPoints([mkp({x:4., y:-11.}), mkp({x:-14., y:110.})])
        assertEq(b->bndMinX, -14.)
        assertEq(b->bndMaxX, 4.)
        assertEq(b->bndMinY, -110.)
        assertEq(b->bndMaxY, 11.)
        let b = b->bndAddPoint(mkp({x:40., y:-30.}))
        assertEq(b->bndMinX, -14.)
        assertEq(b->bndMaxX, 40.)
        assertEq(b->bndMinY, -110.)
        assertEq(b->bndMaxY, 30.)

        //let bndAddPoints: (boundaries,array<point>) => boundaries
        let b = bndFromPoints([mkp({x:4., y:-11.}), mkp({x:-14., y:110.})])
        assertEq(b->bndMinX, -14.)
        assertEq(b->bndMaxX, 4.)
        assertEq(b->bndMinY, -110.)
        assertEq(b->bndMaxY, 11.)
        let b = b->bndAddPoints([mkp({x:-21., y:203.}), mkp({x:59., y:-72.})])
        assertEq(b->bndMinX, -21.)
        assertEq(b->bndMaxX, 59.)
        assertEq(b->bndMinY, -203.)
        assertEq(b->bndMaxY, 72.)

        //let bndMerge: (boundaries,boundaries) => boundaries
        let b1 = bndFromPoints([mkp({x:-1., y:4.})])
        assertEq(b1->bndMinX, -1.)
        assertEq(b1->bndMaxX, -1.)
        assertEq(b1->bndMinY, -4.)
        assertEq(b1->bndMaxY, -4.)
        let b2 = bndFromPoints([mkp({x:3., y:7.})])
        assertEq(b2->bndMinX, 3.)
        assertEq(b2->bndMaxX, 3.)
        assertEq(b2->bndMinY, -7.)
        assertEq(b2->bndMaxY, -7.)
        let b = b1->bndMerge(b2)
        assertEq(b->bndMinX, -1.)
        assertEq(b->bndMaxX, 3.)
        assertEq(b->bndMinY, -7.)
        assertEq(b->bndMaxY, -4.)

        //let bndMergeAll: array<boundaries> => boundaries
        let b1 = bndFromPoints([mkp({x:-1., y:4.})])
        assertEq(b1->bndMinX, -1.)
        assertEq(b1->bndMaxX, -1.)
        assertEq(b1->bndMinY, -4.)
        assertEq(b1->bndMaxY, -4.)
        let b1m = bndMergeAll([b1])
        assertEq(b1m->bndMinX, -1.)
        assertEq(b1m->bndMaxX, -1.)
        assertEq(b1m->bndMinY, -4.)
        assertEq(b1m->bndMaxY, -4.)
        let b2 = bndFromPoints([mkp({x:3., y:7.})])
        assertEq(b2->bndMinX, 3.)
        assertEq(b2->bndMaxX, 3.)
        assertEq(b2->bndMinY, -7.)
        assertEq(b2->bndMaxY, -7.)
        let b2m = bndMergeAll([b1,b2])
        assertEq(b2m->bndMinX, -1.)
        assertEq(b2m->bndMaxX, 3.)
        assertEq(b2m->bndMinY, -7.)
        assertEq(b2m->bndMaxY, -4.)
        let b3 = bndFromPoints([mkp({x:-4., y:2.})])
        assertEq(b3->bndMinX, -4.)
        assertEq(b3->bndMaxX, -4.)
        assertEq(b3->bndMinY, -2.)
        assertEq(b3->bndMaxY, -2.)
        let b3m = bndMergeAll([b1,b2,b3])
        assertEq(b3m->bndMinX, -4.)
        assertEq(b3m->bndMaxX, 3.)
        assertEq(b3m->bndMinY, -7.)
        assertEq(b3m->bndMaxY, -2.)

        //let bndIncludes: (boundaries, point) => bool
        let b = bndFromPoints([mkp({x:4., y:-11.}), mkp({x:-14., y:110.})])
        assertEq(b->bndMinX, -14.)
        assertEq(b->bndMaxX, 4.)
        assertEq(b->bndMinY, -110.)
        assertEq(b->bndMaxY, 11.)
        assertEq(b->bndIncludes(mkp({x:-14., y:110.})), true)
        assertEq(b->bndIncludes(mkp({x:4., y:11.})), false)
        assertEq(b->bndIncludes(mkp({x:3.999, y:10.999})), true)

        //let bndWidth: boundaries => float
        //let bndHeight: boundaries => float
        let b = bndFromPoints([mkp({x:4., y:-11.}), mkp({x:-14., y:110.})])
        assertEq(b->bndWidth, 18.)
        assertEq(b->bndHeight, 121.)
    })
})