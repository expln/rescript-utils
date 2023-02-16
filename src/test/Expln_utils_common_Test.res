open Expln_utils_common
let {describe,it,assertEq,fail} = module(Expln_test)

describe("arrJoin", _ => {
    it("should produce results as expected", _ => {
        assertEq( []->arrJoin(0), [])
        assertEq( [1]->arrJoin(0), [1])
        assertEq( [1,2]->arrJoin(0), [1, 0, 2])
        assertEq( [1,2,3]->arrJoin(0), [1, 0, 2, 0, 3])
    })
})

describe("comparatorBy", _ => {
    it("produces correct comparators", _ => {
        //given
        let cmp1 = comparatorBy(a => a[1])

        //when/then
        assertEq(-1, cmp1([1,30], [2,40]))
        assertEq(0, cmp1([1,40], [2,40]))
        assertEq(1, cmp1([1,50], [2,40]))
    })
})

describe("comparatorInverse", _ => {
    it("creates inverted comparator", _ => {
        //given
        let cmp1 = comparatorBy(a => a[1])->comparatorInverse

        //when/then
        assertEq(1, cmp1([1,30], [2,40]))
        assertEq(0, cmp1([1,40], [2,40]))
        assertEq(-1, cmp1([1,50], [2,40]))
    })
})

describe("comparatorAndThen", _ => {
    it("produces correct results when two comparators are combined", _ => {
        //given
        let cmp1 = (a,b) => a[0] - b[0]
        let cmp2 = (a,b) => a[1] - b[1]
        let cmp12 = cmp1->comparatorAndThen(cmp2)
        let cmp21 = cmp2->comparatorAndThen(cmp1)
        let arr = [
            [1,2],
            [3,1],
            [2,1],
            [3,2],
            [1,1],
            [2,2],
        ]

        //when
        let res12 = arr->Js.Array2.copy->Js.Array2.sortInPlaceWith(cmp12)

        //then
        assertEq(
            res12,
            [
                [1,1],
                [1,2],
                [2,1],
                [2,2],
                [3,1],
                [3,2],
            ]
        )

        //when
        let res21 = arr->Js.Array2.copy->Js.Array2.sortInPlaceWith(cmp21)

        //then
        assertEq(
            res21,
            [
                [1,1],
                [2,1],
                [3,1],
                [1,2],
                [2,2],
                [3,2],
            ]
        )
    })

    it("produces correct results when three comparators are combined", _ => {
        //given
        let cmp1 = (a,b) => a[0] - b[0]
        let cmp2 = (a,b) => a[1] - b[1]
        let cmp3 = (a,b) => a[2] - b[2]
        let cmp123 = cmp1->comparatorAndThen(cmp2)->comparatorAndThen(cmp3)
        let cmp321 = cmp3->comparatorAndThen(cmp2)->comparatorAndThen(cmp1)
        let arr = [
            [2,2,1],
            [1,2,1],
            [1,2,2],
            [2,1,1],
            [1,1,2],
            [3,1,2],
            [3,2,2],
            [2,1,2],
            [2,2,2],
            [3,1,1],
            [1,1,1],
            [3,2,1],
        ]

        //when
        let res123 = arr->Js.Array2.copy->Js.Array2.sortInPlaceWith(cmp123)

        //then
        assertEq(
            res123,
            [
                [1,1,1],
                [1,1,2],
                [1,2,1],
                [1,2,2],
                [2,1,1],
                [2,1,2],
                [2,2,1],
                [2,2,2],
                [3,1,1],
                [3,1,2],
                [3,2,1],
                [3,2,2],
            ]
        )

        //when
        let res321 = arr->Js.Array2.copy->Js.Array2.sortInPlaceWith(cmp321)

        //then
        assertEq(
            res321,
            [
                [1,1,1],
                [2,1,1],
                [3,1,1],
                [1,2,1],
                [2,2,1],
                [3,2,1],
                [1,1,2],
                [2,1,2],
                [3,1,2],
                [1,2,2],
                [2,2,2],
                [3,2,2],
            ]
        )
    })
})
