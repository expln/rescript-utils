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
