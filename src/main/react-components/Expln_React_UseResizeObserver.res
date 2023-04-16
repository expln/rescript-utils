@new external makeResizeObserver: (array<{..}> => unit) => {..} = "ResizeObserver"

let useClientSizeObserver = (ref:React.ref<Js.Nullable.t<Dom.element>>, onClientSizeChange:(int,int)=>unit) => {
    React.useEffect1(() => {
        switch ref.current->Js.Nullable.toOption {
            | Some(domElem) => {
                // Js.Console.log2("domElem", domElem)
                let observer = makeResizeObserver(mutations => {
                    // Js.Console.log2("mutations", mutations)
                    if (mutations->Js.Array2.length != 0) {
                        let target = mutations[mutations->Js.Array2.length - 1]["target"]
                        let clientWidth = target["clientWidth"]
                        let clientHeight = target["clientHeight"]
                        // Js.Console.log2("clientWidth", clientWidth)
                        // Js.Console.log2("clientHeight", clientHeight)
                        onClientSizeChange(clientWidth, clientHeight)
                    }
                })
                observer["observe"](. domElem)->ignore
                Some(() => observer["disconnect"](.))
            }
            | None => None
        }
    }, [ref.current])
}