open Expln_React_common

@react.component
let make = (
    ~gridRef:option<ReactDOM.domRef>=?,
    ~justifyContent:option<Expln_React_Grid.justifyContent>=?,
    ~alignItems:option<Expln_React_Grid.alignItems>=?,
    ~spacing:option<float>=?,
    ~style:option<reStyle>=?, 
    ~childXsOffset:option<int=>option<Js.Json.t>>=?,
    ~children:option<reElem>=?
) => {
    <Expln_React_Grid ref=?gridRef container=true direction=#row ?justifyContent ?alignItems ?spacing ?style >
        {switch children {
            | Some(children) => 
                children->React.Children.mapWithIndex((child,i) => {
                    <Expln_React_Grid 
                        xsOffset=?{ childXsOffset->Belt_Option.flatMap(childXsOffset => childXsOffset(i)) } 
                    >
                        child
                    </Expln_React_Grid>
                } )
            | None => React.null
        }}
    </Expln_React_Grid>
}