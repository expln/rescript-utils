open Expln_React_common

@react.component
let make = (
    ~gridRef:option<ReactDOM.domRef>=?,
    ~justifyContent:option<Expln_React_Grid.justifyContent>=?,
    ~alignItems:option<Expln_React_Grid.alignItems>=?,
    ~spacing:option<float>=?,
    ~style:option<reStyle>=?, 
    ~children:option<reElem>=?
) => {
    <Expln_React_Grid ref=?gridRef container=true direction=#row ?justifyContent ?alignItems ?spacing ?style >
        {switch children {
            | Some(ch) => 
                React.Children.map(ch, c => {
                    <Expln_React_Grid > c </Expln_React_Grid>
                } )
            | None => React.null
        }}
    </Expln_React_Grid>
}